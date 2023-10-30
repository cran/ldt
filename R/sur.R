
#' Create a Model Set for SUR Models
#'
#' Use this function to create a Seemingly Unrelated Regression model set and search for the best models (and other information) based on in-sample and out-of-sample evaluation metrics.
#'
#' @param data A list that determines data and other required information for the search process.
#' Use [get.data()] function to generate it from a \code{matrix} or a \code{data.frame}.
#' @param combinations A list that determines the combinations of endogenous and exogenous variables in the search process.
#' Use [get.combinations()] function to define it.
#' @param metrics A list of options for measuring performance. Use [get.search.metrics] function to get them.
#' @param modelChecks A list of options for excluding a subset of the model set. Use [get.search.modelchecks] function to get them.
#' @param items A list of options for specifying the purpose of the search. Use [get.search.items] function to get them.
#' @param options A list of extra options for performing the search. Use [get.search.options] function to get them.
#' @param searchSigMaxIter Maximum number of iterations in searching for significant coefficients. Use 0 to disable the search.
#' @param searchSigMaxProb Maximum value of type I error to be used in searching for significant coefficients. If p-value is less than this, it is interpreted as significant.
#'
#' @return A nested list with the following members:
#' \item{counts}{Information about the expected number of models, number of estimated models, failed estimations, and some details about the failures.}
#' \item{results}{A data frame with requested information in \code{items} list.}
#' \item{info}{The arguments and some general information about the search process such as the elapsed time.}
#'
#' Note that the output does not contain any estimation results, but minimum required data to estimate the models (Use \code{summary()} function to get the estimation).
#'
#' @export
#'
#' @example man-roxygen/ex-search.sur.R
#'
#' @seealso [estim.sur]
search.sur <- function(data = get.data(),
                       combinations = get.combinations(),
                       metrics = get.search.metrics(),
                       modelChecks = get.search.modelchecks(),
                       items = get.search.items(),
                       options = get.search.options(),
                       searchSigMaxIter = 0,
                       searchSigMaxProb = 0.1){
  stopifnot(is.list(data))
  stopifnot(is.list(combinations))

  if (data$hasWeight)
    stop("SUR search does not support weighted observations.")

  combinations <- get.indexation(combinations, data, FALSE) # it also check for inconsistencies, etc.

  if (is.null(metrics))
    metrics = get.search.metrics()
  else
    stopifnot(is.list(metrics))
  metrics <- get.search.metrics.update(metrics, combinations$numTargets)

  if (is.null(modelChecks))
    modelChecks = get.search.modelchecks()
  else
    stopifnot(is.list(modelChecks))
  if (is.null(items))
    items = get.search.items()
  else
    stopifnot(is.list(items))
  if (is.null(options))
    options = get.search.options()
  else
    stopifnot(is.list(options))


  stopifnot(is.zero.or.positive.number(searchSigMaxIter))
  searchSigMaxIter <- as.integer(searchSigMaxIter)
  stopifnot(is.positive.number(searchSigMaxProb) && searchSigMaxProb < 1)
  if (searchSigMaxIter > 0 && searchSigMaxProb == 0)
    stop("searchSigMaxProb cannot be zero when search is enabled.")



  if (is.list(combinations$sizes)){ # use steps
    # steps will re-call this function with modified combinations in which sizes is no longer a list
    res <- search.steps("sur", isInnerExogenous = FALSE, data = data, combinations = combinations,
                        metrics = metrics, modelChecks = modelChecks, items = items, options = options,
                        searchSigMaxIter = searchSigMaxIter, searchSigMaxProb = searchSigMaxProb)
    res
  }
  else {

    startTime <- Sys.time()
    res <- .SearchSur(data, combinations, metrics, modelChecks, items, options,
                      searchSigMaxIter, searchSigMaxProb)
    endTime <- Sys.time()

    res$info$data <- data
    res$info$combinations <- combinations
    res$info$metrics <- metrics
    res$info$options <- options
    res$info$modelChecks <- modelChecks
    res$info$items <- items
    res$info$startTime <- startTime
    res$info$endTime <- endTime

    res$info$searchSigMaxIter <- searchSigMaxIter
    res$info$searchSigMaxProb <- searchSigMaxProb

    class(res) <- c("ldt.search.sur", "ldt.search", "list")
    attr(res, "method") <- "SUR"
    res
  }
}


#' Estimate a SUR Model
#'
#' Use this function to estimate a Seemingly Unrelated Regression model.
#'
#' @param data A list that determines data and other required information for the search process.
#' Use [get.data()] function to generate it from a \code{matrix} or a \code{data.frame}.
#' @param searchSigMaxIter An integer for the maximum number of iterations in searching for significant coefficients. Use 0 to disable the search.
#' @param searchSigMaxProb A number for the maximum value of type I error to be used in searching for significant coefficients. If p-value is less than this, it is interpreted as significant.
#' @param restriction A \code{km x q} matrix of restrictions where \code{m} is the number of endogenous data, \code{k} is the number of exogenous data, and \code{q} is the number of unrestricted coefficients.
#' @param pcaOptionsY A list of options to use principal components of the endogenous data, instead of the actual values. Set \code{NULL} to disable. Use [get.options.pca()] for initialization.
#' @param pcaOptionsX A list of options to use principal components of the exogenous data, instead of the actual values. Set \code{NULL} to disable. Use [get.options.pca()] for initialization.
#' @param simFixSize An integer that determines the number of out-of-sample simulations. Use zero to disable the simulation.
#' @param simTrainFixSize An integer representing the number of data points in the training sample in the out-of-sample simulation. If zero, \code{trainRatio} will be used.
#' @param simTrainRatio A number representing the size of the training sample relative to the available size, in the out-of-sample simulation. It is effective if \code{trainFixSize} is zero.
#' @param simSeed A seed for the random number generator. Use zero for a random value.
#' @param simMaxConditionNumber A number for the maximum value for the condition number in the simulation.
#'
#' @details
#' As described in section 10.2 in \insertCite{greene2020econometric;textual}{ldt}, this type of statistical model consists of multiple regression equations, where each equation may have a different set of exogenous variables and the disturbances between the equations are assumed to be correlated. The general form with \eqn{m} equations can be written as \eqn{y_i=z_i'\gamma_i+v_i} and \eqn{E(v_i v_j)=\sigma_{ij}^2} for \eqn{i=1,\ldots m}. Assuming that a sample of \eqn{N} independent observations is available, we can stack the observations and use the following system for estimation:
#' \deqn{
#' Y = X B + V, \quad \mathrm{vec}B = R\gamma,
#' }
#'
#' where the columns of \eqn{Y:N \times m} contain the endogenous variables for each equation and the columns of \eqn{X: N\times k} contain the explanatory variables, with \eqn{k} being the number of unique explanatory variables in all equations. Note that \eqn{X} combines the \eqn{z_i} variables, and the restrictions imposed by \eqn{R:mk\times q} and \eqn{\gamma:q\times 1} determine a set of zero constraints on \eqn{B: k \times m}, resulting in a system of equations with different sets of exogenous variables.
#'
#' Imposing restrictions on the model using the \eqn{R} matrix is not user-friendly, but it is suitable for use in this package, as users are not expected to specify such restrictions, but only to provide a list of potential regressors. Note that in this package, most procedures, including significance search, are supposed to be automated.
#'
#' The unrestricted estimators (i.e., \eqn{\hat{B}=(X'X)^{-1}X'Y}, and \eqn{\hat{\Sigma}=(\hat{V}'\hat{V})/N} where \eqn{\hat{V}=Y-X\hat{B}}) are used to initialize the feasible GLS estimators:
#'
#' \deqn{
#'     \tilde{B} = RW^{-1}R'[\hat{V}-1 \otimes x']\mathrm{vec}Y, \quad \tilde{\Sigma}=(\tilde{V}'\tilde{V})/N,
#' }
#' where \eqn{W = R'[\hat{V}^{-1} \otimes X'X]R} and \eqn{\tilde{V}=Y-X\tilde{B}}. The properties of these estimators are discussed in proposition 5.3 in \insertCite{lutkepohl2005new;textual}{ldt}. See also section 10.2 in \insertCite{greene2020econometric;textual}{ldt}. The maximum likelihood value is calculated by \eqn{-\frac{N}{2}(m(\ln 2\pi+1)+\ln|\tilde{\Sigma}|)}. The condition number is calculated by multiplying 1-norm of \eqn{W} and its inverse (e.g., see page 94 in \insertCite{trefethen1997numerical;textual}{ldt}). Furthermore, given an out-of-sample observation such as \eqn{x:k\times 1}, the prediction is \eqn{y^f = \tilde{B}'x}, and its variance is estimated by the following formula:
#'
#' \deqn{
#'      \mathrm{var}y^f = \tilde{V} + (x' \otimes I_m)R W^{-1}R'(x \otimes I_m).
#' }
#'
#'
#' Note that the focus in \code{ldt} is model uncertainty and for more sophisticated implementations of the FGLS estimator, you may consider using other packages such as \code{systemfit}.
#'
#' Finally, note that the main purpose of exporting this method is to show the inner calculations of the search process in [search.sur] function.
#'
#' @references
#'   \insertAllCited{}
#' @importFrom Rdpack reprompt
#'
#' @export
#' @example man-roxygen/ex-estim.sur.R
#'
#' @seealso [search.sur]
estim.sur <- function(data, searchSigMaxIter = 0,
                      searchSigMaxProb = 0.1,
                      restriction = NULL,
                      pcaOptionsY = NULL,
                      pcaOptionsX = NULL,
                      simFixSize = 0,
                      simTrainFixSize = 0,
                      simTrainRatio = 0.75,
                      simSeed = 0,
                      simMaxConditionNumber = Inf){
  stopifnot(is.list(data))

  if (data$hasWeight)
    stop("SUR estimation does not support weighted observations.")

  data <- get.data.keep.complete(data)
  stopifnot(is.zero.or.positive.number(searchSigMaxIter))
  searchSigMaxIter <- as.integer(searchSigMaxIter)
  stopifnot(is.positive.number(searchSigMaxProb) && searchSigMaxProb < 1)
  if (searchSigMaxIter > 0 && searchSigMaxProb == 0)
    stop("searchSigMaxProb cannot be zero when search is enabled.")
  if (searchSigMaxIter > 0 && !is.null(restriction))
    stop("restriction must be null when  significant search is enabled.")

  if (!is.null(restriction))
    stopifnot(is.matrix(restriction))

  stopifnot(is.zero.or.positive.number(simFixSize))
  simFixSize <- as.integer(simFixSize)
  stopifnot(is.zero.or.positive.number(simTrainRatio) && simTrainRatio <= 1)
  stopifnot(is.zero.or.positive.number(simTrainFixSize))
  simTrainFixSize <- as.integer(simTrainFixSize)
  stopifnot(is.zero.or.positive.number(simSeed))
  simSeed <- as.integer(simSeed)
  if (simSeed == 0)
    simSeed = runif(1,10,10e4) # set it here such that it is reported in the info section
  stopifnot(is.number(simMaxConditionNumber))

  if (!is.null(pcaOptionsX))
    stopifnot(is.list(pcaOptionsX))
  if (!is.null(pcaOptionsY))
    stopifnot(is.list(pcaOptionsY))

  res <- .EstimSur(data, searchSigMaxIter, searchSigMaxProb,
                   restriction, pcaOptionsY, pcaOptionsX,
                   simFixSize, simTrainRatio,
                   simTrainFixSize, simSeed,
                   simMaxConditionNumber)


  res$info$data <- data
  res$info$searchSigMaxIter <- searchSigMaxIter
  res$info$searchSigMaxProb <- searchSigMaxProb
  res$info$pcaOptionsY <- pcaOptionsY
  res$info$pcaOptionsX <- pcaOptionsX
  res$info$simFixSize <- simFixSize
  res$info$simTrainFixSize <- simTrainFixSize
  res$info$simTrainRatio <- simTrainRatio
  res$info$simSeed <- simSeed
  res$info$simMaxConditionNumber <- simMaxConditionNumber

  class(res) <- c("ldt.estim.sur", "ldt.estim", "list")
  attr(res, "method") <- "SUR"
  res
}


estim.sur.from.search <- function(searchResult, endogenous, exogenous, extra, ...){
  search_data <- searchResult$info$data
  data <- get.data(search_data$data[,c(endogenous, exogenous), drop = FALSE],
                   endogenous = length(endogenous),
                   weights = NULL,
                   lambdas = search_data$lambdas,
                   addIntercept = FALSE,...)

  estim.sur(
    data = data,
    searchSigMaxIter = searchResult$info$searchSigMaxIter,
    searchSigMaxProb = searchResult$info$searchSigMaxProb,
    simFixSize = searchResult$info$metrics$simFixSize,
    simTrainRatio = searchResult$info$metrics$trainRatio,
    simTrainFixSize = searchResult$info$metrics$trainFixSize,
    simSeed = abs(searchResult$info$metrics$seed),
    simMaxConditionNumber = searchResult$info$modelChecks$maxConditionNumber
  )
}


estim.sur.model.string <- function(obj){
  if (is.null(obj))
    stop("argument is null.")
  if (!inherits(obj, "ldt.estim.sur"))
    stop("Invalid class. An 'ldt.estim.sur' object is expected.")

  y <- obj$info$data$data[,1:obj$info$data$numEndo, drop = FALSE]
  nms <- paste(colnames(y), collapse = ", ")
  if (ncol(y) == 1)
    paste0("Linear Model")
  if (sum(obj$estimations$isRestricted) == 0)
      paste0("Unrestricted SUR: ", nms)
  else
    paste0("SUR: ", nms)
}

#' Generate Random Sample from an SUR Model
#'
#' This function generates a random sample from an Seemingly Unrelated Regression model.
#'
#' @param sigma covariance matrix of the errors.
#' If it is an integer value, it specifies the number of equations in the SUR model and covariance matrix is generated randomly.
#' @param coef Coefficients of the model.
#' If it is an integer value, it specifies the number of exogenous variables in each equation of the SUR model and coefficient matrix is generated randomly.
#' @param nObs Number of observations to generate.
#' @param intercept If \code{TRUE}, an intercept is included in the model as the first exogenous variable.
#'
#' @return A list with the following items:
#'   \item{y}{matrix, the generated endogenous variable(s).}
#'   \item{x}{matrix, the generated exogenous variable(s).}
#'   \item{e}{matrix, the generated errors.}
#'   \item{sigma}{matrix, the covariance matrix of the disturbances.}
#'   \item{coef}{matrix, the coefficients used in the model.}
#'   \item{intercept}{logical, whether an intercept was included in the model.}
#'
#' @export
#' @importFrom stats rnorm
#' @example man-roxygen/ex-sim.sur.R
#' @seealso [sim.varma],[estim.sur],[search.sur]
sim.sur <- function(sigma = 1L, coef = 1L,
                    nObs = 100, intercept = TRUE) {

  nObs = as.integer(nObs)
  if (nObs <= 0)
    stop("nObs must be a positive integer.")

  intercept = as.logical(intercept)

  if (is.null(coef)) {
    stop("coef must be provided")
  }

  if (is.integer(coef) && length(coef) == 1) {
    num_x <- coef - ifelse(intercept, 1, 0)
    if (is.integer(sigma) && length(sigma) == 1) {
      num_y <- sigma
    } else {
      num_y <- nrow(sigma)
    }
    coef <- matrix(rnorm((num_x + ifelse(intercept, 1, 0)) * num_y), ncol = num_y)
  } else {
    num_y <- ncol(coef)
    num_x <- nrow(coef) - ifelse(intercept, 1, 0)
  }

  if (is.null(sigma)) {
    stop("sigma must be provided")
  }

  if (is.integer(sigma) && length(sigma) == 1) {
    sigma <- crossprod(matrix(rnorm(num_y^2), ncol = num_y))
  } else {
    if (!is.matrix(sigma) || nrow(sigma) != ncol(sigma)) {
      stop("sigma must be a square matrix")
    }
    if (nrow(sigma) != num_y) {
      stop("The number of rows and columns in sigma must be equal to num_y")
    }
  }

  if (num_x == 0){
    if (intercept)
      x <- matrix(rep(1,nObs), ncol = 1)
    else
      x <- matrix()
  }
  else{
    x <- matrix(rnorm(nObs * num_x), ncol = num_x)
    if (intercept){
      x <- cbind(rep(1,nObs),x)
    }
  }

  errors <- rand.mnormal(nObs, mu = rep(0, num_y), sigma = sigma)
  e <- errors$sample
  if (length(x) != 0)
    y <- x %*% coef + e
  else
    y <- e

  colnames(y) <- paste0("Y",c(1:ncol(y)))
  if (num_x == 0){
    if (intercept)
      colnames(x) <- c("Intercept")
  }
  else {
    if (intercept)
      colnames(x) <- c("Intercept", paste0("X",c(1:num_x)))
    else
      colnames(x) <- paste0("X",c(1:num_x))
  }
  colnames(e) <- paste0("E",c(1:ncol(e)))

  colnames(sigma) <- colnames(y)
  rownames(sigma) <- colnames(y)
  rownames(coef) <- colnames(x)
  colnames(coef) <- colnames(y)

  result <- list(y = y, x = x, e = e,
                 sigma = sigma, coef = coef,
                 intercept = intercept)

  return(result)
}

