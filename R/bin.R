
#' Create a Model Set for Binary Choice Models
#'
#' Use this function to create a binary choice model set and search for the best models (and other information) based on in-sample and out-of-sample evaluation metrics.
#'
#' @param data A list that determines data and other required information for the search process.
#' Use [get.data()] function to generate it from a \code{matrix} or a \code{data.frame}.
#' @param combinations A list that determines the combinations of the exogenous variables in the search process.
#' Use [get.combinations()] function to define it.
#' @param metrics A list of options for measuring performance. Use [get.search.metrics] function to get them.
#' @param modelChecks A list of options for excluding a subset of the model set. Use [get.search.modelchecks] function to get them.
#' @param items A list of options for specifying the purpose of the search. Use [get.search.items] function to get them.
#' @param options A list of extra options for performing the search. Use [get.search.options] function to get them.
#' @param costMatrices A list of numeric matrices where each one determines how to score the calculated probabilities.
#' Given the number of choices \code{n}, a frequency cost matrix is an \code{m x n+1} matrix.
#' The first column determines the thresholds.
#' Elements in the \code{j}-th column determine the costs corresponding to the \code{j-1}-th choice in \code{y}.
#' It can be \code{NULL} if it is not selected in \code{metrics}.
#' @param searchLogit If \code{TRUE}, logit regressions are added to the model set.
#' @param searchProbit If \code{TRUE}, probit regressions are added to the model set.
#' @param optimOptions A list for Newton optimization options.
#' Use [get.options.newton] function to get the options.
#' @param aucOptions A list for AUC calculation options.
#' Use [get.options.roc] function to get the options.
#'
#' @return A nested list with the following members:
#' \item{counts}{Information about the expected number of models, number of estimated models, failed estimations, and some details about the failures.}
#' \item{results}{A data frame with requested information in \code{items} list.}
#' \item{info}{The arguments and some general information about the search process such as the elapsed time.}
#'
#' Note that the output does not contain any estimation results, but minimum required data to estimate the models (Use \code{summary()} function to get the estimation).
#'
#' @export
#' @importFrom stats glm
#'
#' @example man-roxygen/ex-search.bin.R
#'
#' @seealso [estim.bin]
search.bin <- function(data,
                       combinations,
                       metrics = get.search.metrics(),
                       modelChecks = get.search.modelchecks(),
                       items = get.search.items(),
                       options = get.search.options(),
                       costMatrices = NULL,
                       searchLogit = TRUE,
                       searchProbit = FALSE,
                       optimOptions = get.options.newton(),
                       aucOptions = get.options.roc()){
  stopifnot(is.list(data))
  stopifnot(is.list(combinations))

  numChoices <- get.data.check.discrete(data, 1)
  if (numChoices != 2)
    stop("Invalid endogenous data. Use this function for binary data. Maximum value should be 1.")

  if (get.data.check.intercept(data$data) == -1)
    stop("Data for binary estimation must have an intercept. You can use 'addIntercept' argument in the 'get.data' function.")

  if (data$numEndo > 1)
    stop("Multivariate binary estimation is not implemented. Number of endogenous variables must be 1.")

  combinations <- get.indexation(combinations, data, FALSE) # it also check for inconsistencies, etc.

  # models must contain intercept
  if (length(combinations$partitions[[1]]) != 1 ||
      combinations$partitions[[1]] != 1){
    # remove intercept from other partitions
    combinations$partitions <- lapply(combinations$partitions, function(x) x[x != 1])
    # add intercept as a fixed partition
    combinations$partitions <- c(c(1), combinations$partitions)
    combinations$numFixPartitions <- combinations$numFixPartitions + 1

    warning("Partitions in combinations argument is modified. Models must have intercept and a fixed partition (i.e., 'c(1)') is added.")
  }
  else if (combinations$numFixPartitions == 0)
    combinations$numFixPartitions <- 1 # fix intercept without warning

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

  stopifnot(is.logical(searchLogit))
  stopifnot(is.logical(searchProbit))
  stopifnot(searchLogit || searchProbit)

  check.cost.matrices(costMatrices)

  if (is.null(optimOptions))
    optimOptions = get.options.newton()
  else
    stopifnot(is.list(optimOptions))

  if (is.null(aucOptions))
    aucOptions = get.options.roc()
  else
    stopifnot(is.list(aucOptions))

  if (is.list(combinations$sizes)){ # use steps
    # steps will re-call this function with modified combinations in which sizes is no longer a list
    res <- search.steps("bin", isInnerExogenous = FALSE, data = data, combinations = combinations,
                        metrics = metrics, modelChecks = modelChecks, items = items, options = options,
                        costMatrices = costMatrices, searchLogit = searchLogit,
                        searchProbit = searchProbit, optimOptions = optimOptions,
                        aucOptions = aucOptions)
    res
  }
  else {

    startTime <- Sys.time()
    res <- .SearchDc(data, combinations, metrics, modelChecks, items, options,
                     costMatrices, searchLogit, searchProbit,
                     optimOptions, aucOptions, numChoices)
    endTime <- Sys.time()

    res$info$data <- data
    res$info$combinations <- combinations
    res$info$metrics <- metrics
    res$info$options <- options
    res$info$modelChecks <- modelChecks
    res$info$items <- items
    res$info$startTime <- startTime
    res$info$endTime <- endTime

    res$info$costMatrices <- costMatrices
    res$info$searchLogit <- searchLogit
    res$info$searchProbit <- searchProbit
    res$info$optimOptions <- optimOptions
    res$info$aucOptions <- aucOptions

    class(res) <- c("ldt.search.bin", "ldt.search", "list")
    attr(res, "method") <- "Binary"

    res
  }
}


#' Estimate a Binary Choice Model
#'
#' Use this function to estimate a binary choice model.
#'
#' @param data A list that determines data and other required information for the model search process.
#' Use [get.data()] function to generate it from a \code{matrix} or a \code{data.frame}.
#' @param linkFunc A character string that shows the probability assumption. It can be \code{logit} or \code{probit}.
#' @param pcaOptionsX A list of options to use principal components of the \code{x}, instead of the actual values. Set \code{NULL} to disable. Use [get.options.pca()] for initialization.
#' @param costMatrices A list of numeric matrices where each one determines how to score the calculated probabilities. See and use [search.bin] for more information and initialization.
#' @param optimOptions A list for Newton optimization options. Use [get.options.newton] function to get the options.
#' @param aucOptions A list of options for AUC calculation. See and use \code{[get.options.roc()]} for more information and initialization.
#' @param simFixSize An integer that determines the number of out-of-sample simulations. Use zero to disable the simulation.
#' @param simTrainFixSize An integer representing the number of data points in the training sample in the out-of-sample simulation. If zero, \code{trainRatio} will be used.
#' @param simTrainRatio A number representing the size of the training sample relative to the available size, in the out-of-sample simulation. It is effective if \code{trainFixSize} is zero.
#' @param simSeed A seed for the random number generator. Use zero for a random value.
#' @param weightedEval If \code{TRUE}, weights will be used in evaluations.
#' @param simMaxConditionNumber A number for the maximum value for the condition number in the simulation.
#'
#' @details
#' As documented in chapter 12 in \insertCite{greene2010modeling;textual}{ldt}, binary regression is a statistical technique used to estimate the probability of one of two possible outcomes for a variable such as \eqn{y}, i.e., \eqn{p=P(y=1)} and \eqn{q=P(y=0)}. The most commonly used binary regression models are the logit and probit models. In general, a binary regression model can be written as \eqn{f(p) = z'\gamma+v}, where the first element in \eqn{\gamma} is the intercept and \eqn{f(p)} is a link function. For logit and probit models we have \eqn{f(p) = \ln{\frac{p}{1-p}}} and \eqn{f(p) = \Phi^{-1}(p)} respectively, where \eqn{\Phi^{-1}} is the inverse cumulative distribution function of the standard normal distribution.
#'
#' Given an independent sample of length \eqn{N}, the parameters of the binary regression model are estimated using maximum likelihood estimation. Assuming that some observations are more reliable or informative than others and \eqn{w_i} for \eqn{i=1,\ldots,N} reflects this fact, the likelihood function is given by:
#'
#' \deqn{
#' L(\gamma) = \prod_{i=1}^N (p_i)^{w_i y_i} (1-p_i)^{w_i (1-y_i)},
#' }
#'
#' where \eqn{p_i=\frac{\exp{\gamma z_i}}{1+\exp{\gamma z_i}}} for logit model and \eqn{p_i=\Phi(\gamma z_i)} for probit model. \code{ldt} uses feasible GLS to calculate the initial value of the coefficients and a weighted least squares estimator to calculate the initial variance matrix of the error terms (see page 781 in \insertCite{greene2020econometric;textual}{ldt}). The condition number of the estimation is calculated by multiplying 1-norm of the observed information matrix at the maximum likelihood estimator and its inverse (e.g., see page 94 in \insertCite{trefethen1997numerical;textual}{ldt}). Furthermore, if \eqn{x} is a new observations for the explanatory variables, the predicted probability of the positive class is estimated by \eqn{p_i=\frac{\exp{\gamma x}}{1+\exp{\gamma x}}} for logit model and \eqn{p_i=\Phi(\gamma x)} for probit model.
#'
#' Note that the focus in \code{ldt} is model uncertainty and the main purpose of exporting this method is to show the inner calculations of the search process in [search.bin] function.
#'
#' @references
#'   \insertAllCited{}
#' @importFrom Rdpack reprompt
#'
#' @export
#' @example man-roxygen/ex-estim.bin.R
#' @seealso [search.bin]
estim.bin <- function(data,
                      linkFunc = c("logit", "probit"),
                      pcaOptionsX = NULL,
                      costMatrices = NULL,
                      optimOptions = get.options.newton(),
                      aucOptions = get.options.roc(),
                      simFixSize = 0,
                      simTrainFixSize = 0,
                      simTrainRatio = 0.75,
                      simSeed = 0,
                      weightedEval = FALSE,
                      simMaxConditionNumber = Inf) {
  stopifnot(is.list(data))

  numChoices <- get.data.check.discrete(data,1)

  if (numChoices != 2)
    stop("Invalid endogenous data. Use this function for binary data. Maximum value should be 1.")

  if (get.data.check.intercept(data$data) == -1)
    stop("Data for binary estimation must have an intercept. You can use 'addIntercept' argument in the 'get.data' function.")

  if (data$numEndo > 1)
    stop("Multivariate binary estimation is not implemented. Number of endogenous variables must be 1.")

  stopifnot(is.character(linkFunc))
  linkFunc <- match.arg(as.character(linkFunc), c("logit", "probit"))

  if (!is.null(pcaOptionsX))
    stopifnot(is.list(pcaOptionsX))

  check.cost.matrices(costMatrices)


  #similar to SUR
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

  if (is.null(optimOptions))
    optimOptions = get.options.newton()
  else
    stopifnot(is.list(optimOptions))

  if (is.null(aucOptions))
    aucOptions = get.options.roc()
  else
    stopifnot(is.list(aucOptions))

  res <- .EstimDc(data, linkFunc,
                  pcaOptionsX, costMatrices,
                  optimOptions, aucOptions,
                  simFixSize, simTrainRatio,
                  simTrainFixSize, simSeed,
                  simMaxConditionNumber, numChoices, weightedEval)

  res$info$data = data
  res$info$linkFunc = linkFunc
  res$info$pcaOptionsX = pcaOptionsX
  res$info$costMatrices = costMatrices
  res$info$optimOptions = optimOptions
  res$info$aucOptions = aucOptions
  res$info$simFixSize = simFixSize
  res$info$simTrainFixSize = simTrainFixSize
  res$info$simTrainRatio = simTrainRatio
  res$info$simSeed = simSeed
  res$info$weightedEval = weightedEval
  res$info$simMaxConditionNumber = simMaxConditionNumber

  class(res) <- c("ldt.estim.bin", "ldt.estim", "list")
  attr(res, "method") <- "Binary"

  res
}

estim.binary.from.search <- function(searchResult, endogenous, exogenous, extra, ...){
  search_data <- searchResult$info$data
  weights <- NULL
  if (search_data$hasWeight){
    weights <- search_data$data[,search_data$numEndo+1]
    search_data$data <- search_data$data[,-c(which(colnames(search_data$data)=="(Weights)"))]
  }

  data <- get.data(search_data$data[,c(endogenous, exogenous), drop = FALSE],
                   endogenous = length(endogenous),
                   weights = weights,
                   lambdas = search_data$lambdas,
                   addIntercept = FALSE,...)


  estim.bin(
    data = data,

    linkFunc = ifelse(extra == 0, "logit", "probit"),
    pcaOptionsX = searchResult$info$pcaOptionsX,
    costMatrices = searchResult$info$costMatrices,
    optimOptions =  searchResult$info$optimOptions,
    aucOptions =  searchResult$info$aucOptions,
    simFixSize = searchResult$info$metrics$simFixSize,
    simTrainRatio = searchResult$info$metrics$trainRatio,
    simTrainFixSize = searchResult$info$metrics$trainFixSize,
    simSeed = abs(searchResult$info$metrics$seed),
    simMaxConditionNumber = searchResult$info$modelChecks$maxConditionNumber,
    weightedEval = searchResult$info$metrics$weightedEval
  )
}


#' Generate Random Sample from a DC Model
#'
#' This function generates a random sample from an discrete choice regression model.
#'
#' @param coef Either a single integer specifying the number of variables in the model,
#'   or a numeric vector of coefficients for the regression.
#' @param nObs The number of observations to generate.
#' @param probit Logical value indicating whether to generate data from a probit model
#'   (if \code{TRUE}) or a logit model (if \code{FALSE}).
#' @param maxWeight Integer value indicating the maximum weight of the observations.
#' If \code{1}, observations are not weighted.
#' If larger than \code{1}, a vector of weights is generated and included in the return list. The weights are drawn from a discrete uniform distribution with a maximum value determined by \code{maxWeight}.
#' If weighted, a larger sample is created (\code{nObs * sampleFactor * maxWeight}) and a subset of them is randomly selected, where the probability of selection is determined by the weight.
#' @param pPos The percentage of positive observations (\code{y=1}) in the endogenous variable y.
#'   Must be between 0 and 1.
#'   In the current implementation, this is independent of the weights, if \code{maxWeight} is larger than 1.
#' @param sampleFactor The factor used to control the size of the initial sample.
#'   A larger value generates a larger initial sample, which can increase the accuracy
#'   of the generated sample but also takes more time and memory.
#' @param toNumeric If \code{TRUE}, \code{y} and \code{w} are transformed to have numeric vector.
#' Otherwise, they contain an integer vector.
#'
#' @return A list with the following items:
#'   \item{y}{The endogenous variable.}
#'   \item{x}{The exogenous variables.}
#'   \item{w}{The weights of the observations. It is \code{NULL} if \code{weighted} is \code{FALSE}.}
#'   \item{p1}{Prob(Y=1)}
#'   \item{coef}{The coefficients of the regression.}
#'   \item{probit}{Logical value indicating whether data was generated from a probit model.}
#'   \item{pPos}{The percentage of negative observations in y.}
#'
#' @export
#' @importFrom stats pnorm rnorm rbinom
#' @example man-roxygen/ex-sim.bin.R
#'
#' @seealso [estim.bin], [search.bin]
sim.bin <- function(coef = 2L, nObs = 100, probit = FALSE,
                    maxWeight = 1, pPos = 0.5,
                    sampleFactor = 4, toNumeric = TRUE) {

  if (nObs <= 0)
    stop("nObs must be a positive integer")

  sampleFactor_orig <- sampleFactor
  if (maxWeight > 1)
    sampleFactor = sampleFactor * maxWeight

  probit <- as.logical(probit)

  pPos <- as.numeric(pPos)
  if (pPos < 0 || pPos > 1)
    stop("pPos must be between 0 and 1")

  sampleFactor = as.numeric(sampleFactor)
  if (sampleFactor < 1)
    stop("sampleFactor must be larger than 1")

  # Set the number of variables
  if (length(coef) == 1 && is.integer(coef)) {
    nVar <- coef
    coef <- rnorm(nVar)
  } else {
    nVar <- length(coef)
  }

  # Generate the exogenous variables
  if (nVar == 0) {
    x <- matrix(1, ncol = 1, nrow = nObs * sampleFactor)
    colnames(x) <- c("Intercept")
  } else {
    x <- cbind(1, matrix(rnorm(nObs * sampleFactor * (nVar - 1)), ncol = nVar - 1))
    colnames(x) <- c("Intercept", paste0("X", seq_len(nVar - 1)))
  }

  # Calculate the probability of choosing option 1
  if (probit) {
    p1 <- pnorm(x %*% coef)
  } else {
    p1 <- 1 / (1 + exp(-x %*% coef))
  }

  # Generate the endogenous variable
  y <- matrix(rbinom(n = nObs * sampleFactor, size = 1, prob = p1), ncol = 1)

  if (maxWeight > 1){

    w <- matrix(sample(1:maxWeight, size = nObs * sampleFactor, replace = TRUE), ncol = 1)

    # randomly select a subset the observations
    # observations with larger weight has larger probability of being selected
    idx <- sample(seq_len(nObs * sampleFactor), size = nObs * sampleFactor_orig,
                  prob = w, replace = FALSE)
    y <- y[idx, , drop = FALSE]
    x <- x[idx, , drop = FALSE]
    w <- w[idx, , drop = FALSE]
    p1 <- p1[idx, , drop = FALSE]
  }
  else{
    w <- NULL
  }

  # Remove observations until we have nPos positive observations
  nPos <- round(nObs * pPos)
  posToRemove <- sum(y) - nPos
  if (posToRemove > 0) {
    posIndexes <- sample(which(y == 1), posToRemove)
    y <- y[-posIndexes, , drop = FALSE]
    x <- x[-posIndexes, , drop = FALSE]
    w <- w[-posIndexes, , drop = FALSE]
    p1 <- p1[-posIndexes, , drop = FALSE]

  } else if (posToRemove < 0) {
    stop("Failed to generate enough positive observations. Try increasing the sampleFactor argument.")
  }

  # Remove excess negative observations
  negToRemove <- sum(!y) - (nObs - nPos)
  if (negToRemove > 0) {
    negIndexes <- sample(which(y == 0), negToRemove)
    y <- y[-negIndexes, , drop = FALSE]
    x <- x[-negIndexes, , drop = FALSE]
    w <- w[-negIndexes, , drop = FALSE]
    p1 <- p1[-negIndexes, , drop = FALSE]

  } else if (negToRemove < 0) {
    stop("Failed to generate enough negative observations. Try increasing the sampleFactor argument.")
  }

  if (toNumeric){
    y = matrix(as.numeric(y), ncol = 1)
    w = matrix(as.numeric(w), ncol = 1)
  }
  colnames(y) <- "Y"
  colnames(w) <- "W"
  p1 = matrix(p1, ncol = 1)
  colnames(p1) <- "P1"

  # Return the results as a list
  list(y = y, x = x, w = w, p1 = p1, coef = coef,
       probit = probit, pPos = pPos)
}

#' Get Model Name
#'
#' @param object A \code{estim.bin} object
#'
#' @return model string
estim.binary.model.string <- function(object){
  if (is.null(object))
    stop("argument is null.")
  if (!inherits(object, "ldt.estim.bin"))
    stop("Invalid class. An 'ldt.estim.bin' object is expected.")


  if (object$info$linkFunc == "logit")
    paste0("Logit Model")
  else if (object$info$linkFunc == "probit")
    paste0("Probit Model")
  else
    stop("Invalid binary model")
}

check.cost.matrices <- function(costMatrices){
  if (is.null(costMatrices) == FALSE){
    stopifnot(is.list(costMatrices))
    for (i in c(1:length(costMatrices))){
      stopifnot(is.matrix(costMatrices[[i]]))
      C <- costMatrices[[i]]
      if (nrow(C) <= 1)
        stop("Two or more rows is expected in the frequency cost matrix.")
      if (ncol(C) != 3)
        stop("Number of columns in frequency cost matrix must be 3, first column is for the bounds and the next two columns are for the choices.")
      if (any(C[,1]>1 | C[,1]<0))
        stop("Values in the first column of a frequency cost matrix must be in [0,1].")
      if (any(diff(C[,1]) <= 0))
        stop("Values in the first column of a frequency cost matrix must be strinctly increasing.")
    }
  }
}
