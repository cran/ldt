
#' Create Model Set for VARMA Models
#'
#' Use this function to create a Vector Autoregressive Moving Average model set and search for the best models (and other information) based on in-sample and out-of-sample evaluation metrics.
#'
#' @param data A list that determines data and other required information for the search process.
#' Use [get.data()] function to generate it from a \code{matrix} or a \code{data.frame}.
#' @param combinations A list that determines the combinations of endogenous and exogenous variables in the search process.
#' Use [get.combinations()] function to define it.
#' @param metrics A list of options for measuring performance. Use [get.search.metrics] function to get them.
#' @param modelChecks A list of options for excluding a subset of the model set. Use [get.search.modelchecks] function to get them.
#' @param items A list of options for specifying the purpose of the search. Use [get.search.items] function to get them.
#' @param options A list of extra options for performing the search. Use [get.search.options] function to get them.
#' @param maxParams An integer vector that determines the maximum values for the parameters of the VARMA model: \code{(p,d,q,P,D,Q)}. If \code{NULL}, \code{c(2,0,0,0,0,0)} is used.
#' @param seasonsCount An integer value representing the number of observations per unit of time.
#' @param maxHorizon An integer value representing the maximum value for the prediction horizon if \code{type1} is \code{TRUE} in the \code{modelChecks} argument. Also, it is used as the maximum prediction horizon in checking predictions.
#' @param simUsePreviousEstim If \code{TRUE}, parameters are initialized only in the first step of the simulation. The initial values of the n-th simulation (with one more observation) are the estimations from the previous step.
#' @param olsStdMultiplier A number used as a multiplier for the standard deviation of OLS, used for restricting maximum likelihood estimation.
#' @param lbfgsOptions A list containing L-BFGS optimization options. Use [get.options.lbfgs] function for initialization.
#'
#' @return A nested list with the following members:
#' \item{counts}{Information about the expected number of models, number of estimated models, failed estimations, and some details about the failures.}
#' \item{results}{A data frame with requested information in \code{items} list.}
#' \item{info}{The arguments and some general information about the search process such as the elapsed time.}
#'
#' Note that the output does not contain any estimation results, but minimum required data to estimate the models (Use \code{summary()} function to get the estimation).
#'
#' @export
#' @example man-roxygen/ex-search.varma.R
#' @seealso [estim.varma]
search.varma <- function(data = get.data(),
                         combinations = get.combinations(),
                         metrics = get.search.metrics(),
                         modelChecks = get.search.modelchecks(),
                         items = get.search.items(),
                         options = get.search.options(),
                         maxParams = c(1,0,0,0,0,0),
                         seasonsCount = 0,
                         maxHorizon = 1,
                         simUsePreviousEstim = FALSE,
                         olsStdMultiplier = 2.0,
                         lbfgsOptions = get.options.lbfgs()){
  stopifnot(is.list(data))
  stopifnot(is.list(combinations))
  stopifnot(maxHorizon >= 0)

  if (data$hasWeight)
    stop("VARMA search does not support weighted observations.")

  if (is.null(modelChecks))
    modelChecks = get.search.modelchecks()
  else
    stopifnot(is.list(modelChecks))

  modelChecks <- update.search.modelchecks(data$data, combinations$numTargets, maxHorizon, modelChecks)

  data <- get.data.append.newX(data, maxHorizon = maxHorizon)

  combinations <- get.indexation(combinations, data, TRUE) # it also check for inconsistencies, etc.

  if (is.null(metrics))
    metrics = get.search.metrics()
  else
    stopifnot(is.list(metrics))
  metrics <- get.search.metrics.update(metrics, combinations$numTargets)

  if (length(metrics$typesOut) != 0 && maxHorizon != max(metrics$horizons))
    warning("'maxHorizon' argument is different from the maximum horizon in the 'metrics' argument.")

  if (is.null(items))
    items = get.search.items()
  else
    stopifnot(is.list(items))
  if (is.null(options))
    options = get.search.options()
  else
    stopifnot(is.list(options))

  stopifnot(is.numeric(maxParams))
  stopifnot(is.zero.or.positive.number(seasonsCount))
  seasonsCount <- as.integer(seasonsCount)

  if (length(maxParams) < 6)
    maxParams <- c(maxParams, rep(0, 6 - length(maxParams)))
  maxParams <- as.integer(maxParams)
  stopifnot(any(maxParams[c(1,3,4,6)] != 0))
  stopifnot(all(maxParams >= 0)) #handles NA case

  if (seasonsCount < 2 && any(maxParams[4:6] != 0))
    stop("Invalid 'maxParams' argument. Seasonal part (at indices 4:6) must be zero for non-seasonal model.")

  stopifnot(is.zero.or.positive.number(maxHorizon))
  maxHorizon <- as.integer(maxHorizon)
  if (items$type1 && maxHorizon == 0)
    stop("If 'items$type1' is TRUE, 'maxHorizon' argument cannot be zero.")
  if (maxHorizon > 0)
    modelChecks$prediction = TRUE # should I warn?

  stopifnot(is.logical(simUsePreviousEstim) && length(simUsePreviousEstim) == 1)
  stopifnot(is.positive.number(olsStdMultiplier))

  if (is.null(lbfgsOptions))
    lbfgsOptions = get.options.lbfgs()
  else
    stopifnot(is.list(lbfgsOptions))


  if (is.list(combinations$sizes)){ # use steps
    # steps will re-call this function with modified combinations in which sizes is no longer a list
    res <- search.steps("varma", isInnerExogenous = TRUE, data = data, combinations = combinations,
                        metrics = metrics, modelChecks = modelChecks, items = items, options = options,
                        maxParams = maxParams,
                        seasonsCount = seasonsCount,
                        maxHorizon = maxHorizon,
                        simUsePreviousEstim = simUsePreviousEstim,
                        olsStdMultiplier = olsStdMultiplier,
                        lbfgsOptions = lbfgsOptions)
    res
  }
  else {
    startTime <- Sys.time()
    res <- .SearchVarma(data, combinations, metrics, modelChecks, items, options,
                        maxParams,
                        seasonsCount, maxHorizon,
                        simUsePreviousEstim,
                        olsStdMultiplier, lbfgsOptions)
    endTime <- Sys.time()

    res$info$data <- data
    res$info$combinations <- combinations
    res$info$metrics <- metrics
    res$info$options <- options
    res$info$modelChecks <- modelChecks
    res$info$items <- items
    res$info$startTime <- startTime
    res$info$endTime <- endTime

    res$info$maxParams = maxParams
    res$info$seasonsCount = seasonsCount
    res$info$maxHorizon = maxHorizon
    res$info$simUsePreviousEstim = simUsePreviousEstim
    res$info$olsStdMultiplier = olsStdMultiplier
    res$info$lbfgsOptions = lbfgsOptions

    #res$info$startFrequency <- attr(y, "ldtf") ???? move to data

    class(res) <- c("ldt.search.varma", "ldt.search", "list")
    attr(res, "method") <- "VARMA"
    res
  }
}

#' Estimate a VARMA Model
#'
#' Use this function to estimate a Vector Autoregressive Moving Average model.
#'
#' @param data A list that determines data and other required information for the search process.
#' Use [get.data()] function to generate it from a \code{matrix} or a \code{data.frame}.
#' @param params An integer vector that determines the values for the parameters of the VARMA model: \code{(p,d,q,P,D,Q)}. If \code{NULL}, \code{c(1,0,0,0,0,0)} is used.
#' @param seasonsCount An integer value representing the number of observations per unit of time.
#' @param lbfgsOptions A list containing L-BFGS optimization options. Use [get.options.lbfgs] function for initialization.
#' @param olsStdMultiplier A number used as a multiplier for the standard deviation of OLS, used for restricting maximum likelihood estimation.
#' @param pcaOptionsY A list of options to use principal components of \code{y}, instead of the actual values. Set to \code{NULL} to disable. Use [get.options.pca()] for initialization.
#' @param pcaOptionsX A list of options to use principal components of \code{x}, instead of the actual values. Set to \code{NULL} to disable. Use [get.options.pca()] for initialization.
#' @param maxHorizon An integer representing the maximum prediction horizon. Set to zero to disable prediction.
#' @param simFixSize An integer that determines the number of out-of-sample simulations. Use zero to disable simulation.
#' @param simHorizons An integer vector representing the prediction horizons to be used in out-of-sample simulations. See also [get.search.metrics()].
#' @param simUsePreviousEstim If \code{TRUE}, parameters are initialized only in the first step of the simulation. The initial values of the n-th simulation (with one more observation) are the estimations from the previous step.
#' @param simMaxConditionNumber A number representing the maximum value for the condition number in simulation.
#'
#' @return A nested list with the following items:
#' \item{counts}{Information about different aspects of the estimation such as the number of observation, number of exogenous variables, etc.}
#' \item{estimations}{Estimated coefficients, standard errors, z-statistics, p-values, etc.}
#' \item{metrics}{Value of different goodness of fit and out-of-sample performance metrics. }
#' \item{prediction}{Information on the predicted values.}
#' \item{simulation}{Information on the simulations. }
#' \item{info}{Some other general information.}
#'
#' @details
#' The VARMA model can be used to analyze multivariate time series data with seasonal or non-seasonal patterns. According to \insertCite{lutkepohl2005new;textual}{ldt}, it considers interdependencies between the series, making it a powerful tool for prediction. The specification of this model is given by:
#' \deqn{
#' \Delta^d \Delta_s^D y_t = c + \sum_{i=1}^p A_i y_{t-i} + \sum_{i=1}^q B_i \epsilon_{t-i} + C x_t + \sum_{i=1}^P A_{is} y_{t-is} + \sum_{i=1}^Q B_{is} v_{t-is} + v_t,
#' }
#' where \eqn{y_t:m\times 1} is the vector of endogenous variables, \eqn{x_t:k\times 1} is the vector exogenous variables, \eqn{s} is the number of seasons and \eqn{(p,d,q,P,D,Q)} determines the lag structure of the model. Furthermore, \eqn{c,C,A_i} and \eqn{B_i} for all available \eqn{i} determines the model’s parameters. \eqn{v_t} is the disturbance vector and is contemporaneously correlated between different equations, i.e., \eqn{E(v_tv_t')=\Sigma}.
#' Given a sample of size \eqn{T}, the model can be estimated using maximum likelihood estimation. However, to ensure identifiability, it is necessary to impose additional constraints on the parameters (see chapter 12 in \insertCite{lutkepohl2005new;textual}{ldt}). In this function, diagonal MA equation form is used (see \insertCite{dufour2022practical;textual}{ldt}).
#' In this function, the feasible GLS estimator is used to initialize the maximum likelihood, and the OLS estimator is used to calculate the initial value of the variance matrix of the error term. The condition number is calculated similar to the other models (see [estim.sur] or e.g., page 94 in \insertCite{trefethen1997numerical;textual}{ldt}). Furthermore, given a prediction horizon and required exogenous data, prediction is performed in a recursive schema, in which the actual estimated errors are used if available and zero otherwise. The variance of the predictions is also calculated recursively. Note that this function does not incorporate the coefficients uncertainty in calculation of the variance (see section 3.5 in \insertCite{lutkepohl2005new;textual}{ldt}).
#'
#' Finally, note that the main purpose of exporting this method is to show the inner calculations of the search process in [search.varma] function.
#'
#' @references
#'   \insertAllCited{}
#' @importFrom Rdpack reprompt
#' @export
#' @example man-roxygen/ex-estim.varma.R
#' @seealso [search.varma]
estim.varma <- function(data,
                        params = NULL,
                        seasonsCount = 0,
                        lbfgsOptions = get.options.lbfgs(),
                        olsStdMultiplier = 2,
                        pcaOptionsY = NULL,
                        pcaOptionsX = NULL,
                        maxHorizon = 0,
                        simFixSize = 0,
                        simHorizons = NULL,
                        simUsePreviousEstim = FALSE,
                        simMaxConditionNumber = Inf){
  stopifnot(is.list(data))

  if (data$hasWeight)
    stop("VARMA estimation does not support weighted observations.")

  data <- get.data.append.newX(data, maxHorizon = maxHorizon)


  stopifnot(is.numeric(params) && length(params) <= 6)
  if (length(params) < 6)
    params <- c(params, rep(0, 6 - length(params)))
  params <- as.integer(params)
  stopifnot(all(params>=0)) #handles NA case

  stopifnot(is.zero.or.positive.number(seasonsCount))
  seasonsCount = as.integer(seasonsCount)
  if (seasonsCount < 2 && any(params[4:6] != 0))
    stop("Invalid 'params' argument. Seasonal part (at indices 4:6) must be zero for non-seasonal model.")

  stopifnot(is.zero.or.positive.number(maxHorizon))
  maxHorizon <- as.integer(maxHorizon)
  stopifnot(is.logical(simUsePreviousEstim) && length(simUsePreviousEstim) == 1)
  stopifnot(is.positive.number(olsStdMultiplier))

  stopifnot(is.zero.or.positive.number(simFixSize))
  simFixSize <- as.integer(simFixSize)
  stopifnot(is.number(simMaxConditionNumber))
  if (!is.null(simHorizons)){
    stopifnot(is.numeric(simHorizons) && all(simHorizons >= 1))
    simHorizons <- as.integer(simHorizons)
  }

  if (is.null(lbfgsOptions))
    lbfgsOptions = get.options.lbfgs()
  else
    stopifnot(is.list(lbfgsOptions))

  if (!is.null(pcaOptionsX))
    stopifnot(is.list(pcaOptionsX))
  if (!is.null(pcaOptionsY))
    stopifnot(is.list(pcaOptionsY))

  res <- .EstimVarma(data, params, seasonsCount,
                     lbfgsOptions, olsStdMultiplier,
                     pcaOptionsY, pcaOptionsX,
                     maxHorizon, simFixSize, simHorizons,
                     simUsePreviousEstim, simMaxConditionNumber)

  res$info$data = data
  res$info$params = params
  res$info$seasonsCount = seasonsCount
  res$info$lbfgsOptions = lbfgsOptions
  res$info$olsStdMultiplier = olsStdMultiplier
  res$info$pcaOptionsY = pcaOptionsY
  res$info$pcaOptionsX = pcaOptionsX
  res$info$maxHorizon = maxHorizon
  res$info$simFixSize = simFixSize
  res$info$simHorizons = simHorizons
  res$info$simUsePreviousEstim = simUsePreviousEstim
  res$info$ simMaxConditionNumber = simMaxConditionNumber

  class(res) <- c("ldt.estim.varma", "ldt.estim", "list")
  attr(res, "method") <- "VARMA"

  res
}

estim.varma.from.search <- function(searchResult, endogenous, exogenous, extra, ...){
  search_data <- searchResult$info$data
  data <- get.data(search_data$data[,c(endogenous, exogenous), drop = FALSE],
                   endogenous = length(endogenous),
                   newData = searchResult$info$data$newX,
                   weights = NULL,
                   lambdas = NULL, #don't transform again
                   addIntercept = FALSE,...)
  attr(data, "ldt.new.appended") <- attr(search_data, "ldt.new.appended")
  data$lambdas <- search_data$lambdas #correct value

  estim.varma(
    data = data,
    params = extra,
    seasonsCount = searchResult$info$seasonsCount,
    lbfgsOptions = searchResult$info$lbfgsOptions,
    olsStdMultiplier = searchResult$info$olsStdMultiplier,
    pcaOptionsY = NULL,
    pcaOptionsX = NULL,
    maxHorizon = searchResult$info$maxHorizon,
    simFixSize = searchResult$info$metrics$simFixSize,
    simHorizons = searchResult$info$metrics$horizons,
    simUsePreviousEstim = searchResult$info$simUsePreviousEstim,
    simMaxConditionNumber = searchResult$info$modelChecks$maxConditionNumber
  )
}

#' Get the Specification of an \code{ldt.estim.varma} Model
#'
#' Use this function to get the name of a VARMA model, such that:
#' If It is multivariate, it will be VAR, otherwise AR;
#' If moving average terms are present, it will be ARMA or VARMA;
#' If it is seasonal, it will be S-ARMA or S-VARMA;
#' If it is integrated, it will be S-ARMA (D=?,d=?); ..., and any possible combination.
#' Parameters will be reported in parenthesis after the name of the model.
#'
#' @param obj AN object of class \code{ldt.estim.varma}.
#'
#' @return A character string representing the specification of the model.
#' @export
estim.varma.model.string <- function(obj){
  if (is.null(obj))
    stop("argument is null.")
  if (!inherits(obj, "ldt.estim.varma"))
    stop("Invalid class. An 'ldt.estim.varma' object is expected.")

  y <- obj$info$data$data[,1:obj$info$data$numEndo, drop = FALSE]
  nms <- paste(colnames(y), collapse = ", ")
  params <- obj$info$params[1:6]

  ar <- params[1]
  d <- params[2]
  ma <- params[3]
  sar <- params[4]
  D <- params[5]
  sma <- params[6]

  if (ar == 0 && ma == 0 && sar == 0 && sma == 0){# ordinary linear model
    if (d == 0 && D == 0){
      if (ncol(y) == 1)
        paste0("OLM")
      else
        paste0("OLM: ", nms)
    }
    else if (D == 0){
      if (ncol(y) == 1)
        paste0("OLM (d=",d,")")
      else
        paste0("OLM (d=",d,"): ", nms)
    }
    else if (d == 0){
      if (ncol(y) == 1)
        paste0("OLM (D=",D,")")
      else
        paste0("OLM (D=",D,"): ", nms)
    }
    else {
      if (ncol(y) == 1)
        paste0("OLM (d=",d,"D=",D,")")
      else
        paste0("OLM (d=",d,"D=",D,"): ", nms)
    }
  }
  else if (ncol(y) == 1){
    if (sar == 0 && D == 0 && sma == 0){
      if (d == 0){
        if (ma == 0){
          paste0("AR", "(", ar, ")")
        }
        else if (ar == 0){
          paste0("MA", "(", ma, ")")
        }
        else{
          paste0("ARMA", "(", paste(c(ar,ma), collapse = ", "), ")")
        }
      }
      else{ # write full params
        paste0("ARIMA", "(", paste(c(ar,d,ma), collapse = ", "), ")")
      }
    } else{ # seasonal
      paste0("S-ARIMA", "(", paste(params, collapse = ", "), ")")
    }
  }
  else {
    if (sar == 0 && D == 0 && sma == 0){
      if (d == 0){
        if (ma == 0){
          paste0("VAR", "(", ar, "): ", nms)
        }
        else if (ar == 0){
          paste0("VMA", "(", ma, "): ", nms)
        }
        else{
          paste0("VARMA", "(", paste(c(ar,ma), collapse = ", "), "): ", nms)
        }
      }
      else{ # write full params
        paste0("VARIMA", "(", paste(c(ar,d,ma), collapse = ", "), "): ", nms)
      }
    }
    else { # seasonal
      paste0("S-VARIMA",
             "(", paste(params, collapse = ", "), "): ", nms)
    }
  }

}


seasonal_cumsum_row <- function(x, n_seasons) {
  n <- nrow(x)
  if (n <= n_seasons)
    return(x)

  result <- x
  for (i in (n_seasons+1):n) {
    result[i,] <- result[i,] + result[i-n_seasons,]
  }
  result
}

arima_poly_0 <- function(p, q, P, Q, numS) {

  ar_poly <- integer(max(p,P) * numS)
  if (p != 0 || P != 0){
    for (i in seq_len(p))
      ar_poly[[i]] = 1
    if (numS > 1 && P > 0)
      for (i in seq(numS, numS*P, numS))
        ar_poly[[i]] = 1
  }

  ma_poly <- integer(max(q,Q)*numS)
  if (q != 0 || Q != 0){
    for (i in seq_len(q))
      ma_poly [[i]] = 1
    if (numS > 1 && Q > 0)
      for (i in seq(numS, numS*Q, numS))
        ma_poly[[i]] = 1
  }

  list(ar_poly = ar_poly, ma_poly = ma_poly)
}

#' Generate Random Sample from a VARMA Model
#'
#' This function generates a multivariate time series using a VARMA process.
#'
#' @param sigma A positive definite matrix representing the covariance matrix of the white noise series or an integer representing the dimension of a random covariance matrix to generate.
#' @param arList A list of matrices representing the AR coefficients of the VARMA model or an integer representing the number of random AR coefficients to generate.
#' @param maList A list of matrices representing the MA coefficients of the VARMA model or an integer representing the number of random MA coefficients to generate. For identification purposes, it generates diagonal matrices.
#' @param exoCoef A matrix representing the coefficients of the exogenous variables or an integer representing the number of random exogenous coefficients to generate.
#' @param nObs An integer representing the number of observations to generate.
#' @param nBurn An integer representing the number of burn-in observations to remove from the generated time series.
#' @param intercept A numeric vector representing the intercept of the VARMA model or a logical value indicating whether to generate a random intercept.
#' @param d An integer representing the order of integration.
#' @param startFrequency The frequency of the first observation in the data.
#' @param seasonalCoefs An integer vector of size 4: \code{(P,D,Q,s)} where
#' \code{P} is the number of random seasonal AR coefficients to generate,
#' \code{Q} is the number of random seasonal MA coefficients to generate,
#' \code{D} is the order of seasonal integration,
#' and \code{s} is the number of seasons.
#' These are effective if \code{arList} and \code{maList} are randomly generated within the function.
#'
#' @return A list with the following items:
#'   \item{y}{The simulated endogenous data.}
#'   \item{x}{The simulated exogenous data.}
#'   \item{e}{The simulated white noise series.}
#'   \item{sigma}{The covariance matrix of the white noise series.}
#'   \item{arList}{The list of autoregressive coefficients.}
#'   \item{maList}{The list of moving average coefficients.}
#'   \item{exoCoef}{The matrix of exogenous coefficients.}
#'   \item{intercept}{The intercept vector.}
#'   \item{d}{The order of the integration.}
#'   \item{seasonalCoefs}{The argument \code{seasonalCoefs} }
#'   \item{nObs}{The number of observations generated.}
#'   \item{nBurn}{The number of burn-in observations removed.}
#'
#'
#' @export
#' @importFrom stats rnorm
#' @example man-roxygen/ex-sim.varma.R
#'
sim.varma <- function(sigma = 2L, arList = 1L, maList = 0L,
                      exoCoef = 0L, nObs = 100, nBurn = 10,
                      intercept = TRUE, d = 0,
                      startFrequency = NULL,
                      seasonalCoefs = NULL) {

  #TODO: add identification restriction options
  #TODO: seasonal data (add three arguments for sAr, sMa, sD. These are effective if
  #      You are generating ar and ma parameters randomly. Calculate the lag structure
  #      generate based on maximum lag and make others zero)

  nObs = as.integer(nObs)
  if (nObs <= 0)
    stop("nObs must be a positive integer.")

  nBurn = as.integer(nBurn)
  if (nBurn < 0)
    stop("nBurn cannot be a negative integer.")

  # Set the dimension of the VARMA model
  p <- ifelse(is.integer(sigma) && length(sigma) == 1,sigma,ncol(sigma))

  sAr = 0
  sMa = 0
  D = 0
  s = 1
  isSeasonal <- FALSE
  if (is.null(seasonalCoefs) == FALSE ){
    seasonalCoefs = as.integer(seasonalCoefs)
    if (length(seasonalCoefs) != 4)
      stop("seasonalCoefs must be an integer vector of size 4.")
    s = seasonalCoefs[[4]]
    D = seasonalCoefs[[2]]
    sAr = seasonalCoefs[[1]]
    sMa = seasonalCoefs[[3]]
    isSeasonal <-  s > 1
  }

  arimaPolyZeros <- NULL
  if (isSeasonal && is.integer(arList) && length(arList) == 1 &&
      is.integer(maList) && length(maList) == 1)
    arimaPolyZeros <- arima_poly_0(arList, maList, sAr, sMa, s)

  # Generate random AR coefficients if arList is an integer
  if (is.integer(arList) && length(arList) == 1) {
    if (isSeasonal)
      arList <- lapply(arimaPolyZeros$ar_poly, function(x) if(x==1) matrix(rnorm(p^2)*0.1, p, p) else matrix(0, p, p))
    else
      arList <- lapply(seq_len(arList), function(x) matrix(rnorm(p^2)*0.1, p, p))
  }

  # Generate random MA coefficients if maList is an integer
  if (is.integer(maList) && length(maList) == 1) {
    if (isSeasonal)
      maList <- lapply(arimaPolyZeros$ma_poly, function(x) if(x==1) diag(rnorm(1)*0.1, p, p) else matrix(0, p, p))
    else
      maList <- lapply(seq_len(maList), function(x) diag(rnorm(1)*0.1, p, p))
    # in order to get identifiable model, I make MAs diagonal
    # see https://doi.org/10.1080/07350015.2021.1904960
  }

  # Generate a random covariance matrix if sigma is an integer
  if (is.integer(sigma) && length(sigma) == 1) {
    A <- matrix(rnorm(p^2), p, p)
    sigma <- crossprod(A)
  }

  # Generate random exogenous coefficients if exoCoef is an integer
  if (is.integer(exoCoef) && length(exoCoef) == 1) {
    if (exoCoef == 0)
      exoCoef = NULL
    else
      exoCoef <- matrix(rnorm(p * exoCoef), p, exoCoef)
  }

  # Generate a random intercept if intercept is TRUE
  if (is.logical(intercept) && intercept) {
    intercept <- matrix(rnorm(p), ncol = 1)
  }

  # Check the dimensions of the input arguments
  if (length(arList) > 0 && any(sapply(arList, nrow) != p | sapply(arList, ncol) != p)) {
    stop("Invalid dimensions of AR coefficients")
  }
  if (length(maList) > 0 && any(sapply(maList, nrow) != p | sapply(maList, ncol) != p)) {
    stop("Invalid dimensions of MA coefficients")
  }
  if (!is.null(exoCoef) && (nrow(exoCoef) != p || ncol(exoCoef) < 1)) {
    stop("Invalid dimensions of exogenous coefficients")
  }
  if (!is.null(intercept) && length(intercept) != p) {
    stop("Invalid dimensions of intercept")
  }

  len <- nObs + nBurn + d + ifelse(isSeasonal, D * s, 0)

  # Generate the white noise series
  e <- rand.mnormal(len, NULL, sigma)
  e <- e$sample

  # Generate the exogenous data
  q <- ifelse(is.null(exoCoef),0,ncol(exoCoef))
  x <- matrix(rnorm(len * q, 10), len, q)

  # Initialize the y series
  y <- matrix(0, len, p)

  # Set the maximum lag order
  p_ar <- length(arList)
  p_ma <- length(maList)
  p_max <- max(p_ar, p_ma)

  # Generate the y series using the VARMA process
  for (t in (p_max+1):len) {
    y[t,] <- e[t,]
    if (!is.null(exoCoef)) {
      y[t,] <- y[t,] + x[t,] %*% t(exoCoef)
    }
    if (!is.null(intercept)) {
      y[t,] <- y[t,] + intercept
    }
    for (j in seq_along(maList)) {
      y[t,] <- y[t,] + maList[[j]] %*% e[t-j,]
    }
    for (j in seq_along(arList)) {
      y[t,] <- y[t,] + arList[[j]] %*% y[t-j,]
    }

  }

  # Integrate the series
  if (isSeasonal && D>0){
    for (i in c(1:D))
      y <- seasonal_cumsum_row(y, s)
  }
  if (d > 0){
    for (i in c(1:d))
      y <- seasonal_cumsum_row(y, 1)
  }

  # Remove the burn-in observations
  y <- y[(nBurn+1+d+D*s):len,, drop = FALSE]
  colnames(y) <- paste0("Y",c(1:ncol(y)))

  if (length(x) == 0)
    x <- NULL
  else
  {
    x <- x[(nBurn+1+d+D*s):len,, drop = FALSE]
    colnames(x) <- paste0("X",c(1:ncol(x)))
  }

  e <- e[(nBurn+1+d+D*s):len,, drop = FALSE]
  colnames(e) <- paste0("E",c(1:ncol(e)))

  # Set row and column names for arList items
  if (!is.null(arList)) {
    for (i in seq_along(arList)) {
      colnames(arList[[i]]) <- paste0("Y", seq_len(ncol(arList[[i]])), "(-", i, ")")
      rownames(arList[[i]]) <- paste0("Eq", seq_len(nrow(arList[[i]])))
    }
  }

  if (!is.null(maList)) {
    # Set row and column names for maList items
    for (i in seq_along(maList)) {
      colnames(maList[[i]]) <- paste0("E", seq_len(ncol(maList[[i]])), "(-", i, ")")
      rownames(maList[[i]]) <- paste0("Eq", seq_len(nrow(maList[[i]])))
    }
  }

  # Set row and column names for sigma
  if (!is.null(sigma)) {
    colnames(sigma) <- paste0("Eq", seq_len(ncol(sigma)))
    rownames(sigma) <- paste0("Eq", seq_len(nrow(sigma)))
  }

  # Set row and column names for exoCoef
  if (!is.null(exoCoef)) {
    colnames(exoCoef) <- paste0("X", seq_len(ncol(exoCoef)))
    rownames(exoCoef) <- paste0("Eq", seq_len(nrow(exoCoef)))
  }

  # Set name for intercept
  if (!is.null(intercept)) {
    names(intercept) <- "Intercept"
  }

  if (is.null(startFrequency) == FALSE){
    labs <- tdata::get.seq0(startFrequency, nrow(y), 1)
    rownames(y) <- labs
    if (is.null(x) == FALSE)
      rownames(x) <- labs
    rownames(e) <- labs

    attr(y, "ldtf") <- startFrequency
    if (is.null(x) == FALSE)
      attr(x, "ldtf") <- startFrequency
    attr(e, "ldtf") <- startFrequency
  }

  return(list(y = y,
              x = x,
              e = e,
              sigma = sigma,
              arList = arList,
              maList = maList,
              exoCoef = exoCoef,
              intercept = intercept,
              d = d,
              seasonalCoefs = seasonalCoefs,
              nObs = nObs,
              nBurn = nBurn))
}



#' Split VARMA parameter into its Components
#'
#' Use this function to extract AR, MA, intercept, and exogenous coefficients from the VARMA estimation.
#'
#' @param coef A matrix of coefficients with dimensions \code{numEq} x \code{numAR * numEq + numMA * numEq + numExo + ifelse(intercept, 1, 0)}.
#' @param numAR A non-negative integer scalar specifying the number of AR lags.
#' @param numMA A non-negative integer scalar specifying the number of MA lags.
#' @param numExo A non-negative integer scalar specifying the number of exogenous variables.
#' @param intercept A logical scalar indicating whether an intercept is included in the model.
#' @param numAR_s A non-negative integer scalar specifying the number of seasonal AR lags.
#' @param numMA_s A non-negative integer scalar specifying the number of seasonal MA lags.
#' @param numSeasons A non-negative integer scalar specifying the number of seasons.
#'
#' @return A list with the following items:
#' \itemize{
#'   \item \code{arList}: A list containing the AR coefficients for each lag.
#'   \item \code{intercept}: A numeric vector of length \code{numEq} containing the intercept, or \code{NULL} if \code{intercept = FALSE}.
#'   \item \code{exoCoef}: A matrix of dimensions \code{numEq} x \code{numExo} containing the exogenous coefficients, or \code{NULL} if \code{numExo = 0}.
#'   \item \code{maList}: A list containing the MA coefficients for each lag.
#' }
#' @export
#' @examples
#' # see 'search.varma' or 'estim.varma' functions.
#'
#' @seealso [estim.varma]
get.varma.params <- function(coef, numAR = 1, numMA = 0,
                             numExo = 0, intercept = TRUE, numAR_s = 0, numMA_s = 0, numSeasons = 1) {
  coef <- t(as.matrix(coef))
  numEq <- nrow(coef)
  numAR <- as.integer(numAR)
  numExo <- as.integer(numExo)
  intercept <- as.logical(intercept)

  if (numEq <= 0)
    stop("'numEq' must be a positive integer scalar")
  if (numAR < 0)
    stop("'numAR' must be a non-negative integer scalar")
  if (numMA < 0)
    stop("'numMA' must be a non-negative integer scalar")
  if (numAR_s < 0)
    stop("'numAR_s' must be a non-negative integer scalar")
  if (numMA_s < 0)
    stop("'numMA_s' must be a non-negative integer scalar")
  if (numExo < 0)
    stop("'numExo' must be a non-negative integer scalar")

  isSeasonal <- numSeasons > 1 && (numAR_s>0 || numMA_s >0)

  arimaPolyZeros <- arima_poly_0(numAR, numMA, numAR_s, numMA_s, numSeasons)

  ar_inds <- which(arimaPolyZeros$ar_poly==1)
  ma_inds <- which(arimaPolyZeros$ma_poly==1)
  numNon0Ar <- length(ar_inds)
  numNon0Ma <- length(ma_inds)


  expected_ncol <- numNon0Ar * numEq + numNon0Ma * numEq + numExo + ifelse(intercept, 1, 0)
  if (ncol(coef) != expected_ncol) {
    stop("The number of rows of 'coef' is not consistent with the other arguments")
  }

  start <- 1
  if (length(arimaPolyZeros$ar_poly) > 0) {
    ar <- list()
    for (i in arimaPolyZeros$ar_poly){
      if (i == 0)
        ar[[length(ar) + 1]] <- matrix(0,numEq,numEq)
      else{
        ar[[length(ar) + 1]] <- coef[, start:(start + numEq - 1), drop = FALSE]
        start <- start + numEq
      }
    }
  } else {
    ar <- NULL
  }

  if (intercept) {
    int <- coef[, start, drop = FALSE]
    start <- start + 1
  } else {
    int <- NULL
  }
  if (numExo > 0) {
    exog <- coef[, start:(start + numExo - 1), drop = FALSE]
    start <- start + numExo
  } else {
    exog <- NULL
  }
  if (length(arimaPolyZeros$ma_poly) > 0) {
    ma <- list()
    for (i in arimaPolyZeros$ma_poly){
      if (i == 0)
        ma[[length(ma) + 1]] <- matrix(0,numEq,numEq)
      else{
        ma[[length(ma) + 1]] <- coef[, start:(start + numEq - 1), drop = FALSE]
        start <- start + numEq
      }
    }
  } else {
    ma <- NULL
  }



  #dep_names = rownames(coef)
  #if (is.null(dep_names))
  #  dep_names <- paste0("Y", seq_len(numEq))



  return(list(arList = ar, integer = int,
              exoCoef = exog, maList = ma))
}


