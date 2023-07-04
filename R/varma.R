
#' Search for Best VARMA Models
#'
#' Use this function to create a Vector Autoregressive Moving Average model set and search for the best models (and other information) based on in-sample and out-of-sample evaluation metrics.
#'
#' @param y A matrix of endogenous data with variables in the columns.
#' @param x A matrix of exogenous data with variables in the columns. It can be \code{NULL}.
#' @param numTargets An integer representing the number of target variables.
#' For example, if 2, the first two variables in the first columns of \code{y} will be targets.
#' Information is saved just for the target variables.
#' It must be positive and cannot be larger than the number of endogenous variables.
#' @param ySizes An integer vector specifying the number of endogenous variables in the regressions.
#' For example, \code{c(1,2)} means the model set contains all regressions with 1 and 2 endogenous variables.
#' If \code{NULL}, \code{c(1)} is used.
#' @param yPartitions A list of integer vectors that partition the indexes of the endogenous variables.
#' No regression is estimated with two variables in the same partition.
#' If \code{NULL}, each variable is placed in its own partition, and the size of the model set is maximized.
#' @param xGroups A list of integer vectors that determine different combinations of the indexes of the exogenous variables to be used as exogenous variables in the SUR regressions.
#' @param maxParams An integer vector that determines the maximum values for the parameters of the VARMA model: \code{(p,d,q,P,D,Q)}. If \code{NULL}, \code{c(2,0,0,0,0,0)} is used.
#' @param seasonsCount An integer value representing the number of observations per unit of time.
#' @param maxHorizon An integer value representing the maximum value for the prediction horizon if \code{type1} is \code{TRUE} in the \code{modelCheckItems} argument. Also, it is used as the maximum prediction horizon in checking predictions.
#' @param newX A matrix of new exogenous data for out-of-sample prediction. It must have the same number of columns as the \code{x} argument.
#' @param simUsePreviousEstim If \code{TRUE}, parameters are initialized only in the first step of the simulation. The initial values of the n-th simulation (with one more observation) are the estimations from the previous step.
#' @param olsStdMultiplier A number used as a multiplier for the standard deviation of OLS, used for restricting maximum likelihood estimation.
#' @param lbfgsOptions A list containing L-BFGS optimization options.
#' Use [get.options.lbfgs] function for initialization.
#' @param metricOptions A list of options for measuring performance.
#' Use [get.options.metric] function to get them.
#' @param modelCheckItems A list of options for excluding a subset of the model set.
#' See and use [get.items.modelcheck] function to get them.
#' @param searchItems A list of options for specifying the purpose of the search.
#' See and use [get.items.search] function to get them.
#' @param searchOptions A list of extra options for performing the search.
#' See and use [get.options.search] function to get them.
#'
#' @return A nested list with the following members:
#' \item{counts}{Information about the expected number of models, number of estimated models, failed estimations, and some details about the failures.}
#' \item{...}{Results reported separately for each metric, then for each target variable, then for each requested type of output. This part of the output is highly nested and items are reported based on the arguments of the search.}
#' \item{info}{General information about the search process, some arguments, elapsed time, etc.}
#'
#' Note that the output does not contain any estimation results,
#' but minimum required data to estimate the models (Use \code{summary()} function to get the estimation).
#'
#' @export
#' @example man-roxygen/ex-search.varma.R
#' @seealso [estim.varma], [search.varma.stepwise]
search.varma <- function(y, x = NULL, numTargets = 1,
                         ySizes = NULL, yPartitions = NULL,
                         xGroups = NULL, maxParams = c(1,0,0,0,0,0),
                         seasonsCount = 0, maxHorizon = 0,
                         newX = NULL, simUsePreviousEstim = TRUE,
                         olsStdMultiplier = 2.0, lbfgsOptions = get.options.lbfgs(),
                         metricOptions = get.options.metric(),
                         modelCheckItems = get.items.modelcheck(), searchItems = get.items.search(),
                         searchOptions = get.options.search()){
  y = as.matrix(y)
  x = if (is.null(x)) NULL else as.matrix(x)
  numTargets = as.integer(numTargets)
  ySizes = if (is.null(ySizes)) c(1L) else as.integer(ySizes)
  maxParams = if (is.null(maxParams)) c(2L,0L,0L,0L,0L,0L) else as.integer(maxParams)
  seasonsCount = as.integer(seasonsCount)
  maxHorizon = as.integer(maxHorizon)
  newX = if (is.null(newX)) NULL else as.matrix(newX)
  simUsePreviousEstim = as.logical(simUsePreviousEstim)
  olsStdMultiplier = as.numeric(olsStdMultiplier)

  if (is.null(yPartitions) == FALSE){
    yPartitions = as.list(yPartitions)
    for (i in c(1:length(yPartitions)))
      yPartitions[[i]] = as.integer(yPartitions[[i]])
  }

  if (is.null(xGroups) == FALSE){
    xGroups = as.list(xGroups)
    for (i in c(1:length(xGroups)))
      xGroups[[i]] = as.integer(xGroups[[i]])
  }

  if (is.null(lbfgsOptions))
    lbfgsOptions = get.options.lbfgs()
  else
    lbfgsOptions = CheckLbfgsOptions(lbfgsOptions)

  if (is.null(metricOptions))
    metricOptions = get.options.metric()
  else
    metricOptions <- CheckmetricOptions(metricOptions)

  if (is.null(modelCheckItems))
    modelCheckItems = get.items.modelcheck()
  else
    modelCheckItems <- CheckModelCheckItems(modelCheckItems)

  if (is.null(searchItems))
    searchItems = get.items.search()
  else
    searchItems <- CheckSearchItems(searchItems)

  if (is.null(searchOptions))
    searchOptions = get.options.search()
  else
    searchOptions <- CheckSearchOptions(searchOptions)


  startTime <- Sys.time()

  res <- .SearchVarma(y, x, numTargets, ySizes, yPartitions,
                      xGroups, maxParams, seasonsCount, maxHorizon,
                      newX, simUsePreviousEstim,
                      olsStdMultiplier, lbfgsOptions,
                      metricOptions,modelCheckItems,searchItems,searchOptions)

  endTime <- Sys.time()

  res$info$startTime <- startTime
  res$info$endTime <- endTime

  res$info$startFrequency <- attr(y, "ldtf")

  res
}

#' Estimate a VARMA Model
#'
#' Use this function to estimate a Vector Autoregressive Moving Average model.
#'
#' @param y A matrix of endogenous data with variables in the columns.
#' @param x A matrix of exogenous data with variables in the columns. It can be \code{NULL}.
#' @param params An integer vector that determines the values for the parameters of the VARMA model: \code{(p,d,q,P,D,Q)}. If \code{NULL}, \code{c(1,0,0,0,0,0)} is used.
#' @param seasonsCount An integer value representing the number of observations per unit of time.
#' @param addIntercept If \code{TRUE}, an intercept is automatically added to x.
#' @param lbfgsOptions A list containing L-BFGS optimization options.
#' Use [get.options.lbfgs] function for initialization.
#' @param olsStdMultiplier A number used as a multiplier for the standard deviation of OLS, used for restricting maximum likelihood estimation.
#' @param pcaOptionsY A list of options to use principal components of \code{y}, instead of the actual values. Set to \code{NULL} to disable. Use [get.options.pca()] for initialization.
#' @param pcaOptionsX A list of options to use principal components of \code{x}, instead of the actual values. Set to \code{NULL} to disable. Use [get.options.pca()] for initialization.
#' @param maxHorizon An integer representing the maximum prediction horizon. Set to zero to disable prediction.
#' @param newX A matrix containing new exogenous variables to be used in predictions. Its columns must be the same as \code{x}.
#' @param simFixSize An integer that determines the number of out-of-sample simulations. Use zero to disable simulation.
#' @param simHorizons An integer vector representing the prediction horizons to be used in out-of-sample simulations. See also [get.options.metric()].
#' @param simUsePreviousEstim If \code{TRUE}, parameters are initialized only in the first step of the simulation. The initial values of the n-th simulation (with one more observation) are the estimations from the previous step.
#' @param simMaxConditionNumber A number representing the maximum value for the condition number in simulation.
#' @param printMsg Set to \code{TRUE} to enable printing some details.
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
#' Seasonal Integrated Vector Autoregressive Moving-Average is used for predicting time series variables.
#' Its formula is:
#' \deqn{
#' \Delta^d \Delta_s^D y_t = c + \sum_{i=1}^p A_i y_{t-i} +
#'                               \sum_{i=1}^q B_i \epsilon_{t-i}  +
#'                               C x_t  +
#'                               \sum_{i=1}^P A_{is} y_{t-is} +
#'                               \sum_{i=1}^Q B_{is} \epsilon_{t-is} +
#'                               \epsilon_t,
#' }
#' where \eqn{y_t} is the vector of endogenous variables, \eqn{x_t} is the vector exogenous variables, \eqn{s} is the number of seasons and \eqn{(p,d,q,P,D,Q)} are the lag structure of the model.
#' Furthermore, \eqn{c,\;C,\;A_i} and \eqn{B_i} for all available \eqn{i} are the parameters of the model.
#' We use maximum likelihood estimator to estimate the parameters of the model.
#' If \eqn{B_i} coefficients are not zero, identification restrictions are necessary to ensure that the model is uniquely identifiable.
#' In the current implementation, this function restricts \eqn{B_i} coefficients to be diagonal.
#'
#' Note that the main purpose of exporting this method is to show the inner calculations of the search process in [search.varma] function.
#'
#' @export
#' @example man-roxygen/ex-estim.varma.R
#' @seealso [search.varma], [search.varma.stepwise]
estim.varma <- function(y, x = NULL, params = NULL,
                        seasonsCount = 0, addIntercept = TRUE,
                        lbfgsOptions = get.options.lbfgs(), olsStdMultiplier = 2,
                        pcaOptionsY = NULL, pcaOptionsX = NULL,
                        maxHorizon = 0, newX = NULL, simFixSize = 0,
                        simHorizons = NULL, simUsePreviousEstim = TRUE,
                        simMaxConditionNumber = Inf, printMsg = FALSE){

  y = as.matrix(y)
  x = if (is.null(x)) NULL else as.matrix(x)
  params = if (is.null(params)) c(1L,0L,0L,0L,0L,0L) else as.integer(params)
  seasonsCount = as.integer(seasonsCount)
  addIntercept = as.logical(addIntercept)
  olsStdMultiplier = as.numeric(olsStdMultiplier)
  maxHorizon = as.integer(maxHorizon)
  newX = if (is.null(newX)) NULL else as.matrix(newX)
  simFixSize = as.integer(simFixSize)
  simHorizons = if (is.null(simHorizons)) NULL else as.integer(simHorizons)
  simUsePreviousEstim = as.logical(simUsePreviousEstim)
  simMaxConditionNumber = as.numeric(simMaxConditionNumber)
  printMsg = as.logical(printMsg)

  if (is.null(lbfgsOptions))
    lbfgsOptions = get.options.lbfgs()
  else
    lbfgsOptions = CheckLbfgsOptions(lbfgsOptions)

  if (is.null(pcaOptionsY) == FALSE)
    pcaOptionsY = CheckPcaOptions(as.list(pcaOptionsY))
  if (is.null(pcaOptionsX) == FALSE)
    pcaOptionsX = CheckPcaOptions(as.list(pcaOptionsX))

  res <- .EstimVarma(y, x, params, seasonsCount, addIntercept,
                     lbfgsOptions, olsStdMultiplier,
                     pcaOptionsY, pcaOptionsX,
                     maxHorizon, newX, simFixSize, simHorizons,
                     simUsePreviousEstim, simMaxConditionNumber, printMsg)

  # other information

  res$info$startFrequency <- attr(y, "ldtf")


  if (is.null(res$prediction$means) == FALSE &&
      is.null(res$info$startFrequency) == FALSE){   # update predictions

    hasVar = is.null(res$prediction$vars) == FALSE
    predStart = tdata::next.freq(res$info$startFrequency, res$counts$obs - res$prediction$startIndex + 1)
    labs <- tdata::get.seq0(predStart, ncol(res$prediction$means), by = 1)
    colnames(res$prediction$means) <- labs
    if (hasVar)
      colnames(res$prediction$vars) <- labs
  }

  res
}


# get estimation from search result
GetEstim_varma <- function(searchRes, endoIndices,
                           exoIndices, y, x, printMsg,
                           params, newX = NULL, ...) {
  M <- estim.varma(y[, endoIndices, drop = FALSE],
                   x = if (is.null(exoIndices) || is.null(x)) {
                     NULL
                   } else {
                     x[, exoIndices, drop = FALSE]
                   },
                   params = params,
                   seasonsCount = searchRes$info$seasonsCount,
                   addIntercept = FALSE,
                   lbfgsOptions = searchRes$info$lbfgsOptions,
                   olsStdMultiplier = searchRes$info$olsStdMultiplier,
                   pcaOptionsY = NULL,
                   pcaOptionsX = NULL,
                   maxHorizon = searchRes$info$maxHorizon,
                   newX = if (is.null(exoIndices) || is.null(newX)) {
                     NULL
                   } else {
                     as.matrix(newX[, exoIndices])
                   },
                   simFixSize = searchRes$info$metricOptions$simFixSize,
                   simHorizons = searchRes$info$metricOptions$simHorizons,
                   simUsePreviousEstim = searchRes$info$simUsePreviousEstim,
                   simMaxConditionNumber = searchRes$info$modelCheckItems$maxConditionNumber,
                   printMsg = printMsg
  )

  return(M)
}


#' Step-wise Search for Best VARMA Models
#'
#' For a large model set, use this function to find the best Vector Autoregressive Moving Average models.
#' It selects a subset of variables from smaller models and moves to the bigger ones.
#'
#' @param y A matrix of endogenous data with variables in the columns.
#' @param ySizeSteps A list of model dimensions to be estimated in each step.
#' Its size determines the number of steps.
#' @param countSteps An integer vector to determine the number of variables to be used in each step.
#' \code{NA} means all variables. Variables are selected based on best estimations.
#' All variables in the best models (all metrics and targets) are selected until the corresponding suggested number is reached.
#' Select an appropriate value for \code{bestK} in the options.
#' @param savePre A directory for saving and loading the progress.
#' Each step's result is saved in a file (name=\code{paste0(savePre,i)} where \code{i} is the index of the step.
#' @param ... other arguments to pass to [search.varma] function such as the \code{x} argument.
#' Note that \code{ySizes} is ineffective here.
#'
#' @return Similar to [search.varma] function.
#' @export
#'
#' @examples
#' # See the example in the 'search.varma' function.
#'
#' @seealso [search.varma], [estim.varma]
search.varma.stepwise <- function(y, ySizeSteps = list(c(1,2), c(3)),
                                  countSteps = c(NA, 10),
                                  savePre = NULL, ...) {

  if (length(ySizeSteps[[1]]) == 1)
    stop("First element in 'ySizeSteps' must contain at lease two elements.")

  # or it will never find another potential variables to move to the next step
  # this is specific to VARMA, because targets are in this list.
  #TODO: check for the relationship between the length of this first element and the number of targets

  Search_s("varma", y, ySizeSteps, countSteps, savePre, ...)
}


varma.to.latex.mat <- function(sigma, arList, int, exoCoef, maList, d, D, s, numFormat = "%.2f") {

  #TODO: add breaks based on the number of equations and lags
  #TODO: add three vertical dots for large vectors similar to SUR

  numEq <- nrow(sigma)
  numAR <- ifelse(is.null(arList), 0 , length(arList))
  numMA <- ifelse(is.null(maList), 0 , length(maList))
  numExo <- ifelse(is.null(exoCoef), 0 , ncol(exoCoef))

  # Initialize the LaTeX string
  latex_str <- ""

  delta <- ""
  if (d == 1)
    delta <- paste0(delta, "\\Delta")
  else if (d > 1)
    delta <- paste0(delta, "\\Delta^", d)
  if (D == 1)
    delta <- paste0(delta, "\\Delta_", s)
  else if (D > 1)
    delta <- paste0(delta, "\\Delta_", s, "^", D)

  # Add the dependent variable vector
  latex_str <- paste0(latex_str, " \\begin{bmatrix}", paste(paste0(delta, " Y_{", seq_len(numEq),"t}"), collapse = "\\\\"), "\\end{bmatrix}")

  # Add the equal sign
  latex_str <- paste0(latex_str, " = ")

  # Add the intercept vector
  if (!is.null(int) && any(as.numeric(int) != 0)) {
    latex_str <- paste0(latex_str, "\\begin{bmatrix}", paste(sprintf(numFormat, int), collapse = "\\\\"), "\\end{bmatrix}")

    # Add a plus sign if there are more terms
    if (numAR > 0 || numExo > 0 || numMA > 0) {
      latex_str <- paste0(latex_str, " + ")
    }
  }

  # Add the AR matrices
  for (lag in seq_len(numAR)) {
    if (all(as.numeric(arList[[lag]])== 0))
      next
    ar_mat <- matrix(sprintf(numFormat, arList[[lag]]), nrow = nrow(arList[[lag]]), ncol = ncol(arList[[lag]]))
    latex_str <- paste0(
      latex_str,
      "\\begin{bmatrix}",
      paste(apply(ar_mat, 1, paste, collapse = " & "), collapse = "\\\\"),
      "\\end{bmatrix}"
    )

    # Add the lagged dependent variable vector
    latex_str <- paste0(latex_str,
      " \\begin{bmatrix}",
      paste(paste0(delta, " Y_{", seq_len(numEq), "t-", lag, "}"), collapse = "\\\\"),
      "\\end{bmatrix}"
    )

    # Add a plus sign if there are more terms
    if (lag < numAR || numExo > 0 || numMA > 0) {
      latex_str <- paste0(latex_str, " + ") #break
    }
  }

  if (numExo > 1){
    latex_str <- paste0(latex_str, "\\\\") #break
  }

  # Add the exogenous matrix
  if (numExo > 0 && any(as.numeric(exoCoef) != 0)) {
    exo_mat <- matrix(sprintf(numFormat, exoCoef), nrow = nrow(exoCoef), ncol = ncol(exoCoef))
    latex_str <- paste0(
      latex_str,
      "\\begin{bmatrix}",
      paste(apply(exo_mat, 1, paste, collapse = " & "), collapse = "\\\\"),
      "\\end{bmatrix}"
    )

    # Add the exogenous variable vector
    if (numExo > 0){
      latex_str <- paste0(
        latex_str,
        "\\begin{bmatrix}",
        paste(paste0("X_", seq_len(numExo)), collapse = "\\\\"),
        "\\end{bmatrix}"
      )
    }

    # Add a plus sign if there are more terms
    if (numMA > 0) {
      latex_str <- paste0(latex_str, "+ ")
    }
  }

  if (numMA > 0){
    latex_str <- paste0(latex_str, "\\\\") #break
  }

  # Add the MA matrices
  for (lag in seq_len(numMA)) {
    if (all(as.numeric(maList[[lag]])== 0))
      next
    ma_mat <- matrix(sprintf(numFormat, maList[[lag]]), nrow = nrow(maList[[lag]]), ncol = ncol(maList[[lag]]))
    latex_str <- paste0(
      latex_str,
      "\\begin{bmatrix}",
      paste(apply(ma_mat, 1, paste, collapse = " & "), collapse = "\\\\"),
      "\\end{bmatrix}"
    )

    # Add the lagged error vector
    latex_str <- paste0(
      latex_str,
      "\\begin{bmatrix}",
      paste(paste0("E_{", seq_len(numEq), "t-", lag, "}"), collapse = "\\\\"),
      "\\end{bmatrix}"
    )

    # Add a plus sign if there are more terms
    if (lag < numMA) {
      latex_str <- paste0(latex_str, " + ")
    }
  }

  # Add the error vector
  latex_str <- paste0(latex_str, " + \\begin{bmatrix}", paste(paste0("E_{", seq_len(numEq),"t}"), collapse = "\\\\"), "\\end{bmatrix}")

  s_mat <- latex.matrix(mat = sigma, numFormat = numFormat)
  latex_str <- paste0(latex_str, ",\\\\ \\Sigma = ", s_mat)

  return(latex_str)
}

varma.to.latex.eqs <- function(sigma, arList, int, exoCoef, maList, d, D, s, numFormat = "%.2f") {

  #TODO: handle zero coefficients

  numEq <- nrow(sigma)
  numAR <- ifelse(is.null(arList), 0 , length(arList))
  numMA <- ifelse(is.null(maList), 0 , length(maList))
  numExo <- ifelse(is.null(exoCoef), 0 , ncol(exoCoef))

  # Initialize the LaTeX string
  latex_str <- ""

  delta <- ""
  if (d == 1)
    delta <- paste0(delta, "\\Delta")
  else if (d > 1)
    delta <- paste0(delta, "\\Delta^", d)
  if (D == 1)
    delta <- paste0(delta, "\\Delta_", s)
  else if (D > 1)
    delta <- paste0(delta, "\\Delta_", s, "^", D)

  # Add the equations
  for (eq in seq_len(numEq)) {
    # Add the dependent variable
    latex_str <- paste0(latex_str, delta," Y_{", eq, "t}")

    # Add the intercept
    if (!is.null(int) && int[eq] != 0) {
      latex_str <- paste0(latex_str, " = ", sprintf0(numFormat, int[eq]))
    }

    # Add the AR terms
    for (lag in seq_len(numAR)) {
      coefs <- arList[[lag]][eq, ]
      if (all(as.numeric(coefs) == 0))
        next
      signs <- ifelse(coefs >= 0, " + ", " - ")
      coefs <- sprintf0(numFormat, abs(coefs))

      terms <- paste0(signs, coefs, delta, " Y_{", seq_len(numEq), "t-", lag, "}")
      latex_str <- paste0(latex_str, paste(terms, collapse = ""))
    }

    # Add the exogenous terms
    if (numExo > 0){
      for (exo in seq_len(numExo)) {
        coef <- exoCoef[eq, exo]
        if (all(as.numeric(coefs) == 0))
          next
        sign <- ifelse(coef >= 0, " + ", " - ")
        coef <- sprintf0(numFormat, abs(coef))
        term <- paste0(sign, coef, " X_", exo)
        latex_str <- paste0(latex_str, term)
      }
    }

    # Add break
    latex_str <- paste0(latex_str, "\\\\")

    # Add the MA terms
    for (lag in seq_len(numMA)) {
      coefs <- maList[[lag]][eq, ]
      if (all(as.numeric(coefs) == 0))
        next
      signs <- ifelse(coefs >= 0, " + ", " - ")
      coefs <- sprintf0(numFormat, abs(coefs))
      terms <- paste0(signs, coefs, " E_{", seq_len(numEq), "t-", lag, "}")
      latex_str <- paste0(latex_str, paste(terms, collapse = ""))
    }

    # Add the error term
    latex_str <- paste0(latex_str, " + E_{", eq, "t},\\quad \\sigma_", eq,"^2 = ",
                        sprintf0(numFormat, sigma[[eq,eq]]))

    # Add a line break if this is not the last equation
    if (eq < numEq)
      latex_str <- paste0(latex_str, "\\\\")
  }

  return(latex_str)
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
#' @param numFormat A character string that determines how to format the numbers, to be used as the argument of the \code{sprintf} function.
#' If \code{NULL}, conversion to latex or html representations are disabled.
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
#'   \item{eqsLatex}{character string, Latex representation of the equations of the system.}
#'   \item{eqsLatexSys}{character string, Latex representation of the system in matrix form.}
#'
#'
#' @export
#' @importFrom stats rnorm
#' @example man-roxygen/ex-sim.varma.R
#'
sim.varma <- function(sigma = 2L, arList = 1L, maList = 0L,
                      exoCoef = 0L, nObs = 100, nBurn = 10,
                      intercept = TRUE, d = 0, numFormat = "%.2f",
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
              nBurn = nBurn,
              eqsLatex = ifelse(is.null(numFormat), NULL,
                                varma.to.latex.eqs(sigma, arList, intercept,
                                                   exoCoef, maList, d, D, s, as.character(numFormat))),
              eqsLatexSys = ifelse(is.null(numFormat), NULL,
                                   varma.to.latex.mat(sigma, arList, intercept,
                                                      exoCoef, maList, d, D, s, as.character(numFormat)))))
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
  coef <- as.matrix(coef)
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
    stop("The number of columns of 'coef' is not consistent with the other arguments")
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


