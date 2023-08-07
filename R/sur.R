
#' Search for Best SUR Models
#'
#' Use this function to create a Seemingly Unrelated Regression model set and search for the best models (and other information) based on in-sample and out-of-sample evaluation metrics.
#'
#' @param y A matrix of endogenous data with variables in the columns.
#' @param x A matrix of exogenous data with variables in the columns.
#' @param numTargets An integer for the number of targets variables.
#' If for example 2, the first two variable in the first columns of \code{y} will be target.
#' Information is saved just for the target variables.
#' It must be positive and cannot be larger than the number of endogenous variables.
#' @param xSizes An integer vector specifying the number of exogenous variables in the regressions.
#' E.g., \code{c(1,2)} means the model set contains all regressions with 1 and 2 exogenous variables.
#' If \code{NULL}, \code{c(1)} is used.
#' @param xPartitions A list of integer vectors that partition the indexes of the exogenous variables.
#' No regression is estimated with two variables in the same partition.
#' If \code{NULL}, each variable is placed in its own partition, and the size of the model set is maximized.
#' @param numFixXPartitions Number of partitions at the beginning of \code{xPartitions} to be included in all regressions.
#' @param yGroups A list of integer vectors that determine different combinations of the indexes of the endogenous variables to be used as endogenous variables in the SUR regressions.
#' @param searchSigMaxIter An integer for the maximum number of iterations in searching for significant coefficients. Use 0 to disable the search.
#' @param searchSigMaxProb A number for the maximum value of type I error to be used in searching for significant coefficients. If p-value is less than this, it is interpreted as significant.
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
#'
#' @example man-roxygen/ex-search.sur.R
#'
#' @seealso [estim.sur], [search.sur.stepwise]
search.sur <- function(y, x, numTargets = 1, xSizes = NULL,
                      xPartitions = NULL, numFixXPartitions = 0,
                      yGroups = NULL, searchSigMaxIter = 0,
                      searchSigMaxProb = 0.1, metricOptions = get.options.metric(),
                      modelCheckItems = get.items.modelcheck(), searchItems = get.items.search(),
                      searchOptions = get.options.search()){

  y = as.matrix(y)
  x = as.matrix(x)
  numTargets = as.integer(numTargets)
  xSizes = if (is.null(xSizes)) c(1L) else as.integer(xSizes)
  numFixXPartitions = as.integer(numFixXPartitions)
  searchSigMaxIter = as.integer(searchSigMaxIter)
  searchSigMaxProb = as.numeric(searchSigMaxProb)

  if (is.null(xPartitions) == FALSE){
    xPartitions = as.list(xPartitions)
    for (i in c(1:length(xPartitions)))
      xPartitions[[i]] = as.integer(xPartitions[[i]])
  }

  if (is.null(yGroups) == FALSE){
    yGroups = as.list(yGroups)
    for (i in c(1:length(yGroups)))
      yGroups[[i]] = as.integer(yGroups[[i]])
  }

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

  res <- .SearchSur(y, x, numTargets, xSizes, xPartitions, numFixXPartitions,
                    yGroups, searchSigMaxIter, searchSigMaxProb, metricOptions,
                    modelCheckItems, searchItems, searchOptions)

  endTime <- Sys.time()

  res$info$startTime <- startTime
  res$info$endTime <- endTime

  res
}


#' Estimate a SUR Model
#'
#' Use this function to estimate a Seemingly Unrelated Regression model.
#'
#' @param y A matrix of endogenous data with variables in the columns.
#' @param x A matrix of exogenous data with variables in the columns.
#' @param addIntercept If \code{TRUE}, intercept is added automatically to \code{x}.
#' @param searchSigMaxIter An integer for the maximum number of iterations in searching for significant coefficients. Use 0 to disable the search.
#' @param searchSigMaxProb A number for the maximum value of type I error to be used in searching for significant coefficients. If p-value is less than this, it is interpreted as significant.
#' @param restriction A \code{km x q} matrix of restrictions where \code{m=ncols(y)}, \code{k=ncols(x)} and \code{q} is the number of unrestricted coefficients.
#' @param newX A matrix with new exogenous data to be used in the projections. Its number of columns must be equal to \code{x}. It can be \code{NULL}.
#' @param pcaOptionsY A list of options to use principal components of the \code{y}, instead of the actual values. Set \code{NULL} to disable. Use [get.options.pca()] for initialization.
#' @param pcaOptionsX A list of options to use principal components of the \code{x}, instead of the actual values. Set \code{NULL} to disable. Use [get.options.pca()] for initialization.
#' @param simFixSize An integer that determines the number of out-of-sample simulations. Use zero to disable the simulation.
#' @param simTrainFixSize An integer representing the number of data points in the training sample in the out-of-sample simulation. If zero, \code{trainRatio} will be used.
#' @param simTrainRatio A number representing the size of the training sample relative to the available size, in the out-of-sample simulation. It is effective if \code{trainFixSize} is zero.
#' @param simSeed A seed for the random number generator. Use zero for a random value.
#' @param simMaxConditionNumber A number for the maximum value for the condition number in the simulation.
#' @param simTransform Use a character string (e.g. \code{exp} for exponential function) or a function to transform data before calculating RMSE, MAE, RMSPE, MAPE, CRPS metrics. To disable this feature, use \code{NULL}.
#' @param printMsg Set to \code{TRUE} to enable printing some details.
#'
#' @return A nested list with the following items:
#' \item{counts}{Information about different aspects of the estimation such as the number of observation, number of exogenous variables, etc.}
#' \item{estimations}{Estimated coefficients, standard errors, z-statistics, p-values, etc.}
#' \item{metrics}{Value of different goodness of fit and out-of-sample performance metrics. }
#' \item{projections}{Information on the projected values, if \code{newX} is provided.}
#' \item{info}{Some other general information.}
#'
#' @details
#' Seemingly Unrelated Regression (SUR) is a type of statistical model that includes multiple regression equations.
#' The general form of an SUR model with m equations can be written as: \eqn{y_i=X_i\beta_i+\epsilon_i}, where $i=1\ldots m$ determines the index of the equation.
#' In this model, each equation may have different sets of independent variables and it is assumed that the disturbances between the equations are correlated.
#' The OLS estimator is a consistent estimator for this model, but it is not generally asymptotically efficient (except when disturbances are uncorrelated between equations or each equation contains exactly the same set of regressors).
#' The OLS variance matrix is used to calculate the Feasible Generalized Least Squares (FGLS) estimator, which is both consistent and asymptotically efficient (under regularity conditions).
#'
#' In the current implementation, this function focuses on zero restrictions and/or significance search.
#' Therefore, there is a common \code{x} argument for all equations.
#' In fact, the main purpose of exporting this method is to show the inner calculations of the search process in [search.sur] function.
#' Note that the focus in \code{ldt} is model uncertainty and for more sophisticated implementations of the FGLS estimator, you may consider using other packages such as \code{systemfit}.
#'
#' @export
#' @example man-roxygen/ex-estim.sur.R
#'
#' @seealso [search.sur], [search.sur.stepwise]
estim.sur <- function(y, x, addIntercept = TRUE,
                     searchSigMaxIter = 0, searchSigMaxProb = 0.1,
                     restriction = NULL, newX = NULL,
                     pcaOptionsY = NULL, pcaOptionsX = NULL,
                     simFixSize = 0, simTrainFixSize = 0,
                     simTrainRatio = 0.75, simSeed = 0,
                     simMaxConditionNumber = Inf, simTransform = NULL,
                     printMsg = FALSE){
  y = as.matrix(y)
  x = as.matrix(x)
  addIntercept = as.logical(addIntercept)
  searchSigMaxIter = as.integer(searchSigMaxIter)
  searchSigMaxProb = as.numeric(searchSigMaxProb)
  restriction = if (is.null(restriction)) NULL else as.matrix(restriction)
  newX = if (is.null(newX)) NULL else as.matrix(newX)
  simFixSize = as.integer(simFixSize)
  simTrainRatio = as.numeric(simTrainRatio)
  simTrainFixSize = as.integer(simTrainFixSize)
  simSeed = as.integer(simSeed)
  simMaxConditionNumber = as.numeric(simMaxConditionNumber)
  printMsg = as.logical(printMsg)

  if (is.null(simTransform) == FALSE && is.character(simTransform) == FALSE && is.function(simTransform) == FALSE)
    stop("Invalid 'simTransform'. It should be a character string or a function.")

  if (is.null(pcaOptionsY) == FALSE)
    pcaOptionsY = CheckPcaOptions(as.list(pcaOptionsY))
  if (is.null(pcaOptionsX) == FALSE)
    pcaOptionsX = CheckPcaOptions(as.list(pcaOptionsX))

  res <- .EstimSur(y, x, addIntercept,
                   searchSigMaxIter, searchSigMaxProb,
                   restriction, newX, pcaOptionsY, pcaOptionsX,
                   simFixSize, simTrainRatio,
                   simTrainFixSize, simSeed,
                   simMaxConditionNumber, simTransform, printMsg)

  res$info$searchSigMaxIter = searchSigMaxIter
  res$info$addIntercept = addIntercept
  res$info$searchSigMaxProb=searchSigMaxProb
  res$info$simFixSize=simFixSize
  res$info$simTrainFixSize=simTrainFixSize
  res$info$simTrainRatio=simTrainRatio
  res$info$simSeed=simSeed
  res$info$simMaxConditionNumber=simMaxConditionNumber

  res
}

# get estimation from search result
GetEstim_sur <- function(searchRes, endoIndices,
                         exoIndices, y, x, printMsg, ...) {
  y <- y[, endoIndices, drop = FALSE]
  x <- if (is.null(exoIndices) || is.null(x)) {
    NULL
  } else {
    x[, c(exoIndices), drop = FALSE]
  }

  M <- estim.sur(
    y = y,
    x = x,
    addIntercept = FALSE,
    searchSigMaxIter = searchRes$info$searchSigMaxIter,
    searchSigMaxProb = searchRes$info$searchSigMaxProb,
    restriction = NULL,
    newX = NULL,
    pcaOptionsY = NULL,
    pcaOptionsX = NULL,
    simFixSize = searchRes$info$metricOptions$simFixSize,
    simTrainRatio = searchRes$info$metricOptions$trainRatio,
    simTrainFixSize = searchRes$info$metricOptions$trainFixSize,
    simSeed = abs(searchRes$info$metricOptions$seed),
    simMaxConditionNumber = searchRes$info$modelCheckItems$maxConditionNumber,
    simTransform = searchRes$info$metricOptions$transform,
    printMsg = printMsg
  )

  return(M)
}


#' Step-wise Search for Best SUR Models
#'
#' For a large model set, use this function to find the best seemingly unrelated regression models.
#' It selects a subset of variables from smaller models and moves to the bigger ones.
#'
#' @param y A matrix of endogenous data with variables in the columns.
#' @param x A matrix of exogenous data with variables in the columns.
#' @param xSizeSteps A list of model dimensions to be estimated in each step.
#' Its size determines the number of steps.
#' @param countSteps An integer vector to determine the number of variables to be used in each step.
#' \code{NA} means all variables. Variables are selected based on best estimations.
#' All variables in the best models (all metrics and targets) are selected until the corresponding suggested number is reached.
#' Select an appropriate value for \code{bestK} in the options.
#' @param savePre A directory for saving and loading the progress.
#' Each step's result is saved in a file (name=\code{paste0(savePre,i)} where \code{i} is the index of the step.
#' @param ... other arguments to pass to [search.sur] function such as the \code{numTargets} argument.
#' Note that \code{xSizes} is ineffective here.
#'
#' @return Similar to [search.sur] function.
#' @export
#'
#' @examples
#' # See the example in the 'search.sur' function.
#'
#' @seealso [search.sur], [estim.sur]
search.sur.stepwise <- function(y, x, xSizeSteps = list(c(1), c(2)),
                                countSteps = c(NA, 10),
                                savePre = NULL, ...) {

  if (is.null(savePre) == FALSE && (is.character(savePre) == FALSE || length(savePre) > 1))
    stop("'savePre' must be a character string.")

  Search_s("sur", data = x, sizes = xSizeSteps, counts = countSteps,
          savePre = savePre, y = y, ...)
}


sur.to.latex.eqs <- function(sigma, coef, intercept, numFormat = "%.2f") {
  num_y <- ncol(coef)
  num_x <- nrow(coef) - ifelse(intercept, 1, 0)
  eqs <- character(num_y)
  for (i in seq_len(num_y)) {
    b <- coef[, i]
    if (intercept) {
      eqs[i] <- paste0("Y_", i, " = ", sprintf0(numFormat, b[1]))
      if (num_x > 0) {
        eqs[i] <- paste0(eqs[i], " + ", paste0(sprintf0(numFormat, b[-1]), " X_", seq_len(num_x), collapse = " + "))
      }
    } else {
      eqs[i] <- paste0("Y_", i, " = ", paste0(sprintf0(numFormat, b), " X_", seq_len(num_x), collapse = " + "))
    }
    eqs[i] <- paste0(eqs[i], " + E_", i, ", \\sigma_",i,"^2 = ", sprintf0(numFormat, sigma[[i,i]]))
  }
  eqs_latex <- paste(eqs, collapse = " \\\\ ")

  return(eqs_latex)
}

sur.to.latex.mat <- function(sigma, coef, intercept = TRUE, numFormat = "%.2f",
                             num_x_break = 3, y_label = "Y", x_label= "X", e_label = "E") {
  num_y <- ncol(coef)
  num_x <- nrow(coef)

  y_vec <- latex.variable.vector(num_y, y_label)

  coef_t <- t(coef)
  x_mat <- latex.matrix(mat = coef_t, numFormat = numFormat)

  x_vec <- latex.variable.vector(num_x, x_label, intercept, num_x_break)

  e_vec <- latex.variable.vector(num_y, e_label)

  s_mat <- latex.matrix(mat = sigma, numFormat = numFormat)

  eq_latex <- paste0(y_vec, " = ", x_mat," ", x_vec," + ", e_vec, ", \\Sigma = ", s_mat)



  return(eq_latex)
}


#' Generate Random Sample from an SUR Model
#'
#' This function generates a random sample from an Seemingly Unrelated Regression model.
#'
#' @param sigma covariance matrix of the errors.
#' If it is an integer value, it specifies the number of equations in the SUR model and covariance matrix is generated randomly.
#' @param coef Coefficients of the model.
#' If it is an integer value, it specifies the number of independent variables in each equation of the SUR model and coefficient matrix is generated randomly.
#' @param nObs Number of observations to generate.
#' @param intercept If \code{TRUE}, an intercept is included in the model as the first exogenous variable.
#' @param numFormat A character string that determines how to format the numbers, to be used as the argument of the \code{sprintf} function.
#' If \code{NULL}, conversion to latex or html representations are disabled.
#'
#' @return A list with the following items:
#'   \item{y}{matrix, the generated dependent variable.}
#'   \item{x}{matrix, the generated independent variable.}
#'   \item{e}{matrix, the generated errors.}
#'   \item{sigma}{matrix, the covariance matrix of the disturbances.}
#'   \item{coef}{matrix, the coefficients used in the model.}
#'   \item{intercept}{logical, whether an intercept was included in the model.}
#'   \item{eqsLatex}{character string, Latex representation of the equations of the system.}
#'   \item{eqsLatexSys}{character string, Latex representation of the system in matrix form.}
#'
#' @export
#' @importFrom stats rnorm
#' @example man-roxygen/ex-sim.sur.R
#' @seealso [sim.varma],[estim.sur],[search.sur]
sim.sur <- function(sigma = 1L, coef = 1L,
                    nObs = 100, intercept = TRUE,
                    numFormat = "%.2f") {

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
                 intercept = intercept,
                 eqsLatex = ifelse(is.null(numFormat), NULL, sur.to.latex.eqs(sigma, coef, intercept, as.character(numFormat))),
                 eqsLatexSys = ifelse(is.null(numFormat), NULL, sur.to.latex.mat(sigma, coef, intercept, as.character(numFormat))))

  return(result)
}
