
#' Search for Best Discrete-Choice Models
#'
#' Use this function to create a discrete-choice model set and search for the best models (and other information) based on in-sample and out-of-sample evaluation metrics.
#'
#' @param y A matrix of endogenous data with variable in the column.
#' @param x A matrix of exogenous data with variables in the columns.
#' @param w Weights of the observations in \code{y}.
#' \code{NULL} means equal weights for all observations.
#' @param xSizes An integer vector specifying the number of exogenous variables in the regressions.
#' E.g., \code{c(1,2)} means the model set contains all regressions with 1 and 2 exogenous variables.
#' If \code{NULL}, \code{c(1)} is used.
#' @param xPartitions A list of integer vectors that partition the indexes of the exogenous variables.
#' No regression is estimated with two variables in the same partition.
#' If \code{NULL}, each variable is placed in its own group, and the size of the model set is maximized.
#' @param costMatrices A list of numeric matrices where each one determines how to score the calculated probabilities.
#' Given the number of choices \code{n}, a frequency cost matrix is an \code{m x n+1} matrix.
#' The first column determines the thresholds.
#' Elements in the \code{j}-th column determine the costs corresponding to the \code{j-1}-th choice in \code{y}.
#' It can be \code{NULL} if it is not selected in \code{metricOptions}.
#' @param searchLogit If \code{TRUE}, logit regressions are added to the model set.
#' @param searchProbit If \code{TRUE}, probit regressions are added to the model set.
#' @param optimOptions A list for Newton optimization options.
#' Use [get.options.newton] function to get the options.
#' @param aucOptions A list for AUC calculation options.
#' Use [get.options.roc] function to get the options.
#' @param metricOptions A list of options for measuring performance.
#' Use [get.options.metric] function to get them.
#' @param modelCheckItems A list of options for excluding a subset of the model set.
#' See and use [get.items.modelcheck] function to get them.
#' @param searchItems A list of options for specifying the purpose of the search.
#' See and use [get.items.search] function to get them.
#' @param searchOptions A list of extra options for performing the search.
#' See and use [get.options.search] function to get them.
#'
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
#' @importFrom stats glm
#'
#' @example man-roxygen/ex-search.bin.R
#'
#' @seealso [estim.bin], [search.bin.stepwise]
search.bin <- function(y, x, w = NULL, xSizes = NULL,
                      xPartitions = NULL, costMatrices = NULL,
                      searchLogit = TRUE, searchProbit = FALSE,
                      optimOptions = get.options.newton(), aucOptions = get.options.roc(),
                      metricOptions = get.options.metric(),
                      modelCheckItems = get.items.modelcheck(),
                      searchItems = get.items.search(),
                      searchOptions = get.options.search()){

  y = as.matrix(y)
  x = as.matrix(x)
  w = if (is.null(w)) NULL else as.matrix(w)
  xSizes = if (is.null(xSizes)) NULL else as.integer(xSizes)
  searchLogit = as.logical(searchLogit)
  searchProbit = as.logical(searchProbit)


  if (is.null(xPartitions) == FALSE){
    xPartitions = as.list(xPartitions)
    for (i in c(1:length(xPartitions)))
      xPartitions[[i]] = as.integer(xPartitions[[i]])
  }

  if (is.null(costMatrices) == FALSE){
    costMatrices = as.list(costMatrices)
    for (i in c(1:length(costMatrices)))
      costMatrices[[i]] = as.matrix(costMatrices[[i]])
  }

  if (is.null(optimOptions))
    optimOptions = get.options.newton()
  else
    optimOptions = CheckNewtonOptions(optimOptions)

  if (is.null(aucOptions))
    aucOptions = get.options.roc()
  else
    aucOptions = CheckRocOptions(aucOptions)

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

  res <- .SearchDc(y, x, w, xSizes, xPartitions, costMatrices,
                   searchLogit, searchProbit,
                   optimOptions, aucOptions, metricOptions ,
                   modelCheckItems, searchItems,
                   searchOptions)

  endTime <- Sys.time()

  res$info$startTime <- startTime
  res$info$endTime <- endTime

  res$info$isWeighted <- ifelse(is.null(w),FALSE, TRUE)

  res
}


#' Estimate a Discrete Choice Model
#'
#' Use this function to estimate a discrete choice model.
#'
#' @param y A matrix of endogenous data with variable in the column.
#' @param x A matrix of exogenous data with variables in the columns.
#' @param w Weights of the observations in \code{y}.
#' \code{NULL} means equal weights for all observations.
#' @param linkFunc A character string that shows the probability assumption. It can be \code{logit} or \code{probit}.
#' @param newX A numeric matrix where for each row in it, probabilities are projected and reported. It can be \code{NULL}.
#' @param pcaOptionsX A list of options to use principal components of the \code{x}, instead of the actual values. Set \code{NULL} to disable. Use [get.options.pca()] for initialization.
#' @param costMatrices A list of numeric matrices where each one determines how to score the calculated probabilities. See and use [search.bin] for more information and initialization.
#' @param aucOptions A list of options for AUC calculation. See and use \code{[get.options.roc()]} for more information and initialization.
#' @param simFixSize An integer that determines the number of out-of-sample simulations. Use zero to disable the simulation.
#' @param simTrainFixSize An integer representing the number of data points in the training sample in the out-of-sample simulation. If zero, \code{trainRatio} will be used.
#' @param simTrainRatio A number representing the size of the training sample relative to the available size, in the out-of-sample simulation. It is effective if \code{trainFixSize} is zero.
#' @param simSeed A seed for the random number generator. Use zero for a random value.
#' @param weightedEval If \code{TRUE}, weights will be used in evaluations.
#' @param printMsg Set to \code{TRUE} to enable printing some details.
#'
#' @return A nested list with the following items:
#' \item{counts}{Information about different aspects of the estimation such as the number of observation, number of exogenous variables, etc.}
#' \item{estimations}{Estimated coefficients, standard errors, z-statistics, p-values, etc.}
#' \item{metrics}{Value of different goodness of fit and out-of-sample performance metrics.}
#' \item{projections}{Information on the projected values, if \code{newX} is provided.}
#' \item{info}{Some other general information.}
#'
#' @details
#'
#' Binary regression is a statistical technique used to estimate the probability of one of two possible outcomes, represented by a binary dependent variable that takes on two values, such as 0 and 1.
#' This is achieved by modeling the relationship between one or more independent variables and the binary dependent variable.
#' The most commonly used binary regression models are the logit model, also known as logistic regression, and the probit model, also known as probit regression.
#' In general, a binary regression model can be written as \eqn{f(p) = \beta_0 + \beta_1x_1 + \ldots + \beta_kx_k}, where \eqn{p} is the probability that \eqn{y} is 1 and \eqn{f} is the link function.
#' For logistic regression, the logit function is used as the link function: \eqn{f(p) = \ln{\frac{p}{1-p}}}.
#' For probit regression, the probit function is used as the link function: \eqn{f(p) = \Phi^{-1}(p)}, where \eqn{\Phi^{-1}} is the inverse cumulative distribution function of the standard normal distribution.
#' The parameters of the binary regression model are estimated using maximum likelihood estimation.
#'
#' Note that the focus in \code{ldt} is model uncertainty and the main purpose of exporting this method is to show the inner calculations of the search process in [search.bin] function.
#'
#' @export
#' @example man-roxygen/ex-estim.bin.R
#' @seealso [search.bin], [search.bin.stepwise]
estim.bin <- function(y, x, w = NULL,
                     linkFunc = c("logit", "probit"), newX = NULL,
                     pcaOptionsX = NULL, costMatrices = NULL,
                     aucOptions = get.options.roc(), simFixSize = 0,
                     simTrainFixSize = 0, simTrainRatio = 0.75,
                     simSeed = 0, weightedEval = FALSE, printMsg = FALSE)
{

  y = as.matrix(y)
  x = as.matrix(x)
  w = if (is.null(w)) NULL else as.matrix(w)
  newX = if (is.null(newX)) NULL else as.matrix(newX)
  simFixSize = as.integer(simFixSize)
  simTrainRatio = as.numeric(simTrainRatio)
  simTrainFixSize = as.integer(simTrainFixSize)
  simSeed = as.integer(simSeed)
  weightedEval = as.logical(weightedEval)
  printMsg = as.logical(printMsg)

  linkFunc <- match.arg(as.character(linkFunc), c("logit", "probit"))

  if (is.null(pcaOptionsX) == FALSE)
    pcaOptionsX = CheckPcaOptions(as.list(pcaOptionsX))

  if (is.null(costMatrices) == FALSE){
    costMatrices = as.list(costMatrices)
    for (i in c(1:length(costMatrices)))
      costMatrices[[i]] = as.matrix(costMatrices[[i]])
  }

  if (is.null(aucOptions))
    aucOptions = get.options.roc()
  else
    aucOptions = CheckRocOptions(aucOptions)

  res <- .EstimDc(y, x, w, linkFunc, newX,
                  pcaOptionsX, costMatrices,
                  aucOptions, simFixSize, simTrainRatio,
                  simTrainFixSize, simSeed,
                  weightedEval, printMsg)
  res
}

# get estimation from search result
GetEstim_bin <- function(searchRes, endoIndices, exoIndices, y, x, printMsg, w, linkFunc, newX, ...) {
  M <- estim.bin(y,
                x = if (is.null(exoIndices) || is.null(x)) NULL else x[, exoIndices, drop = FALSE],
                w = w,
                linkFunc = linkFunc,
                newX = if (is.null(exoIndices) || is.null(newX)) NULL else newX[, exoIndices, drop=FALSE],
                pcaOptionsX = NULL,
                costMatrices = searchRes$info$costMatrices,
                simFixSize = searchRes$info$metricOptions$simFixSize,
                simTrainRatio = searchRes$info$metricOptions$trainRatio,
                simTrainFixSize = searchRes$info$metricOptions$trainFixSize,
                simSeed = abs(searchRes$info$metricOptions$seed),
                printMsg = printMsg
  )

  return(M)
}

#' Step-wise Search for Best Discrete-Choice Models
#'
#' For a large model set, use this function to find the best discrete choice models.
#' It selects a subset of variables from smaller models and moves to the bigger ones.
#'
#' @param y A matrix of endogenous data with variable in the column.
#' @param x A matrix of exogenous data with variables in the columns.
#' @param xSizeSteps A list of model dimensions to be estimated in each step. Its size determines the number of steps.
#' @param countSteps A integer vector to determine the number of variables to be used in each step.
#' \code{NA} means all variables. Variables are selected based on best estimations.
#' All variables in the best models (all metrics and targets) are selected until the corresponding suggested number is reached.
#' Select an appropriate value for \code{bestK} in the options.
#' @param savePre A directory for saving and loading the progress.
#' Each step's result is saved in a file (name=\code{paste0(savePre,i)} where \code{i} is the index of the step.
#' @param ... other arguments to pass to [search.bin()] function such as the \code{w} argument.
#' Note that \code{xSizes} is ineffective here.
#'
#' @return Similar to [search.bin] function.
#' @export
#'
#' @examples
#' # See the example in the 'search.bin' function.
#'
#' @seealso [search.bin], [estim.bin]
search.bin.stepwise <- function(y, x, xSizeSteps = list(c(1), c(2)),
                               countSteps = c(NA, 10),
                               savePre = NULL, ...) {
  Search_s("bin", x, xSizeSteps, countSteps, savePre, y = y, ...)
}


bin.to.latex.eq <- function(coef, probit, numFormat = "%.2f") {

  xNames <- paste0("X_",c(1:length(coef)))

  terms <- character(length(coef))
  terms[1] <- sprintf0(numFormat, coef[1])
  for (i in seq_along(coef)[-1]) {
    # avoid +- signs
    if (coef[i] >= 0) {
      sign <- " + "
    } else {
      sign <- " - "
    }
    terms[i] <- paste0(sign, sprintf(numFormat, abs(coef[i])), " ", xNames[i])
  }

  formula_str <- paste(terms, collapse = "")

  cond_str = ifelse(length(xNames) == 4, paste(xNames[-1], collapse = ", "), paste(c(xNames[2], "...",xNames[length(xNames)]), collapse = ", "))

  if (probit) {
    formula_str <- paste0("P(Y = 1 | ", cond_str, ") = \\Phi(", formula_str, ")")
  } else {
    formula_str <- paste0("P(Y = 1 | ", cond_str, ") = \\frac{1}{1 + e^{-(", formula_str, ")}}")
  }

  return(formula_str)
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
#' @param pPos The percentage of positive observations (\code{y=1}) in the dependent variable y.
#'   Must be between 0 and 1.
#'   In the current implementation, this is independent of the weights, if \code{maxWeight} is larger than 1.
#' @param sampleFactor The factor used to control the size of the initial sample.
#'   A larger value generates a larger initial sample, which can increase the accuracy
#'   of the generated sample but also takes more time and memory.
#' @param numFormat A character string that determines how to format the numbers, to be used as the argument of the \code{sprintf} function.
#' If \code{NULL}, conversion to latex or html representations are disabled.
#' @param toNumeric If \code{TRUE}, \code{y} and \code{w} are transformed to have numeric vector.
#' Otherwise, they contain an integer vector.
#'
#' @return A list with the following items:
#'   \item{y}{The dependent variable.}
#'   \item{x}{The independent variables.}
#'   \item{w}{The weights of the observations. It is null if \code{weighted} is \code{FALSE}.}
#'   \item{p1}{Prob(Y=1)}
#'   \item{coef}{The coefficients of the regression.}
#'   \item{probit}{Logical value indicating whether data was generated from a probit model.}
#'   \item{pPos}{The percentage of negative observations in y.}
#'   \item{eqLatex}{Latex representation of the model formula.}
#'
#' @export
#' @importFrom stats pnorm rnorm rbinom
#' @example man-roxygen/ex-sim.bin.R
#'
#' @seealso [estim.bin], [search.bin]
sim.bin <- function(coef = 2L, nObs = 100, probit = FALSE,
                    maxWeight = 1, pPos = 0.5,
                    sampleFactor = 4, numFormat = "%.2f", toNumeric = TRUE) {

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

  # Generate the independent variables
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

  # Generate the dependent variable
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
       probit = probit, pPos = pPos,
       eqLatex = bin.to.latex.eq(coef, probit, numFormat))
}

