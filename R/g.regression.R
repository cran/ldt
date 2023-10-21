
checkEquation <- function(object, equations, justOne){
  if (justOne && is.null(equations))
    stop("'equation' cannot be null.")
  if (justOne && length(equations) != 1)
    stop("Length of 'equation' must be one.")
  if (justOne == FALSE && is.null(equations))
    equations <- c(1L:object$info$data$numEndo)
  stopifnot(is.numeric(equations) || is.character(equations))
  if (is.numeric(equations))
    equations <- as.integer(equations)
  else
    equations <- match(equations, colnames(object$info$data$data))
  if (any(equations < 1 | equations > object$info$data$numEndo)){
    if (justOne)
      stop("'equation' is not found.")
    else
      stop("At least one element in 'equations' is not valid or not found.")
  }
  equations
}

#' Extract Endogenous Variable(s) Data
#'
#' This function extracts data of a endogenous variable(s) from an estimated model.
#'
#' @param object An object of class \code{ldt.estim}.
#' @param equations A number, a numeric array or a string array specifying the indices or names of the endogenous variables in the equations. \code{NULL} means all equations.
#' @param ... Additional arguments.
#'
#' @return A data frame containing the endogenous data.
#'
#' @export
endogenous <- function(object, equations = NULL, ...){
  equations <- checkEquation(object, equations, FALSE)
  object$estimations$Y[, equations, drop=FALSE]
}

#' Extract Exogenous Variable(s) Data
#'
#' This function extracts data of an exogenous variable(s) in an equation from an estimated model.
#' It takes zero restrictions imposed into account.
#'
#' @param object An object of class \code{ldt.estim}.
#' @param equation A number or a string specifying the equation with exogenous data.
#' @param ... Additional arguments.
#'
#' @return A matrix containing the exogenous data.
#'
#' @export
exogenous <- function(object, equation = 1, ...){
  equation <- checkEquation(object, equation, TRUE)
  X <- object$estimations$X
  if (!is.null(object$estimations$isRestricted)){
    not_restricted <- as.logical(1 - object$estimations$isRestricted[,equation])
    X <- X[,not_restricted]
  }
  X
}


#' Extract Coefficients Matrix
#'
#' This function extracts coefficient matrix from an \code{ldt.estim} object.
#'
#' @param object An object of class \code{ldt.estim}
#' @param equations A number, a numeric array or a string array specifying the indices or names of the endogenous variables in the equations.
#' \code{NULL} means all equations.
#' @param removeZeroRest If \code{TRUE} and model is restricted, zero restrictions are removed.
#' @param ... Additional arguments.
#'
#' @return If zero restrictions are not removed, it is a matrix containing the coefficients of the system.
#' Each column of the matrix belongs to an equation. Explanatory variables are in the rows.
#' Otherwise, coefficients of different equations are reported in a list.
#' @export
coef.ldt.estim <- function(object, equations = NULL, removeZeroRest = FALSE, ...){
  equations <- checkEquation(object, equations, FALSE)
  c <- object$estimations$coefs[, equations]
  if (removeZeroRest == FALSE ||
      is.null(object$estimations$isRestricted) ||
      sum(object$estimations$isRestricted) == 0)
    return(c)

  result <- list()
  for (j in 1:ncol(c)){
    notRest <- as.logical(1 - object$estimations$isRestricted[,j])
    result[[colnames(c)[j]]] <- c[,j,drop=FALSE][notRest]
  }
  result
}


hatvalues0 <- function(object, equation, addNA = TRUE){
  #TODO:can be more efficient
  method <- tolower(attr(object, "method"))

  X <- exogenous(object, equation = equation) # it takes PCA and restrictions into account

  H <- X %*% solve(t(X) %*% X) %*% t(X)
  res <- diag(H)

  if (addNA && method == "varma"){
    omit_obs <- object$info$data$obsCount - nrow(object$estimations$resid)
    res <- c(rep(NA, omit_obs), res)
  }
  res
}

cooks.distance0 <- function(object, equation, addNA = TRUE){
  method <- tolower(attr(object, "method"))
  omit_obs <- 0
  if (method == "varma")
    omit_obs <- object$info$data$obsCount - nrow(object$estimations$resid)

  h <- hatvalues0(object, equation, FALSE)
  r <- residuals.ldt.estim(object, equation)
  if (omit_obs > 0)
    r <- r[(omit_obs+1):length(r)]

  mse <- sum(r^2)/length(r)
  c <- coef.ldt.estim(object, equations = equation, removeZeroRest = TRUE)
  p <- length(c[1])
  if (object$info$data$hasWeight){
    stop("Cook's distance is not implemented for weighted observations.") # couldn't find a reference
  }
  else
    res <- r^2 * h / (p*mse*(1-h^2))

  if (addNA && omit_obs > 0)
    res <- c(rep(NA, omit_obs), res)
  res
}

#' Extract Residuals Data
#'
#' This function returns residuals from or calculates the standardized residuals for an \code{ldt.estim} object.
#'
#' @param object An object of class \code{ldt.estim}.
#' @param equations A number, a numeric array or a string array specifying the equations with residual data. If \code{NULL}, residuals in all equations are returned.
#' @param standardized If \code{TRUE}, standardized residuals are returned. See details.
#' @param pearson If \code{TRUE}, it returns (or uses) Pearson residuals for binary choice regression.
#' @param ... Additional arguments.
#'
#' @details
#' The standardized residuals have identical variance.
#' In order to calculate the standardized residuals, each residual is divided by \eqn{s\sqrt{w_i(1-h_{ii})}} where \eqn{s} is the standard error of residuals and \eqn{h_{ii}} is the leverage of \eqn{i}-th observation. \eqn{w_i} is the weight of the \eqn{i}-th observation if data is weighted, and 1 otherwise.
#' Note that while the residuals are estimated in a system, the \eqn{h_{ii}} is calculated in a univariate context as the \eqn{i}-th diagonal of \eqn{X(X'X)^{-1}X'} matrix, where \eqn{X} is the exogenous variables in the corresponding equation.
#'
#' @return A matrix containing the residuals data.
#' @export
residuals.ldt.estim <- function(object, equations = NULL, standardized = FALSE, pearson = TRUE, ...){
  method <- tolower(attr(object, "method"))
  stopifnot(is.logical(standardized))
  equations <- checkEquation(object, equations, FALSE)

  if (method == "binary" && pearson)
    resid <- object$estimations$residPearson[, equations, drop=FALSE]
  else
    resid <- object$estimations$resid[, equations, drop=FALSE]

  if (standardized) {
    for (i in equations){
      hii <- hatvalues0(object, equation = i, addNA = FALSE)
      s <- sd(resid)
      if (object$info$data$hasWeight){
        weights <- object$info$data[,object$info$data$numEndo + 1]
        resid[,i] <- resid[,i] / (s * weights * sqrt(1 - hii))
      }
      else{
        resid[,i] <- resid[,i] / (s * sqrt(1 - hii))
      }
    }
  }

  omit_obs <- object$info$data$obsCount - nrow(object$estimations$resid) # for VARMA
  if (method == "varma")
    resid <- rbind(matrix(NA, ncol = ncol(resid), nrow = omit_obs), resid)


  resid
}

#' Extract Fitted Data
#'
#' This function calculates and returns fitted values for an \code{ldt.estim} object.
#'
#' @param object An object of class \code{ldt.estim}.
#' @param equations A number, a numeric array or a string array specifying the equations with residual data. If \code{NULL}, residuals in all equations are returned.
#' @param ... Additional arguments.
#'
#' @return A matrix containing the exogenous data.
#'
#' @export
fitted.ldt.estim <- function(object, equations = NULL, ...){
  equations <- checkEquation(object, equations, FALSE)
  resid <- residuals.ldt.estim(object, equations = equations, standardized = FALSE, pearson = FALSE)
  y <- object$info$data$data[, equations, drop=FALSE]
  y - resid
}

#' Extract Maximum Log-Likelihood
#'
#' This function extracts maximum log-likelihood from an \code{ldt.estim} object.
#'
#' @param object An object of class \code{ldt.estim}
#' @param ... Additional arguments.
#'
#' @return The value of the maximum log-likelihood for the whole system.
#' @export
logLik.ldt.estim <- function(object, ...){
  #equation <- checkEquation(object, equation, TRUE) It is not equation-specific
  object$metrics["logL", 1]
}

#' Akaike Information Criterion
#'
#' This function extracts Akaike information criterion from an \code{ldt.estim} object.
#'
#' @param object An object of class \code{ldt.estim}
#' @param ... Additional arguments.
#' @param k Unused parameter.
#'
#' @return The value of AIC for the whole system.
#' #' @importFrom stats AIC
#' @export
AIC.ldt.estim <- function(object, ..., k = NA){
  #equation <- checkEquation(object, equation, TRUE) It is not equation-specific
  object$metrics["aic", 1]
}

#' Bayesian Information Criterion
#'
#' This function extracts Baysian information criterion from an \code{ldt.estim} object.
#'
#' @param object An object of class \code{ldt.estim}
#' @param ... Additional arguments.
#'
#' @return The value of BIC for the whole system.
#' @importFrom stats BIC
#' @export
BIC.ldt.estim <- function(object, ...){
  #equation <- checkEquation(object, equation, TRUE) It is not equation-specific
  object$metrics["sic", 1]
}



#' Extract Prediction Results
#'
#' This function extracts predicted mean and its variance from an \code{ldt.estim} object.
#' new data must be provided while estimating the model.
#'
#' @param object An object of class \code{ldt.estim}
#' @param ... Additional arguments.
#'
#' @return A list containing the predicted (projected) means and variances.
#' @export
predict.ldt.estim <- function(object, ...){
  if (is.null(object))
    stop("object is null.")
  if (!inherits(object, "ldt.estim"))
    stop("Invalid class. An 'ldt.estim' object is expected.")

  res <- list()
  res$means <- object$projection$means
  res$vars <- object$projection$vars

  res$method <- attr(object, "method")
  res$newX <- object$info$data$newX
  class(res) <- c("ldt.estim.projection", "list")
  res
}

#' Extract Prediction Results from a \code{ldt.estim.varma} Object
#'
#' This function extracts predicted mean and its variance from an \code{ldt.estim.varma} object.
#' new data must be provided while estimating the model.
#'
#' @param object An object of class \code{ldt.estim.varma}
#' @param actualCount Number of actual observations to be included in the result.
#' @param startFrequency Frequency of the first observation used in the estimation.
#' This is object of class \code{ldtf}.
#' @param ... Additional arguments.
#'
#' @return An object of class \code{ldt.varma.prediction}, which is a list with predicted \code{means} and (if available) \code{variances}.
#'
#' @export
predict.ldt.estim.varma <- function(object,
                                    actualCount = 0,
                                    startFrequency = NULL,
                                    ...){
  if (is.null(object))
    stop("object is null.")
  if (!inherits(object, "ldt.estim.varma"))
    stop("Invalid class. An 'ldt.estim.varma' object is expected.")
  if (is.null(object$prediction) || is.null(object$prediction$means))
    stop("Predictions are not available. Make sure you requested prediction in the 'estim.varma(...)' function.")

  if (is.na(actualCount))
    actualCount <- nrow(object$estimations$Y)

  hasVar = !is.null(object$prediction$vars)
  if (is.null(startFrequency))
    startFrequency = object$info$startFrequency
  if (is.null(startFrequency))
    startFrequency = tdata::f.cross.section(1)

  # get predictions:
  if (object$prediction$startIndex > 1)
    preds <- t(object$prediction$means[,-(seq_len(object$prediction$startIndex-1)), drop = FALSE])
  else
    preds <- t(object$prediction$means)
  # get actual
  if (actualCount > 0){
    actuals <- tail(object$estimations$Y, actualCount)
    preds <- rbind(actuals, preds)
  }
  preds_var <- NULL
  if (hasVar){
    if (object$prediction$startIndex > 1)
      vars <- t(object$prediction$vars[,-(1:(object$prediction$startIndex-1)), drop = FALSE])
    else
      vars <- t(object$prediction$vars)
    if (actualCount > 0)
      vars <- rbind(matrix(0,nrow = actualCount, ncol = ncol(preds)), # use 0 for plotting
                    vars)
  }

  dstart <- tdata::next.freq(startFrequency, nrow(object$estimations$Y) - actualCount)
  freqs <- tdata::get.seq0(dstart, nrow(preds))

  rownames(preds) <- freqs
  rownames(vars) <- freqs

  attr(preds, "ldtf") <- dstart
  attr(vars, "ldtf") <- dstart

  res <- list(means = preds,
              vars = vars,
              actualCount = actualCount)

  if (!is.null(object$simulation))
    res$simulation <- object$simulation
  res$lambda <- object$info$data$lambdas

  # We should not use use Box-cox lambdas in this function for transforming variances
  # In plotting, there will be an option to transform the intervals

  class(res) <- c("ldt.varma.prediction", "list")
  res
}





