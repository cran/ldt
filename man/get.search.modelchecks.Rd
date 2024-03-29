% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/search.R
\name{get.search.modelchecks}
\alias{get.search.modelchecks}
\title{Set Options to Exclude a Model Subset}
\usage{
get.search.modelchecks(
  estimation = TRUE,
  maxConditionNumber = Inf,
  minObsCount = 0,
  minDof = 0,
  minOutSim = 0,
  minR2 = -Inf,
  maxAic = Inf,
  maxSic = Inf,
  prediction = FALSE,
  predictionBound = 10
)
}
\arguments{
\item{estimation}{If \code{TRUE}, the model is estimated with all data and is ignored if this estimation fails. If \code{FALSE}, you might get a 'best model' that cannot be estimated.}

\item{maxConditionNumber}{A number used to ignore an estimation that has a high condition number (if implemented in the search).}

\item{minObsCount}{An integer used to ignore an estimation where the number of observations (after dealing with \code{NA}) is low. Use 0 to disable this check.}

\item{minDof}{An integer used to ignore an estimation with low degrees of freedom (equation-wise). Use 0 to disable this check.}

\item{minOutSim}{An integer used to ignore estimations with a low number of out-of-sample simulations (if implemented in the search).}

\item{minR2}{A number used to ignore estimations with a low value for 'R2' (if implemented in the search).}

\item{maxAic}{A number used to ignore estimations with a high 'AIC' (if implemented in the search).}

\item{maxSic}{A number used to ignore estimations with a high 'SIC' (if implemented in the search).}

\item{prediction}{If \code{TRUE}, model data is predicted given all data and is ignored if this process fails. If \code{FALSE}, you might get a 'best model' that cannot be used for prediction.}

\item{predictionBound}{A list containing two matrices: \code{lower} and \code{upper}, which represent the bounds for checking predictions. Each column corresponds to a target variable, and each row corresponds to a horizon. If the data has been transformed using a Box-Cox transformation, these bounds will be compared with the transformed data.
Alternatively, \code{predictionBound} can be a numeric value. In this case, the bounds are created by creating a confidence interval, assuming normality and using mean and standard errors of the growth rates.
Any model that produces a prediction outside of these bounds will be ignored. To disable this check, set \code{predictionBound} to \code{NULL}.}
}
\value{
A list with the given options.
}
\description{
Use this function to determine which models should be skipped in the search process.
}
