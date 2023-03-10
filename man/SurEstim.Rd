% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{SurEstim}
\alias{SurEstim}
\title{Estimates an SUR Model}
\usage{
SurEstim(
  y,
  x,
  addIntercept = TRUE,
  searchSigMaxIter = 0L,
  searchSigMaxProb = 0.1,
  restriction = NULL,
  newX = NULL,
  pcaOptionsY = NULL,
  pcaOptionsX = NULL,
  simFixSize = 0L,
  simTrainRatio = 0.75,
  simTrainFixSize = 0L,
  simSeed = 0L,
  simMaxConditionNumber = 1.7e+308,
  printMsg = FALSE
)
}
\arguments{
\item{y}{(numeric matrix) Endogenous data with variables in the columns.}

\item{x}{(numeric matrix) Exogenous data with variables in the columns.}

\item{addIntercept}{(bool) If true, intercept is added automatically to x.}

\item{searchSigMaxIter}{(int) Maximum number of iterations in searching for significant coefficients. Use 0 to disable the search.}

\item{searchSigMaxProb}{(double) Maximum value of type I error to be used in searching for significant coefficients. If p-value is less than this, it is interpreted as significant and removed in the next iteration (if any exists).}

\item{restriction}{(nullable numeric matrix) A km x q matrix in which m=ncols(y), k=ncols(x) and q is the number of unrestricted coefficients.}

\item{newX}{(nullable numeric matrix) Data of new exogenous variables to be used in the predictions. Its columns must be the same as \code{x}. If null, projection is disabled.}

\item{pcaOptionsY}{(nullable list) A list of options in order to use principal components of the \code{y}, instead of the actual values. Set null to disable. Use \code{[GetPcaOptions()]} for initialization.}

\item{pcaOptionsX}{(nullable list) Similar to \code{pcaOptionsY} but for \code{x}. see \code{pcaOptionsY}.}

\item{simFixSize}{(int) Number of pseudo out-of-sample simulations. Use zero to disable the simulation. See also \code{GetMeasureOptions()]}.}

\item{simTrainRatio}{(double) Size of the training sample as a ratio of the number of the observations. It is effective only if \code{simTrainFixSize} is zero.}

\item{simTrainFixSize}{(int) A fixed size for the training sample. If zero, \code{simTrainRatio} is used.}

\item{simSeed}{(int) A seed for the pseudo out-of-sample simulation.}

\item{simMaxConditionNumber}{(double) Maximum value for the condition number in the simulation.}

\item{printMsg}{(bool) Set true to enable printing details.}
}
\value{
A list:
}
\description{
Estimates an SUR Model
}
