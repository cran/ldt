% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{VarmaSearch}
\alias{VarmaSearch}
\title{VARMA Search}
\usage{
VarmaSearch(
  y,
  x = NULL,
  numTargets = 1L,
  ySizes = NULL,
  yPartitions = NULL,
  xGroups = NULL,
  maxParams = NULL,
  seasonsCount = 0L,
  maxHorizon = 0L,
  newX = NULL,
  interpolate = TRUE,
  adjustLeadsLags = TRUE,
  simUsePreviousEstim = TRUE,
  olsStdMultiplier = 2,
  lmbfgsOptions = NULL,
  measureOptions = NULL,
  modelCheckItems = NULL,
  searchItems = NULL,
  searchOptions = NULL
)
}
\arguments{
\item{y}{(numeric vector) Endogenous data with variables in the columns.}

\item{x}{(nullable numeric matrix) Exogenous data with variables in the columns. It can be null.}

\item{numTargets}{(int) Number of variables in the first columns of \code{y}, regarded as targets. It must be positive and cannot be larger than the number of endogenous variables.}

\item{ySizes}{(nullable integer vector) Determines the number of endogenous variables (or equations) in the regressions.}

\item{yPartitions}{(nullable list of int vector) A partition over the indexes of the endogenous variables. No regression is estimated with two variables in the same group. If \code{NULL}, each variable is placed in its own group.}

\item{xGroups}{(nullable list of int vector) different combinations of the indexes of the exogenous variables to be used as exogenous variables in the SUR regressions.}

\item{maxParams}{(integer vector, length=6) Maximum values for the parameters of the VARMA model (p,d,q,P,D,Q). If null, c(1,1,1,0,0,0) is used.}

\item{seasonsCount}{(integer) number of observations per unit of time}

\item{maxHorizon}{(integer) maximum value for the prediction horizon if \code{type1} is \code{TRUE} in \code{checkItems}. Also, it is used as the maximum prediction horizon in checking the predictions.}

\item{newX}{(matrix) New exogenous data for out-of-sample prediction. It must have the same number of columns as \code{x}.}

\item{interpolate}{(logical) if \code{TRUE}, missing observations are interpolated.}

\item{adjustLeadsLags}{(logical) if \code{TRUE}, leads and lags in the sample are adjusted.}

\item{simUsePreviousEstim}{(logical) if \code{TRUE}, parameters are initialized in just the first step of the simulation. The initial values of the n-th simulation (with one more observation) is the estimations in the previous step.}

\item{olsStdMultiplier}{(numeric) a multiplier for the standard deviation of OLS, used for restricting the maximum likelihood estimation.}

\item{lmbfgsOptions}{(list) Optimization options. see \code{[GetLmbfgsOptions()]}. Use null for default values.}

\item{measureOptions}{(nullable list) see \code{[GetMeasureOptions()]}.}

\item{modelCheckItems}{(nullable list) see \code{[GetModelCheckItems()]}.}

\item{searchItems}{(nullable list) see \code{[GetSearchItems()]}.}

\item{searchOptions}{(nullable list) see \code{[GetSearchOptions()]}.}
}
\value{
A list
}
\description{
VARMA Search
}
