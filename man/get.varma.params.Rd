% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/varma.R
\name{get.varma.params}
\alias{get.varma.params}
\title{Split VARMA parameter into its Components}
\usage{
get.varma.params(
  coef,
  numAR = 1,
  numMA = 0,
  numExo = 0,
  intercept = TRUE,
  numAR_s = 0,
  numMA_s = 0,
  numSeasons = 1
)
}
\arguments{
\item{coef}{A matrix of coefficients with dimensions \code{numEq} x \code{numAR * numEq + numMA * numEq + numExo + ifelse(intercept, 1, 0)}.}

\item{numAR}{A non-negative integer scalar specifying the number of AR lags.}

\item{numMA}{A non-negative integer scalar specifying the number of MA lags.}

\item{numExo}{A non-negative integer scalar specifying the number of exogenous variables.}

\item{intercept}{A logical scalar indicating whether an intercept is included in the model.}

\item{numAR_s}{A non-negative integer scalar specifying the number of seasonal AR lags.}

\item{numMA_s}{A non-negative integer scalar specifying the number of seasonal MA lags.}

\item{numSeasons}{A non-negative integer scalar specifying the number of seasons.}
}
\value{
A list with the following items:
\itemize{
\item \code{arList}: A list containing the AR coefficients for each lag.
\item \code{intercept}: A numeric vector of length \code{numEq} containing the intercept, or \code{NULL} if \code{intercept = FALSE}.
\item \code{exoCoef}: A matrix of dimensions \code{numEq} x \code{numExo} containing the exogenous coefficients, or \code{NULL} if \code{numExo = 0}.
\item \code{maList}: A list containing the MA coefficients for each lag.
}
}
\description{
Use this function to extract AR, MA, intercept, and exogenous coefficients from the VARMA estimation.
}
\examples{
# see 'search.varma' or 'estim.varma' functions.

}
\seealso{
\link{estim.varma}
}
