% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{GetLmbfgsOptions}
\alias{GetLmbfgsOptions}
\title{Options for LMBFGS Optimization}
\usage{
GetLmbfgsOptions(
  maxIterations = 100L,
  factor = 1e+07,
  projectedGradientTol = 0,
  maxCorrections = 5L
)
}
\arguments{
\item{maxIterations}{(int) A positive integer for maximum number of iterations.}

\item{factor}{(double) A condition for stopping the iterations. The iteration will stop when (f^k - f^{k+1})/max{|f^k|,|f^{k+1}|,1} < \code{factor}*epsmch where epsmch is the machine precision, which is automatically generated by the code. Use e.g., 1e12 for low accuracy, 1e7 (default) for moderate accuracy and 1e1 for extremely high accuracy. default is 1e7}

\item{projectedGradientTol}{(double) The iteration will stop when \code{max{|proj g_i | i = 1, ..., n} < projectedGradientTol} where \code{pg_i} is the ith component of the projected gradient. default is zero.}

\item{maxCorrections}{(int) Maximum number of variable metric corrections allowed in the limited memory Matrix. default is 5.}
}
\value{
A list with the given options.
}
\description{
Options for LMBFGS Optimization
}
