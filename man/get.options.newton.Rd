% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/options.R
\name{get.options.newton}
\alias{get.options.newton}
\title{Get Options for Newton Optimization}
\usage{
get.options.newton(
  maxIterations = 100,
  functionTol = 1e-04,
  gradientTol = 0,
  useLineSearch = TRUE
)
}
\arguments{
\item{maxIterations}{An integer representing the maximum number of iterations.}

\item{functionTol}{A small value used to test the convergence of the objective function.}

\item{gradientTol}{A small value used to test the convergence of the gradient.}

\item{useLineSearch}{If \code{TRUE}, line search is used.}
}
\value{
A list with the given options.
}
\description{
Use this function to get optimization options in \link{estim.bin} or \link{search.bin} functions.
}
