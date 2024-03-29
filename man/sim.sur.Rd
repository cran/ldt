% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sur.R
\name{sim.sur}
\alias{sim.sur}
\title{Generate Random Sample from an SUR Model}
\usage{
sim.sur(sigma = 1L, coef = 1L, nObs = 100, intercept = TRUE)
}
\arguments{
\item{sigma}{covariance matrix of the errors.
If it is an integer value, it specifies the number of equations in the SUR model and covariance matrix is generated randomly.}

\item{coef}{Coefficients of the model.
If it is an integer value, it specifies the number of exogenous variables in each equation of the SUR model and coefficient matrix is generated randomly.}

\item{nObs}{Number of observations to generate.}

\item{intercept}{If \code{TRUE}, an intercept is included in the model as the first exogenous variable.}
}
\value{
A list with the following items:
\item{y}{matrix, the generated endogenous variable(s).}
\item{x}{matrix, the generated exogenous variable(s).}
\item{e}{matrix, the generated errors.}
\item{sigma}{matrix, the covariance matrix of the disturbances.}
\item{coef}{matrix, the coefficients used in the model.}
\item{intercept}{logical, whether an intercept was included in the model.}
}
\description{
This function generates a random sample from an Seemingly Unrelated Regression model.
}
\examples{
num_y <- 2L
num_x <- 3L
n_obs = 100
data <- sim.sur(sigma = num_y, coef = num_x, nObs = n_obs)

# see the examples in 'estim.sur' or 'search.sur' functions
}
\seealso{
\link{sim.varma},\link{estim.sur},\link{search.sur}
}
