% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/varma.R
\name{estim.varma.model.string}
\alias{estim.varma.model.string}
\title{Get the Specification of an \code{ldt.estim.varma} Model}
\usage{
estim.varma.model.string(obj)
}
\arguments{
\item{obj}{AN object of class \code{ldt.estim.varma}.}
}
\value{
A character string representing the specification of the model.
}
\description{
Use this function to get the name of a VARMA model, such that:
If It is multivariate, it will be VAR, otherwise AR;
If moving average terms are present, it will be ARMA or VARMA;
If it is seasonal, it will be S-ARMA or S-VARMA;
If it is integrated, it will be S-ARMA (D=?,d=?); ..., and any possible combination.
Parameters will be reported in parenthesis after the name of the model.
}
