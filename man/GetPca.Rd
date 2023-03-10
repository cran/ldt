% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{GetPca}
\alias{GetPca}
\title{Principle Component Analysis}
\usage{
GetPca(x, center = TRUE, scale = TRUE, newX = NULL)
}
\arguments{
\item{x}{(numeric matrix) data with variables in columns.}

\item{center}{(bool) if \code{TRUE}, it demeans the variables.}

\item{scale}{(bool) if \code{TRUE}, it scales the variables to unit variance.}

\item{newX}{(numeric matrix) data to be used in projection. Its structure must be similar to the \code{x}.}
}
\value{
(list) results
\item{removed0Var}{(integer vector) Zero-based indices of removed columns with zero variances.}
\item{directions}{(numeric matrix) Directions}
\item{stds}{(integer vector) Standard deviation of the principle components}
\item{stds2Ratio}{(integer vector) stds^2/sum(stds^2)}
\item{projections}{(numeric matrix) Projections if \code{newX} is given.}
}
\description{
Principle Component Analysis
}
