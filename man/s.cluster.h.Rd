% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statistics.R
\name{s.cluster.h}
\alias{s.cluster.h}
\title{Hierarchical Clustering}
\usage{
s.cluster.h(distances, linkage = "single")
}
\arguments{
\item{distances}{Lower triangle of a symmetric distance matrix (without the diagonal).
This can be the output of \code{\link{s.distance}} function.}

\item{linkage}{Character string specifying the method for calculating the distance in a left-right node merge.
It can be \code{single}, \code{complete}, \code{uAverage}, \code{wAverage}, or \code{ward}.}
}
\value{
A list with the following items:
\item{merge}{An integer matrix representing the merge matrix. }
\item{height}{A numeric vector representing the heights. }
\item{order}{An integer vector representing the orders.}
}
\description{
This function performs hierarchical clustering on a group of variables, given their distances from each other.
}
\details{
The main purpose of exporting this statistics helper method is to show the inner calculations of the package.
}
\examples{
n <- 10
data <- data.frame(x = rnorm(n), y = rnorm(n), z = rnorm(n))
distances <- s.distance(data)
clusters <- s.cluster.h(distances)

}
