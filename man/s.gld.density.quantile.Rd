% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statistics.R
\name{s.gld.density.quantile}
\alias{s.gld.density.quantile}
\title{GLD Density-Quantile Function}
\usage{
s.gld.density.quantile(probs, p1, p2, p3, p4)
}
\arguments{
\item{probs}{A numeric vector representing the probabilities.}

\item{p1}{Numeric value representing the first parameter (location) of the distribution.}

\item{p2}{Numeric value representing the second parameter (scale) of the distribution.}

\item{p3}{Numeric value representing the third parameter (skewness) of the distribution.}

\item{p4}{Numeric value representing the fourth parameter (kurtosis) of the distribution.}
}
\value{
A numeric vector representing the densities for each probability in \code{probs}.
}
\description{
This function calculates the densities of a Generalized Lambda Distribution (FKLM) given a vector of probabilities.
}
\details{
It is a helper statistics method in this package and is generally used to plot density function of a GLD distribution.
}
\examples{
# In this example we use this function and plot the density function for
# standard normal distribution:
probs <- seq(0.1,0.9,0.1)
x <- s.gld.quantile(probs, 0,1,0,0)
y <- s.gld.density.quantile(probs, 0,1,0,0)
plot(x,y)
lines(x,y)
}
\seealso{
\link{s.gld.quantile}
}
