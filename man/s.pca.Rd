% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statistics.R
\name{s.pca}
\alias{s.pca}
\title{Principal Component Analysis}
\usage{
s.pca(x, center = TRUE, scale = TRUE, newX = NULL)
}
\arguments{
\item{x}{A numeric matrix with variables in the columns.}

\item{center}{Logical value indicating whether to demean the columns of \code{x}.}

\item{scale}{Logical value indicating whether to scale the columns of \code{x} to unit variance.}

\item{newX}{A numeric matrix to be used in projection.
Its structure must be similar to \code{x}.}
}
\value{
A list with the following items:
\item{removed0Var}{An integer vector showing the zero-based indices of removed columns with zero variances.}
\item{directions}{Directions matrix.}
\item{stds}{An integer vector showing the standard deviation of the principal components.}
\item{stds2Ratio}{Shows \code{stds^2/sum(stds^2)}.}
\item{projections}{Projections matrix if \code{newX} is provided.}
}
\description{
This function performs PCA on the columns of a matrix.
}
\details{
The main purpose of exporting this statistics helper method is to show the inner calculations of the package.
}
\examples{

set.seed(340)
data <- matrix(rnorm(500), nrow = 50, ncol = 10)

# using prcomp function
resR = prcomp(data, center = TRUE, scale. = TRUE)

# using s.pca in this package
res = s.pca(data,TRUE,TRUE,data)

# res$projections and resR$x must be equal
# res$directions and t(resR$rotation) must be equal

# ----- ANOTHER EXAMPLE: PCA where there is a constant variable:
data <- data.frame( x = rnorm(100), y = rnorm(100), z = rep(0, 100))

# using s.pca in this package
res <- s.pca(data)

# using prcomp function
res_invalid <- try(prcomp(data, center = TRUE,
                          scale. = TRUE))
# Fails, we should remove 'z' first

}
\seealso{
\link{get.options.pca}
}
