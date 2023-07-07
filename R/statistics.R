

#' Convert a Metric to Weight
#'
#' This function converts a metric to its weight equivalent.
#'
#' @param value Numeric value of the metric.
#' @param metricName Character string specifying the name of the metric.
#' See [get.options.metric] function for the list of available options.
#'
#' @details
#' Given a collection of models for the data, a metric is not
#' generally a metric of the relative quality of a model. This function
#' converts the value of a metric to such a number.
#'
#' These are the details of the transformations:
#' \itemize{
#' \item direction, sign, AUC -> weight = metric
#' \item AIC, SIC, RMSE, Brier, MAE, CRPS -> weight = exp(-0.5 metric)
#' }
#'
#' The main purpose of exporting this statistics helper method is to show the inner calculations of the package.
#'
#' @return A numeric value representing the converted metric.
#' @export
#'
#' @examples
#' weight <- s.weight.from.metric(-3.4, "sic")
#' metric <- s.metric.from.weight(weight, "sic")
#'
#' @seealso [s.metric.from.weight]
s.weight.from.metric <- function(value, metricName)
{
  value = as.numeric(value)
  metricName = as.character(metricName)
  res <- .GetWeightFromMetric(value, metricName)
  res
}


#' Convert a Weight to Metric
#'
#' This function converts a weight to its metric equivalent.
#'
#' @param value Numeric value of the weight.
#' @param metricName Character string specifying the name of the metric.
#' See \code{\link{get.options.metric}} function for the list of available options.
#'
#' @details
#' See [s.weight.from.metric] for a discussion.
#'
#' The main purpose of exporting this statistics helper method is to show the inner calculations of the package.
#'
#' @return A numeric value representing the converted weight.
#' @export
#'
#' @examples
#' weight <- s.weight.from.metric(-3.4, "sic")
#' metric <- s.metric.from.weight(weight, "sic")
#'
#' @seealso [s.weight.from.metric]
s.metric.from.weight <- function(value, metricName)
{
  value = as.numeric(value)
  metricName = as.character(metricName)

  res <- .GetMetricFromWeight(value, metricName)
  res
}

#' Get ROC Curve Data for Binary Classification
#'
#' This function calculates the required points for plotting the ROC curve and the AUC.
#'
#' @param y A numeric vector (\code{Nx1}) representing the actual values.
#' @param scores A numeric vector (\code{Nx1}) representing the calculated probabilities for the **negative** observations.
#' @param weights A numeric vector (\code{Nx1}) representing the weights of the observations.
#' Use \code{NULL} for equal weights.
#' @param options A list from \code{\link{get.options.roc}} function for more options.
#' @param printMsg Set to \code{TRUE} to enable printing some details.
#'
#' @details
#' This is generally a statistics helper method in this package and it shows the inner calculations.
#'
#'
#' @return A list with the following items:
#' \item{n}{Number of observations. }
#' \item{auc}{Value of AUC. }
#' \item{points}{Points for plotting ROC.}
#'
#' @export
#'
#' @examples
#' y <- c(1, 0, 1, 0, 1, 1, 0, 0, 1, 0)
#' scores <- c(0.1, 0.2, 0.3, 0.5, 0.5, 0.5, 0.7, 0.8, 0.9, 1)
#' res1 <- s.roc(y,scores, printMsg = FALSE)
#' costs <- c(1,2,1,4,1,5,1,1,0.5,1)
#' costMatrix <- matrix(c(0.02,-1,-3,3),2,2)
#' opt <- get.options.roc(costs = costs, costMatrix = costMatrix)
#' res2 <- s.roc(y,scores,NULL,options = opt, printMsg = FALSE)
s.roc <- function(y, scores, weights = NULL,
                  options = get.options.roc(), printMsg = FALSE)
{
  y <- as.numeric(y)
  scores <- as.numeric(scores)
  if (length(y) != length(scores))
    stop("Inconsistent length between actual data and the scores.")
  if (is.null(weights) == FALSE){
    weights <- as.numeric(weights)
    if (length(y) != length(weights))
      stop("Inconsistent length between actual data and the weights.")
  }
  if (is.null(options))
    options <- get.options.roc()
  options <-as.list(options)
  printMsg <- as.logical(printMsg)

  res <- .GetRoc(y, scores, weights, options, printMsg)
  res
}


#' Get the GLD Parameters from the moments
#'
#' Calculates the parameters of the generalized lambda distribution (FKML), given the first four moments of the distribution.
#'
#' @param mean A number for the mean of the distribution.
#' @param variance A number for the variance of the distribution.
#' @param skewness A number for the skewness of the distribution.
#' @param excessKurtosis A number for the excess kurtosis of the distribution.
#' @param type An integer to restrict the shape of the distribution. See details section.
#' @param start A numeric vector of size 2 for the starting value.
#' @param nelderMeadOptions A list of options for Nelder-Mead algorithm. Use [get.options.neldermead] for initialization.
#' @param printMsg Set to \code{TRUE} to enable printing some details.
#'
#' @details
#' The type of the distribution is determined by one or two restrictions:
#' \itemize{
#' \item **type 0:** general, no restriction
#' \item **type 1:** symmetric 'type 0', p3 == p4
#' \item **type 2:** uni-modal continuous tail, p3 < 1 & p4 < 1
#' \item **type 3:** symmetric 'type 2', p3 == p4
#' \item **type 4:** uni-modal continuous tail finite slope, p3 <= 0.5 & p4 <= 0.5
#' \item **type 5:** symmetric 'type 4', p3 == p4
#' \item **type 6:** uni-modal truncated density curves, p3 >= 2 & p4 >= 2 (includes uniform distribution)
#' \item **type 7:** symmetric 'type 6', p3 == p4
#' \item **type 8:** S shaped, (p3 > 2 & 1 < p4 < 2) or (1 < p3 < 2 & p4 > 2)
#' \item **type 9:** U shaped, (1 < p3 <= 2) and (1 < p4 <= 2)
#' \item **type 10:** symmetric 'type 9', p3 == p4
#' \item **type 11:** monotone, p3 > 1 & p4 <= 1
#' }
#'
#' @return A vector of length 5. The first 4 elements are the parameters of the GLD distribution.
#' The last one is the number of iterations.
#'
#' @export
#' @examples
#' res <- s.gld.from.moments(0,1,0,0, start = c(0,0), type = 4)
#' probs <- seq(0.1,0.9,0.1)
#' x <- s.gld.quantile(probs, res[1],res[2],res[3],res[4])
#' y <- s.gld.density.quantile(probs, res[1],res[2],res[3],res[4])
#' plot(x,y)
#' lines(x,y)
#'
#'
s.gld.from.moments <- function(mean = 0, variance = 1,
                               skewness = 0, excessKurtosis = 0,
                               type = 0, start = NULL,
                               nelderMeadOptions = get.options.neldermead(),
                               printMsg = FALSE)
{
  mean <- as.numeric(mean)
  variance <- as.numeric(variance)
  skewness <- as.numeric(skewness)
  excessKurtosis <- as.numeric(excessKurtosis)
  type <- as.integer(type)
  if (is.null(start))
    start <- c(0,0)
  start <- as.numeric(start)
  if (length(start) != 2)
    stop("start must be a numeric vector of size 2.")
  if (is.null(nelderMeadOptions))
    nelderMeadOptions <- get.options.neldermead()
  else
    nelderMeadOptions <- as.list(nelderMeadOptions)
  CheckNelderMeadOptions(nelderMeadOptions)
  printMsg <- as.logical(printMsg)

  res <- .GetGldFromMoments(mean, variance, skewness, excessKurtosis,
                            type, start[[1]], start[[2]], nelderMeadOptions, printMsg)
  res
}

#' GLD Quantile Function
#'
#' This function calculates the quantiles of a Generalized Lambda Distribution (FKML).
#'
#' @param probs A numeric vector of probabilities.
#' @param p1 Numeric value representing the first parameter of the distribution (location of the distribution).
#' @param p2 Numeric value representing the second parameter of the distribution (scale of the distribution).
#' @param p3 Numeric value representing the third parameter of the distribution (skewness of the distribution).
#' @param p4 Numeric value representing the fourth parameter of the distribution (kurtosis of the distribution).
#'
#' @details
#' It is a helper statistics method in this package and is generally used to plot density function of a GLD distribution.
#' See the example of \code{\link{s.gld.density.quantile}} function for more details.
#'
#' @return A numeric vector representing the quantiles for each probability in \code{probs}.
#' @export
#' @examples
#' res = s.gld.quantile(c(0.1,0.5,0.95), 0,1,0,0) # standard normal distribution
#'
#' @seealso [s.gld.density.quantile]
s.gld.quantile <- function(probs, p1, p2, p3, p4)
{
  probs <- as.numeric(probs)
  p1 <- as.numeric(p1)
  p2 <- as.numeric(p2)
  p3 <- as.numeric(p3)
  p4 <- as.numeric(p4)

  res <- .GldQuantile(probs, p1, p2, p3, p4)
  res
}

#' GLD Density-Quantile Function
#'
#' This function calculates the densities of a Generalized Lambda Distribution (FKLM) given a vector of probabilities.
#'
#' @param probs A numeric vector representing the probabilities.
#' @param p1 Numeric value representing the first parameter (location) of the distribution.
#' @param p2 Numeric value representing the second parameter (scale) of the distribution.
#' @param p3 Numeric value representing the third parameter (skewness) of the distribution.
#' @param p4 Numeric value representing the fourth parameter (kurtosis) of the distribution.
#'
#' @details
#' It is a helper statistics method in this package and is generally used to plot density function of a GLD distribution.
#'
#' @return A numeric vector representing the densities for each probability in \code{probs}.
#' @export
#' @examples
#' # In this example we use this function and plot the density function for
#' # standard normal distribution:
#' probs <- seq(0.1,0.9,0.1)
#' x <- s.gld.quantile(probs, 0,1,0,0)
#' y <- s.gld.density.quantile(probs, 0,1,0,0)
#' plot(x,y)
#' lines(x,y)
#' @seealso [s.gld.quantile]
s.gld.density.quantile <- function(probs, p1, p2, p3, p4)
{
  probs <- as.numeric(probs)
  p1 <- as.numeric(p1)
  p2 <- as.numeric(p2)
  p3 <- as.numeric(p3)
  p4 <- as.numeric(p4)

  res <- .GldDensityQuantile(probs, p1, p2, p3, p4)
  res
}

#' Combine Mean, Variance, Skewness, and Kurtosis
#' This function combines two sets of mean, variance, skewness, and kurtosis and generates the combined statistics.
#' @param list1 A list representing the first \code{mean}, \code{variance}, \code{skewness}, \code{kurtosis}, \code{weight}, and \code{count}.
#' @param list2 A list representing the second distribution (similar to \code{list1}).
#' @details
#' Assume there are two samples with \eqn{mean_i}, \eqn{variance_i}, \eqn{skewness_i}, and \eqn{kurtosis_i} for \eqn{i=1,2},
#' this function calculates the mean, variance, skewness, and kurtosis of the combined sample.
#' It does not need the data itself.
#' It is based on population variance, skewness, and kurtosis and calculates the population statistics.
#' Note that the kurtosis is not excess kurtosis.
#'
#' @return A list similar to \code{list1}.
#' @export
#' @importFrom stats rchisq
#'
#' @examples
#' n <- 1000 # sample size (increase it for more accurate result)
#' sample1 <- rchisq(n,3)
#' sample2 <- rchisq(n,5)
#'
#' d1 <- list(mean = mean(sample1),
#'            variance = var(sample1),
#'            skewness = moments::skewness(sample1),
#'            kurtosis = moments::kurtosis(sample1),
#'            count=length(sample1),
#'            weight = length(sample1))
#' d2 <- list(mean = mean(sample2),
#'            variance = var(sample2),
#'            skewness = moments::skewness(sample2),
#'            kurtosis = moments::kurtosis(sample2),
#'            count=length(sample2),
#'            weight = length(sample2))
#' c <- s.combine.stats4(d1,d2)
#'
#' # we can compare the results:
#' combined <- c(sample1,sample2)
#' mean_c = mean(combined)
#' variance_c = var(combined)
#' skewness_c = moments::skewness(combined)
#' kurtosis_c = moments::kurtosis(combined)
#'
s.combine.stats4 <- function(list1, list2)
{
  list1 <- as.list(list1)
  list2 <- as.list(list2)

  res <- .CombineStats4(list1, list2)
  res
}

#' Principal Component Analysis
#'
#' This function performs PCA on the columns of a matrix.
#'
#' @param x A numeric matrix with variables in the columns.
#' @param center Logical value indicating whether to demean the columns of \code{x}.
#' @param scale Logical value indicating whether to scale the columns of \code{x} to unit variance.
#' @param newX A numeric matrix to be used in projection.
#' Its structure must be similar to \code{x}.
#'
#' @details
#' The main purpose of exporting this statistics helper method is to show the inner calculations of the package.
#'
#' @return A list with the following items:
#' \item{removed0Var}{An integer vector showing the zero-based indices of removed columns with zero variances.}
#' \item{directions}{Directions matrix.}
#' \item{stds}{An integer vector showing the standard deviation of the principal components.}
#' \item{stds2Ratio}{Shows \code{stds^2/sum(stds^2)}.}
#' \item{projections}{Projections matrix if \code{newX} is provided.}
#'
#' @export
#' @importFrom stats prcomp
#' @importFrom stats rnorm
#' @example man-roxygen/ex-s.pca.R
#' @seealso [get.options.pca]
s.pca <- function(x, center = TRUE,
                  scale = TRUE, newX = NULL)
{
  x <- as.matrix(x)
  center <- as.logical(center)
  scale <- as.logical(scale)
  if (is.null(newX) == FALSE){
    newX <- as.matrix(newX)
    if (ncol(x) != ncol(newX))
      stop("Inconsistent number of columns between 'x' and 'newX'.")
  }

  res <- .GetPca(x, center, scale, newX)
  res
}


#' Get the Distances Between Variables
#'
#' This function calculates the distances between the columns of a numeric matrix.
#'
#' @param data A numeric matrix with variables in the columns.
#' @param distance Character string specifying the type of distance.
#' It can be \code{correlation}, \code{absCorrelation}, \code{euclidean}, \code{manhattan}, or \code{maximum}.
#' @param correlation Character string specifying the type of correlation if \code{distance} is correlation.
#' It can be \code{pearson} or \code{spearman}.
#' @param checkNan Logical value indicating whether to check for \code{NA}s (and omit them if any exist).
#'
#' @details
#' The main purpose of exporting this statistics helper method is to show the inner calculations of the package.
#'
#' @return A symmetric matrix (lower triangle as a vector).
#'
#' @export
#' @examples
#' n <- 10
#' data <- data.frame(x = rnorm(n), y = rnorm(n), z = rnorm(n))
#' distances <- s.distance(data)
#'
s.distance <- function(data, distance = "correlation",
                       correlation = "pearson", checkNan = TRUE) {

  data <- as.matrix(data)
  distance <- as.character(distance)
  correlation <- as.character(correlation)
  checkNan <- as.logical(checkNan)

  res <- .GetDistance(data, distance, correlation, checkNan)
  res
}

#' Hierarchical Clustering
#'
#' This function performs hierarchical clustering on a group of variables, given their distances from each other.
#'
#' @param distances Lower triangle of a symmetric distance matrix (without the diagonal).
#' This can be the output of \code{\link{s.distance}} function.
#' @param linkage Character string specifying the method for calculating the distance in a left-right node merge.
#' It can be \code{single}, \code{complete}, \code{uAverage}, \code{wAverage}, or \code{ward}.
#'
#' @details
#' The main purpose of exporting this statistics helper method is to show the inner calculations of the package.
#'
#'
#' @return A list with the following items:
#' \item{merge}{An integer matrix representing the merge matrix. }
#' \item{height}{A numeric vector representing the heights. }
#' \item{order}{An integer vector representing the orders.}
#'
#' @export
#' @examples
#' n <- 10
#' data <- data.frame(x = rnorm(n), y = rnorm(n), z = rnorm(n))
#' distances <- s.distance(data)
#' clusters <- s.cluster.h(distances)
#'
s.cluster.h <- function(distances, linkage = "single"){

  distances <- as.numeric(distances)
  linkage <- as.character(linkage)

  res <- .ClusterH(distances, linkage)
  res
}


#' Group Variables with Hierarchical Clustering
#'
#' This function groups the columns of a numeric matrix based on the hierarchical clustering algorithm.
#'
#' @param data A numeric matrix with variables in the columns.
#' @param nGroups Integer value specifying the number of required groups.
#' @param threshold Numeric value specifying a threshold for omitting variables.
#' If the distance between two variables in a group is less than this value, the second one will be omitted.
#' Note that a change in the order of the columns might change the results.
#' @param distance Character string specifying how distances are calculated.
#' It can be \code{correlation}, \code{absCorrelation}, \code{euclidean}, \code{manhattan}, or \code{maximum}.
#' See \code{\link{s.distance}} function.
#' @param linkage Character string specifying how distances are calculated in a left-right node merge.
#' It can be \code{single}, \code{complete}, \code{uAverage}, \code{wAverage}, or \code{ward}.
#' See \code{\link{s.cluster.h}} function.
#' @param correlation Character string specifying the type of correlation if \code{distance} is correlation.
#' It can be \code{pearson} or \code{spearman}. See \code{\link{s.distance}} function.
#'
#' @details
#' The results might be different from R's 'cutree' function.
#' (I don't know how 'cutree' works) Here this function iterates over the nodes and
#' whenever a split occurs, it adds a group until the required number of groups is reached.
#'
#' @return A list with the following items:
#' \item{groups}{A list of integer vectors representing the indexes of variables in each group. }
#' \item{removed}{An integer vector representing the indexes of removed variables.}
#'
#' @export
s.cluster.h.group <- function(data, nGroups = 2, threshold = 0,
                             distance = "correlation",
                             linkage = "single",
                             correlation = "pearson")

{
  data <- as.matrix(data)
  nGroups <- as.integer(nGroups)
  threshold <- as.numeric(threshold)
  distance <- as.character(distance)
  linkage <- as.character(linkage)
  correlation <- as.character(correlation)

  res <- .ClusterHGroup(data, nGroups, threshold,
                        distance, linkage, correlation)
  res
}




#' Generate Random Samples from a Multivariate Normal Distribution
#'
#' Use this function to get random samples from a multivariate normal distribution.
#'
#' @param n The number of samples to generate.
#' @param mu The mean vector of the distribution.
#' If \code{NULL}, it defaults to a zero vector of length \code{p}.
#' If \code{NA}, it is set to a random vector.
#' @param sigma The covariance matrix of the distribution.
#' If \code{NULL}, it defaults to an identity matrix of size \code{p x p}.
#' If \code{NA}, it is set to a random positive definite matrix.
#' @param p The dimension of the distribution, if both \code{mu} and \code{sigma} are \code{NA} or \code{NULL}.
#' @param byRow If \code{TRUE}, generated samples are stored in the rows. Otherwise, they are stored in the columns.
#'
#' @return A list containing the generated sample (\code{p x n}), \code{mu}, and \code{sigma}.
#'
#' @export
#' @importFrom stats runif
#' @importFrom stats rnorm
#'
#' @examples
#' s1 <- rand.mnormal(10, mu = c(0, 0), sigma = matrix(c(1, 0.5, 0.5, 1), ncol = 2))
#' s2 <- rand.mnormal(10, mu = c(1,1), sigma = NA, p = 2)
#' s3 <- rand.mnormal(10, p = 2, byRow = FALSE) #standard normal
#'
rand.mnormal <- function(n, mu = NULL,
                         sigma = NULL,
                         p = NULL,
                         byRow = TRUE) {
  if (length(mu) == 1 && is.na(mu) && length(sigma) == 1 && is.na(sigma)) {
    if (is.null(p)) stop("Please specify the dimension 'p' of the distribution")
    mu <- runif(p)
    x <- matrix(rnorm(p * p), ncol = p)
    sigma <- crossprod(x)
  } else if (is.null(mu) && is.null(sigma)) {
    if (is.null(p)) stop("Please specify the dimension 'p' of the distribution")
    mu <- rep(0, p)
    sigma <- diag(p)
  } else if (length(mu) == 1 && is.na(mu)) {
    p <- ncol(sigma)
    mu <- runif(p)
  } else if (length(sigma) == 1 && is.na(sigma)) {
    p <- length(mu)
    x <- matrix(rnorm(p * p), ncol = p)
    sigma <- crossprod(x)
  } else if (is.null(mu)) {
    p <- ncol(sigma)
    mu <- rep(0, p)
  } else if (is.null(sigma)) {
    p <- length(mu)
    sigma <- diag(p)
  } else {
    p <- length(mu)
    if (p != ncol(sigma)) stop("The dimensions of 'mu' and 'sigma' are inconsistent")
  }

  e <- matrix(rnorm(n * p), ncol = p)

  if (byRow) {
    sample <- e %*% chol(sigma) + matrix(mu, nrow = n, ncol = p, byrow = TRUE)
  } else {
    sample <- t(chol(sigma)) %*% t(e) + mu
  }
  list(sample = sample, mu = mu, sigma = sigma)
}
