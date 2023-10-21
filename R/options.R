
#' Get Options for ROC and AUC Calculations
#'
#' Use this function to get the required options for [search.bin], [estim.bin], or [s.roc] functions.
#'
#' @param lowerThreshold A number representing the lower bound for calculating partial AUC.
#' @param upperThreshold A number representing the upper bound for calculating partial AUC.
#' @param epsilon A small number used to ignore small floating point differences when comparing scores.
#' @param pessimistic If \code{TRUE}, sequences of equally scored instances are treated differently and a pessimistic metric is calculated (see Fawcett (2006) An introduction to ROC analysis, fig. 6).
#' @param costs The cost of each observation. If \code{NULL}, the cost of all observations will be 1.
#' @param costMatrix A \code{2x2} cost matrix in which: (1,1) is the cost of TN,
#' (2,2) is the cost of TP, (1,2) is the cost of FP and (2,1) is the cost of FN. The first
#' column is multiplied by the corresponding value in the costs vector (see
#' Fawcett (2006), ROC graphs with instance-varying costs).
#'
#' @details
#' See details of [s.roc] function.
#'
#' @return A list with the given options.
#' @examples
#' # See 's.roc' function.
#'
#'
#' @export
#' @seealso [search.bin], [estim.bin], [s.roc]
get.options.roc <- function(lowerThreshold = 0, upperThreshold = 1, epsilon = 1e-12,
                            pessimistic = FALSE, costs = NULL, costMatrix = NULL){
  stopifnot(is.zero.or.positive.number(lowerThreshold))
  stopifnot(is.zero.or.positive.number(upperThreshold) && upperThreshold <= 1)
  stopifnot(is.zero.or.positive.number(epsilon) && epsilon <= 1)

  if (!is.null(costs)) {
    stopifnot(is.numeric(costs))
    if (!(!is.null(costMatrix) && is.matrix(costMatrix) &&
                nrow(costMatrix) == 2 && ncol(costMatrix) == 2))
      stop("Cost matrix must be 2x2 numeric matrix.")
  }

  res = list(lowerThreshold = lowerThreshold,
             upperThreshold = upperThreshold,
             epsilon=epsilon,
             pessimistic = pessimistic,
             costs=costs,
             costMatrix = costMatrix)
  class(res) <- c("ldt.list", "list")
  res
}



#' Options for Nelder-Mead Optimization
#'
#' Use this function to get the required options when Nelder-Mead optimization is needed such as [s.gld.from.moments] function.
#'
#' @param maxIterations (int) Maximum number of iterations.
#' @param tolerance A small number to determine the convergence.
#' The algorithm terminates when the difference between the best and worst points in the simplex is less than this value.
#' @param reflection A number for reflection coefficient.
#' It controls how far the worst point is reflected through the centroid of the remaining points.
#' @param expansion A number that determines the expansion coefficient.
#' It controls how far the reflected point is expanded along the line connecting it to the centroid.
#' @param contraction A number that determines the contraction coefficient.
#' It controls how far the worst point is contracted towards the centroid.
#' @param shrink A number that determines the shrink coefficient.
#' It controls how much the simplex is shrunk towards the best point when all other moves are rejected.
#'
#' @return A list with the given options.
#' @export
get.options.neldermead <- function(maxIterations = 100, tolerance = 1e-6,
                                   reflection = 1, expansion = 2, contraction = 0.5,
                                   shrink = 1){
  stopifnot(is.positive.number(maxIterations) && maxIterations >= 0)
  stopifnot(is.zero.or.positive.number(tolerance))
  stopifnot(is.positive.number(reflection))
  stopifnot(is.positive.number(expansion))
  stopifnot(is.positive.number(contraction))
  stopifnot(is.positive.number(shrink))

  res = list(maxIterations = maxIterations,
           tolerance = tolerance, reflection = reflection,
           expansion = expansion, contraction = contraction,
           shrink = shrink)
  class(res) <- c("ldt.list", "list")
  res
  res
}


#' Get Options for PCA
#'
#' Use this function to get PCA options in [estim.bin], [estim.sur], [estim.varma], or [s.pca] functions.
#'
#' @param ignoreFirst A number representing the number of variables to exclude at the beginning of data matrices (such as intercept) from PCA.
#' @param exactCount A number that determines the number of components to be used. If zero, the number of components is determined by the \code{cutoffRate}.
#' @param cutoffRate A number between 0 and 1 that determines the cutoff rate for the cumulative variance ratio in order to determine the number of PCA components. It is not used if \code{exactCount} is positive.
#' @param max A number representing the maximum number of components when \code{cutoffRate} is used.
#'
#' @details
#' See details of [s.pca] function.
#'
#' @return A list with the given options.
#' @examples
#' # See 's.pca' function.
#'
#' @export
#' @seealso [estim.bin], [estim.sur], [estim.varma], [s.pca]
get.options.pca <- function(ignoreFirst = 1, exactCount = 0, cutoffRate = 0.8, max = 1000){

  stopifnot(is.zero.or.positive.number(ignoreFirst))
  stopifnot(is.zero.or.positive.number(exactCount))
  stopifnot(is.positive.number(cutoffRate) && cutoffRate < 1)
  stopifnot(is.positive.number(max) && max > 0)

  res = list(
    ignoreFirst = ignoreFirst,
    exactCount = exactCount,
    cutoffRate = cutoffRate,
    max = max)
  class(res) <- c("ldt.list", "list")
  res
}

#' Get Options for L-BFGS Optimization
#'
#' Use this function to get optimization options in [estim.varma] or [search.varma] functions.
#'
#' @param maxIterations A positive integer representing the maximum number of iterations.
#' @param factor A number that determines the condition for stopping the iterations. Use, for example, 1e12 for low accuracy, 1e7 (default) for moderate accuracy, and 1e1 for extremely high accuracy. The default is 1e7.
#' @param projectedGradientTol A number used to stop the iteration using the projected gradient. The default is zero.
#' @param maxCorrections The maximum number of variable metric corrections allowed in the limited memory matrix. The default is 5.
#'
#' @return A list with the given options.
#'
#' @export
get.options.lbfgs <- function(maxIterations = 100, factor = 1e7,
                               projectedGradientTol = 0, maxCorrections = 5){
  stopifnot(is.positive.number(maxIterations) && maxIterations >= 1)
  stopifnot(is.positive.number(factor))
  stopifnot(is.zero.or.positive.number(projectedGradientTol))
  stopifnot(is.positive.number(maxCorrections) && maxIterations >= 1)

  res = list(maxIterations = maxIterations,
           factor = factor,
           projectedGradientTol = projectedGradientTol,
           maxCorrections = maxCorrections)
  class(res) <- c("ldt.list", "list")
  res
}


#' Get Options for Newton Optimization
#'
#' Use this function to get optimization options in [estim.bin] or [search.bin] functions.
#'
#' @param maxIterations An integer representing the maximum number of iterations.
#' @param functionTol A small value used to test the convergence of the objective function.
#' @param gradientTol A small value used to test the convergence of the gradient.
#' @param useLineSearch If \code{TRUE}, line search is used.
#'
#' @return A list with the given options.
#'
#' @export
get.options.newton <- function(maxIterations = 100, functionTol = 1e-4,
                               gradientTol = 0, useLineSearch = TRUE){

  stopifnot(is.positive.number(maxIterations) && maxIterations >= 1)
  stopifnot(is.zero.or.positive.number(functionTol))
  stopifnot(is.zero.or.positive.number(gradientTol))
  stopifnot(is.logical(useLineSearch) && length(useLineSearch) == 1)

  res = list(maxIterations = maxIterations,
           functionTol = functionTol,
           gradientTol = gradientTol,
           useLineSearch = useLineSearch)
  class(res) <- c("ldt.list", "list")
  res
}

