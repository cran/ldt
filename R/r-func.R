

#' Create a Model Set for an R Function
#'
#' Use this model to create a model set for an R function.
#'
#' @param data A list that determines data and other required information for the search process.
#' Use [get.data()] function to generate it from a \code{matrix} or a \code{data.frame}.
#' @param combinations A list that determines the combinations of endogenous and exogenous variables in the search process.
#' Use [get.combinations()] function to define it.
#' @param metrics A list of options for measuring performance.
#' Use [get.search.metrics] function to get them.
#' @param modelChecks A list of options for excluding a subset of the model set.
#' See and use [get.search.modelchecks] function to get them.
#' @param items A list of options for specifying the purpose of the search.
#' See and use [get.search.items] function to get them.
#' @param options A list of extra options for performing the search.
#' See and use [get.search.options] function to get them.
#' @param rFuncName Name of a function that uses column indices and number of endogenous variables with respect to \code{data}.
#' It should estimate a model and return a list with required performance statistics. See details.
#' @param length1 An integer for the length of requested information. This can be the number of exogenous variables.
#' @param isInnerExogenous If \code{TRUE}, exogenous indices are defined by \code{innerGroups} in the \code{combinations} argument.
#'
#' @details
#' The central part of calling this function is to write a function with \code{rFuncName} name.
#' This function must have the following arguments:
#' \itemize{
#' \item \code{columnIndices}: determines the variables to be used in the current iteration. These indices point to the column of \code{data$data} matrix. E.g., you can create a matrix of available data by using \code{data$data[,colIndices]}. It contains weight column index (at \code{numEndo+1}), if \code{data$hasWeight} is \code{TRUE}.
#' \item \code{numEndo}: can be used to divide the \code{columnIndices} into endogenous and exogenous indices.
#' \item \code{data, metrics, modelChecks, items}: The arguments of current function which are passed to this function.
#' }
#'
#' The \code{rFuncName} function should use these arguments and estimate or predict by using any available R function.
#'
#' This function must return a \code{List} with the following items:
#' \itemize{
#' \item \code{error} (Character string or NULL): It not \code{NULL} or empty, it is considered as a failed estimation with the given message.
#' \item \code{metrics} (Numeric Matrix): Model performance for each target variable. Available target variables must be in the columns and metrics in the rows.
#' \item \code{extra} (Numeric Vector or NULL): Extra information in form of integers, which defines the current model.
#' \item \code{type1means} (Numeric Matrix or NULL): Means of \code{type1} (coefficients or predictions) for each target variable. Target variables must be in the columns. Make sure to skip the rows which the model does not present any information.
#' \item \code{type1vars} (Numeric Matrix or NULL): similar to \code{type1means} but for reporting the variances.
#' }
#'
#' @return A nested list with the following members:
#' \item{counts}{Information about the expected number of models, number of estimated models, failed estimations, and some details about the failures.}
#' \item{results}{A data frame with requested information in \code{items} list.}
#' \item{info}{The arguments and some general information about the search process such as the elapsed time.}
#'
#' Note that the output does not contain any estimation results, but minimum required data to estimate the models (Use \code{summary()} function to get the estimation).
#'
search.rfunc <- function(data = get.data(),
                         combinations = get.combinations(),
                         metrics = get.search.metrics(),
                         modelChecks = get.search.modelchecks(),
                         items = get.search.items(),
                         options = get.search.options(),
                         rFuncName,
                         length1,
                         isInnerExogenous){

  stopifnot(is.character(rFuncName))
  rFuncName <- as.character(rFuncName)
  if (!exists(rFuncName, mode = "function"))
    stop("The function '", rFuncName, "' does not exist.")

  func <- get(rFuncName)
  args <- formals(func)
  required_args <- c("columnIndices", "numEndo", "data", "metrics", "items", "modelChecks")

  for (arg in required_args) {
    if (!(arg %in% names(args)))
      stop(paste("The function 'func' does not have the required argument:", arg))
    #if (arg %in% required_args[3,5] && !is.null(args[[arg]]))
    #  print(paste("The argument", arg, "in function 'func' does not have a default value of 'NULL'."))
  }

  stopifnot(is.number(length1))
  length1 <- as.integer(length1)
  stopifnot(is.logical(isInnerExogenous) && length(isInnerExogenous) == 1)
  stopifnot(is.list(data))
  stopifnot(is.list(combinations))

  combinations <- get.indexation(combinations, data, isInnerExogenous) # it also check for inconsistencies, etc.

  if (options$parallel){
    options$parallel <- FALSE
    warning("Parallel computation is disabled. It is not implemented in this function.")
  }

  if (is.null(metrics))
    metrics = get.search.metrics()
  else
    stopifnot(is.list(metrics))
  metrics <- get.search.metrics.update(metrics, combinations$numTargets)

  if (is.null(modelChecks))
    modelChecks = get.search.modelchecks()
  else
    stopifnot(is.list(modelChecks))
  if (is.null(items))
    items = get.search.items()
  else
    stopifnot(is.list(items))
  if (is.null(options))
    options = get.search.options()
  else
    stopifnot(is.list(options))

  if (is.list(combinations$sizes)){ # use steps
    # steps will re-call this function with modified combinations in which sizes is no longer a list
    res <- search.steps("rfunc", isInnerExogenous = isInnerExogenous, data = data, combinations = combinations,
                        metrics = metrics, modelChecks = modelChecks, items = items, options = options,
                        rFuncName = rFuncName, length1 = length1)
    res
  }
  else {

    startTime <- Sys.time()
    res <- .SearchRFunc(data, combinations, metrics, modelChecks, items, options,
                        rFuncName, length1, isInnerExogenous)
    endTime <- Sys.time()

    res$info$data <- data
    res$info$combinations <- combinations
    res$info$metrics <- metrics
    res$info$options <- options
    res$info$modelChecks <- modelChecks
    res$info$items <- items
    res$info$startTime <- startTime
    res$info$endTime <- endTime

    class(res) <- c("ldt.search", "list")
    attr(res, "method") <- "rFunc"
    res
  }
}

