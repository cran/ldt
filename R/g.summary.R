

get.metric.from.estim <- function(model, metricName, targetName) {
  row <- which(rownames(model$metrics) == metricName)
  if (length(row) != 1)
    stop("metric not found. method=", attr(model, "method"), "metric=", metricName, "target=", targetName)
  col <- which(colnames(model$metrics) == targetName)
  if (length(col) != 1)
    stop("target not found. method=", attr(model, "method"), "metric=", metricName, "target=", targetName)
  r <- model$metrics[row[[1]], col[[1]]]
  r
}

#' Summary for an \code{ldt.search.item} object
#'
#' While you can get a summary of an item in a search result, this function is mainly designed to be called from [print.ldt.search] function.
#' Its main job is to estimate the full model using the reported indices from the search process.
#'
#' @param object An \code{ldt.search.item} object.
#' @param searchResult Parent list of \code{object}, which is an \code{ldt.search} object.
#' @param test If \code{TRUE} and applicable (e.g., in model estimation), it checks the metrics and throws error for any inconsistencies between the current estimation and the one calculated in the search process.
#' @param ... Additional arguments.
#'
#' @details
#' An \code{ldt.search.item} object is a member of \code{ldt.search} object.
#' An \code{ldt.search} object is an output from one of the \code{search.?} functions (see \code{search.sur}, \code{search.varma}, or \code{search.bin}).

#'
#'
#' @return If the object contains the indices of endogenous variables of an estimated model, it returns the estimation output.
#' Otherwise, it returns object.
#' @export
summary.ldt.search.item <- function(object, searchResult = NULL, test = FALSE, ...) {

  if (is.null(object))
    stop("argument is null.")
  if (!inherits(object, "ldt.search.item"))
    stop("Invalid class. An 'ldt.search.item' object is expected.")

  method <- tolower(attr(searchResult, "method"))

  if (!is.null(object$value) && is.list(object$value) && !is.null(object$value$endogenous)) { # estimate the model
    if (is.null(searchResult))
      stop("'searchResult' is NULL. In order to re-estimate the model, you should set this argument.")

    fun.name <- paste0("estim.", method, ".from.search")
    if (!exists(fun.name))
      stop("Invalid or not implemented type of method: ", method)

    res <- do.call(fun.name, list(searchResult = searchResult,
                                  endogenous = object$value$endogenous,
                                  exogenous = object$value$exogenous,
                                  extra = object$value$extra))

    if (test){

      metric <- get.metric.from.estim(res, object$evalName, object$targetName)
      if (abs(metric - object$value$metric)>1e-8){
        print(object)
        stop(paste0("Inconsistent metric: target=",object$targetName,
                    ", metric=", object$evalName,
                    ", search metric=", object$value$metric,
                    ", estimation metric=", metric))
      }

      if (!is.null(object$value$mean)){ #test parameter or forecasts, etc.
        # get x name from the type name (see "r_ldt.cpp" code)
        xName = strsplit(sub(".*'([^']*)'.*", "\\1", object$typeName), split = "'")[[1]]

        if (method == "sur" || method == "binary") {
          row = which(xName == rownames(res$estimations$coefs))
          col <- which(object$targetName == colnames(res$estimations$coefs))
          mean <- res$estimations$coefs[row, col]
          var <-  res$estimations$stds[row, col]^2
        } else if (method == "varma") {
          # xName is Horizon1,...
          row <- which(object$targetName == rownames(res$prediction$means))
          col <- as.integer(substr(xName, 8, nchar(xName))) + res$prediction$startIndex - 1
          mean <- res$prediction$means[row, col]
          var <- res$prediction$vars[row, col]
        }

        if (abs(mean - object$value$mean)>1e-8)
          stop(paste0("Inconsistent mean: target=",object$targetName,
                      ", metric=", object$evalName,
                      ", search mean=", object$value$mean,
                      ", estimation mean=", mean))


        if (abs(var - object$value$var)>1e-8)
          stop(paste0("Inconsistent variance: target=",object$targetName,
                      ", metric=", object$evalName,
                      ", search var=", object$value$var,
                      ", estimation var=", var))
      }
    }
    return(res)
  }

  #TODO:
  #if (object$typeName == "inclusion")
  #if (object$typeName == "extreme bound")
  #if (object$typeName == "cdf")
  #if (object$typeName == "mixture")

  return(object$value)
}




#' Summary for an \code{ldt.search} object
#'
#' Use this function to get the full estimation of the models reported in the output of a search process.
#'
#' @param object An \code{ldt.search} object.
#' @param test If \code{TRUE} and applicable (e.g., in model estimation), it checks the metrics and throws error for any inconsistencies between the current estimation and the one calculated in the search process (Provided that negative seed is used).
#' @param ... Additional arguments.
#'
#' @details
#' An \code{ldt.search} object is an output from one of the \code{search.?} functions (see \code{search.sur}, \code{search.varma}, or \code{search.bin}).
#'
#'
#' @return The output replaces the value of \code{object$results} with the summary from [summary.ldt.search.item].
#' @export
summary.ldt.search <- function(object, test = FALSE, ...) {

  if (is.null(object))
    stop("argument is null.")
  if (!inherits(object, "ldt.search"))
    stop("Invalid class. An 'ldt.search' object is expected.")
  if (length(object$results) == 0)
    warning("Result list is empty. Check failed estimations.")

  for (i in seq_along(object$results)){
    object$results[[i]]$value <- summary.ldt.search.item(object = object$results[[i]],
                                                         searchResult =  object,
                                                         test = test, ...)
  }
  class(object) <- c("ldt.search.summary", "ldt.search", "list")
  object
}
