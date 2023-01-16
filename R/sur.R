
# get estimation from search result
GetEstim_sur <- function(searchRes, endoIndices,
                         exoIndices, y, x, printMsg, ...) {
  y <- y[, endoIndices, drop = FALSE]
  x <- if (is.null(exoIndices) || is.null(x)) {
    NULL
  } else {
    x[, c(exoIndices), drop = FALSE]
  }

  M <- SurEstim(
    y = y,
    x = x,
    addIntercept = FALSE,
    searchSigMaxIter = searchRes$info$searchSigMaxIter,
    searchSigMaxProb = searchRes$info$searchSigMaxProb,
    restriction = NULL,
    newX = NULL,
    pcaOptionsY = NULL,
    pcaOptionsX = NULL,
    simFixSize = searchRes$info$measureOptions$simFixSize,
    simTrainRatio = searchRes$info$measureOptions$trainRatio,
    simTrainFixSize = searchRes$info$measureOptions$trainFixSize,
    simSeed = abs(searchRes$info$measureOptions$seed),
    simMaxConditionNumber = searchRes$info$modelCheckItems$maxConditionNumber,
    printMsg = printMsg
  )

  return(M)
}


#' Step-wise SUR Search
#'
#' A helper class to deal with large model sets.
#' It selects a subset of variables from smaller models and
#' moves to the bigger ones.
#'
#' @param x exogenous data
#' @param xSizes a list of model dimension to be estimated in each step.
#' @param counts a list of suggested number of variables to be used
#' in each step. \code{NA} means all variables. Variables are selected
#' based on best estimations (select an appropriate value for
#' \code{searchItems$bestK}). All variables in the best models (all measures
#' and targets) are selected until corresponding suggested number is reached.
#' @param savePre if not \code{NULL}, it saves and tries to load the progress
#' of search step in a file (name=\code{paste0(savePre,i)} where \code{i}
#' is the index of the step).
#' @param ... other arguments to pass to [SurSearch()] function such
#' as endogenous data. Note that \code{xSizes} is treated differently.
#'
#' @return A combined \code{LdtSearch} object
#' @export
SurSearch_s <- function(x, xSizes = list(c(1, 2), c(3, 4), c(5), c(6:10)),
                        counts = c(NA, 40, 30, 20),
                        savePre = NULL, ...) {
  Search_s("sur", x, xSizes, counts, savePre, ...)
}
