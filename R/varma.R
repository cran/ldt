
# get estimation from search result
GetEstim_varma <- function(searchRes, endoIndices,
                           exoIndices, y, x, printMsg,
                           params, newX = NULL, ...) {
  M <- VarmaEstim(y[, endoIndices, drop = FALSE],
    x = if (is.null(exoIndices) || is.null(x)) {
      NULL
    } else {
      x[, exoIndices, drop = FALSE]
    },
    params = params,
    seasonsCount = searchRes$info$seasonsCount,
    addIntercept = FALSE,
    lmbfgsOptions = searchRes$info$lmbfgsOptions,
    olsStdMultiplier = searchRes$info$olsStdMultiplier,
    pcaOptionsY = NULL,
    pcaOptionsX = NULL,
    maxHorizon = searchRes$info$maxHorizon,
    newX = if (is.null(exoIndices) || is.null(newX)) {
      NULL
    } else {
      as.matrix(newX[, exoIndices])
    },
    simFixSize = searchRes$info$measureOptions$simFixSize,
    simHorizons = searchRes$info$measureOptions$simHorizons,
    simUsePreviousEstim = searchRes$info$simUsePreviousEstim,
    simMaxConditionNumber = searchRes$info$modelCheckItems$maxConditionNumber,
    printMsg = printMsg
  )

  return(M)
}


#' Step-wise VARMA Search
#'
#' A helper class to deal with large model sets.
#' It selects a subset of variables from smaller
#' models and moves to the bigger ones.
#'
#' @param y endogenous data
#' @param ySizes a list of model dimension to be estimated in each step.
#' @param counts a list of suggested number of variables to be used in
#' each step. \code{NA} means all variables. Variables are selected based
#' on best estimations (select an appropriate value for
#' \code{searchItems$bestK}). All variables in the best models (all
#' measures and targets) are selected until corresponding suggested
#' number is reached.
#' @param savePre if not \code{NULL}, it saves and tries to load the
#' progress of search step in a file (name=\code{paste0(savePre,i)}
#' where \code{i} is the index of the step).
#' @param ... other arguments to pass to [VarmaSearch()] function such
#' as endogenous data. Note that \code{ySizes} is treated differently.
#'
#' @return A combined \code{LdtSearch} object
#' @export
VarmaSearch_s <- function(y, ySizes = list(c(1, 2), c(3, 4), c(5), c(6:10)),
                          counts = c(NA, 40, 30, 20),
                          savePre = NULL, ...) {
  Search_s("varma", y, ySizes, counts, savePre, ...)
}
