

#' Determines if an email address is valid (this is not exact. Just use it to avoid mistakes)
#'
#' @param x email
#'
#' @return \code{TRUE} if email is valid, \code{FALSE} otherwise.
IsEmailValid <- function(x) {
  x <- as.character(x)
  grepl("^[A-Z0-9._%+-]+@[A-Z0-9._%+-]+\\.[A-Z0-9._%+-]+$", x, ignore.case = TRUE)
}

#' Determines if a GUID is valid
#'
#' @param x GUID
#'
#' @return \code{TRUE} if GUID is valid, \code{FALSE} otherwise.
IsGuidValid <- function(x) {
  x <- as.character(x)
  grepl("^[{]?[A-F0-9]{8}-([A-F0-9]{4}-){3}[A-F0-9]{12}[}]?$", x, ignore.case = TRUE)
}




#' Remove NA and Count the Number of Observations in Different Scenarios
#'
#' When a matrix has NA, one can omit columns with NA or rows with NA or a combination of these two.
#' Total number of observations is a function the order.
#' This function tries all combinations returns the results.
#'
#' @param data A matrix with NA
#' @param countFun a function to determine how strategies are sorted.
#' Default counts the number of observations. You might want to give columns a
#' higher level of importance for example by using \code{nRows*nCols^1.5}.
#'
#' @return a list of lists with four elements:
#' \itemize{
#'   \item nRows: number of rows in the matrix
#'   \item nCols: number of cols in the matrix
#'   \item colFirst: whether to remove columns or rows first
#'   \item colRemove: indexes of the columns to be removed
#'   \item rowRemove: indexes of the rows to be removed
#' }
#'
#' @export
#'
#' @examples
#' data <- matrix(c(NA, 2, 3, 4, NA, 5, NA, 6, 7, NA, 9, 10, 11, 12, 13, 14, 15, NA, 16, 17), 4, 5)
#' RemoveNaStrategies(data)
RemoveNaStrategies <- function(data, countFun = function(nRows, nCols) nRows * nCols) {
  data <- as.matrix(data)
  data_na <- is.na(data)
  c_cols <- sort(colSums(data_na), index.return = TRUE, decreasing = TRUE)

  result <- list()
  for (i in c(1:length(c_cols$ix))) { # remove i columns with most NAs
    colRemove <- c(c_cols$ix[1:i])
    if (length(colRemove) > 0) {
      mat <- data[, -colRemove, drop = FALSE]
    }
    rowRemove <- which(rowSums(is.na(mat)) > 0)
    result[[length(result) + 1]] <- list(
      nRows = nrow(data) - length(rowRemove),
      nCols = ncol(data) - length(colRemove), colFirst = TRUE,
      colRemove = sort(colRemove), rowRemove = as.integer(sort(rowRemove))
    )
  }

  c_rows <- sort(rowSums(data_na), index.return = TRUE, decreasing = TRUE)
  for (i in c(1:length(c_rows$ix))) { # remove i row with most NAs
    rowRemove <- c(c_rows$ix[1:i])
    if (length(rowRemove) > 0) {
      mat <- data[-rowRemove, , drop = FALSE]
    }
    colRemove <- which(colSums(is.na(mat)) > 0)
    result[[length(result) + 1]] <- list(
      nRows = nrow(data) - length(rowRemove),
      nCols = ncol(data) - length(colRemove), colFirst = FALSE,
      colRemove = as.integer(sort(colRemove)), rowRemove = sort(rowRemove)
    )
  }

  inx <- sort(sapply(result, function(r) countFun(r$nRows, r$nCols)),
    index.return = TRUE, decreasing = TRUE
  )

  result <- result[inx$ix]
  result
}


#' Combine More Than One \code{ldtsearch} Objects
#'
#' @param list a list with \code{ldtsearch} objects
#' @param type1Name the name of 'type1' in the object
#'
#' @return the combined \code{ldtsearch} object
combineSearch <- function(list, type1Name = "coefs") {
  if (length(list) == 1) {
    return(list[[1]])
  }

  cmb_best_ind <- function(item, oldYNames, oldXNames, newYNames, newXNames) {
    if (is.null(item)) {
      return(NULL)
    }
    # send the best one to this function along with the old and new names
    # we should just update the indexes
    item$depIndices <- sort(as.integer(sapply(
      item$depIndices,
      function(q) which(oldYNames == newYNames[q])
    )))
    item$exoIndices <- sort(as.integer(sapply(
      item$exoIndices,
      function(q) which(oldXNames == newXNames[q])
    )))

    return(item)
  }

  cmb_inclusion <- function(first, second) {
    # assuming first column is mean and second column counts
    firstNames <- rownames(first)
    secondNames <- rownames(second)
    first[, 1] <- first[, 1] * first[, 2] # convert first column to sum
    j <- 0
    for (n in secondNames) {
      j <- j + 1
      ind <- which(n == firstNames)
      if (length(ind) > 0) {
        ind <- ind[[1]]
        first[[ind, 1]] <- first[[ind, 1]] + second[[j, 1]] * second[[j, 2]]
        first[[ind, 2]] <- first[[ind, 2]] + second[[j, 2]]
      }
    }
    first[, 1] <- first[, 1] / first[, 2] # convert back to mean
    return(first)
  }

  cmb_cdf <- function(first, second) {
    # assuming columns are: mean, count, weights
    firstNames <- rownames(first)
    secondNames <- rownames(second)
    first[, 1] <- first[, 1] * first[, 3] # convert first column to weighted sum
    j <- 0
    for (n in secondNames) {
      j <- j + 1
      ind <- which(n == firstNames)
      if (length(ind) > 0) {
        ind <- ind[[1]]
        first[[ind, 1]] <- first[[ind, 1]] + second[[j, 1]] * second[[j, 3]]
        first[[ind, 2]] <- first[[ind, 2]] + second[[j, 2]]
        first[[ind, 3]] <- first[[ind, 3]] + second[[j, 3]]
      }
    }
    first[, 1] <- first[, 1] / first[, 3] # convert back to mean
    return(first)
  }

  cmb_extremebound <- function(first, second) {
    # assuming first is lower and second column is upper bounds
    firstNames <- rownames(first)
    secondNames <- rownames(second)
    j <- 0
    for (n in secondNames) {
      j <- j + 1
      ind <- which(n == firstNames)
      if (length(ind) > 0) {
        ind <- ind[[1]]
        first[[ind, 1]] <- min(first[[ind, 1]], second[[j, 1]])
        first[[ind, 2]] <- max(first[[ind, 2]], second[[j, 2]])
      }
    }
    return(first)
  }

  cmb_mixture <- function(first, second) {
    # assuming that columns are: mean, variance, skewness, kurtosis,count, sumWeights

    firstNames <- rownames(first)
    secondNames <- rownames(second)
    j <- 0
    for (n in secondNames) {
      j <- j + 1
      ind <- which(n == firstNames)
      if (length(ind) > 0) {
        ind <- ind[[1]]
        mix <- GetCombination4Moments(
          list(
            mean = first[[ind, 1]], variance = first[[ind, 2]],
            skewness = first[[ind, 3]], kurtosis = first[[ind, 4]],
            count = first[[ind, 5]], sumWeights = first[[ind, 6]]
          ),
          list(
            mean = second[[j, 1]], variance = second[[j, 2]],
            skewness = second[[j, 3]], kurtosis = second[[j, 4]],
            count = second[[j, 5]], sumWeights = second[[j, 6]]
          )
        )
        first[ind, 1] <- mix$mean
        first[ind, 2] <- mix$variance
        first[ind, 3] <- mix$skewness
        first[ind, 4] <- mix$kurtosis
        first[ind, 5] <- mix$count
        first[ind, 6] <- mix$sumWeights
      }
    }

    return(first)
  }


  result <- list[[1]]
  for (i in c(2:length(list))) {
    newR <- list[[i]]

    result$info$endTime <- newR$info$endTime

    result$counts$expectedCount <- result$counts$expectedCount + newR$counts$expectedCount
    result$counts$searchedCount <- result$counts$searchedCount + newR$counts$searchedCount
    result$counts$failedCount <- result$counts$failedCount + newR$counts$failedCount
    if (is.null(result$counts$failedDetails))
      result$counts$failedDetails = list()

    for (f in newR$counts$failedDetails) {
      j <- 0
      added <- FALSE
      for (f0 in result$counts$failedDetails){
        j <- j + 1
        if (f$message == f0$message){
          added <- TRUE
          result$counts$failedDetails[[j]]$count = f0$count + f$count
          break
        }
      }

      if (added == FALSE){
        ind <- length(result$counts$failedDetails) + 1
      }
    }

    for (m in c(2:(length(result) - 1))) {
      mea0 <- result[[m]]
      mea1 <- newR[[m]]
      for (t in c(1:length(mea0))) {
        tar0 <- mea0[[t]]
        tar1 <- mea1[[t]]

        # BEST
        if (is.null(tar0$model$bests) == FALSE) {
          # update indexes
          news <- lapply(tar1$model$bests, function(a) {
            cmb_best_ind(
              a, result$info$yNames,
              result$info$xNames, newR$info$yNames, newR$info$xNames
            )
          })
          # combine, sort by weights and select a subset
          b_count <- length(tar0$model$bests)
          all <- as.list(append(tar0$model$bests, news))

          idx <- as.numeric(sapply(all, function(v) v$weight))
          all <- all[order(idx, decreasing = TRUE)][1:b_count]
          names(all) <- paste0("best", c(1:b_count))

          result[[m]][[t]]$model$bests <- all
        }

        # ALL
        if (is.null(tar0$model$all) == FALSE) {
          for (a in tar1$model$all) {
            result[[m]][[t]]$model$all[[length(result[[m]][[t]]$model$all) + 1]] <-
              cmb_best_ind(
                a, result$info$yNames,
                result$info$xNames, newR$info$yNames, newR$info$xNames
              )
          }
        }
        # INCLUSION
        if (is.null(tar0$model$inclusion) == FALSE) {
          result[[m]][[t]]$model$inclusion <- cmb_inclusion(
            tar0$model$inclusion,
            tar1$model$inclusion
          )
        }


        # EXTREME BOUND
        if (is.null(tar0[[type1Name]]$extremeBounds) == FALSE) {
          eb <- cmb_extremebound(tar0[[type1Name]]$extremeBounds, tar1[[type1Name]]$extremeBounds)
          result[[m]][[t]][[type1Name]]$extremeBounds <- eb
        }

        # CDFs
        if (is.null(tar0[[type1Name]]$cdfs) == FALSE) {
          j <- 0
          for (c0 in tar0[[type1Name]]$cdfs) {
            j <- j + 1
            result[[m]][[t]][[type1Name]]$cdfs[[j]] <- cmb_cdf(c0, tar1[[type1Name]]$cdfs[[j]])
          }
        }

        # MIXTURE
        if (is.null(tar0[[type1Name]]$mixture) == FALSE) {
          a <- cmb_mixture(tar0[[type1Name]]$mixture, tar1[[type1Name]]$mixture)
          result[[m]][[t]][[type1Name]]$mixture <- a
        }

        # TYPE1 BESTS
        if (is.null(tar0[[type1Name]]$bests) == FALSE) {
          # for each item in this list, there is a list of bests

          for (item1 in tar1[[type1Name]]$bests) {
            item1 <- item1[lengths(item1) != 0]
            # update its indexes

            news <- lapply(
              item1[2:length(item1)] # first one is the name
              , function(a) {
                cmb_best_ind(
                  a, result$info$yNames, result$info$xNames,
                  newR$info$yNames, newR$info$xNames
                )
              }
            )
            # we should whether append it or update an existing list
            found <- FALSE
            o <- 0
            for (item0 in tar0[[type1Name]]$bests) {
              item0 <- item0[lengths(item0) != 0]
              o <- o + 1
              if (item1$name == item0$name) {
                found <- TRUE
                # combine, sort by weights and select a subset
                b_count <- length(item0)
                all <- as.list(append(item0[2:length(item0)], news))
                idx <- as.numeric(sapply(all, function(v) v$weight))
                all <- all[order(idx, decreasing = TRUE)][1:b_count]
                names(all) <- paste0("best", c(1:b_count))

                all <- c(name = item1$name, all)

                result[[m]][[t]][[type1Name]]$bests[[o]] <- all
                break
              }
            }
            if (found == FALSE) {
              news <- c(name = item1$name, news)
              result[[m]][[t]][[type1Name]]$bests[[o]] <- news
            }
          }
        }
      }
    }
  }
  return(result)
}


#' Stepwise estimation
#'
#' @param method sur, dc or varma
#' @param data exogenous (for sur and dc) or endogenous (for varma)
#' @param sizes detemines the steps
#' @param counts determines the size in each step
#' @param savePre if not \code{NULL}, it saves and tries to load the
#' progress of search step in a file (name=\code{paste0(savePre,i)} where
#' \code{i} is the index of the step).
#' @param printMsg If true, some information about the steps is printed.
#' Note that it is different from searchers' \code{printMsg}.
#' @param ... Additional arguments
#'
#' @return the result
Search_s <- function(method, data, sizes = list(c(1, 2), c(3, 4), c(5), c(6:10)),
                     counts = c(NA, 40, 30, 20), savePre, printMsg = FALSE, ...) {
  dots <- list(...)

  # TODO: check search items here
  # dots <- list(...)
  # if (dots$searchItems)
  if (length(sizes) != length(counts)) {
    stop("Invalid number of elements in 'counts'.")
  }

  if (is.null(colnames(data))) {
    stop("'data' must have column names.")
  }

  estims <- list()
  data_i <- NULL

  for (i in c(1:length(sizes))) {
    if (printMsg) {
      cat(paste0("Step ", i, ". estimation started...\n"))
    }

    if (is.null(savePre) == FALSE) {
      res <- suppressWarnings(tryCatch(readRDS(paste0(
        savePre, i,
        ".RData"
      )), error = function(e) NULL))
      if (is.null(res) == FALSE) {
        if (printMsg) {
          cat(paste0("  ... finished (read from file).\n"))
        }
        estims[[i]] <- res$estim
        data_i <- res$data
        next()
      }
    }

    size_i <- sizes[[i]]
    count_i <- counts[[i]]
    if (is.na(count_i)) {
      count_i <- ncol(data)
    }

    if (i == 1) {
      if (count_i < ncol(data)) {
        warning(paste0("First ", count_i, " variables are selected in the first step."))
        if (printMsg) {
          cat(paste0("Selected Variables: First", count_i, "Variables.\n"))
        }
      } else if (printMsg) {
        cat(paste0("Selected Variables: All Variables.\n"))
      }
      data_i <- data[, c(1:count_i)] # however, the first one should
      # be NA such that no arbitrary selection happens here
    } else { # use previous to select
      est <- estims[[i - 1]]
      x_i_names <- colnames(data_i)

      # find best models and sort them
      bests <- list()
      for (m in c(2:(length(est) - 1))) {
        mea <- est[[m]]
        for (t in c(1:length(m))) {
          tar <- mea[[t]]
          if (is.null(tar$model$bests) == FALSE && length(tar$model$bests) > 0) {
            for (b in c(1:length(tar$model$bests))) {
              bst <- tar$model$bests[[b]]
              bests[[length(bests) + 1]] <- list(index = b, names = x_i_names[bst$exoIndices])
            }
          }
        }
      }

      # sort by index
      idx <- as.numeric(sapply(bests, function(b) {
        b$index
      }))
      bests <- bests[order(idx, decreasing = FALSE)]

      vars <- c()
      for (bns in bests) {
        vars <- append(vars, bns$names) # we add a group and its length might exceed the count
        vars <- unique(vars)
        if (length(vars) >= count_i) {
          break
        }
      }

      # TODO: we can use or fill the gap with Inclusion weights. However,
      # first we should determine which measure or target to use ?!

      if (printMsg) {
        cat(paste0("Selected Variables: ", paste0(vars, sep = ";", collapse = ""), "\n"))
      }

      data_i <- data_i[, vars, drop = FALSE]
    }
    if (any(ncol(data_i) < size_i)) {
      stop("There is not enough variables in this step. Increase the value of
      'bestK' or if you have fix variables, adjust the sizes.")
    }

    if (method == "sur") {
      estims[[i]] <- SurSearch(x = data_i, xSizes = size_i, ...)
    } else if (method == "dc") {
      estims[[i]] <- DcSearch(x = data_i, xSizes = size_i, ...)
    } else if (method == "varma") {
      estims[[i]] <- VarmaSearch(y = data_i, ySizes = size_i, ...)
    } else {
      stop("invalid method")
    }

    if (estims[[i]]$counts$searchedCount == estims[[i]]$counts$failedCount) {
      if (printMsg){
         print("......Failures.......")
         print(estims[[i]]$counts$failedDetails)
      }
      stop("all estimations failed")
    }

    if (is.null(savePre) == FALSE) {
      saveRDS(list(estim = estims[[i]], data = data_i), paste0(savePre, i, ".RData"))
    }

    if (printMsg) {
      cat(paste0("  ... finished.\n\n"))
    }
  }


  result <- combineSearch(estims, if (method == "varma") "predictions" else "coefs")



  if (method == "sur") {
    class(result) <- c("ldtsearchsur", "ldtsearch", "list")
    attr(result, "method") <- "sur"
  } else if (method == "dc") {
    class(result) <- c("ldtsearchdc", "ldtsearch", "list")
    attr(result, "method") <- "dc"
  } else if (method == "varma") {
    class(result) <- c("ldtsearchvarma", "ldtsearch", "list")
    attr(result, "method") <- "varma"
  }

  return(result)
}




#' Extract Coefficients from a list of \code{ldtestim} object
#'
#'
#' @param list a named list of \code{ldtestim} objects.
#' @param depInd index of the dependent variable.
#' @param regInfo A list of pairs of keys and names to determine
#' the information at the bottom of the table. Use "" (empty) for
#' empty rows. \code{num_eq} and \code{num_endo} (and \code{num_x} and
#' \code{num_exo}) will be different with PCA analysis enabled.
#' @param hnameFun A function to change the name of the headers.
#' @param vnamesFun A function to change the name of the variables or the codes in \code{regInfo}.
#' @param vnamesFun_sub A list for replacing special characters vectors in \code{vnamesFun}.
#' @param vnamesFun_max Maximum length for names in \code{vnamesFun}.
#' @param tableFun A function (i.e., \code{function(coef,std,pvalue,minInColm,maxInCol)})
#' one of the following for default sign or coefficients table: "sign",
#' "sign_star", "coef", "coef_star", "coef_star_std"
#' @param formatNumFun A function to format the numbers if \code{tableFun} uses default values.
#' @param numCoefs if \code{NA}, it inserts all coefficients. If a positive number,
#' it inserts that number of coefficients.
#' @param formatLatex If true, default options are for 'latex', otherwise, 'html'.
#'
#' @details
#' #' Possible codes (first element) for \code{regInfo}:
#' \itemize{
#' \item "" : empty line
#' \item num_obs : No. Obs.; number of observations.
#' \item num_endo : No. Eq. (orig.); original number of equations
#' or endogenous variables before being changed by PCA analysis.
#' \item pca_y_exact : PCA Count (y);
#' \item pca_y_cutoff : PCA Cutoff (y)
#' \item pca_y_max : PCA Max (y)
#' \item num_eq : No. Eq.; number of equations after PCA analysis.
#' \item num_exo : No. Exo. (orig.)
#' \item pca_x_exact : PCA Count (x)
#' \item pca_x_cutoff : PCA Cutoff (x)
#' \item pca_x_max : PCA Max (x)
#' \item num_x : No. Exo.
#' \item num_x_all : No. Exo. (all); number of explanatory variables in all equations.
#' \item num_rest : No. Rest.; number of restrictions in the equation
#' \item sigma2 : S.E. Reg.
#' \item ... others can be a measure name (i.e., elements of 'measures' item in the results)
#' }
#'
#' @return the generated table.
#' @export
CoefTable <- function(list, depInd = 1,
                      regInfo = list(
                        c("", " "), c("num_obs", "No. Obs."), c("num_eq", "No. Eq."),
                        c("num_x", "No. Exo."), c("sigma2", "S.E. Reg."),
                        c("aic", "AIC"), c("sic", "SIC")
                      ),
                      hnameFun = function(x) x,
                      vnamesFun = function(x) x,
                      vnamesFun_sub = list(c("%", "\\\\%"), c("_", "\\\\_")), vnamesFun_max = 20,
                      tableFun = "coef_star", formatNumFun = function(colIndex, x) {
                        x
                      }, numCoefs = NA, formatLatex = TRUE) {
  get_stars <- function(pvalue) {
    if (is.nan(pvalue)) { # e.g., restricted to zero
      return(if (formatLatex) "\\textsuperscript{(r)}" else "<sup>(r)</sup>")
    }
    paste0(
      (if (formatLatex) "\\textsuperscript{" else "<sup>"),
      if (pvalue <= 0.01) {
        "***"
      } else if (pvalue <= 0.05) {
        "**"
      } else if (pvalue <= 0.1) {
        "*"
      } else {
        ""
      }, (if (formatLatex) "}" else "</sup>")
    )
  }

  if (tableFun == "sign") {
    tableFun <- function(j, coef, std, pvalue) {
      if (coef > 0) {
        "+"
      } else if (coef < 0) {
        "-"
      } else {
        "0"
      }
    }
  }
  if (tableFun == "sign_star") {
    tableFun <- function(j, coef, std, pvalue) {
      paste0(if (coef > 0) {
        "+"
      } else if (coef < 0) {
        "-"
      } else {
        "0"
      }, get_stars(pvalue))
    }
  } else if (tableFun == "coef") {
    tableFun <- function(j, coef, std, pvalue) {
      formatNumFun(j, coef)
    }
  } else if (tableFun == "coef_star") {
    tableFun <- function(j, coef, std, pvalue) {
      paste0(formatNumFun(j, coef), get_stars(pvalue))
    }
  } else if (tableFun == "coef_star_std") {
    tableFun <- function(j, coef, std, pvalue) {
      paste0(formatNumFun(j, coef), get_stars(pvalue), " (", formatNumFun(j, std), ")")
    }
  }

  list_names <- names(list)
  if (is.null(list_names)) {
    col_names <- paste0("m", rep(1:length(list)))
  } else {
    col_names <- lapply(list_names, function(n) hnameFun(n))
  }
  vnames <- unique(unlist(lapply(list, function(e) row.names(e$estimations$coefs))))
  vnames_0 <- sapply(vnames, function(n) {
    r <- vnamesFun(n)
    for (sg in vnamesFun_sub) {
      r <- gsub(sg[[1]], sg[[2]], r, fixed = TRUE)
    }
    if (nchar(r) > vnamesFun_max) {
      r <- paste0(substr(r, 1, vnamesFun_max - 3), "...")
    }
    r
  })
  regnam_0 <- sapply(regInfo, function(n) n[[2]])
  numCoefs <- as.integer(numCoefs)
  if (is.na(numCoefs)) {
    vvnames <- c(vnames_0, regnam_0)
  } else if (numCoefs > 0) {
    vvnames <- c(vnames_0[1:numCoefs], regnam_0)
  } else {
    vvnames <- regnam_0
  }

  r_table <- matrix("", (if (is.na(numCoefs)) {
    length(vnames) + length(regInfo)
  } else {
    length(regInfo) + numCoefs
  }), length(col_names))
  rownames(r_table) <- vvnames
  colnames(r_table) <- col_names

  j <- 0
  for (e in list) {
    j <- j + 1

    ns <- row.names(e$estimations$coefs)
    i <- 0
    # insert coefficients
    for (v in ns) {
      i <- i + 1
      if (is.na(numCoefs) == FALSE && numCoefs < i) {
        break
      }
      ind <- which(vnames == v)
      if (length(ind) == 1) {
        k <- ind[[1]]
        coef <- e$estimations$coefs[i, depInd]
        std <- e$estimations$stds[i, depInd]
        pv <- e$estimations$pValues[i, depInd]
        r <- tableFun(j, coef, std, pv)
        r_table[k, j] <- r
      }
    }


    # insert regression information
    simResNames <- names(e$simulation$results)
    i <- if (is.na(numCoefs)) length(vnames) else numCoefs
    for (rn in regInfo) {
      i <- i + 1
      v <- NULL
      r <- rn[[1]]

      if (r == "") {
        v <- "" # for an empty cell/line
      } else if (r == "num_obs") {
        v <- formatNumFun(j, as.integer(e$counts$obs))
      } else if (r == "num_eq") {
        v <- formatNumFun(j, as.integer(e$counts$eq))
      } else if (r == "num_endo") {
        v <- formatNumFun(j, as.integer(ncol(e$info$y)))
      } else if (r == "num_x") {
        v <- formatNumFun(j, as.integer(length(which(
          is.na(e$estimations$coefs[, depInd]) == FALSE
        ))))
      } else if (r == "num_exo") {
        v <- formatNumFun(j, as.integer(ncol(e$info$x)))
      } else if (r == "num_x_all") {
        v <- formatNumFun(j, as.integer(e$counts$exoAll))
      } else if (r == "num_rest") {
        v <- if (is.null(e$estimations$isRestricted)) {
          0L
        } else {
          as.integer(sum(e$estimations$isRestricted[, depInd]))
        }
      } else if (r == "pca_y_exact") {
        v <- formatNumFun(j, as.integer(e$info$pcaOptionsY$exactCount))
      } else if (r == "pca_x_exact") {
        v <- formatNumFun(j, as.integer(e$info$pcaOptionsX$exactCount))
      } else if (r == "pca_y_cutoff") {
        v <- formatNumFun(j, if (is.null(e$info$pcaOptionsY$exactCount) == F &&
          e$info$pcaOptionsY$exactCount == 0) {
          e$info$pcaOptionsY$cutoffRate
        } else {
          NA
        })
      } else if (r == "pca_x_cutoff") {
        v <- formatNumFun(j, if (is.null(e$info$pcaOptionsX$exactCount) == F &&
          e$info$pcaOptionsX$exactCount == 0) {
          e$info$pcaOptionsX$cutoffRate
        } else {
          NA
        })
      } else if (r == "pca_y_max") {
        v <- formatNumFun(j, as.integer(if (is.null(e$info$pcaOptionsY$exactCount) == F &&
          e$info$pcaOptionsY$exactCount == 0) {
          e$info$pcaOptionsY$max
        } else {
          NA
        }))
      } else if (r == "pca_x_max") {
        v <- formatNumFun(j, as.integer(if (is.null(e$info$pcaOptionsX$exactCount) == F &&
          e$info$pcaOptionsX$exactCount == 0) {
          e$info$pcaOptionsX$max
        } else {
          NA
        }))
      } else if (r == "sigma2") {
        v <- formatNumFun(j, e$estimations$sigma[depInd, depInd])
      } else { # must be a measure
        ind <- which(r == rownames(e$measures))
        if (length(ind) != 0) {
          v <- formatNumFun(j, e$measures[ind, depInd])

          if (r == "f") {
            ind0 <- which("fProb" == rownames(e$measures))
            fp <- e$measures[ind0, depInd]
            if (is.na(fp)) {
              warning("p-value of 'F' statistics is NA.")
            }
            v <- paste0(v, get_stars(fp))
          }
        }
      }

      if (is.null(v) || length(v) == 0) {
        warning(paste0("Invalid 'regInfo' member is requested. 'NA' is used. code=", r))
        v <- NA
      }
      r_table[i, j] <- v
    }
  }

  return(r_table)
}




is.empty <- function(arg) {
  if (is.null(arg) || is.na(arg)) {
    return(TRUE)
  }
  if (is.list(arg)) {
    if (length(arg) == 0) {
      return(TRUE)
    }
  } else if (is.character(arg)) {
    if (arg == "") {
      return(TRUE)
    }
    return(FALSE)
  } else {
    stop("invalid type")
  }
}

list.has.array <- function(List, arr) {
  for (a in List) {
    if (length(a) == length(arr)) {
      if (all(a == arr)) {
        return(TRUE)
      }
    }
  }
  return(FALSE)
}

if.not.null <- function(arg, default = NULL) {
  if (is.null(arg)) {
    return(default)
  }
  return(arg)
}
