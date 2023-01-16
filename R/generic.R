
#' Prints an \code{ldtf} object
#'
#' @param x An \code{ldtf} object
#' @param ... additional arguments
#'
#' @return \code{NULL}
#' @export
print.ldtf <- function(x, ...) {
  if (is.null(x)) {
    stop("argument is null.")
  }
  if (any(class(x) == "ldtf") == FALSE) {
    stop("Invalid class")
  }
  s <- ToString_F0(x)

  cat("Frequency: ", s$value, " (", s$classType, ": ", s$class, ")", sep = "")
  return(NULL)
}

#' Prints an \code{ldtv} object
#'
#' @param x An \code{ldtv} object
#' @param ... additional arguments
#'
#' @return \code{NULL}
#' @export
print.ldtv <- function(x, ...) {
  if (is.null(x)) {
    stop("argument is null.")
  }
  if (any(class(x) == "ldtv") == FALSE) {
    stop("Invalid class")
  }
  if (any(class(x$startFrequency) == "ldtf") == FALSE) {
    stop("Invalid frequency class")
  }

  n <- length(x$fields)
  if (n > 0) {
    fields <- paste0(
      unlist(sapply(
        c(1:n),
        function(i) {
          paste0(
            x$fields[[i]][1], " = ",
            x$fields[[i]][2]
          )
        }
      )),
      sep = "\n", strrep(" ", 8), collapse = ""
    )
  } else {
    fields <- NULL
  }
  s <- ToString_F0(x$startFrequency)
  cat("Variable:\n",
    "    Name = ", x$name, "\n",
    "    Length = ", length(x$data), "\n",
    "    Frequency Class = ", s$classType, ": ", s$class, "\n",
    "    Start Frequency = ", s$value, "\n",
    "    Fields:\n", strrep(" ", 8), if (is.null(fields)) "" else fields,
    sep = ""
  )
  return(NULL)
}

#' Converts an \code{ldtv} object to a \code{data.frame}
#'
#' @param x An \code{ldtv} object
#' @param ... additional arguments
#'
#' @export
to.data.frame <- function(x, ...) {
  if (is.null(x)) {
    stop("argument is null.")
  }
  if (any(class(x) == "ldtv") == FALSE) {
    stop("Invalid class.")
  }
  if (is.null(x$data)) {
    stop("Variable's data array is null.")
  }
  if (is.null(x$startFrequency)) {
    stop("Variable's frequency is null.")
  }

  df <- as.data.frame(x$data)
  colnames(df) <- if (is.null(x$name)) "V" else x$name

  freqs <- Sequence_F(x$startFrequency, length(x$data))
  rownames(df) <- freqs

  df
}

#' Print an \code{ldtsearch} object
#'
#' @param x \code{ldtsearch} object
#' @param ... additional arguments
#'
#' @return \code{NULL}
#' @export
print.ldtsearch <- function(x, ...) {
  if (is.null(x)) {
    stop("argument is null.")
  }
  if (any(class(x) == "ldtsearch") == FALSE) {
    stop("Invalid class")
  }

  cat("method:", attr(x, "method"), "\n")
  cat("expected: ", prettyNum(x$counts$expectedCount, big.mark = ","), ", searched: ",
    prettyNum(x$counts$searchedCount, big.mark = ","), " (",
    round(x$counts$searchedCount / x$counts$expectedCount * 100,1), "%)",
    ", failed: ", prettyNum(x$counts$failedCount, big.mark = ","), " (",
    round(x$counts$failedCount / x$counts$searchedCount * 100,1), "%)\n",
    sep = ""
  )
  cat("--------\n")
  if (x$counts$failedCount > 0){
    cat("Failures:\n")
    i <- 0
    for (fd in x$counts$failedDetails){
      i <- i + 1
      cat(i,". ",fd$message, ": ", fd$count
          ," (", round((fd$count/x$counts$failedCount)*100,1) ,"%)\n", sep = "")
    }
    cat("--------\n")
  }

  nms <- names(x)
  i <- 0
  for (m in x) {
    i <- i + 1
    if (i == 1 || i > length(x) - 1 || is.null(m)) {
      next()
    }
    cat(i - 1, ". ", nms[[i]], ":\n", sep = "")
    j <- 0
    for (t in m) {
      j <- j + 1
      if (is.null(t)) {
        next()
      }
      cat(" ", t$name, " ", sep = "")
      if (is.null(t$model) || is.null(t$model$bests) || length(t$model$bests) > 0) {
        cat("(best=", round(GetMeasureFromWeight(t$model$bests[[1]]$weight, nms[[i]]), 3), ")\n", sep = "")
      } else {
        cat("(best model is missing)\n", sep = "")
      }
    }
  }
  return(NULL)
}


#' Get Estimation from Search Result
#'
#' @param searchRes an object of class \code{ldtsearch}
#' @param endoIndices endogenous indices
#' @param exoIndices exogenous indices
#' @param y dependent variables data
#' @param x exogenous variables data
#' @param printMsg argument to be passed to the estimation methods
#' @param ... additional arguments
#'
#' @return estimation result
#' @export
GetEstim <- function(searchRes, endoIndices, exoIndices, y, x, printMsg, ...) {
  if (is.null(endoIndices) == FALSE && is.null(y) == FALSE) {
    y <- as.matrix(y)
  }
  if (is.null(exoIndices) == FALSE && is.null(x) == FALSE) {
    x <- as.matrix(x)
  }

  method <- attr(searchRes, "method")
  # tryCatch({
  if (method == "sur") {
    GetEstim_sur(searchRes, endoIndices, exoIndices, y, x, printMsg = printMsg, ...)
  } else if (method == "dc") {
    GetEstim_dc(searchRes, endoIndices, exoIndices, y, x, printMsg = printMsg, ...)
  } else if (method == "varma") {
    GetEstim_varma(searchRes, endoIndices, exoIndices, y, x, printMsg = printMsg, ...)
  } else {
    stop("not implemented method")
  }
  # },error = function(e) e)
}

getMeasureFrom <- function(model, measureName, tarIndex, method) {
  rowNames <- rownames(model$measures)
  ind <- which(rowNames == measureName)
  if (length(ind) == 0) {
    stop(paste0("measure not found (method = sur, measure = ", measureName, ")"))
  }
  r <- model$measures[ind[[1]], tarIndex]
  r
}

#' Summarize an \code{ldtsearch} object
#'
#' @param object \code{ldtsearch} object
#' @param y dependent variables data (Data is not saved in \code{object})
#' @param x exogenous variables data (Data is not saved in \code{object})
#' @param addModelBests if \code{TRUE} and 'model$bests' exists
#' (see \code{[GetSearchItems()]}), it estimates them.
#' @param addModelAll if \code{TRUE} and 'all' exists
#' (see \code{[GetSearchItems()]}), it estimates them.
#' @param addItem1Bests if \code{TRUE} and 'item1' exists
#' (see \code{[GetSearchItems()]}), it estimates them.
#' @param printMsg if \code{TRUE} details are printed.
#' @param w weight of observations (if available, e.g., in discrete choice estimation.
#' Data is not saved in \code{object})
#' @param newX new exogenous data (if available, e.g., in varma estimation.
#' Data is not saved in \code{object})
#' @param test If \code{TRUE}, it helps you make sure everything is working. Please report errors.
#' @param ... additional arguments
#'
#' @return a list with estimated models along with other kind of information.
#' Its structure is similar to the given \code{ldtsearch} object.
#' @export
summary.ldtsearch <- function(object, y, x = NULL, addModelBests = TRUE,
                              addModelAll = FALSE, addItem1Bests = FALSE,
                              printMsg = FALSE, w = NULL, newX = NULL,
                              test = FALSE, ...) {
  test_perc <- 1e-12

  if (is.null(object)) {
    stop("argument is null.")
  }
  if (any(class(object) == "ldtsearch") == FALSE) {
    stop("Invalid class")
  }


  result <- list()
  result$method <- attr(object, "method")
  result$SearchPerc <- object$counts$searchedCount / object$counts$expectedCount * 100
  result$FailedPerc <- object$counts$failedCount / object$counts$searchedCount * 100
  result$MeasureNames <-
    names(object)[2:(length(object) - 1)] # the last element is the inputs

  i <- 0
  for (mea in result$MeasureNames) {
    i <- i + 1
    x_mea <- object[[mea]]
    targets <- names(x_mea)
    if (is.null(result$TargetNames)) {
      result$TargetNames <- c()
      for (targ in targets) {
        result$TargetNames <- append(result$TargetNames, x_mea[[targ]]$name)
      }
    }

    j <- 0
    for (targ in targets) {
      j <- j + 1
      x_tar <- x_mea[[targ]]
      x_names <- names(x_tar)


      x_mod <- x_tar$model
      if (is.null(x_mod) == FALSE) {
        if (addModelBests) {
          if (is.null(x_mod$bests) == FALSE) {
            k <- 0
            for (b in x_mod$bests) {
              k <- k + 1
              su_m <- GetEstim(object, b$depIndices, b$exoIndices, y, x,
                printMsg = printMsg,
                params = b$parameters, newX = newX, w = w,
                distType = if (is.integer(b$dist) && b$dist == 0) "logit" else "probit"
              )
              if (test) {
                jt <- if (is.null(b$depIndices) || length(b$depIndices) == 0) {
                  1
                } else {
                  which(b$depIndices == j)[[1]]
                } # index of target in endogenous data
                testthat::expect_equal(
                  GetWeightFromMeasure(
                    getMeasureFrom(su_m, mea, jt, result$method), mea
                  ),
                  b$weight,
                  tolerance = test_perc
                )
              }
              result[[mea]][[targ]][[paste0("model")]][[paste0("bests")]][[paste0("best", k)]] <- su_m # nolint
            }
          }
        }

        if (addModelAll) {
          if (is.null(x_mod$all) == FALSE) {
            k <- 0
            for (b in x_mod$all) {
              k <- k + 1

              su_m <- GetEstim(object, b$depIndices, b$exoIndices, y, x,
                printMsg = printMsg,
                params = b$parameters, newX = newX, w = w,
                distType = if (is.integer(b$dist) && b$dist == 0) "logit" else "probit"
              )
              if (test) {
                jt <- if (is.null(b$depIndices) || length(b$depIndices) == 0) {
                  1
                } else {
                  which(b$depIndices == j)[[1]]
                }
                testthat::expect_equal(
                  GetWeightFromMeasure(getMeasureFrom(su_m, mea, jt, result$method), mea),
                  b$weight,
                  tolerance = test_perc
                )
              }
              result[[mea]][[targ]][[paste0("model")]][[paste0("all")]][[paste0("model", k)]] <- su_m # nolint
            }
          }
        }
      }

      p <- if (is.null(x_mod)) 2 else 3
      if (length(x_names) >= p) {
        type1_name <- x_names[[p]]
        x_type1 <- x_tar[[type1_name]]
        if (is.null(x_type1) == FALSE) {
          if (addItem1Bests) {
            if (is.null(x_type1$bests) == FALSE) {
              items_names <- names(x_type1$bests)

              n <- 0
              for (item in x_type1$bests) {
                n <- n + 1
                k <- 0
                for (b in item) {
                  if (is.null(b)) {
                    next
                  }

                  k <- k + 1
                  if (k != 1) { # first one is name (note k-1 in the next line)

                    su_m <-
                      GetEstim(object, b$depIndices, b$exoIndices, y, x,
                        printMsg = printMsg,
                        params = b$parameters, newX = newX, w = w,
                        distType = if (is.integer(b$dist) && b$dist == 0) "logit" else "probit"
                      )

                    if (test) {
                      jt <- if (is.null(b$depIndices) || length(b$depIndices) == 0) {
                        1
                      } else {
                        which(b$depIndices == j)[[1]]
                      }
                      testthat::expect_equal(
                        GetWeightFromMeasure(
                          getMeasureFrom(su_m, mea, jt, result$method), mea
                        ), b$weight,
                        tolerance = test_perc
                      )

                      if (result$method == "sur" || result$method == "dc") {
                        ind <- which(item$name == rownames(su_m$estimations$coefs))
                        testthat::expect_equal(su_m$estimations$coefs[ind, jt], b$mean,
                          tolerance = 1e-14
                        )
                        testthat::expect_equal(su_m$estimations$stds[ind, jt], sqrt(b$var),
                          tolerance = 1e-14
                        )
                      } else if (result$method == "varma") {
                        # item$name is Horizon1,...
                        ind <- as.integer(substr(item$name, 8, 8)) + su_m$prediction$startIndex - 1
                        testthat::expect_equal(as.numeric(su_m$prediction$means[jt, ind]),
                          as.numeric(b$mean),
                          tolerance = 1e-14
                        )
                        testthat::expect_equal(as.numeric(su_m$prediction$vars[jt, ind]),
                          as.numeric(b$var),
                          tolerance = 1e-14
                        )
                      }
                    }

                    result[[mea]][[targ]][[type1_name]][[paste0("bests")]][[items_names[[n]]]][[paste0("best", k - 1)]] <- su_m # nolint
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  class(result) <- c("summary.ldtsearch", "list")
  return(result)
}


#' Gets Data of an \code{ldtsearch} Object
#'
#'
#' There are five types of indices in this function: measures,
#' targets, bests, type1's items, equations.
#' Use \code{NULL} to use all available information or specify them.
#'
#' @param x an \code{ldtsearch} object
#' @param types (string vector) one or more that one type of information
#' to be included in the the data.frame
#' @param measures (integer or character array) measures to be used.
#' @param targets (integer or character array) targets to be used.
#' @param rows (integer or character array) If the requested object is a matrix
#' (or an array), it determines the rows and cannot be \code{NULL}. For
#' \code{type1bests} this is the name of the variables.
#' @param columns (integer or character array) If the requested object is
#' a matrix, it determines the columns and cannot be \code{NULL}. For
#' \code{type1bests} this is the name of the fields: \code{weight}, \code{mean}, \code{var}
#' @param itemIndices (integer array) items such as \code{bests} to be used.
#' @param colNamFun (function) a function to determine the column names. The
#' argument is a list of names, i.e., one of the following items: \code{target},
#' \code{measure}, \code{row}, \code{column}, \code{item}.
#' @param rowContent (string) determines the type of information in the rows of
#' returned \code{data.frame}. Some items are not available for some \code{types}.
#' \code{row} is generally for variables in the rows of matrices such as \code{inclusion} or
#' \code{mixture}. \code{column} is generally for the columns of such matrices.
#' \code{item} is for the \code{best} models or models in the \code{all} field.
#' @param cdfIndex (integer) The index of CDF if \code{type} is \code{cdf}
#' @param ... additional arguments
#'
#' @return a data.frame that contains data.
#'
#' @export
to.data.frame <- function(x, types = c(
                                      "bestweights", "allweights", "inclusion",
                                      "type1bests", "cdf", "extremebounds", "mixture"
                                    ),
                                    measures = NULL, targets = NULL,
                                    rows = NULL, columns = NULL, itemIndices = NULL,
                                    colNamFun = function(ns) {
                                      paste(ns[lengths(ns) > 0],
                                        collapse = "."
                                      )
                                    },
                                    rowContent = c("measure", "target", "item", "row", "column"),
                                    cdfIndex = 0, ...) {
  if (is.null(x)) {
    stop("argument is null.")
  }

  getValueInMat <- function(mat, rowName, colName) {
    tryCatch(mat[[which(rownames(mat) == rowName), which(colnames(mat) == colName)]],
      error = function(e) NA
    )
  }

  c_fmi <- 2 # first measure index
  method <- attr(x, "method")

  type1name <- if (method == "sur") "coefs" else stop("Not implemented")

  if (is.vector(measures) == FALSE) measures <- c(measures)
  if (is.vector(targets) == FALSE) measures <- c(targets)
  if (is.vector(rows) == FALSE) measures <- c(rows)
  if (is.vector(columns) == FALSE) measures <- c(columns)
  if (is.vector(itemIndices) == FALSE) measures <- c(itemIndices)

  m_names <- names(x)
  t_names <- sapply(c(1:x$info$numTargets), function(j) x[[c_fmi]][[j]]$name)
  b_names <- paste0("best", itemIndices)

  # find indexes
  if (is.null(measures)) {
    measures <- c(c_fmi:(length(x) - 1))
  } else {
    if (is.numeric(measures)) {
      measures <- as.integer(measures) + c_fmi - 1
    } else {
      measures <- unlist(sapply(measures, function(m) which(m_names == tolower(m))))
    }
  }
  if (length(measures) == 0) {
    stop("no valid 'measure' is found.")
  }

  if (is.null(targets)) {
    targets <- c(1:x$info$numTargets)
  } else {
    if (is.numeric(targets)) {
      targets <- as.integer(targets)
    } else {
      targets <- unlist(sapply(targets, function(m) which(t_names == m)))
    }
  }

  if (length(targets) == 0) {
    stop("no valid 'target' is found.")
  }

  if (is.null(itemIndices)) {
    itemIndices <- c(1:x$info$searchItems$bestK)
  }

  m_names <- m_names[measures]
  t_names <- t_names[targets]

  types <- c(types)
  types <- sapply(types, function(ty) {
    tolower(match.arg(
      ty,
      c("bestweights", "allweights", "inclusion", "type1bests", "cdf", "extremebounds", "mixture")
    ))
  })
  if (is.vector(rowContent)) {
    rowContent <- rowContent[[1]]
  }
  rowContent <- match.arg(rowContent, c("measure", "target", "item", "row", "column"))

  if ((rowContent == "item" || rowContent == "column" || rowContent == "row") &&
    length(types) > 1) {
    warning("There are more than 1 element in 'types' and for this 'rowContent'
    the result (if no error occurs) might be misleading (row names are not generally the same).")
  }



  ws <- data.frame()

  for (type in types) {
    # for distinguishing type in multiple lists:
    if (length(types) > 1) {
      stype <- if (type == "bestweights") {
        "bw"
      } else if (type == "allweights") {
        "aw"
      } else if (type == "inclusion") {
        "inc"
      } else if (type == "type1bests") {
        "t1b"
      } else if (type == "cdf") {
        "cdf"
      } else if (type == "extremebounds") {
        "eb"
      } else if (type == "mixture") {
        "mix"
      } else {
        stop("Invalid type")
      }
    } else {
      stype <- NULL
    }

    if (type == "bestweights") {
      if (rowContent == "measure") {
        ws0 <- as.data.frame(lapply(itemIndices, function(i) {
          sapply(targets, function(j) {
            sapply(measures, function(m) x[[m]][[j]]$model$bests[[i]]$weight)
          })
        }))
        row.names(ws0) <- m_names
        colnames(ws0) <- c(sapply(
          paste0("best", itemIndices),
          function(b) sapply(t_names, function(t) colNamFun(list(type = stype, target = t, item = b)))
        ))
      } else if (rowContent == "target") {
        ws0 <- as.data.frame(lapply(itemIndices, function(i) {
          sapply(measures, function(m) {
            sapply(targets, function(j) x[[m]][[j]]$model$bests[[i]]$weight)
          })
        }))
        row.names(ws0) <- t_names
        colnames(ws0) <- c(sapply(
          paste0("best", itemIndices),
          function(b) {
            sapply(m_names, function(m) {
              colNamFun(list(
                type = stype,
                measure = m, item = b
              ))
            })
          }
        ))
      } else if (rowContent == "item") {
        ws0 <- as.data.frame(lapply(targets, function(j) {
          sapply(measures, function(m) {
            sapply(itemIndices, function(i) x[[m]][[j]]$model$bests[[i]]$weight)
          })
        }))
        row.names(ws0) <- paste0("best", itemIndices)
        colnames(ws0) <- c(sapply(t_names, function(t) {
          sapply(
            m_names,
            function(m) colNamFun(list(type = stype, measure = m, target = t))
          )
        }))
      } else {
        stop("invalid or not implemented 'rowContent'.")
      }

      ws <- tryCatch(as.data.frame(append(ws, ws0, length(ws)), row.names = row.names(ws0)),
        error = function(e) {
          warning(paste0("Could not append a type of infomation (type=", type, ")"))
          ws
        }
      )
    }

    if (type == "allweights") {
      allcount <- max(as.numeric(sapply(measures, function(m) {
        sapply(
          targets,
          function(t) length(x[[m]][[t]]$model$all)
        )
      })))
      if (allcount > 0) {
        if (rowContent == "measure") {
          ws0 <- as.data.frame(lapply(c(1:allcount), function(i) {
            sapply(targets, function(j) {
              sapply(measures, function(m) {
                tryCatch(x[[m]][[j]]$model$all[[i]]$weight,
                  error = function(e) NA
                )
              })
            })
          }))
          row.names(ws0) <- m_names
          colnames(ws0) <- c(sapply(paste0("model", c(1:allcount)), function(b) {
            sapply(
              t_names,
              function(t) colNamFun(list(type = stype, target = t, item = b))
            )
          }))
        } else if (rowContent == "target") {
          ws0 <- as.data.frame(lapply(c(1:allcount), function(i) {
            sapply(measures, function(m) {
              sapply(targets, function(j) {
                tryCatch(x[[m]][[j]]$model$all[[i]]$weight,
                  error = function(e) NA
                )
              })
            })
          }))
          row.names(ws0) <- t_names
          colnames(ws0) <- c(sapply(paste0("model", c(1:allcount)), function(b) {
            sapply(
              m_names,
              function(m) colNamFun(list(type = stype, measure = m, item = b))
            )
          }))
        } else if (rowContent == "item") {
          ws0 <- as.data.frame(lapply(targets, function(j) {
            sapply(measures, function(m) {
              sapply(c(1:allcount), function(i) {
                tryCatch(x[[m]][[j]]$model$all[[i]]$weight,
                  error = function(e) NA
                )
              })
            })
          }))
          row.names(ws0) <- paste0("model", c(1:allcount))
          colnames(ws0) <- c(sapply(t_names, function(t) {
            sapply(
              m_names,
              function(m) colNamFun(list(type = stype, measure = m, target = t))
            )
          }))
        } else {
          stop("invalid or not implemented 'rowContent'.")
        }

        ws <- tryCatch(as.data.frame(append(ws, ws0, length(ws)), row.names = row.names(ws0)),
          error = function(e) {
            warning(paste0("Could not append a type of infomation (type=", type, ")"))
            ws
          }
        )
      }
    }

    if (type == "type1bests") {
      if (is.null(rows)) { # get unique variable names
        rows <- unique(c(sapply(measures, function(m) {
          sapply(
            targets,
            function(t) sapply(x[[m]][[t]]$coefs$bests, function(c) c$name)
          )
        })))
      } else if (is.numeric(rows)) {
        rows <- tryCatch(
          sapply(measures, function(m) {
            sapply(
              targets,
              function(t) sapply(x[[m]][[t]]$coefs$bests, function(c) c$name)
            )
          })[rows],
          error = function(e) stop("Check the indices in the 'rows'.")
        )
      }

      if (is.null(columns)) { # get unique column.names
        columns <- c("weight", "mean", "var")
      } else if (is.numeric(rows)) {
        columns <- tryCatch(c("weight", "mean", "var")[columns],
          error = function(e) stop("Check the indices in the 'columns'.")
        )
      }


      if (rowContent == "measure") {
        ws0 <- as.data.frame(lapply(rows, function(r) {
          lapply(columns, function(c) {
            lapply(itemIndices, function(b) {
              lapply(targets, function(j) {
                sapply(measures, function(m) {
                  cind <- which(r == sapply(
                    x[[m]][[j]][[type1name]]$bests,
                    function(cb) cb$name
                  ))
                  if (length(cind) > 0) {
                    return(x[[m]][[j]][[type1name]]$bests[[cind[[1]]]][[b + 1]][[c]])
                  }
                  return(NA)
                })
              })
            })
          })
        }))
        row.names(ws0) <- m_names
        colnames(ws0) <- c(sapply(rows, function(r) {
          sapply(
            columns,
            function(c) {
              sapply(
                paste0("best", itemIndices),
                function(b) {
                  sapply(
                    t_names,
                    function(t) {
                      colNamFun(list(
                        type = stype, target = t,
                        column = c, row = r, item = b
                      ))
                    }
                  )
                }
              )
            }
          )
        }))
      } else if (rowContent == "target") {
        ws0 <- as.data.frame(lapply(rows, function(r) {
          lapply(columns, function(c) {
            lapply(itemIndices, function(b) {
              lapply(measures, function(m) {
                sapply(targets, function(j) {
                  cind <- which(r == sapply(
                    x[[m]][[j]][[type1name]]$bests,
                    function(cb) cb$name
                  ))
                  if (length(cind) > 0) {
                    return(x[[m]][[j]][[type1name]]$bests[[cind[[1]]]][[b + 1]][[c]])
                  }
                  return(NA)
                })
              })
            })
          })
        }))
        row.names(ws0) <- t_names
        colnames(ws0) <- c(sapply(rows, function(r) {
          sapply(
            columns,
            function(c) {
              sapply(
                paste0("best", itemIndices),
                function(b) {
                  sapply(
                    m_names,
                    function(m) {
                      colNamFun(list(
                        type = stype, measure = m,
                        column = c, row = r, item = b
                      ))
                    }
                  )
                }
              )
            }
          )
        }))
      } else if (rowContent == "item") {
        ws0 <- as.data.frame(lapply(rows, function(r) {
          lapply(columns, function(c) {
            lapply(measures, function(m) {
              lapply(targets, function(j) {
                sapply(itemIndices, function(b) {
                  cind <- which(r == sapply(
                    x[[m]][[j]][[type1name]]$bests,
                    function(cb) cb$name
                  ))
                  if (length(cind) > 0) {
                    return(x[[m]][[j]][[type1name]]$bests[[cind[[1]]]][[b + 1]][[c]])
                  }
                  return(NA)
                })
              })
            })
          })
        }))
        row.names(ws0) <- paste0("best", itemIndices)
        colnames(ws0) <- c(sapply(rows, function(r) {
          sapply(
            columns,
            function(c) {
              sapply(
                m_names,
                function(m) {
                  sapply(
                    t_names,
                    function(t) {
                      colNamFun(list(
                        type = stype, target = t,
                        column = c, row = r, measure = m
                      ))
                    }
                  )
                }
              )
            }
          )
        }))
      } else if (rowContent == "row") {
        ws0 <- as.data.frame(lapply(measures, function(m) {
          lapply(columns, function(c) {
            lapply(itemIndices, function(b) {
              lapply(targets, function(j) {
                sapply(rows, function(r) {
                  cind <- which(r == sapply(
                    x[[m]][[j]][[type1name]]$bests,
                    function(cb) cb$name
                  ))
                  if (length(cind) > 0) {
                    return(x[[m]][[j]][[type1name]]$bests[[cind[[1]]]][[b + 1]][[c]])
                  }
                  return(NA)
                })
              })
            })
          })
        }))
        row.names(ws0) <- rows
        colnames(ws0) <- c(sapply(m_names, function(m) {
          sapply(
            columns,
            function(c) {
              sapply(
                paste0("best", itemIndices),
                function(b) {
                  sapply(
                    t_names,
                    function(t) colNamFun(list(type = stype, target = t, column = c, measure = m, item = b))
                  )
                }
              )
            }
          )
        }))
      } else if (rowContent == "column") {
        ws0 <- as.data.frame(lapply(rows, function(r) {
          lapply(measures, function(m) {
            lapply(itemIndices, function(b) {
              lapply(targets, function(j) {
                sapply(columns, function(c) {
                  cind <- which(r == sapply(
                    x[[m]][[j]][[type1name]]$bests,
                    function(cb) cb$name
                  ))
                  if (length(cind) > 0) {
                    return(x[[m]][[j]][[type1name]]$bests[[cind[[1]]]][[b + 1]][[c]])
                  }
                  return(NA)
                })
              })
            })
          })
        }))
        row.names(ws0) <- columns
        colnames(ws0) <- c(sapply(rows, function(r) {
          sapply(
            m_names,
            function(m) {
              sapply(
                paste0("best", itemIndices),
                function(b) {
                  sapply(
                    t_names,
                    function(t) {
                      colNamFun(list(
                        type = stype, target = t,
                        measure = m, row = r, item = b
                      ))
                    }
                  )
                }
              )
            }
          )
        }))
      } else {
        stop("invalid or not implemented 'rowContent'.")
      }


      ws <- tryCatch(as.data.frame(append(ws, ws0, length(ws)), row.names = row.names(ws0)),
        error = function(e) {
          warning(paste0("Could not append a type of infomation (type=", type, ")"))
          ws
        }
      )
    }

    if (type == "inclusion" || type == "cdf" || type == "mixture" || type == "extremebounds") { # matrices
      cdfname <- paste0("cdf", cdfIndex)
      getMat <- function(m, t) {
        if (type == "inclusion") {
          x[[m]][[t]]$model$inclusion
        } else if (type == "cdf") {
          x[[m]][[t]][[type1name]]$cdfs[[cdfname]]
        } else if (type == "extremebounds") {
          x[[m]][[t]][[type1name]]$extremeBounds
        } else if (type == "mixture") {
          x[[m]][[t]][[type1name]]$mixture
        }
      }

      if (is.null(rows)) { # get unique row.names
        rows <- unique(c(sapply(
          measures,
          function(m) {
            sapply(
              targets,
              function(t) rownames(getMat(m, t))
            )
          }
        )))
      } else if (is.numeric(rows)) {
        rows <- tryCatch(
          sapply(
            measures,
            function(m) {
              sapply(
                targets,
                function(t) rownames(getMat(m, t))
              )
            }
          )[rows],
          error = function(e) stop("Check the indices in the 'rows'.")
        )
      }

      if (is.null(columns)) { # get unique column.names
        columns <- unique(c(sapply(
          measures,
          function(m) {
            sapply(
              targets,
              function(t) colnames(getMat(m, t))
            )
          }
        )))
      } else if (is.numeric(rows)) {
        columns <- tryCatch(
          sapply(measures, function(m) {
            sapply(
              targets,
              function(t) colnames(getMat(m, t))
            )
          })[columns],
          error = function(e) stop("Check the indices in the 'columns'.")
        )
      }


      if (rowContent == "measure") {
        ws0 <- as.data.frame(lapply(rows, function(r) {
          lapply(columns, function(c) {
            sapply(targets, function(j) {
              sapply(measures, function(m) getValueInMat(getMat(m, j), r, c))
            })
          })
        }))
        row.names(ws0) <- m_names
        colnames(ws0) <- c(sapply(
          rows,
          function(r) {
            sapply(
              columns,
              function(c) {
                sapply(
                  t_names,
                  function(t) colNamFun(list(type = stype, target = t, column = c, row = r))
                )
              }
            )
          }
        ))
      } else if (rowContent == "target") {
        ws0 <- as.data.frame(lapply(rows, function(r) {
          lapply(columns, function(c) {
            sapply(measures, function(m) {
              sapply(targets, function(j) getValueInMat(getMat(m, j), r, c))
            })
          })
        }))
        row.names(ws0) <- t_names
        colnames(ws0) <- c(sapply(
          rows,
          function(r) {
            sapply(
              columns,
              function(c) {
                sapply(
                  m_names,
                  function(m) colNamFun(list(type = stype, measure = m, column = c, row = r))
                )
              }
            )
          }
        ))
      } else if (rowContent == "row") {
        ws0 <- as.data.frame(lapply(measures, function(m) {
          lapply(columns, function(c) {
            sapply(targets, function(j) {
              sapply(rows, function(r) getValueInMat(getMat(m, j), r, c))
            })
          })
        }))
        row.names(ws0) <- rows
        colnames(ws0) <- c(sapply(
          m_names,
          function(m) {
            sapply(
              columns,
              function(c) {
                sapply(
                  t_names,
                  function(t) colNamFun(list(type = stype, target = t, column = c, measure = m))
                )
              }
            )
          }
        ))
      } else if (rowContent == "column") {
        ws0 <- as.data.frame(lapply(rows, function(r) {
          lapply(measures, function(m) {
            sapply(targets, function(j) {
              sapply(columns, function(c) getValueInMat(getMat(m, j), r, c))
            })
          })
        }))
        row.names(ws0) <- columns
        colnames(ws0) <- c(sapply(
          rows,
          function(r) {
            sapply(
              m_names,
              function(m) {
                sapply(
                  t_names,
                  function(t) colNamFun(list(type = stype, target = t, measure = m, row = r))
                )
              }
            )
          }
        ))
      } else {
        stop("invalid or not implemented 'rowContent'.")
      }

      ws <- tryCatch(as.data.frame(append(ws, ws0, length(ws)), row.names = row.names(ws0)),
        error = function(e) {
          warning(paste0("Could not append a type of infomation (type=", type, ")"))
          ws
        }
      )
    }
  }
  return(ws)
}

