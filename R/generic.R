
#' Prints the Output of a Search Process
#'
#' @param x An output from one of the \code{search.?} functions (see \code{search.sur}, \code{search.varma}, or \code{search.bin}).
#' @param ... Additional arguments
#'
#' @return This function has no output.
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
  et <- difftime(x$info$endTime, x$info$startTime, units = "mins")
  cat("elapsed time:", et, "minutes \n")
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
        cat("(best=", round(s.metric.from.weight(t$model$bests[[1]]$weight, nms[[i]]), 3), ")\n", sep = "")
      } else {
        cat("(best model is missing)\n", sep = "")
      }
    }
  }
  return(NULL)
}


#' Gets Estimation from Search Result
#'
#' This function retrieves the estimation result from the output of a search process.
#' It uses the search options to re-estimate the same model estimated in the search process,
#' given a list of required indices.
#'
#' @param searchRes Output from one of the \code{search.?} functions (see [search.sur], [search.varma], or [search.bin]).
#' @param endoIndices Endogenous indices corresponding to the columns of the dependent variable.
#' @param exoIndices Exogenous indices corresponding to the columns of the exogenous variable.
#' @param y Data for dependent variables.
#' @param x Data for exogenous variables.
#' @param printMsg Argument passed to estimation methods
#' @param ... Additional arguments
#'
#' @return Estimation result similar to output of [estim.sur], [estim.varma], or [estim.bin].
#' @export
#' @seealso [estim.sur], [estim.varma], [estim.bin]
h.get.estim <- function(searchRes, endoIndices, exoIndices, y, x, printMsg, ...) {
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
  } else if (method == "bin") {
    GetEstim_bin(searchRes, endoIndices, exoIndices, y, x, printMsg = printMsg, ...)
  } else if (method == "varma") {
    GetEstim_varma(searchRes, endoIndices, exoIndices, y, x, printMsg = printMsg, ...)
  } else {
    stop("not implemented method")
  }
  # },error = function(e) e)
}

getMetricFrom <- function(model, metricName, tarIndex, method) {
  rowNames <- rownames(model$metrics)
  ind <- which(rowNames == metricName)
  if (length(ind) == 0) {
    stop(paste0("metric not found (method = sur, metric = ", metricName, ")"))
  }
  r <- model$metrics[ind[[1]], tarIndex]
  r
}

#' Summarizes Model Search Output
#'
#' This function estimates the reported models in the output of a \code{search.?} function and provides additional information.
#'
#' @param object Output from one of the \code{search.?} functions (see [search.sur], [search.varma], or [search.bin]).
#' @param y Data for dependent variables (Note that data is not saved in \code{object}).
#' @param x Data for exogenous variables (Note that data is not saved in \code{object}).
#' @param addModelBests If \code{TRUE} and search output contains best models, estimates them.
#' @param addModelAll If \code{TRUE} and search output contains all models, estimates them.
#' @param addItem1Bests If \code{TRUE} and search output contains \code{item1} part, estimates them.
#' @param printMsg If \code{TRUE}, prints details.
#' @param w Weight of observations in discrete choice estimation (Note that data is not saved in \code{object}).
#' @param newX New exogenous data in VARMA estimation (Note that data is not saved in \code{object}).
#' @param test If \code{TRUE}, verifies everything is working correctly. Please report errors.
#' @param ... Additional arguments
#'
#' @details
#' The output of \code{search.?} functions in this package only contains the information required
#' to re-estimate the models, not the actual estimation results. You can use [h.get.estim] to
#' get the estimation of specific indices. This function re-estimates everything in the \code{search.?}
#' output with a similar structure.
#'
#' @return A list with estimated models and other information, structured similarly to the given \code{search.?} output.
#'
#' @export
summary.ldtsearch <- function(object, y, x = NULL, addModelBests = TRUE,
                              addModelAll = FALSE, addItem1Bests = FALSE,
                              printMsg = FALSE, w = NULL, newX = NULL,
                              test = FALSE, ...) {
  test_perc <- 1e-6

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
  result$MetricNames <-
    names(object)[2:(length(object) - 1)] # the last element is the inputs

  if (result$method == "bin" && object$info$isWeighted && is.null(w))
    stop("w is missing in the binary regression.")
  #TODO: check other properties

  i <- 0
  for (mea in result$MetricNames) {

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

              su_m <- h.get.estim(object, b$depIndices, b$exoIndices, y, x,
                                  printMsg = printMsg,
                                  params = b$parameters, newX = newX, w = w,
                                  linkFunc = if (is.integer(b$dist) && b$dist == 0) "logit" else "probit")

              if (test) {
                jt <- if (is.null(b$depIndices) || length(b$depIndices) == 0) {
                  1
                } else {
                  which(b$depIndices == j)[[1]]
                } # index of target in endogenous data
                wd <- s.weight.from.metric(getMetricFrom(su_m, mea, jt, result$method), mea)
                if (abs(wd - b$weight)> test_perc)
                  warning(paste0("Inconsistent weight: target=",targ,
                               ", metric=",mea,
                               ", search weight=", b$weight,
                               ", model weight=", wd))

              }
              result[[mea]][[targ]][[paste0("model")]][[paste0("bests")]][[paste0("best", k)]] <- su_m # nolint
            }
          }
        }

        if (addModelAll) {
          if (test && printMsg)
            cat(paste0("    Testing All Models: ", targ , "\n"))

          if (is.null(x_mod$all) == FALSE) {
            k <- 0
            for (b in x_mod$all) {
              k <- k + 1

              if (test && printMsg)
                cat(paste0("            All Index: ", k , "\n"))

              su_m <- h.get.estim(object, b$depIndices, b$exoIndices, y, x,
                                  printMsg = printMsg,
                                  params = b$parameters, newX = newX, w = w,
                                  linkFunc = if (is.integer(b$dist) && b$dist == 0) "logit" else "probit"
              )
              if (test) {
                jt <- if (is.null(b$depIndices) || length(b$depIndices) == 0) {
                  1
                } else {
                  which(b$depIndices == j)[[1]]
                }
                wd <- s.weight.from.metric(getMetricFrom(su_m, mea, jt, result$method), mea)
                if (abs(wd - b$weight)> test_perc)
                  warning(paste0("Inconsistent weight: target=",targ,
                                 ", metric=",mea,
                                 ", search weight=", b$weight,
                                 ", model weight=", wd))

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
                      h.get.estim(object, b$depIndices, b$exoIndices, y, x,
                                  printMsg = printMsg,
                                  params = b$parameters, newX = newX, w = w,
                                  linkFunc = if (is.integer(b$dist) && b$dist == 0) "logit" else "probit"
                      )

                    if (test) {
                      jt <- if (is.null(b$depIndices) || length(b$depIndices) == 0) {
                        1
                      } else {
                        which(b$depIndices == j)[[1]]
                      }
                      testthat::expect_equal(
                        s.weight.from.metric(
                          getMetricFrom(su_m, mea, jt, result$method), mea
                        ), b$weight,
                        tolerance = test_perc
                      )

                      if (result$method == "sur" || result$method == "bin") {
                        ind <- which(item$name == rownames(su_m$estimations$coefs))
                        testthat::expect_equal(su_m$estimations$coefs[ind, jt], b$mean,
                                               tolerance = test_perc
                        )
                        testthat::expect_equal(su_m$estimations$stds[ind, jt], sqrt(b$var),
                                               tolerance = test_perc
                        )
                      } else if (result$method == "varma") {
                        # item$name is Horizon1,...
                        ind <- as.integer(substr(item$name, 8, 8)) + su_m$prediction$startIndex - 1
                        testthat::expect_equal(as.numeric(su_m$prediction$means[jt, ind]),
                                               as.numeric(b$mean),
                                               tolerance = test_perc
                        )
                        testthat::expect_equal(as.numeric(su_m$prediction$vars[jt, ind]),
                                               as.numeric(b$var),
                                               tolerance = test_perc
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


#' Get Data from Model Search Output
#'
#' This function converts a specific part of the output from one of the \code{search.?} functions into a \code{data.frame}.
#'
#' @param x Output from one of the \code{search.?} functions (see [search.sur], [search.varma], or [search.bin]).
#' @param types One or more types of information to include in the data frame.
#' Can be \code{bestweights}, \code{allweights}, \code{inclusion}, \code{type1bests}, \code{cdf}, \code{extremebounds}, and/or \code{mixture}.
#' @param metrics Indices or names of metrics to use.
#' @param targets Indices or names of targets to use.
#' @param rows Indices or names of rows to use. If the requested object is a matrix
#' (or an array), determines the rows and cannot be \code{NULL}. For \code{type1bests}, this is the name of the variables.
#' @param columns Indices or names of columns to use. If the requested object is a matrix, determines the columns and cannot be \code{NULL}. For
#' \code{type1bests}, this is the name of the fields: \code{weight}, \code{mean}, and/or \code{var}.
#' @param itemIndices Indices of items such as \code{bests} to use.
#' @param colNameFun Function to determine column names. Argument is a list of names, i.e., one of the following items: \code{target},
#' \code{metric}, \code{row}, \code{column}, \code{item}. If \code{NULL}, uses \code{paste} function.
#' @param rowContent Character string determining type of information in rows of returned
#' data frame. Some items are not available for some \code{types}.
#' Use \code{row} for variables in rows of matrices such as \code{inclusion} or
#' \code{mixture}. Use \code{column} for columns of such matrices.
#' Use \code{item} for best models or models in the all field.
#' @param cdfIndex Integer for index of CDF if type is cdf
#' @param ... Additional arguments
#'
#' @details
#' There are five types of indices in this function: metrics,
#' targets, bests, type1's items, equations.
#' Use \code{NULL} to use all available information or specify them.
#'
#'
#' @return A data frame containing requested data.
#'
#' @export
to.data.frame <- function(x, types = c(
  "bestweights", "allweights", "inclusion",
  "type1bests", "cdf", "extremebounds", "mixture"
),
metrics = NULL, targets = NULL,
rows = NULL, columns = NULL, itemIndices = NULL,
colNameFun = NULL,
rowContent = c("metric", "target", "item", "row", "column"),
cdfIndex = 0, ...) {
  if (is.null(x)) {
    stop("argument is null.")
  }
  if (is.null(colNameFun)){
    colNameFun <- function(ns) {
      colNameFun <- paste(ns[lengths(ns) > 0],  collapse = ".")
    }
  }

  getValueInMat <- function(mat, rowName, colName) {
    tryCatch(mat[[which(rownames(mat) == rowName), which(colnames(mat) == colName)]],
             error = function(e) NA
    )
  }

  c_fmi <- 2 # first metric index
  method <- attr(x, "method")

  type1name <- if (method == "sur") "coefs" else stop("Not implemented")

  if (is.vector(metrics) == FALSE) metrics <- c(metrics)
  if (is.vector(targets) == FALSE) metrics <- c(targets)
  if (is.vector(rows) == FALSE) metrics <- c(rows)
  if (is.vector(columns) == FALSE) metrics <- c(columns)
  if (is.vector(itemIndices) == FALSE) metrics <- c(itemIndices)

  m_names <- names(x)
  t_names <- sapply(c(1:x$info$numTargets), function(j) x[[c_fmi]][[j]]$name)
  b_names <- paste0("best", itemIndices)

  # find indexes
  if (is.null(metrics)) {
    metrics <- c(c_fmi:(length(x) - 1))
  } else {
    if (is.numeric(metrics)) {
      metrics <- as.integer(metrics) + c_fmi - 1
    } else {
      metrics <- unlist(sapply(metrics, function(m) which(m_names == tolower(m))))
    }
  }
  if (length(metrics) == 0) {
    stop("no valid 'metric' is found.")
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

  m_names <- m_names[metrics]
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
  rowContent <- match.arg(rowContent, c("metric", "target", "item", "row", "column"))

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
      if (rowContent == "metric") {
        ws0 <- as.data.frame(lapply(itemIndices, function(i) {
          sapply(targets, function(j) {
            sapply(metrics, function(m) x[[m]][[j]]$model$bests[[i]]$weight)
          })
        }))
        row.names(ws0) <- m_names
        colnames(ws0) <- c(sapply(
          paste0("best", itemIndices),
          function(b) sapply(t_names, function(t) colNameFun(list(type = stype, target = t, item = b)))
        ))
      } else if (rowContent == "target") {
        ws0 <- as.data.frame(lapply(itemIndices, function(i) {
          sapply(metrics, function(m) {
            sapply(targets, function(j) x[[m]][[j]]$model$bests[[i]]$weight)
          })
        }))
        row.names(ws0) <- t_names
        colnames(ws0) <- c(sapply(
          paste0("best", itemIndices),
          function(b) {
            sapply(m_names, function(m) {
              colNameFun(list(
                type = stype,
                metric = m, item = b
              ))
            })
          }
        ))
      } else if (rowContent == "item") {
        ws0 <- as.data.frame(lapply(targets, function(j) {
          sapply(metrics, function(m) {
            sapply(itemIndices, function(i) x[[m]][[j]]$model$bests[[i]]$weight)
          })
        }))
        row.names(ws0) <- paste0("best", itemIndices)
        colnames(ws0) <- c(sapply(t_names, function(t) {
          sapply(
            m_names,
            function(m) colNameFun(list(type = stype, metric = m, target = t))
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
      allcount <- max(as.numeric(sapply(metrics, function(m) {
        sapply(
          targets,
          function(t) length(x[[m]][[t]]$model$all)
        )
      })))
      if (allcount > 0) {
        if (rowContent == "metric") {
          ws0 <- as.data.frame(lapply(c(1:allcount), function(i) {
            sapply(targets, function(j) {
              sapply(metrics, function(m) {
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
              function(t) colNameFun(list(type = stype, target = t, item = b))
            )
          }))
        } else if (rowContent == "target") {
          ws0 <- as.data.frame(lapply(c(1:allcount), function(i) {
            sapply(metrics, function(m) {
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
              function(m) colNameFun(list(type = stype, metric = m, item = b))
            )
          }))
        } else if (rowContent == "item") {
          ws0 <- as.data.frame(lapply(targets, function(j) {
            sapply(metrics, function(m) {
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
              function(m) colNameFun(list(type = stype, metric = m, target = t))
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
        rows <- unique(c(sapply(metrics, function(m) {
          sapply(
            targets,
            function(t) sapply(x[[m]][[t]]$coefs$bests, function(c) c$name)
          )
        })))
      } else if (is.numeric(rows)) {
        rows <- tryCatch(
          sapply(metrics, function(m) {
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


      if (rowContent == "metric") {
        ws0 <- as.data.frame(lapply(rows, function(r) {
          lapply(columns, function(c) {
            lapply(itemIndices, function(b) {
              lapply(targets, function(j) {
                sapply(metrics, function(m) {
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
                      colNameFun(list(
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
              lapply(metrics, function(m) {
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
                      colNameFun(list(
                        type = stype, metric = m,
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
            lapply(metrics, function(m) {
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
                      colNameFun(list(
                        type = stype, target = t,
                        column = c, row = r, metric = m
                      ))
                    }
                  )
                }
              )
            }
          )
        }))
      } else if (rowContent == "row") {
        ws0 <- as.data.frame(lapply(metrics, function(m) {
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
                    function(t) colNameFun(list(type = stype, target = t, column = c, metric = m, item = b))
                  )
                }
              )
            }
          )
        }))
      } else if (rowContent == "column") {
        ws0 <- as.data.frame(lapply(rows, function(r) {
          lapply(metrics, function(m) {
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
                      colNameFun(list(
                        type = stype, target = t,
                        metric = m, row = r, item = b
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
          metrics,
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
            metrics,
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
          metrics,
          function(m) {
            sapply(
              targets,
              function(t) colnames(getMat(m, t))
            )
          }
        )))
      } else if (is.numeric(rows)) {
        columns <- tryCatch(
          sapply(metrics, function(m) {
            sapply(
              targets,
              function(t) colnames(getMat(m, t))
            )
          })[columns],
          error = function(e) stop("Check the indices in the 'columns'.")
        )
      }


      if (rowContent == "metric") {
        ws0 <- as.data.frame(lapply(rows, function(r) {
          lapply(columns, function(c) {
            sapply(targets, function(j) {
              sapply(metrics, function(m) getValueInMat(getMat(m, j), r, c))
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
                  function(t) colNameFun(list(type = stype, target = t, column = c, row = r))
                )
              }
            )
          }
        ))
      } else if (rowContent == "target") {
        ws0 <- as.data.frame(lapply(rows, function(r) {
          lapply(columns, function(c) {
            sapply(metrics, function(m) {
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
                  function(m) colNameFun(list(type = stype, metric = m, column = c, row = r))
                )
              }
            )
          }
        ))
      } else if (rowContent == "row") {
        ws0 <- as.data.frame(lapply(metrics, function(m) {
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
                  function(t) colNameFun(list(type = stype, target = t, column = c, metric = m))
                )
              }
            )
          }
        ))
      } else if (rowContent == "column") {
        ws0 <- as.data.frame(lapply(rows, function(r) {
          lapply(metrics, function(m) {
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
                  function(t) colNameFun(list(type = stype, target = t, metric = m, row = r))
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




#' Plot Estimation Results
#'
#' This function plots estimated coefficients, which may be point estimates, bounds, intervals, or distributions.
#'
#' @param points A list where each element is a point estimate to be drawn
#' as a shape. Each element must also be a list with the following elements:
#' 1.\code{value}: estimated coefficient,
#' 2.\code{y}: vertical position (default=0),
#' 3.\code{pch}: \code{pch} of the point (default="1"), and other usual attributes.
#' @param bounds A list where each element is bound estimation information (e.g.
#' output of an extreme bound analysis) to be drawn as a rectangle. Each element is defined by the following items:
#' 1.\code{xmin}: where bound starts on x-axis,
#' 2.\code{xmax}: where bound ends on x-axis,
#' 3.\code{ymin}: where bound starts on y-axis (default=-0.1),
#' 4.\code{ymax}: where bound ends on y-axis (default=+0.1),
#' 5.\code{alpha}: alpha for \code{rgb} color, and other usual attributes.
#' @param intervals A list where each element is interval estimation information. Each element is a
#' list similar to \code{bounds} but with a value to be drawn at the middle of the interval. In other words,
#' each element is defined by 1.\code{value}: center of the interval, 2.\code{xmin}: see \code{bound}, 3.\code{xmax}: see \code{bound}, etc.
#' @param distributions A list where each element is a distribution estimate to be drawn
#' by its density function. Each element is defined by a
#' 1.\code{type} which is the name of the distribution. Other elements of the list depend on the type of distribution.
#' For \code{type=normal}, elements must be 2.\code{mean}, 3.\code{var}, 4.\code{sdMultiplier}. For
#' \code{type=GLD}, they can be 2.\code{p1},..., 5.\code{p4}, 6.\code{quantiles}. For \code{type==cdfs}
#' they can be 2.\code{xs}, 3.\code{cdfs}, 4.\code{smoothFun}.
#' @param newPlot If \code{TRUE}, initializes a new plot.
#' @param xlim Array of size two for x-axis limits. If \code{NULL}, auto-generated.
#' @param ylim Array of size two for y-axis limits. If \code{NULL}, auto-generated.
#' @param boundFun Function to control \code{xlim} and \code{ylim} in the plot, with arguments being the computed bounds.
#' @param legendsTitle List of titles for legends.
#' @param legendSize Numeric value for size of legend (width or height) in lines of text (passed to \code{oma}).
#'
#' @param ... Additional properties for plot or legend, such as \code{xlab} and \code{ylab}.
#'
#' @return This function does not return any value.
#' @export
#' @importFrom graphics legend par
#' @importFrom grDevices rgb
#' @importFrom stats dnorm
#' @examples
#' points <- list()
#' points$one <- list(value = 1, label = "Point 1")
#' points$two <- list(value = 2, label = "Point 2", col = "red", pch = 22, cex = 4)
#' coefs.plot(points = points)
#'
#' bounds <- list()
#' bounds$one <- list(xmin = -1, xmax = 0.5, label = "Bound 1")
#' bounds$two <- list(
#'   xmin = 0, xmax = 1, ymin = 0.2, ymax = 0.3,
#'   label = "Bound 2", alpha = 0.2, col = rgb(0, 0, 1.0, alpha = 0.3)
#' )
#' coefs.plot(points = points, bounds = bounds)
#'
#' intervals <- list()
#' intervals$one <- list(value = 2, xmin = 0, xmax = 3, label = "Interval 1")
#' intervals$two <- list(
#'   value = 1.5, xmin = 1, xmax = 2, y = 4,
#'   label = "Interval 2", col = "blue", lwd = 3, pch = 11, cex = c(1.2, 3, 1.2)
#' )
#' coefs.plot(points = points, bounds = bounds, intervals = intervals)
#'
#' distributions <- list()
#' distributions$one <- list(type = "normal", mean = 0, var = 1, label = "Distribution 1")
#' distributions$two <- list(
#'   type = "gld", p1 = 0, p2 = 1.5, p3 = 1.2,
#'   p4 = 1.2, label = "Distribution 2", col = "blue", lwd = 3
#' )
#' distributions$three <- list(
#'   type = "cdfs", xs = seq(-2, 2, 0.1),
#'   cdfs = pnorm(seq(-2, 2, 0.1)), label = "Distribution 3",
#'   col = rgb(1, 0, 0, alpha = 0.5), lwd = 8
#' )
#' coefs.plot(
#'   points = points, bounds = bounds, intervals = intervals,
#'   distributions = distributions, legendsTitle = NULL, legendSize = 7
#' )
#'
coefs.plot <- function(points = NULL, bounds = NULL, intervals = NULL, distributions = NULL,
                       newPlot = TRUE, xlim = NULL, ylim = NULL,
                       boundFun = function(b, type) ifelse(type == "xmin" || type == "ymin",0.9 * b,1.1 * b),
                       legendsTitle = c("Point", "Bound", "Interval", "Density"),
                       legendSize = 5, ...) {

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  dots <- list(...)

  def_bound_ymin <- -0.1
  def_bound_ymax <- 0.1
  numPoints <- 100


  points <- as.list(points)
  bounds <- as.list(bounds)
  intervals <- as.list(intervals)
  distributions <- as.list(distributions)

  if (is.null(legendsTitle)) {
    legendsTitle <- c(NULL, NULL, NULL, NULL)
  }

  dists <- list()
  if (length(distributions) > 0) {
    for (g in distributions) {
      type <- g$type
      if (is.null(type)) {
        stop("missing: type of the distribution, e.g., normal, gld, cdfs.")
      }

      if (type == "normal") {
        sd <- sqrt(g$var)
        sdint <- if.not.null(g$sdmultiplier, 2) * sd
        x <- seq(g$mean - sdint, g$mean + sdint, length.out = numPoints)
        dists[[length(dists) + 1]] <- list(x = x, y = dnorm(x, g$mean, sd))
      } else if (type == "gld") {
        quantiles <- if.not.null(g$quantiles, seq(0.01, 0.99, length.out = numPoints))
        dists[[length(dists) + 1]] <- list(
          x = s.gld.quantile(quantiles, g$p1, g$p2, g$p3, g$p4),
          y = s.gld.density.quantile(quantiles, g$p1, g$p2, g$p3, g$p4)
        )
      } else if (type == "cdfs") {
        y <- diff(g$cdfs) / diff(g$xs)
        if (is.null(g$smoothFun) == FALSE) {
          y <- g$smoothFun(y)
        }

        dists[[length(dists) + 1]] <- list(
          x = g$xs,
          y = c(y, NA)
        )
      } else {
        stop("not supported distribution.")
      }
    }
  }



  if (is.null(xlim)) {
    xlim <- numeric(2) * NA
  }
  if (is.null(ylim)) {
    ylim <- numeric(2) * NA
  }
  if (is.na(xlim[[1]])) { # TODO: for normal, gld, first get the data
    xlim[[1]] <- boundFun(min(as.numeric(c(
      sapply(points, function(p) p$value),
      sapply(bounds, function(p) p$xmin),
      sapply(intervals, function(p) p$xmin),
      sapply(dists, function(p) min(p$x, na.rm = TRUE))
    )), na.rm = TRUE), "xmin")
  }
  if (is.na(xlim[[2]])) {
    xlim[[2]] <- boundFun(max(as.numeric(c(
      sapply(points, function(p) p$value),
      sapply(bounds, function(p) p$xmax),
      sapply(intervals, function(p) p$xmax),
      sapply(dists, function(p) max(p$x, na.rm = TRUE))
    )), na.rm = TRUE), "xmax")
  }
  if (is.na(ylim[[1]])) {
    ylim[[1]] <- boundFun(min(as.numeric(c(
      sapply(points, function(p) if.not.null(p$y, 0)),
      sapply(bounds, function(p) if.not.null(p$ymin, def_bound_ymin)),
      sapply(intervals, function(p) if.not.null(p$y, (def_bound_ymin + def_bound_ymax) / 2)),
      sapply(dists, function(p) min(p$y, na.rm = TRUE))
    )), na.rm = TRUE), "ymin")
  }

  if (is.na(ylim[[2]])) {
    ylim[[2]] <- boundFun(max(as.numeric(c(
      sapply(points, function(p) if.not.null(p$y, 0)),
      sapply(bounds, function(p) if.not.null(p$ymax, def_bound_ymax)),
      sapply(intervals, function(p) if.not.null(p$y, (def_bound_ymin + def_bound_ymax) / 2)),
      sapply(dists, function(p) max(p$y, na.rm = TRUE))
    )), na.rm = TRUE), "ymax")
  }

  legendPos <- "right" # TODO: other options

  xjust <- 0
  yjust <- 1
  leg_x_f <- NULL
  leg_y_f <- NULL
  if (newPlot) {
    L <- list(rect = list(top = ylim[[2]], left = par("usr")[2], h = 0))
    if (legendPos == "bottom") {
      par(oma = c(legendSize, 0, 0, 0))
    } else if (legendPos == "left") {
      par(oma = c(0, legendSize, 0, 0))

      leg_x_f <- function(p, rect) p[1] - 3
      leg_y_f <- function(p, rect) {
        rect$top - rect$h
      }
    } else if (legendPos == "top") {
      par(oma = c(0, 0, legendSize, 0))
    } else if (legendPos == "right") {
      par(oma = c(0, 0, 0, legendSize))
      leg_x_f <- function(p, rect) p[2]
      leg_y_f <- function(p, rect) rect$top - rect$h
    }


    plot(NULL, xlim = xlim, ylim = ylim, ylab = dots$ylab, xlab = dots$xlab)
  }


  # draw points
  if (length(points) > 0) {
    lgn_lgn <- character(length(points))
    lgn_col <- character(length(points))
    lgn_pch <- integer(length(points))
    lgn_cex <- integer(length(points))

    i <- 0
    for (g in points) {
      i <- i + 1
      pch <- if.not.null(g$pch, 4)
      col <- if.not.null(g$col, "black")
      cex <- if.not.null(g$cex, 3)
      graphics::points(
        x = g$value, y = if.not.null(g$y, 0), type = "p",
        pch = pch, col = col, cex = cex
      )
      lgn_lgn[[i]] <- g$label
      lgn_col[[i]] <- col
      lgn_pch[[i]] <- pch
      lgn_cex[[i]] <- cex
    }
    L <- legend(leg_x_f(par("usr"), L$rect), leg_y_f(par("usr"), L$rect),
                xpd = NA, xjust = xjust, yjust = yjust,
                legend = lgn_lgn, bty = "n", title = legendsTitle[[1]],
                col = lgn_col, pch = lgn_pch, x.intersp = 1.4,
                pt.cex = 1.5, fill = NA, border = NA
    ) #
  }

  if (length(bounds) > 0) {
    lgn_lgn <- character(length(bounds))
    lgn_fill <- character(length(bounds))

    i <- 0
    for (g in bounds) {
      i <- i + 1

      col <- if.not.null(g$col, rgb(0, 0, 0.0, alpha = 0.2))
      density <- if.not.null(g$density, NULL)
      border <- if.not.null(g$border, NA)
      graphics::rect(
        xleft = g$xmin, xright = g$xmax,
        ybottom = if.not.null(g$ymin, def_bound_ymin),
        ytop = if.not.null(g$ymax, def_bound_ymax), density = density,
        col = col, border = border
      )
      lgn_lgn[[i]] <- g$label
      lgn_fill[[i]] <- col
    }

    L <- legend(leg_x_f(par("usr"), L$rect), leg_y_f(par("usr"), L$rect),
                xpd = NA, xjust = xjust, yjust = yjust,
                legend = lgn_lgn, bty = "n", title = legendsTitle[[2]],
                col = lgn_col,
                fill = lgn_fill, border = lgn_fill,
                pch = NA, x.intersp = 1.4
    )
  }

  if (length(intervals) > 0) {
    lgn_lgn <- character(length(intervals))
    lgn_col <- character(length(intervals))
    lgn_lwd <- integer(length(intervals))
    lgn_lty <- integer(length(intervals))
    lgn_pch <- integer(length(intervals))

    i <- 0
    for (g in intervals) {
      i <- i + 1

      col <- if.not.null(g$col, "black")
      lty <- if.not.null(g$lty, 1)
      lwd <- if.not.null(g$lwy, 1)
      pch <- if.not.null(g$pch, 8)
      y <- if.not.null(g$y, 0)
      cex <- if.not.null(g$cex, c(1, 1, 1))

      graphics::points(
        x = c(g$xmin, g$value, g$xmax), y = c(y, y, y), type = "b",
        col = col, lwd = lwd, lty = lty, pch = c(15, pch, 15), cex = cex
      )

      lgn_lgn[[i]] <- g$label
      lgn_col[[i]] <- col
      lgn_lty[[i]] <- lty
      lgn_lwd[[i]] <- lwd
      lgn_pch[[i]] <- pch
    }

    L <- legend(leg_x_f(par("usr"), L$rect), leg_y_f(par("usr"), L$rect),
                xpd = NA, xjust = xjust, yjust = yjust,
                legend = lgn_lgn, bty = "n", title = legendsTitle[[3]],
                col = lgn_col, pch = lgn_pch, lty = lgn_lty, lwd = lgn_lwd
    )
  }

  if (length(distributions) > 0) {
    lgn_lgn <- character(length(distributions))
    lgn_col <- character(length(distributions))
    lgn_lwd <- integer(length(distributions))
    lgn_lty <- integer(length(distributions))

    i <- 0
    for (g in distributions) {
      i <- i + 1
      dist <- dists[[i]]
      col <- if.not.null(g$col, "black")
      lty <- if.not.null(g$lty, 1)
      lwd <- if.not.null(g$lwd, 1)

      graphics::lines(
        x = dist$x, y = dist$y, type = "l",
        col = col, lwd = lwd, lty = lty
      )

      lgn_lgn[[i]] <- g$label
      lgn_col[[i]] <- col
      lgn_lty[[i]] <- lty
      lgn_lwd[[i]] <- lwd
    }

    L <- legend(leg_x_f(par("usr"), L$rect), leg_y_f(par("usr"), L$rect),
                xpd = NA, xjust = xjust, yjust = yjust,
                legend = lgn_lgn, bty = "n", title = legendsTitle[[4]],
                col = lgn_col, lty = lgn_lty, lwd = lgn_lwd,
                pch = NA
    ) # set pch for better placement
  }
}


get_coef_stars <- function(pvalue, formatLatex) {
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

get_coef_func <- function(tableFun, formatNumFun, formatLatex){

  if (is.function(tableFun)){
    # no action
  }
  else if (tableFun == "sign") {
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
  else if (tableFun == "sign_star") {
    tableFun <- function(j, coef, std, pvalue) {
      paste0(if (coef > 0) {
        "+"
      } else if (coef < 0) {
        "-"
      } else {
        "0"
      }, get_coef_stars(pvalue, formatLatex))
    }
  }
  else if (tableFun == "coef") {
    tableFun <- function(j, coef, std, pvalue) {
      formatNumFun(j, coef)
    }
  }
  else if (tableFun == "coef_star") {
    tableFun <- function(j, coef, std, pvalue) {
      paste0(formatNumFun(j, coef), get_coef_stars(pvalue, formatLatex))
    }
  }
  else if (tableFun == "coef_star_std") {
    tableFun <- function(j, coef, std, pvalue) {
      paste0(formatNumFun(j, coef), get_coef_stars(pvalue, formatLatex), " (", formatNumFun(j, std), ")")
    }
  }
  else
    stop("tableFun must be a function or a valid character string.")

  tableFun
}

#' Create Table of Coefficients
#'
#' This function summarizes a list of estimated models (output of \code{estim.?} functions) and creates
#' a table of coefficients.
#'
#' @param estimList A named list where each element is output from a \code{estim.?} function, all belonging to a common analysis.
#' @param depList List of dependent variable name to be included in the columns of the table. If \code{NULL}, everything is added.
#' @param tableFun Function with arguments \code{coef}, \code{std}, \code{pvalue}, \code{minInColm}, \code{maxInCol} for formatting estimated coefficients.
#' Can also use one of the following character strings for predefined formatting: \code{sign}, \code{sign_star}, \code{coef}, \code{coef_star}, \code{coef_star_std}.
#' @param formatNumFun Function to format numbers if \code{tableFun} uses predefined formatting. It takes two arguments: \code{colIndex} (determines the column index) and \code{x} (determines the value).
#' @param regInfo List of keys (such as \code{num_eq}, \code{num_dep}, ...) to determine information at bottom of table. Use "" (empty) for empty rows.
#' A list of available options is given in the details section.
#' @param textFun Function to change any text in columns or rows of the table to a more informative text.
#' It has two arguments: \code{text} and \code{type}.
#' @param textFun_sub List for replacing special characters. If \code{NULL}, 'list(c("%", "\\\\%"), c("_", "\\\\_"))' is used.
#' @param textFun_max Maximum length for texts in the table.
#' @param expList Determines the name of the explanatory variables to insert in table.
#' If \code{NA}, it inserts all coefficients.
#' If it is an integer, it insert that number of explanatory variables in the table.
#' It can be a list of available explanatory variables.
#' Use \code{...} for empty rows.
#' @param formatLatex If \code{TRUE}, default options are for 'latex', otherwise, 'html'.
#' @param numFormat default formatting for the numbers.
#'
#' @details
#' The first part of the table is the header, followed by the coefficients. At the bottom, you can insert
#' the following items by specifying \code{regInfo}:
#' \itemize{
#' \item An empty character string (i.e., "") for inserting empty line.
#' \item \code{"sigma2"} for the covariance of regression, if it is available.
#' \item Name of an element in \code{estimList[[...]]$counts}.
#' \item An available metric name in the row names of \code{estimList[[...]]$metrics}.
#' }
#'
#' Furthermore, second argument in \code{textFun} can be:
#' \itemize{
#' \item \code{hname}: shows that the text is a header name from the \code{estimList} elements.
#' \item \code{dname}: shows that the text is a dependent variable name from the columns of \code{coefs} matrix.
#' \item \code{rname}: shows that the text is a key given in \code{regInfo}.
#' \item \code{ename}: shows that the text is an explanatory variable name from the rows of \code{coefs} matrix.
#' \item \code{NULL}: shows that the text is a specific code or something else.
#' }
#'
#' @return A data frame with (formatted) requested information.
#' @export
#' @examples
#' # See 'search.?' or 'estim.?' functions for some examples.
#'
coefs.table <- function(estimList, depList = NULL, tableFun = "coef_star", formatNumFun  = NULL,
                            regInfo = NULL, textFun = NULL,
                            textFun_sub = NULL, textFun_max = 20,
                            expList = NA, formatLatex = TRUE,
                            numFormat = "%.2f") {

  if (is.null(estimList$counts) == FALSE)
    estimList <- list(m = estimList)


  if (is.null(regInfo))
    regInfo <- c("", "obs")

  # there are some functions that format the inputs:
  # Function to change or truncate the text:
  if (is.null(textFun))
    textFun <- function(text, type) text
  textFunCopy = textFun
  textFun <- function(text, type){
    text <- textFunCopy(text, type)
    if (nchar(text) > textFun_max)
      text <- paste0(substr(text, 1, textFun_max - 3), "...")
    text
  }

  # Function to format the numbers

  if (is.null(formatNumFun))
    formatNumFun = function(colIndex, x) {
      if (is.integer(x))
        x
      else
        sprintf0(numFormat, x)
    }


  # Function to replace special characters
  if(is.null(textFun_sub))
    textFun_sub = list(c("%", "\\\\%"), c("_", "\\\\_"))
  textFun_sub <- as.list(textFun_sub)
  textSubFun = function(text){
    for (sg in textFun_sub)
      text <- gsub(sg[[1]], sg[[2]], text, fixed = TRUE)
    text
  }

  # Function to format the coefficients:
  tableFun <- get_coef_func(tableFun, formatNumFun, formatLatex)

  # Create column names
  col_names <- names(estimList)
  if (is.null(col_names))
    col_names <- paste0("m", rep(1:length(estimList)))
  col_names <- lapply(col_names, function(n) textFun(n, "hname"))

  if (is.null(depList)) # get all names
    depList <- unique(unlist(lapply(estimList, function(e) colnames(e$estimations$coefs))))
  else
    depList <- as.character(depList)
  depList <- lapply(depList, function(n) textFun(n, "dname"))

  col_names <- unlist(lapply(seq_along(col_names), function(i) {
    rep(col_names[i], sum(colnames(estimList[[i]]$estimations$coefs) %in% depList))
  }))

  # Create row names
  if (is.character(expList))
    row_names <- expList
  else{
    row_names <- unique(unlist(lapply(estimList, function(e) row.names(e$estimations$coefs)))) # get all names
    if (length(expList) == 1 && is.numeric(expList))
      row_names <- unlist(c(row_names[1:expList], "..."))
  }
  row_names0 <- row_names # without formatting
  last_x_ind <- length(row_names)
  #    include other rows:
  dep_cel_code <- "dep."
  row_names <- unlist(c(textFun(dep_cel_code, NULL),
                        lapply(row_names, function(n) textFun(n, "ename")),
                        lapply(regInfo, function(n) textFun(n, "rname"))))
  row_names0 <- unlist(c(dep_cel_code, row_names0, regInfo))


  # Create the table
  r_table <- matrix("", length(row_names), length(col_names))
  rownames(r_table) <- row_names
  colnames(r_table) <- col_names

  # start the loop
  j <- 0
  for (e in estimList) {
    for (d in depList){
      if (!d %in% colnames(e$estimations$coefs)) next
      j <- j + 1
      r_table[1, j] <- d

      ns <- row.names(e$estimations$coefs)
      i <- 0

      # Insert coefficients:
      for (v in ns) {
        i <- i + 1

        ind <- which(row_names0 == v)
        if (length(ind) == 0) # not found in the table
          next
        if (ind[[1]] > last_x_ind+1) # it's a regInfo name
          next
        if (length(ind) > 1)
          warning(paste0("Multiple name exists (name= ", v ,"estim. index = ", j, ")"))

        if (v == "...") # allow it
          next

        coef <- e$estimations$coefs[i, d]
        std <- e$estimations$stds[i, d]
        pv <- e$estimations$pValues[i, d]
        r <- tableFun(j, coef, std, pv)
        r_table[ind[[1]], j] <- r
      }

      # Insert regression information:
      i <- last_x_ind + 1 # because of the first row
      for (rn in regInfo) {
        i <- i + 1
        v <- NULL
        r <- rn[[1]]

        if (r == "")
          v <- "" # for an empty cell/line
        else if (r == "sigma2")
          v <- formatNumFun(j, e$estimations$sigma[d, d])
        else{
          # check and see if it is a metric
          ind <- which(r == rownames(e$metrics))
          if (length(ind) != 0) {
            v <- formatNumFun(j, e$metrics[ind, d])

            if (r == "f") {
              ind0 <- which("fProb" == rownames(e$metrics))
              fp <- e$metrics[ind0, d]
              if (is.na(fp)) {
                warning("p-value of 'F' statistics is NA.")
              }
              v <- paste0(v, get_coef_stars(fp))
            }

          }
          else{

            # check and see if it is a count metric
            ind <- which(r == names(e$counts))
            if (length(ind) != 0) {
              v <- formatNumFun(j, e$counts[[ind]])
            }

          }
        }

        if (is.null(v) || length(v) == 0) {
          warning(paste0("Invalid 'regInfo' member is requested. 'NA' is used. code=", r))
          v <- NA
        }
        if (ncol(r_table) < j) {
          d <- 0
        }

        r_table[i, j] <- v

      }
    }
  }

  if (length(estimList) == 1){ # header is the same for all
    colnames(r_table) <- r_table[1,]
    r_table <- r_table[2:nrow(r_table),,drop=FALSE]
  }
  else if (length(depList) == 1){
    r_table <- r_table[2:nrow(r_table),,drop=FALSE]
  }

  return(r_table)
}



#' Create a fan plot from a matrix of distribution parameters
#'
#' @param data A matrix with n columns where each row represents a prediction as a probability distribution.
#' @param dist The type of distribution to use: "normal" or "log-normal".
#' @param quantiles A list of quantiles to plot.
#' @param gradient Logical value indicating whether to add a gradient plot from the median to the minimum and maximum \code{quantiles}.
#' @param gradPointShape The shape of the point to use when gradient is TRUE.
#' @param limExpand An array of size 2 where the elements can be \code{NA} or a multiplier to  expand the range of the x and y axes.
#' @param limExpand A multiplier to to expand the range of the y-axis.
#' @param new.plot Logical value indicating whether to create a new plot or use the current plot.
#' @param boundColor The color to use for the bounds around the median.
#' @param midColor The color to use for the median or points.
#' @param ... Other arguments to pass to \code{plot} function if \code{new.plot} is \code{TRUE}.
#'
#' @examples
#' data <- matrix(c(0, 1, 1, 4), ncol = 2)
#' rownames(data) <- c("A", "B")
#' fan.plot(data)
#' fan.plot(data, dist = "log-normal")
#' fan.plot(data, gradient = TRUE)
#'  fan.plot(data, gradient = TRUE)
#' fan.plot(data, gradient = FALSE, new.plot = FALSE,
#'         boundColor = adjustcolor("red", alpha.f = 0.2),
#'         midColor = NA)
#'
#' @export
#' @importFrom graphics points lines polygon axis
#' @importFrom grDevices colorRampPalette
#' @importFrom stats qlnorm qnorm
fan.plot <- function(data, dist = "normal",
                     quantiles = c(0.05, 0.1, 0.25, 0.75, 0.9, 0.95),
                     gradient = FALSE, gradPointShape = 19,
                     limExpand = c(0.1,0.1), new.plot = TRUE,
                     boundColor = "blue", midColor = "black", ...) {
  if (dist == "normal") {
    if (ncol(data) != 2) {
      stop("For normal distribution, the matrix must have two columns: mean and variance.")
    }
    means <- data[, 1]
    sds <- sqrt(data[, 2])
    medians <- means
    quants <- t(apply(data, 1, function(x) qnorm(quantiles, x[1], sqrt(x[2]))))

  } else if (dist == "log-normal") {
    if (ncol(data) != 2) {
      stop("For log-normal distribution, the matrix must have two columns: meanlog and sdlog.")
    }
    meanlogs <- data[, 1]
    sdlogs <- data[, 2]
    medians <- exp(meanlogs + sdlogs^2 / 2)
    quants <- t(apply(data, 1, function(x) qlnorm(quantiles, x[1], x[2])))

  } else {
    stop(paste("Distribution", dist, "is not supported."))
  }

  quants <- cbind(quants[, quantiles <= 0.5], medians, quants[, quantiles > 0.5])

  ymin <- min(quants)
  ymax <- max(quants)

  if (length(limExpand) != 2)
    stop("Invalid 'limExpand'. An array of size 2 is expected")
  if ( is.na(limExpand[[2]]) == FALSE && limExpand[[2]] > 0) {
    yrange <- ymax - ymin
    ymin <- ymin - limExpand[[2]] * yrange
    ymax <- ymax + limExpand[[2]] * yrange
  }

  xmin = 1
  xmax = length(medians)
  if ( is.na(limExpand[[1]]) == FALSE && limExpand[[1]] > 0) {
    xrange <- xmax - xmin
    xmin <- xmin - limExpand[[1]] * xrange
    xmax <- xmax + limExpand[[1]] * xrange
  }

  cols <- colorRampPalette(c("white", boundColor, "white"), alpha = TRUE)(ncol(quants) + 1)
  cols <- cols[2:(length(cols)-1)] # remove whites

  if (new.plot) {
    plot(medians, col = midColor,
         ylim = c(ymin, ymax),
         xlim = c(xmin, xmax),
         ...)
    axis(1, at = seq_along(medians), labels = rownames(data))
  }

  if (!gradient) {
    for (i in seq_len(ncol(quants) - 1)) {
      polygon(c(seq_along(medians), rev(seq_along(medians))),
              c(quants[, i], rev(quants[, i + 1])),
              col = cols[i], border = NA)
    }

    lines(medians, col = midColor)

  } else {
    gradient_cols <- colorRampPalette(c(boundColor, "white"), alpha = TRUE)(100)

    for (i in seq_along(medians)) {
      x <- c(i - 0.4, i + 0.4)
      y1 <- seq(medians[i], quants[i, ncol(quants)], length.out = 100)
      y2 <- seq(medians[i], quants[i, 1], length.out = 100)

      for (j in seq_along(y1)) {
        polygon(c(x[1], x[2], x[2], x[1]), c(y1[j], y1[j], y1[j + 1], y1[j + 1]), col = gradient_cols[j], border = NA)
        polygon(c(x[1], x[2], x[2], x[1]), c(y2[j], y2[j], y2[j + 1], y2[j + 1]), col = gradient_cols[j], border = NA)
      }
    }

    points(seq_along(medians), medians, pch = gradPointShape, col = midColor)

  }
}


