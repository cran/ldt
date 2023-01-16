

#' Calculate Long-run Growth
#'
#'
#' @param data (integer vector) data
#' @param trimStart (integer) if the number of leading NAs is larger
#' than this number, it returns NA. Otherwise, it finds the first
#' number and continue the calculations.
#' @param trimEnd (integer) if the number of trailing NAs is larger
#' than this number, it returns NA. Otherwise, it finds the first number
#' and continue the calculations.
#' @param cont (logical) if \code{TRUE} it will use the continuous formula.
#' @param skipZero (logical) if \code{TRUE} leading and trailing
#' zeros are skipped.
#' @param isPercentage (logical) if the unit of measurement in \code{data}
#' is percentage (e.g., growth rate) use \code{TRUE}. Long-run growth rate
#' is calculated by arithmetic mean for continuous case, and geometric mean
#' otherwise. If missing data exists, it returns NA.
#' @param ... additional arguments
#'
#' @return the growth rate (percentage)
#' @export
#'
#' @examples
#' y <- c(NA, 0, c(60, 70, 80, 90), 0, NA, NA)
#' g <- LongrunGrowth(y, 2, 3, skipZero = TRUE, isPercentage = TRUE, cont = TRUE)
#'
LongrunGrowth <- function(data, trimStart = 0, trimEnd = 0,
                          cont = FALSE, skipZero = TRUE, isPercentage = FALSE, ...) {
  data <- as.numeric(data)
  trimStart <- as.integer(trimStart)
  trimEnd <- as.integer(trimEnd)

  N <- length(data)
  I <- 1
  J <- N
  start <- data[[I]]
  end <- data[[J]]

  is.na.zero <- function(d, skipz) {
    return(is.na(d) || (skipz && d == 0))
  }

  if (is.na.zero(start, skipZero) && trimStart > 0) {
    for (i in c(1:(trimStart + 1))) {
      I <- i
      start <- data[[I]]
      if (is.na.zero(start, skipZero) == FALSE) {
        break
      }
    }
  }
  if (is.na.zero(start, skipZero)) {
    return(NA)
  }

  if (is.na.zero(end, skipZero) && trimEnd > 0) {
    for (i in c(1:trimEnd)) {
      J <- N - i
      end <- data[[J]]
      if (is.na.zero(end, skipZero) == FALSE) {
        break
      }
    }
  }
  if (is.na.zero(end, skipZero)) {
    return(NA)
  }
  if (J < I) {
    return(NA)
  }

  if (isPercentage) {
    data <- data[I:J]
    if (cont) {
      return(mean(data))
    } else {
      d <- 1
      for (i in data) {
        d <- d * (i / 100 + 1)
      }
      return((d^(1 / (J - I + 1)) - 1) * 100)
    }
  } else {
    if (cont) {
      return(log(end / start) / (J - I) * 100)
    } else {
      return(((end / start)^(1 / (J - I)) - 1) * 100)
    }
  }
}
