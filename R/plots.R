

#' Plots Estimated Coefficients
#'
#' @param points (list of list) each element is a point estimation to be drawn
#' as a shape; defined by 1.\code{value}, 2.\code{y} (default=0), 3.\code{shape}
#' (default="circle"), ...
#' @param bounds (list of list) each element is a bound estimation (e.g.
#' extreme bound analysis) to be drawn as a rectangle; defined by 1.\code{xmin},
#' 2.\code{xmax}, 3.\code{ymin} (default=-0.1), 4.\code{ymax}, (default=+0.1), 5.\code{alpha}, ...
#' @param intervals (list of list) each element is an interval estimation
#' (similar to \code{bounds} but with a value) to be drawn as an interval;
#' defined by 1.\code{value}, 2.\code{xmin}, 3.\code{xmax}, ...
#' @param distributions (list of list) each element is a distribution estimation
#' (eg., a known distribution) to be drawn as its density function; defined by 1.\code{type},
#' and for \code{type=normal}, 2.\code{mean}, 3.\code{var}, 4.\code{sdMultiplier}, for
#' \code{type=GLD}, 2.\code{p1},..., 5.\code{p4}, 6.\code{quantiles}, for \code{type==cdfs}
#' 2.\code{xs}, 3.\code{cdfs}, 4.\code{smoothFun}, ...
#' @param newPlot (logical) if \code{TRUE}, a new plot is initialized.
#' @param xlim (numeric vector) two limits for the x axis. If \code{NULL}, it is auto generated.
#' @param ylim (numeric vector) two limits for the y axis. If \code{NULL}, it is auto generated.
#' @param boundFun (function) a function to control the \code{xlim} and \code{ylim}
#' in the \code{plot}. Its arguments are the computed bounds.
#' @param legendsTitle (list) a list of titles for legends.
#' @param legendSize (numeric) size of the legend (width or height)
#' in lines of text (it is passed to \code{oma}).
#'
#' @param ... additional properties for plot or legend: \code{xlab}, \code{ylab}
#'
#' @return if \code{plot} is \code{FALSE}, a \code{ggplot} to be printed.
#' @export
#' @importFrom graphics legend par
#' @importFrom grDevices rgb
#' @importFrom stats dnorm
#' @examples
#' points <- list()
#' points$one <- list(value = 1, label = "Point 1")
#' points$two <- list(value = 2, label = "Point 2", col = "red", pch = 22, cex = 4)
#' PlotCoefs(points = points)
#'
#' bounds <- list()
#' bounds$one <- list(xmin = -1, xmax = 0.5, label = "Bound 1")
#' bounds$two <- list(
#'   xmin = 0, xmax = 1, ymin = 0.2, ymax = 0.3,
#'   label = "Bound 2", alpha = 0.2, col = rgb(0, 0, 1.0, alpha = 0.3)
#' )
#' PlotCoefs(points = points, bounds = bounds)
#'
#' intervals <- list()
#' intervals$one <- list(value = 2, xmin = 0, xmax = 3, label = "Interval 1")
#' intervals$two <- list(
#'   value = 1.5, xmin = 1, xmax = 2, y = 4,
#'   label = "Interval 2", col = "blue", lwd = 3, pch = 11, cex = c(1.2, 3, 1.2)
#' )
#' PlotCoefs(points = points, bounds = bounds, intervals = intervals)
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
#' PlotCoefs(
#'   points = points, bounds = bounds, intervals = intervals,
#'   distributions = distributions, legendsTitle = NULL, legendSize = 7
#' )
#'
PlotCoefs <- function(points = NULL, bounds = NULL, intervals = NULL, distributions = NULL,
                      newPlot = TRUE, xlim = NULL, ylim = NULL,
                      boundFun = function(b, type) {
                        if
                        (type == "xmin" || type == "ymin") {
                          0.9 * b
                        } else {
                          1.1 * b
                        }
                      },
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
          x = GldQuantile(quantiles, g$p1, g$p2, g$p3, g$p4),
          y = GldDensityQuantile(quantiles, g$p1, g$p2, g$p3, g$p4)
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
      pch <- if.not.null(g$pch, 21)
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
