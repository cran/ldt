
expand_lim0 <- function(min_data, max_data, percentage = 0.15, fix_min = NA, fix_max = NA){
  if (is.na(fix_min))
    fix_min <- min_data - percentage*abs(min_data)
  if (is.na(fix_max))
    fix_max <- max_data + percentage * abs(max_data)
  c(fix_min, fix_max)
}

expand_lim <- function(data, percentage = 0.15, fix_min = NA, fix_max = NA){
  if (is.na(fix_min))
    min_data <- min(data, na.rm = TRUE)
  if (is.na(fix_max))max_data <- max(data, na.rm = TRUE)
  expand_lim0(min_data, max_data, percentage, fix_min, fix_max)
}



#' Plot Diagnostics for \code{ldt.estim} Object
#'
#' This function creates diagnostic plots for estimated regression models of \code{ldt.estim} class.
#'
#' @param x An object of type \code{ldt.estim}.
#' @param equation A number or a name of endogenous variable specifying an equation in the estimated system.
#' @param type One of these numbers: 1, 2, 3, or 5. See \code{which} argument in [plot.lm] documentation.
#' @param textArgs A list of additional arguments to customize the "abline" function.
#' @param ablineArgs A list of additional arguments to customize the "text" function used for labeling influential observations.
#' @param ... additional arguments to be passed to "plot" (or "qqnorm" function for \code{type=2}, or "barplot" for \code{type=4}).
#'
#' @details
#' This function is designed to be similar to [plot.lm] function.
#' However, note that an \code{ldt.estim} object might be a system estimation.
#'
#' Some plots use standardized residuals. Note that they are not calculated in a system estimation context. See [residuals.ldt.estim] documentation for a description.
#' Cook's distance is also calculated equation-wise. Its formula is:
#' \deqn{
#' d = \frac{r_i^2}{k*var(r)}\frac{h_{ii}}{(1-h_{ii})^2}
#' }
#' where \eqn{r_i} and \eqn{h_{ii}} are residual and leverage in \eqn{i}-th observation, respectively. \eqn{var(r)} is variance of residuals and \eqn{k} is the number of estimated coefficients in the equation.
#' Note that Cook's distance is not implemented for weighted observations.
#'
#'
#' @return This function creates diagnostic plots for regression models.
#' It also returns a list with \code{x} and \code{y} data used in plot functions.
#' @export
plot.ldt.estim <- function(x,
                           equation = 1,
                           type = c(1,2,3,4,5,6),
                           ablineArgs = list(col = "lightblue"),
                           textArgs = list(pos = 3, cex = 0.7, col = "red"),
                           ...) {
  method <- tolower(attr(x, "method"))
  equation <- checkEquation(x, equation, TRUE)
  stopifnot(is.numeric(type))
  if (length(type) > 1)
    type = type[1]
  stopifnot(type %in% c(1, 2, 3, 4, 5, 6))

  fitted <- fitted(x, equation = equation)
  fitted <- fitted - min(fitted)
  if (type == 1)
    residuals <- resid(x, equation = equation, standardized = FALSE, pearson = TRUE)
  else
    residuals <- resid(x, equation = equation, standardized = TRUE, pearson = TRUE)

  args <- list(...)

  x_data <- NULL
  y_data <- NULL
  if (type == 1) { # Residuals vs Fitted
    x_data <- fitted
    y_data <- residuals
    ylab = "Residulas"
    #if (method == "binary")
    #  ylab = "Pearson Residulas"
    args <- modifyList(list(main = "Residuals vs Fitted", xlab = "Fitted Values", ylab = ylab,
                            ylim = expand_lim(y_data)), args)

    do.call(plot, c(list(x=x_data, y = y_data), args))
    do.call(abline, c(list(h = 0), ablineArgs))

    high_data <- order(abs(y_data), decreasing = TRUE)[1:(length(y_data)/10)]
    if (length(high_data) > 0)
      do.call(text, c(list(x = x_data[high_data], y = y_data[high_data], labels = high_data), textArgs))
  }
  else if (type == 2) { # Residual Q-Q
    y_data <- residuals

    args <- modifyList(list(main = "Normal Q-Q", ylab = "Standardized Residuals",
                            ylim = expand_lim(y_data)), args)

    points <- do.call(qqnorm, c(list(y_data), args))
    qqline(y_data)

    high_data <- order(abs(y_data), decreasing = TRUE)[1:(length(y_data)/10)]
    if (length(high_data) > 0)
      do.call(text, c(list(x = points$x[high_data], y = y_data[high_data], labels = high_data), textArgs))
  }
  else if (type == 3) { # Scale-Location
    x_data <- fitted
    y_data <- sqrt(abs(residuals))
    args <- modifyList(list(main = "Scale-Location", xlab = "Fitted Values", ylab = expression(sqrt(abs("Standardized Residuals"))),
                            ylim = expand_lim(y_data, fix_min = 0)), args)

    do.call(plot, c(list(x=x_data, y = y_data), args))
    do.call(abline, c(list(h = 0), ablineArgs))

    high_data <- order(abs(y_data), decreasing = TRUE)[1:(length(y_data)/10)]
    if (length(high_data) > 0)
      do.call(text, c(list(x = x_data[high_data], y = y_data[high_data], labels = high_data), textArgs))
  }
  else if (type == 4) {
    x_data <- as.numeric(cooks.distance0(x, equation = equation))
    args <- modifyList(list(main = "Cook's distance", xlab = "Observation Number", ylab = "Cook's distance",
                            ylim = expand_lim(x_data, fix_min = 0)), args)
    midpoints <- do.call(barplot, c(list(x_data), args))

    high_data <- x_data > 7/length(x_data)
    if (length(high_data) > 0)
      do.call(text, c(list(x = midpoints, y = x_data, labels = ifelse(high_data, 1:length(x_data), "")), textArgs))
  }
  else if (type == 5) {
    x_data <- hatvalues0(x, equation = equation)
    y_data <- residuals
    args <- modifyList(list(main = "Standardized Residuals vs Leverage", xlab = "Leverage", ylab = "Standardized Residuals",
                            ylim = expand_lim(y_data)), args)

    do.call(plot, c(list(x=x_data, y = y_data), args))
    do.call(abline, c(list(h = 0), ablineArgs))

    high_data <- order(abs(y_data), decreasing = TRUE)[1:(length(y_data)/10)]
    if (length(high_data) > 0)
      do.call(text, c(list(x = x_data[high_data], y = y_data[high_data], labels = high_data), textArgs))
  }
  else if (type == 6) {
    x_data <- hatvalues0(x, equation = equation)
    y_data <- cooks.distance0(x, equation = equation)
    args <- modifyList(list(main = "Cook's dist vs Leverage/(1-Leverage)", xlab = "Leverage/(1-Leverage)", ylab = "Cook's distance",
                            ylim = expand_lim(y_data, fix_min = 0)), args)

    do.call(plot, c(list(x=x_data, y = y_data), args))
    do.call(abline, c(list(h = 0), ablineArgs))

    high_y <- 5 / length(y_data)
    high_x <- 1.5 * mean(x_data, na.rm = TRUE)
    high_data <- which(y_data > high_y & x_data > high_x)
    if (length(high_data) > 0)
      do.call(text, c(list(x = x_data[high_data], y = y_data[high_data], labels = high_data), textArgs))
  }
  else {
    print("Invalid 'type' argument.")
  }

  return(list(x = x_data, y = y_data))
}



#' Fan Plot Function
#'
#' This function creates a fan plot.
#'
#' @param data A matrix where columns represent the parameters of distributions.
#' E.g., for normal distribution, the columns will be \code{mean} and \code{variance}.
#' The first rows can be actual values given in the first column.
#' @param dist A string indicating the type of distribution. Currently, it can be either "normal" or "log-normal". Default is "normal".
#' @param lambda A numeric value for Box-Cox transformation. If \code{NA}, no transformation is applied. Default is \code{NULL}.
#' @param quantiles A numeric vector of quantiles for shading. Default is c(0.05, 0.1, 0.25, 0.75, 0.9, 0.95).
#' @param gradient A logical value indicating whether to create a gradient fan plot. If FALSE, a standard fan plot is created. Default is FALSE.
#' @param ylimSuggest A numeric vector of length 2 indicating the suggested y-axis limits. Use \code{NA} for automatic calculation. Default is c(NA, NA).
#' @param ylimExpand A numeric value indicating the proportion to expand the y-axis limits. Default is 0.1.
#' @param newPlot A logical value indicating whether to create a new plot. If FALSE, the fan plot is added to the existing plot. Default is TRUE.
#' @param boundColor A string indicating the color of the boundary of the fan plot. Default is "blue".
#' @param plotArgs A list of additional arguments passed to the plot function when creating a new plot.
#' @param actualArgs A list of additional arguments passed to the lines function when plotting actual values.
#' @param medianArgs A list of additional arguments passed to the lines function when plotting median values.
#' @param polygonArgs A list of additional arguments passed to the polygon function when creating the fan plot.
#'
#' @return This function does not return a value but creates a fan plot as a side effect.
#' @export
#' @importFrom graphics points lines polygon axis
#' @importFrom grDevices colorRampPalette
#' @importFrom stats qlnorm qnorm
fan.plot <- function (data,
                      dist = "normal",
                      lambda = NA,
                      quantiles = c(0.05, 0.1, 0.25, 0.75, 0.9, 0.95),
                      gradient = FALSE,
                      ylimSuggest = c(NA,NA), # c(#,#) use it to reserve space for further plots
                      ylimExpand = 0.1,
                      newPlot = TRUE,
                      boundColor = "blue",
                      plotArgs = list(), # for plotting the main plot if newPlot is true
                      actualArgs = list(),
                      medianArgs = list(),
                      polygonArgs = list(border = NA))
{
  startInd = 1 #where fan starts

  if (dist == "normal") {
    if (ncol(data) != 2) {
      stop("For normal distribution, the matrix must have two columns: mean and variance.")
    }
    means <- data[, 1]
    sds <- sqrt(data[, 2])
    startInd <- which(sds!=0)
    startInd <- ifelse(length(startInd) == 0, length(sds), startInd[[1]])
    medians <- means
    quants <- t(apply(data, 1, function(x) qnorm(quantiles,
                                                 x[1], sqrt(x[2]))))
  }
  else if (dist == "log-normal") {
    if (ncol(data) != 2) {
      stop("For log-normal distribution, the matrix must have two columns: meanlog and sdlog.")
    }
    meanlogs <- data[, 1]
    sdlogs <- data[, 2]
    startInd <- which(sdlogs!=0)
    startInd <- ifelse(length(startInd) == 0, length(sdlogs), startInd[[1]])
    medians <- exp(meanlogs + sdlogs^2/2)
    quants <- t(apply(data, 1, function(x) qlnorm(quantiles,
                                                  x[1], x[2])))
  }
  else {
    stop(paste("Distribution", dist, "is not supported."))
  }

  quants <- cbind(quants[, quantiles <= 0.5], medians, quants[, quantiles > 0.5])

  if (!is.null(lambda) && !is.na(lambda)){

    inverse_boxcox <- function(y, lambda) {
      if (lambda == 0)
        exp(y)
      else
        (lambda * y + 1)^(1 / lambda)
    }
    medians <- inverse_boxcox(medians, lambda)
    quants <- apply(quants, c(1,2), inverse_boxcox, lambda = lambda)
  }

  ymin <- min(quants)
  ymax <- max(quants)
  if (!is.null(ylimSuggest)){
    stopifnot(length(ylimSuggest) == 2)
    ymin <- ifelse(is.na(ylimSuggest[[1]]), ymin, min(ymin, ylimSuggest[[1]]))
    ymax <- ifelse(is.na(ylimSuggest[[2]]), ymax, max(ymax, ylimSuggest[[2]]))
  }
  if (!is.na(ylimExpand)) {
    e <- expand_lim0(ymin, ymax, ylimExpand, NA, NA)
    ymin <- e[1]
    ymax <- e[2]
  }

  if (newPlot){
    plotArgs <- modifyList(list(ylab = NA, ylim = c(ymin, ymax)), plotArgs)
    do.call(plot, c(list(medians, type = "n"), plotArgs))
  }
  if (startInd > 1)
    do.call(lines, c(list(medians[1:startInd]), actualArgs))

  cols <- colorRampPalette(c("white", boundColor, "white"),
                           alpha = TRUE)(ncol(quants) + 1)
  cols <- cols[2:(length(cols) - 1)]

  if (!gradient) {
    for (i in seq_len(ncol(quants) - 1)) {
      do.call(polygon, c(list(c(seq_along(medians), rev(seq_along(medians))),
                              c(quants[, i], rev(quants[, i + 1])), col = cols[i]), polygonArgs))
    }
  }
  else {
    gradient_cols <- colorRampPalette(c(boundColor, "white"),
                                      alpha = TRUE)(100)
    for (i in c((startInd):length(medians))) {
      x <- c(i - 0.4, i + 0.4)
      y1 <- seq(medians[i], quants[i, ncol(quants)], length.out = 100)
      y2 <- seq(medians[i], quants[i, 1], length.out = 100)
      for (j in c(1:(length(y1)-1))) {
        do.call(polygon, c(list(x = c(x[1], x[2], x[2], x[1]),
                                y = c(y1[j], y1[j], y1[j + 1], y1[j + 1]),
                                col = gradient_cols[j]), polygonArgs))

        do.call(polygon, c(list(x = c(x[1], x[2], x[2], x[1]),
                                y = c(y2[j], y2[j], y2[j + 1], y2[j + 1]),
                                col = gradient_cols[j]), polygonArgs))

      }
    }
  }
  do.call(lines,c(list(c(numeric(startInd-1)*NA,tail(medians, length(medians)-startInd+1))), medianArgs))
}



#' Plot Predictions from a VARMA model
#'
#' @param x An object of class \code{ldt.varma.prediction}, which is the output of [predict.ldt.estim.varma] function.
#' @param variable Index or name of the variable to be plotted.
#' @param xAxisArgs Arguments to pass to \code{axis} function
#' @param fanPlotArgs Additional arguments for the [fan.plot] function. \code{lambda} is added automatically.
#' @param simMetric Name of metric to plot its details, provided that simulation details are available. If \code{NULL}, simulation details are not plotted.
#' @param simLineArgs Arguments to pass to \code{line} function for simulation lines (if available).
#' @param simPointsArgs Arguments to pass to \code{points} function for simulation points (if available).
#' @param ... Additional parameters (unused)
#'
#' @return This function does not return a value.
#' @export
#'
plot.ldt.varma.prediction <- function(x,
                                      variable = 1,
                                      xAxisArgs = list(),
                                      fanPlotArgs = list(),
                                      simMetric = NULL,
                                      simLineArgs = list(),
                                      simPointsArgs = list(), ...){

  if (is.null(x))
    stop("argument is null.")
  if (!inherits(x, "ldt.varma.prediction"))
    stop("Invalid class. An 'ldt.varma.prediction' object is expected.")
  if (is.null(x$means))
    stop("Invalid data. Predictions are not available.")

  max_horizon <- nrow(x$means) - x$actualCount
  if (max_horizon <= 0) # user might edit the matrix to decrease the maximum horizon
    stop("Predictions are not available. Make sure you did not omit the predictions.")

  stopifnot(is.number(variable) || is.character(variable))
  if (is.character(variable)){
    variable <- match(variable, colnames(x$info$y))
    if (is.na(variable))
      stop("Invalid variable name.")
  }
  if (variable > ncol(x$means))
    stop("Invalid variable.")

  var_name <- colnames(x$means)[variable]
  mat <- cbind(x$means[, variable, drop=FALSE], x$vars[, variable, drop=FALSE])
  colnames(mat) <- c("means", "vars")

  v_name <- colnames(x$means)[variable]
  attr(mat, "variable name") <- v_name
  lambda <- ifelse(is.null(x$lambdas), NA, x$lambdas[variable])

  if (is.null(fanPlotArgs))
    fanPlotArgs <- list()
  plotArgs <- fanPlotArgs$plotArgs
  if (is.null(plotArgs))
    plotArgs <- list()
  plotArgs <- modifyList(list(xaxt = "n"), plotArgs)
  fanPlotArgs$plotArgs <- plotArgs

  ylimSuggest <- c(NA,NA) # forecasts in evaluation might need more vertical space

  if (!is.null(simMetric) && !is.null(x$simulation)){ # plot evaluations:

    stopifnot(is.character(simMetric))
    simMetric <- tolower(simMetric)

    details <- x$simulation$details[x$simulation$details[,"metric"] == simMetric &
                                      x$simulation$details[,"target"] == var_name,]

    #ldt does the transformation in details
    #what if forecasts are not transformed?!

    ylimSuggest <- c(min(details[,"prediction"]),max(details[,"prediction"]))
  }

  do.call(fan.plot, c(list(mat, dist = "normal", lambda = lambda, ylimSuggest = ylimSuggest), fanPlotArgs))

  freqs <- rownames(mat)
  # there should also be a "at"
  if (is.null(xAxisArgs))
    xAxisArgs = list()
  if (is.null(xAxisArgs$at))
    xAxisArgs$at <- c(1:length(freqs))
  if (length(xAxisArgs$at) > length(freqs))
    xAxisArgs$at <- xAxisArgs$at[1:length(freqs)]
  do.call(axis, c(list(1, labels = freqs[xAxisArgs$at]), xAxisArgs))

  if (is.null(simMetric) == FALSE){

    simLineArgs <- modifyList(list(lty = 2, col = "gray", lwd = 0.5), simLineArgs)
    simPointsArgs <- modifyList(list(pch = 19, col = "red", cex = 0.5), simPointsArgs)

    # for each sampleEnd, there are at most h predictions. plot them on a line
    usends <- unique(details[,"sampleEnd"])

    for (p in usends){
      det <- details[details[,"sampleEnd"]==p,, drop = FALSE]
      det <- det[order(det[,"horizon"]),, drop = FALSE]
      #if (x$actualCount - p < 1)  shouldn't the plot handle it?
      xs <- c((x$actualCount - p): (x$actualCount - p + max(det[,"horizon"])))
      ys <- c(det[1,"last"], det[,"prediction"]) # last value & forecasts
      do.call(lines, c(list(x = xs, y = ys), simLineArgs))
      do.call(points, c(list(x = xs, y = ys), simPointsArgs))

      # for checking:
      # points(x = c(x$actualCount - sampleEnd + horizon), y = c(actual), col = "red")

    }
  }
}



