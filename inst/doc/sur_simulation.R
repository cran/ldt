## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "  >",
  warning = FALSE,
  message = FALSE,
  echo = TRUE,
  results = "hide",
  eval = TRUE
)

## ----setup--------------------------------------------------------------------
library(ldt)
library(kableExtra)
library(MASS)

## ----def_sim_parameters-------------------------------------------------------
sampleSizes <- c(30, 50)
sMultipliers <- c(0.1, 2)

## ----def_search_assumptions---------------------------------------------------
xSizes <- list(as.integer(c(1, 2)), as.integer(c(3)))
xCounts <- c(NA, 5)
yGroups <- list(as.integer(c(1, 2)))

## -----------------------------------------------------------------------------
GetSample <- function(n, s) {
  z <- NULL
  for (i in (1:18)) {
    z <- cbind(z, rnorm(n, i / 18, 1))
  }
  z <- as.matrix(z)
  x <- z[, c(6, 12, 18)]
  e <- as.matrix(mvrnorm(n, c(0, 0), s * s * matrix(c(5, 2, 2, 4), 2, 2)))
  y <- x %*% t(matrix(c(1, 2, 0, 0, 3, 4), 2, 3, byrow = TRUE)) + e
  colnames(y) <- c("y1", "y2")
  colnames(z) <- paste0("z", seq(1, 18, 1))
  return(list(y = y, z = z))
}

## ----estimation_Seed----------------------------------------------------------
seed <- 340
set.seed(seed)

## ----estimation_measureOptions------------------------------------------------
measureOptions <- GetMeasureOptions(
  typesIn = c("aic", "sic"),
  typesOut = c("rmse", "crps")
)
measureNames <- c("AIC", "SIC", "RMSE", "CRPS")

## ----scoring_schema_best------------------------------------------------------
GetScore <- function(indices1, indices2) {
  if (length(indices1) == 2 && length(indices2) == 2 &&
      all(indices1 == c(6, 12)) && all(indices2 == c(12, 18))) {
    return(1)
  }
  score <- 0
  if (any(indices1 == 6)) score <- score + 0.1
  if (any(indices1 == 12)) score <- score + 0.1
  if (any(indices2 == 12)) score <- score + 0.1
  if (any(indices2 == 18)) score <- score + 0.1
  return(score)
}

## ----helper_indices-----------------------------------------------------------
helper_GetIndices <- function(measureRes, measureSumRes) {
  isRest1 <- measureSumRes$target1$model$bests$best1$coefs$isRestricted
  isRest2 <- measureSumRes$target2$model$bests$best1$coefs$isRestricted
  
  if (is.null(isRest1)) {
    mindices1 <- sort(measureRes$target1$model$bests$best1$exoIndices)
  } else {
    mindices1 <- sort(measureRes$target1$model$bests$best1$exoIndices[isRest1[, 1] != 1])
  }
  if (is.null(isRest2)) {
    mindices2 <- sort(measureRes$target2$model$bests$best1$exoIndices)
  } else {
    mindices2 <- sort(measureRes$target2$model$bests$best1$exoIndices[isRest2[, 2] != 1])
  }
  
  end <- nrow(measureRes$target1$model$inclusion)
  iindices1 <- sort(measureRes$target1$model$inclusion[3:end, 1, drop = F],
                    index.return = TRUE, decreasing = TRUE
  )
  iindices2 <- sort(measureRes$target2$model$inclusion[3:end, 1, drop = F],
                    index.return = TRUE, decreasing = TRUE
  )
  
  return(list(
    model1 = sort(mindices1), model2 = sort(mindices2),
    inclusion1 = sort(iindices1$ix[1:2]), inclusion2 = sort(iindices2$ix[1:2])
  ))
}

## ----estimation_main_function-------------------------------------------------
SampleAndEstimate <- function(i, res, n, s, xSizes, xCounts, yGroups,
                              measureOptions, searchItems, checkItems) {
  sample <- GetSample(n, s)
  search_res <- SurSearch_s(
    y = sample$y, x = sample$z, numTargets = 2,
    xSizes = xSizes, counts = xCounts,
    yGroups = yGroups,
    searchSigMaxIter = 1,
    searchSigMaxProb = 0.1,
    searchItems = searchItems,
    measureOptions = measureOptions,
    modelCheckItems = checkItems,
    searchOptions = GetSearchOptions(printMsg = FALSE),
    savePre = NULL
  )
  
  result_sum <- summary(search_res, y = sample$y, x = sample$z, test = FALSE, printMsg = FALSE)
  if (i < 0) {
    return(list(search_res = search_res, result_sum = result_sum))
  }
  
  j <- 0
  for (mea in c("aic", "sic", "rmse", "crps")) {
    j <- j + 1
    h <- helper_GetIndices(search_res[[mea]], result_sum[[mea]])
    res$model[[i, j]] <- res$model[[i, j]] + GetScore(h$model1, h$model2)
    res$inclusion[[i, j]] <- res$inclusion[[i, j]] + GetScore(h$inclusion1, h$inclusion2)
  }
  return(res)
}

## ----estimation_sim-----------------------------------------------------------
simCount <- 2
sur_sim_res <- list()
for (n in sampleSizes) {
  res <- list(model = matrix(0, length(sMultipliers), length(measureNames),
                             dimnames = list(paste0("sigma=", sMultipliers), measureNames)
  ))
  res$inclusion <- res$model
  
  i <- 0
  for (s in sMultipliers) {
    i <- i + 1
    for (iter in seq(1, simCount)) {
      seedi <- seed + iter
      measureOptions$seed <- seedi
      measureOptions$simFixSize <- 6
      measureOptions$trainRatio <- 0.75
      searchItems <- GetSearchItems(model = TRUE, type1 = FALSE, bestK = 10, inclusion = TRUE)
      res <- SampleAndEstimate(
        i, res, n, s, xSizes, xCounts, yGroups, measureOptions,
        searchItems, NULL
      )
    }
  }
  res$model <- res$model / simCount
  res$inclusion <- res$inclusion / simCount
  sur_sim_res[[length(sur_sim_res) + 1]] <- list(n = n, sMultipliers = sMultipliers, res = res)
} 


## ----result_compare_measures_model, echo=FALSE, results='hide', fig.show='hold', fig.cap='Scores in finding the true model (n=30, 50)', out.width='45%'----
op <- par(cex = 0.6)
for (c in sur_sim_res) {
  col <- c("red", "black", "blue", "green")
  lwd <- 1
  pch <- 1:4
  lty <- 1:2
  matplot(as.matrix(sMultipliers),
          y = c$res$model, type = "b", lwd = lwd, col = col, lty = lty, pch = pch,
          ylab = "Score", xlab = "s", ylim = c(0, 1)
  )
  legend("bottom", colnames(c$res$model), lwd = lwd, col = col, lty = lty, pch = pch, box.lwd = 0, trace = TRUE, yjust = -0.25) #
}

## ----result_sim_one, echo=FALSE, results='hide', fig.show='hold', fig.cap='Scores in finding the true inclusion variables (n=30, 100, 100)', out.width='45%'----
op <- par(cex = 0.6)
for (c in sur_sim_res) {
  col <- c("red", "black", "blue", "green")
  lwd <- 1
  pch <- 1:4
  lty <- 1:2
  matplot(as.matrix(sMultipliers),
          y = c$res$model, type = "b", lwd = lwd, col = col, lty = lty, pch = pch,
          ylab = "Score", xlab = "s", ylim = c(0, 1)
  )
  legend("bottom", colnames(c$res$inclusion), lwd = lwd, col = col, lty = lty, pch = pch, box.lwd = 0, trace = TRUE, yjust = -0.25) #
}

## -----------------------------------------------------------------------------
searchItems <- GetSearchItems(
  model = FALSE, type1 = TRUE,
  bestK = 1, cdfs = seq(0.8, 1.8, 0.03),
  extremeMultiplier = 2, mixture4 = TRUE
)

## -----------------------------------------------------------------------------
s <- 1
n <- 50

## -----------------------------------------------------------------------------
checkItems <- GetModelCheckItems(maxAic = 10)

## ----sur_estiamation_one------------------------------------------------------

sur_sim_one <- list(
  unr = SampleAndEstimate(
    -1, NULL, n, s, as.integer(c(3)), c(NA), list(as.integer(c(1, 2))), measureOptions,
    searchItems, checkItems
  ),
  res = SampleAndEstimate(
    -1, NULL, n, s, as.integer(c(3)), c(NA), list(as.integer(c(1, 2))), measureOptions,
    searchItems, checkItems
  )
) 


## ----result_compare_measures_inclusion, echo=FALSE, results='hide', fig.cap='Estimated coefficient (searches with and without restricting AIC)', fig.show='hold', out.width="45%"----

intervalMultiplier <- 1.95
use_gld_package <- FALSE

plot_estim <- function(estim, legendSize, intialGld) {
  dat <- to.data.frame(estim, types = c("type1bests"), targets = c("y1"), rows = c("z6"))
  intervals <- lapply(c(1:4), function(i) list(label = "Best CI (95%)", value = dat$y1.mean.z6.best1[[i]], xmin = dat$y1.mean.z6.best1[[i]] - intervalMultiplier * sqrt(dat$y1.var.z6.best1[[i]]), xmax = dat$y1.mean.z6.best1[[i]] + intervalMultiplier * sqrt(dat$y1.var.z6.best1[[i]]), col = "green", pch = 8))
  
  dat <- to.data.frame(estim, types = c("extremebounds"), targets = c("y1"), rows = c("z6"))
  bounds <- lapply(c(1:4), function(i) list(label = "EB (95%)", xmin = dat$y1.Lower.z6[[i]], xmax = dat$y1.Upper.z6[[i]], col = rgb(0, 0, 1, alpha = 0.4)))
  
  dat <- to.data.frame(estim, types = c("mixture"), targets = c("y1"), rows = c("z6"))
  dat0 <- by(dat, c(1:nrow(dat)), function(r) {
    tryCatch(
      {
        if (use_gld_package) {
          #gld <- gld::fit.fkml.moments.val(moments = c(r[[1]], r[[2]], r[[3]], r[[4]] + 3), starting.point = intialGld)
          #list(p1 = gld$lambda[[1]], p2 = gld$lambda[[2]], p3 = gld$lambda[[3]], p4 = gld$lambda[[4]])
        } else {
          GldFromMoments(r[[1]], r[[2]], r[[3]], r[[4]], type = 0, start = intialGld)
        }
      },
      error = function(e) c(NA, NA, NA, NA)
    )
  }, simplify = FALSE)
  mixtures <- lapply(dat0, function(r) list(label = "GLD", quantiles = seq(0.001, 0.999, 0.001), type = "gld", p1 = r[[1]], p2 = r[[2]], p3 = r[[3]], p4 = r[[4]], lty = 2, col = "black"))
  
  cdfs1 <- sapply(
    c(1:length(estim$aic$target1$coefs$cdfs)),
    function(i) {
      a <- to.data.frame(estim, types = c("cdf"), cdfIndex = i, targets = c("y1"), rows = c("z6"))
      a$y1.Mean.z6[[1]]
    }
  )
  cdfs <- list(type = "cdfs", label = "CDFs", xs = searchItems$cdfs, cdfs = cdfs1, smoothFun = function(y) smooth(y), col = "brown", lty = 1)
  
  PlotCoefs(
    points = list(list(label = "Parameter", value = 1, col = "red", pch = 11, cex = 2)),
    bounds = list(bounds[[1]]),
    intervals = list(intervals[[1]]),
    distributions = list(a = mixtures[[1]], b = cdfs),
    legendsTitle = NULL, legendSize = legendSize,
    boundFun = function(b, type) if (type == "xmax") b else if (type == "xmin" || type == "ymin") 0.96 * b else 1.04 * b, ylab = "", xlab = ""
  )
}
op <- par(cex = 0.7)
plot_estim(sur_sim_one$unr$search_res, 7, c(0, 0))
plot_estim(sur_sim_one$res$search_res, 7, c(0, 0))

