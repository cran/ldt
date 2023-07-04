## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "  >",
  warning = FALSE
)

## -----------------------------------------------------------------------------
library(ldt) 
library(tdata)

seed <- 123
set.seed(seed)

## -----------------------------------------------------------------------------
numObs <- 100
numHorizon <- 10
startDate <- f.yearly(1900)

numEndo <- 2L
numExo <- 3L
numAr <- 2L
numMa <- 1L 
d <- 1

sample <- sim.varma(numEndo, numAr, numMa, 
                    numExo, numObs, 10, TRUE, d, 
                    startFrequency = startDate)

## -----------------------------------------------------------------------------
y <- structure(sample$y[1:(numObs-numHorizon), , drop = FALSE], 
                                 ldtf = attr(sample$y, "ldtf"))
x <- sample$x[1:(numObs-numHorizon), , drop = FALSE]

fit <- estim.varma(y = y, x = x, 
                   params = c(numAr, d, numMa, 0, 0, 0))

params <- get.varma.params(fit$estimations$coefs, numAr, numMa, numExo, TRUE)
s0 <- sim.varma(fit$estimations$sigma, params$arList, params$maList,
                params$exoCoef, 10, 0, params$integer)

## -----------------------------------------------------------------------------
fit <- estim.varma(y = y, x = x,
                   params = c(numAr, 1, numMa, 0, 0, 0),
                   newX = sample$x[(numObs-numHorizon+1):numObs, , drop = FALSE],
                   maxHorizon = numHorizon)

y_actual <- sample$y[(numObs-numHorizon+1):numObs, , drop = FALSE]

## ---- echo=FALSE, fig.width=6, fig.height=4, fig.show='hold', fig.cap="Plotting prediction bounds (0.95 percent) for the two endogenous variables of the system."----
x_labels <- colnames(fit$prediction$means)[fit$prediction$startIndex:ncol(fit$prediction$means)]

par(mfrow = c(1, 2))

fan.plot(data.frame(fit$prediction$means[1,fit$prediction$startIndex:ncol(fit$prediction$means)],
                    fit$prediction$vars[1,fit$prediction$startIndex:ncol(fit$prediction$vars)]), 
         quantiles = c(0.05,0.95),
         boundColor = "red", midColor = "green", pch = 19,
         type = "l", xaxt = "n", xlab = "Period", ylab = "Y1", gradient = TRUE)
lines(y_actual[,1], col = "black")
axis(side = 1, at = 1:length(x_labels), labels = x_labels)

legend("topleft", legend = c("Actuals", "Preds. ", "Preds. Mean"), fill = c("NA", "red", NA),
       lty = c(1, NA, NA) , pch = c(NA, NA, 19), bg="transparent", bty = "n",
       col=c("black", "red", "green"), border=c(NA, NA, NA)) 


fan.plot(data.frame(fit$prediction$means[2,fit$prediction$startIndex:ncol(fit$prediction$means)],
                    fit$prediction$vars[2,fit$prediction$startIndex:ncol(fit$prediction$vars)]), 
         boundColor = "red", midColor = "green", pch = 19,
         type = "l", xaxt = "n", xlab = "Period", ylab = "Y1", gradient = TRUE)
lines(y_actual[,2], col = "black")
axis(side = 1, at = 1:length(x_labels), labels = x_labels)

legend("topleft", legend = c("Actuals", "Preds. ", "Preds. Mean"), fill = c("NA", "red", NA),
       lty = c(1, NA, NA) , pch = c(NA, NA, 19), bg="transparent", bty = "n",
       col=c("black", "red", "green"), border=c(NA, NA, NA))  

## -----------------------------------------------------------------------------
numObs_s <- 400
numHorizon_s <- 40
startDate_s <- f.quarterly(1900, 1)

numAr_s <- 1L
numMa_s <- 1L

d_s <- 1
D_s <- 1

sample_s <- sim.varma(numEndo, numAr, numMa, 
                    numExo, numObs_s, 10, TRUE, d_s, 
                    startFrequency =startDate_s,
                    seasonalCoefs = c(numAr_s,D_s,numMa_s,4))

## -----------------------------------------------------------------------------
y <- structure(sample_s$y[1:(numObs_s-numHorizon_s), , drop = FALSE], 
                                 ldtf = attr(sample_s$y, "ldtf"))
x <- sample_s$x[1:(numObs_s-numHorizon_s), , drop = FALSE]

fit <- estim.varma(y = y, x = x, 
                   params = c(numAr_s, d_s, numMa_s, numAr_s, D_s, numMa_s),
                   newX = sample_s$x[(numObs_s-numHorizon_s+1):numObs_s, , drop = FALSE],
                   maxHorizon = numHorizon_s,
                   seasonsCount = 4)

params <- get.varma.params(fit$estimations$coefs, numAr_s, numMa_s, numExo, TRUE, numAr_s, numMa_s, 4)
s0 <-  sim.varma(fit$estimations$sigma, params$arList, params$maList, 
                 params$exoCoef, d = d_s, nObs =  10, intercept =  params$integer,
                 seasonalCoefs = c(numAr_s, D_s, numMa_s, 4))

## ---- echo=FALSE, fig.width=6, fig.height=4, fig.show='hold', fig.cap="Plotting prediction bounds (0.95 percent) for the two endogenous variables of the seasonal system."----
y_actual <- sample_s$y[(numObs_s-numHorizon_s+1):numObs_s, , drop = FALSE]
x_labels <- colnames(fit$prediction$means)[fit$prediction$startIndex:ncol(fit$prediction$means)]

par(mfrow = c(1, 2))

fan.plot(data.frame(fit$prediction$means[1,fit$prediction$startIndex:ncol(fit$prediction$means)],
                    fit$prediction$vars[1,fit$prediction$startIndex:ncol(fit$prediction$vars)]), 
         quantiles = c(0.05,0.95),
         boundColor = "red", midColor = "green", pch = 19,
         type = "l", xaxt = "n", xlab = "Period", ylab = "Y1", gradient = TRUE)
lines(y_actual[,1], col = "black")
axis(side = 1, at = 1:length(x_labels), labels = x_labels)

legend("topleft", legend = c("Actuals", "Preds. ", "Preds. Mean"), fill = c("NA", "red", NA),
       lty = c(1, NA, NA) , pch = c(NA, NA, 19), bg="transparent", bty = "n",
       col=c("black", "red", "green"), border=c(NA, NA, NA)) 


fan.plot(data.frame(fit$prediction$means[2,fit$prediction$startIndex:ncol(fit$prediction$means)],
                    fit$prediction$vars[2,fit$prediction$startIndex:ncol(fit$prediction$vars)]), 
         boundColor = "red", midColor = "green", pch = 19,
         type = "l", xaxt = "n", xlab = "Period", ylab = "Y1", gradient = TRUE)
lines(y_actual[,2], col = "black")
axis(side = 1, at = 1:length(x_labels), labels = x_labels)

legend("topleft", legend = c("Actuals", "Preds. ", "Preds. Mean"), fill = c("NA", "red", NA),
       lty = c(1, NA, NA) , pch = c(NA, NA, 19), bg="transparent", bty = "n",
       col=c("black", "red", "green"), border=c(NA, NA, NA))  

## -----------------------------------------------------------------------------
sample$y <- cbind(sample$y, matrix(rnorm(numObs * 50), ncol = 50, 
                                   dimnames = list(NULL,paste0("w",1:50))))

## ----eval=FALSE, include=TRUE-------------------------------------------------
#  search_res <- search.varma(sample$y, sample$x, numTargets = 1,
#                          ySizes = c(1:3),
#                          maxParams = c(2,1,2,0,0,0),
#                          metricOptions = get.options.metric(typesIn = c("sic")),
#                          searchOptions = get.options.search(printMsg = TRUE, parallel = TRUE))

## -----------------------------------------------------------------------------
y_size_steps = list(c(1,2), c(3))
count_steps = c(NA, 10)

search_step_res <-
  search.varma.stepwise(y = sample$y, x = sample$x, numTargets = 1,
                        maxParams = c(2,1,2,0,0,0),
                        ySizeSteps = y_size_steps, countSteps = count_steps,
                        metricOptions = get.options.metric(typesIn = c("aic","sic")),
                        searchItems = get.items.search(bestK = 10),
                        searchOptions = get.options.search(printMsg = FALSE, parallel = TRUE))
search_step_res

## -----------------------------------------------------------------------------
ssum <- summary(search_step_res, 
                y = sample$y, x = sample$x, test = TRUE)

