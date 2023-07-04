## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "  >"
)

## -----------------------------------------------------------------------------
library(ldt)

seed <- 123
set.seed(seed)

## -----------------------------------------------------------------------------
num_obs <- 100
num_eq <- 2L
num_exo <- 3L
sample <- sim.sur(num_eq, num_exo, num_obs, TRUE)

print(sample$coef)

## -----------------------------------------------------------------------------
exp_names <- paste0(colnames(sample$x), collapse = " + ")

fmla <- lapply(1:ncol(sample$y), 
               function(i) as.formula(paste0("Y", i, " ~ -1 +", exp_names)))

fit_sf <- systemfit::systemfit(fmla, data = data.frame(sample$y, sample$x), 
                               method = "SUR")

fit_sf_sys <- sim.sur(fit_sf$residCov, 
                      matrix(fit_sf$coefficients, ncol = 2), 10, TRUE)

## -----------------------------------------------------------------------------
fit <- estim.sur(sample$y, sample$x[,-1], addIntercept = TRUE) 

res_table <- coefs.table(fit, regInfo = c("obs", "sigma2"), 
                             formatLatex = FALSE)

## ----echo=FALSE---------------------------------------------------------------
kableExtra::kable(res_table, "html", escape = FALSE, table.attr = "style='width:50%;border-bottom: 1px solid;'",
                  caption = "Results of estimation using `ldt::estim.sur` function.")

## ----fig.width=6, fig.height=4, fig.cap='Plot of an estimated coefficient, its confidence interval, and the actual value.'----
coefs <- fit$estimations$coefs
stds <- fit$estimations$stds
coefs.plot(intervals = list(list(value = coefs[3,1], label = "X2->Y1", col = "red",
                                 xmin = coefs[3,1] - 2 * stds[3,1],
                                 xmax = coefs[3,1] + 2 * stds[3,1])),
           points = list(list(value = sample$coef[3,1], col = "blue",
                              label = "Actual")),
           xlab = "Value", ylab = "")

## -----------------------------------------------------------------------------
num_obs <- 100
exo <- matrix(c(-1,1,-0.5,0,0,0,0,1,-1,0.5), ncol = 2)
sample <- sim.sur(ncol(exo), exo, num_obs, TRUE)

## -----------------------------------------------------------------------------
fit <- estim.sur(sample$y, sample$x[,-1], addIntercept = TRUE,
                 searchSigMaxIter = 10, searchSigMaxProb = 0.05) 

res_table <- coefs.table(fit, regInfo = c("obs", "sigma2"), 
                             formatLatex = FALSE)

## ----echo=FALSE---------------------------------------------------------------
kableExtra::kable(res_table, "html", escape = FALSE, table.attr = "style='width:50%;border-bottom: 1px solid;'",
                  caption = "Results of estimation using `ldt::estim.sur` function with significance search.")

## -----------------------------------------------------------------------------
sample$x <- cbind(sample$x, matrix(rnorm(num_obs * 50), ncol = 50, 
                                   dimnames = list(NULL,paste0("z",1:50))))

## -----------------------------------------------------------------------------
fit <- estim.sur(sample$y, sample$x[,-1], addIntercept = TRUE,
                 searchSigMaxIter = 10, searchSigMaxProb = 0.05) 

res_table <- coefs.table(fit, regInfo = c("obs", "sigma2"), formatLatex = FALSE,
                             expList = c("Intercept", "X1", "X2", "X3", "X4", 
                                         "z3", "z6", "z14", "z32", "z42", "z47"))

## ----echo=FALSE---------------------------------------------------------------
kableExtra::kable(res_table, "html", escape = FALSE, table.attr = "style='width:50%;border-bottom: 1px solid;'",
                  caption = "Results of estimation using `ldt::estim.sur` function with significance search and variable selection uncertainty.")

## ----eval=FALSE, include=TRUE-------------------------------------------------
#  search_res <- search.sur(sample$y, sample$x, numTargets = ncol(sample$y),
#                           xSizes = c(1:4),
#                           metricOptions = get.options.metric(typesIn = c("sic")))
#  search_res

## -----------------------------------------------------------------------------
x_size_steps = list(c(1, 2), c(3), c(4), c(5))
count_steps = c(NA, 20, 10, 9)

search_step_res <-
  search.sur.stepwise(sample$y, sample$x, numTargets = ncol(sample$y),
                      xSizeSteps = x_size_steps, countSteps = count_steps,
                      metricOptions = get.options.metric(typesIn = c("sic", "aic")),
                      searchItems = get.items.search(bestK = 10)
  )
search_step_res

## -----------------------------------------------------------------------------
ssum <- summary(search_step_res, y = sample$y, x = sample$x)

## -----------------------------------------------------------------------------
mod_list <- list("SIC" = ssum$sic$target1$model$bests$best1,
                 "AIC" = ssum$aic$target1$model$bests$best1)
res_table <- coefs.table(mod_list, regInfo = c("obs", "sigma2"), formatLatex = FALSE)

## ----echo=FALSE---------------------------------------------------------------
kableExtra::kable(res_table, "html", escape = FALSE, table.attr = "style='width:50%;border-bottom: 1px solid;'",
                  caption = "Estimation result of best models found using `ldt::search.sur.stepwise` function.")

## -----------------------------------------------------------------------------
metric_options <- get.options.metric(typesOut = c("rmse", "crps"), 
                                     seed = -seed, simFixSize = 5, trainRatio = 0.75)
search_step_res <-
  search.sur.stepwise(sample$y, sample$x, numTargets = ncol(sample$y),
                      xSizeSteps = x_size_steps, countSteps = count_steps,
                      metricOptions = metric_options,
                      searchItems = get.items.search(bestK = 10)
  )
search_step_res

## -----------------------------------------------------------------------------
ssum <- summary(search_step_res, y = sample$y, x = sample$x)
 
mod_list <- list("RMSE_T1" = ssum$rmse$target1$model$bests$best1,
                 "RMSE_T2" = ssum$rmse$target2$model$bests$best1,
                 "CRPS_T1" = ssum$crps$target1$model$bests$best1,
                 "CRPS_T2" = ssum$crps$target2$model$bests$best1)
res_table <- coefs.table(mod_list, regInfo = c("obs", "sigma2"), formatLatex = FALSE)
res_table <- res_table[,c(1,4,5,8)]

## ---- echo=FALSE--------------------------------------------------------------
kableExtra::kable(res_table, "html", escape = FALSE, table.attr = "style='width:50%;border-bottom: 1px solid;'",
                  caption = "Estimation result of best models found using `ldt::search.sur.stepwise` function and out-of-sample evaluation.")

## -----------------------------------------------------------------------------
search_items <- get.items.search(type1 = TRUE, bestK = 10,
                                 inclusion = TRUE, 
                                 cdfs = seq(-0.8, 0.3, 0.01),
                                 extremeMultiplier = 1.95, 
                                 mixture4 = TRUE)

## -----------------------------------------------------------------------------
search_step_res <-
  search.sur.stepwise(sample$y, sample$x, numTargets = ncol(sample$y),
                      xSizeSteps = x_size_steps, countSteps = count_steps,
                      metricOptions = get.options.metric(typesIn = c("aic")),
                      searchItems = search_items)

## ----echo=FALSE, fig.width=6, fig.height=4, fig.cap='Sorted Inclusion weights.'----
inclusions <- search_step_res$aic$target2$model$inclusion
inclusions <- inclusions[3:nrow(inclusions),]
inclusions <- inclusions[order(inclusions[, "Mean"], decreasing = TRUE), ]
inclusions <- inclusions[1:6,1]
barplot(inclusions, beside = TRUE, col = rainbow(6), legend.text = rownames(inclusions))

## -----------------------------------------------------------------------------
actual <- list(value = sample$coef[3,1], label = "Actual")

b <- search_step_res$aic$target1$coefs$bests$item3$best1
interval <- list(value = b$mean, label = "X2 -> Y1",
                 xmin = b$mean - 1.95 * sqrt(b$var),
                 xmax = b$mean + 1.95 * sqrt(b$var))

## -----------------------------------------------------------------------------
e <- search_step_res$aic$target1$coefs$extremeBounds
bound <- list(xmin = e[3,1], xmax = e[3,2], label= "E.B.")

## -----------------------------------------------------------------------------
cdfs <- list(type = "cdfs", 
             cdfs = sapply(search_step_res$aic$target1$coefs$cdfs, function(c)c[3,1]),
             xs = search_items$cdfs, 
             label = "CDFs", smoothFun = function(y) smooth(y), col = "brown", lty = 1)

## -----------------------------------------------------------------------------
g <- search_step_res$aic$target1$coefs$mixture[3,]
g <- s.gld.from.moments(g[1], g[2], g[3], g[4], type = 3)
glds <- list(type = "gld",
                      p1 = g[1], p2 = g[2], p3 = g[3], p4 = g[4],
                      label = "GLD", quantiles = seq(0.001, 0.999, 0.001), lty = 2, col = "black")

## ----fig.width=6, fig.height=4, fig.cap='Plot of an estimated coefficient in the search process.'----
coefs.plot(intervals = list(interval), bounds = list(bound),
           points = list(actual), distributions = list(cdfs, glds),
           xlab = "Value", ylab = "")

## -----------------------------------------------------------------------------
num_exo <- 60L
sample <- sim.sur(num_eq, num_exo, num_obs, TRUE)

## -----------------------------------------------------------------------------
fit <- estim.sur(sample$y, sample$x, addIntercept = FALSE,
                  pcaOptionsX = get.options.pca(2,4))

## ----echo=FALSE---------------------------------------------------------------
res_table <- coefs.table(fit, regInfo = c("obs", "sigma2"), formatLatex = FALSE)

kableExtra::kable(res_table, "html", escape = FALSE, table.attr = "style='width:50%;border-bottom: 1px solid;'",
                  caption = "Results of estimation using `ldt::estim.sur` function with significance search and variable selection uncertainty.")

