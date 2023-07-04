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
num_exo <- 4L 
p_positive <- 0.4
max_weight <- 2
sample_l <- sim.bin(num_exo, num_obs, probit = FALSE, pPos = p_positive, maxWeight = max_weight)
sample_p <- sim.bin(sample_l$coef, num_obs, probit = TRUE, pPos = p_positive, maxWeight = max_weight)
 
print(sample_l$coef)

## -----------------------------------------------------------------------------
eq_str <- paste0("Y ~ ", paste0(colnames(sample_l$x[,-1]), collapse = " + "))

fit_l <- glm(eq_str, data = data.frame(sample_l$y, sample_l$x), 
             family = binomial(link = "logit"), weights = sample_l$w) 
fit_p <- glm(eq_str, data = data.frame(sample_p$y, sample_p$x), 
             family = binomial(link = "probit"), weights = sample_l$w) 
 
fit_sf_l <- sim.bin(fit_l$coefficients, probit = FALSE )
fit_sf_p <- sim.bin(fit_p$coefficients, probit = TRUE)

## -----------------------------------------------------------------------------
fit_l <- estim.bin(sample_l$y, sample_l$x[,-1], sample_l$w) 

fit_p <- estim.bin(sample_p$y, sample_p$x[,-1], sample_p$w, linkFunc = "probit") 

res_table <- coefs.table(list(Logit = fit_l, Probit = fit_p), 
                             regInfo = c("obs"), formatLatex = FALSE)

## ----echo=FALSE---------------------------------------------------------------
kableExtra::kable(res_table, "html", escape = FALSE, table.attr = "style='width:50%;border-bottom: 1px solid;'",
                  caption = "Results of estimation using `ldt::estim.bin` function.")

## -----------------------------------------------------------------------------
sample_l_new <- sim.bin(sample_l$coef, 10, probit = FALSE)

fit_l <- estim.bin(sample_l$y, sample_l$x[,-1], sample_l$w, newX = sample_l_new$x[,-1])

## ----echo=FALSE---------------------------------------------------------------
actual_classes <- as.numeric(sample_l_new$y)
probabilities <- as.numeric(sample_l_new$p1)
predictions <- as.numeric(fit_l$projection[,2])
barplot(actual_classes, col = "lightblue", ylim = c(0, 1), ylab = "Class / Probability") 
points(seq_along(probabilities), probabilities, col = "green", pch = 19) 
points(seq_along(predictions), predictions, col = "red", pch = 3) 
par(xpd=TRUE)
legend(x=0,y=1.5, legend = c("Actual Class", "Probability", "Prediction"), fill = c("lightblue", NA, NA),
       pch = c(NA, 19, 3), bg="transparent", bty = "n",
       col=c("lightblue" ,"green", "red"), border=c(NA, NA, NA))

## -----------------------------------------------------------------------------
res_table <- coefs.table(list(Logit = fit_l), 
                             regInfo = c("obs", "aic", "sic", "aucIn", "brierIn"), 
                             formatLatex = FALSE)

## ----echo=FALSE---------------------------------------------------------------

kableExtra::kable(res_table, "html", escape = FALSE, table.attr = "style='width:50%;border-bottom: 1px solid;'",
                  caption = "Results of estimation using `ldt::estim.bin` function with prediction.")

## -----------------------------------------------------------------------------
sample_l$x <- cbind(sample_l$x, matrix(rnorm(num_obs * 50), ncol = 50, 
                                   dimnames = list(NULL,paste0("z",1:50))))

sample_l_new$x <- cbind(sample_l_new$x, matrix(rnorm(nrow(sample_l_new$x) * 50), ncol = 50, 
                                   dimnames = list(NULL,paste0("z",1:50))))

## ----eval=FALSE, include=TRUE-------------------------------------------------
#  search_res <- search.bin(sample_l$y, sample_l$x[,-1], sample_l$w,
#                          xSizes = c(1:5),
#                          metricOptions = get.options.metric(typesIn = c("sic")))

## -----------------------------------------------------------------------------
x_size_steps = list(c(1, 2), c(3), c(4), c(5))
count_steps = c(NA, 20, 10, 9)

search_step_res <-
  search.bin.stepwise(y = sample_l$y, x = sample_l$x, w = sample_l$w,
                      xSizeSteps = x_size_steps, countSteps = count_steps,
                      metricOptions = get.options.metric(typesIn = c("aic","sic", "auc", "brier")),
                      searchItems = get.items.search(bestK = 10))
search_step_res

## -----------------------------------------------------------------------------
ssum <- summary(search_step_res, 
                y = sample_l$y, x = sample_l$x, w = sample_l$w, 
                newX = sample_l_new$x, 
                printMsg = FALSE)

## -----------------------------------------------------------------------------
mod_list <- list("SIC" = ssum$sic$target1$model$bests$best1,
                 "AIC" = ssum$aic$target1$model$bests$best1,
                 "AUC" = ssum$aucIn$target1$model$bests$best1,
                 "Brier" = ssum$brierIn$target1$model$bests$best1)
res_table <- coefs.table(mod_list, 
                             regInfo = c("obs", "aic", "sic", "aucIn", "brierIn"), 
                             formatLatex = FALSE)

## ----echo=FALSE---------------------------------------------------------------
actual_classes <- as.numeric(sample_l_new$y)
probabilities <- as.numeric(sample_l_new$p1)

predictions_sic <- as.numeric(mod_list$SIC$projection[,2])
predictions_aic <- as.numeric(mod_list$AIC$projection[,2])
predictions_auc <- as.numeric(mod_list$AUC$projection[,2])
predictions_bri <- as.numeric(mod_list$Brier$projection[,2])

barplot(actual_classes, col = "lightblue", ylim = c(0, 1), ylab = "Class / Probability") 
points(seq_along(probabilities), probabilities, col = "green", pch = 19) 

points(seq_along(predictions_sic), predictions_sic, col = "red", pch = 3) 
points(seq_along(predictions_aic), predictions_aic, col = "pink", pch = 4) 
points(seq_along(predictions_auc), predictions_auc, col = "orange", pch = 5) 
points(seq_along(predictions_bri), predictions_bri, col = "brown", pch = 6) 

par(xpd=TRUE)
legend(x=0,y=1.5, legend = c("Actual Class", "Probability", "p: SIC", "p: AIC", "p: AUC", "p: Brier"), fill = c("lightblue", NA, NA, NA, NA, NA),
       pch = c(NA, 19, 3, 4, 5, 6), bg="transparent", bty = "n",
       col=c("lightblue" ,"green", "red", "pink", "orange", "brown"), border=c(NA, NA, NA,NA, NA, NA))

## -----------------------------------------------------------------------------
metric_options <- get.options.metric(typesOut = c("auc", "brier"), 
                                       seed = -seed, simFixSize = 5, trainRatio = 0.75)
search_step_res <-
  search.bin.stepwise(y = sample_l$y, x = sample_l$x, w = sample_l$w,
                      xSizeSteps = x_size_steps, countSteps = count_steps,
                      metricOptions = metric_options,
                      searchItems = get.items.search(bestK = 10),
                      searchOptions = get.options.search(printMsg = FALSE))
search_step_res

## ----echo=FALSE---------------------------------------------------------------
ssum <- summary(search_step_res, 
                y = sample_l$y, x = sample_l$x, w = sample_l$w, 
                newX = sample_l_new$x, 
                printMsg = FALSE)
mod_list <- list("AUC" = ssum$aucOut$target1$model$bests$best1,
                 "Brier" = ssum$brierOut$target1$model$bests$best1)
res_table <- coefs.table(mod_list, 
                             regInfo = c("obs", "aucOut", "brierOut"), 
                             formatLatex = FALSE)

