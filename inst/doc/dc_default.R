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

## ----dc_measure_options-------------------------------------------------------
measureOptions <- GetMeasureOptions(
  typesIn = c("aucIn", "frequency"),
  typesOut = c("aucOut", "frequency")
)

## ----cost_matrix--------------------------------------------------------------
frequencyCost <- matrix(c(0.5, 1, 1, 0, 1, 0), 2, 3)

## ----data_get, eval=FALSE-----------------------------------------------------
#  data <- Data_BerkaLoan(positive = c("B", "D"), negative = c("A", "C"))
#  #data <- Data_BerkaLoan(positive = c("B"), negative = c("A"))

## ----data_define, eval=FALSE--------------------------------------------------
#  y <- data[, c("label"), drop = FALSE]
#  x <- data[, 4:ncol(data)]

## ----save_data, eval=FALSE,echo=FALSE-----------------------------------------
#  vig_data = ldt::vig_data
#  vig_data$berka = list()
#  vig_data$berka$y = y
#  vig_data$berka$x = x[,1:10]
#  
#  vig_data$berka$A_finished = round(sum(data$status=="A")/length(data$status)*100,1)
#  vig_data$berka$B_finished = round(sum(data$status=="B")/length(data$status)*100,1)
#  vig_data$berka$C_running =round(sum(data$status=="C")/length(data$status)*100,1)
#  vig_data$berka$D_running = round(sum(data$status=="D")/length(data$status)*100,1)
#  #usethis::use_data(vig_data, overwrite = TRUE)

## ----data_load----------------------------------------------------------------
x = as.matrix(ldt::vig_data$berka$x)
y = as.matrix(ldt::vig_data$berka$y)

## -----------------------------------------------------------------------------
weight <- as.matrix((y == 1) * (nrow(y) / sum(y == 1)) + (y == 0))

## ----modelset_steps-----------------------------------------------------------
xSizes <- list(as.integer(c(1, 2)), as.integer(c(3)))
xCounts <- c(NA, 4)

## ----seed---------------------------------------------------------------------
measureOptions$seed <- 340
measureOptions$simFixSize <- 10
measureOptions$trainRatio <- 0.75

## ----dc_estimate--------------------------------------------------------------
berka_res <- list(
  logit = DcSearch_s(
    x = x, y = y, w = weight, costMatrices = list(frequencyCost),
    xSizes = xSizes, counts = xCounts,
    searchLogit = TRUE, searchProbit = FALSE,
    searchItems = GetSearchItems(bestK = 20, inclusion = TRUE),
    measureOptions = measureOptions,
    searchOptions = GetSearchOptions(printMsg = FALSE),
    savePre = NULL
  ),
  probit = DcSearch_s(
    x = x, y = y, w = weight, costMatrices = list(frequencyCost),
    xSizes = xSizes, counts = xCounts,
    searchLogit = FALSE, searchProbit = TRUE,
    searchItems = GetSearchItems(bestK = 20, inclusion = TRUE),
    measureOptions = measureOptions,
    searchOptions = GetSearchOptions(printMsg = FALSE),
    savePre = NULL
  )
)

## ----plot_logit_probit, echo=FALSE, results='hide', fig.show='hold', fig.cap='Comparing the performance of best logit model and best probit model (cost-matrix and AUC)', out.width='45%'----
op <- par(cex = 0.6)
data_cost <- data.frame(
  cost_in = c(
    berka_res$logit$frequencyCostIn$target1$model$bests$best1$weight,
    berka_res$probit$frequencyCostOut$target1$model$bests$best1$weight
  ),
  cost_out = c(
    berka_res$logit$frequencyCostIn$target1$model$bests$best1$weight,
    berka_res$probit$frequencyCostIn$target1$model$bests$best1$weight
  )
)

data_auc <- data.frame(
  auc_in = c(
    berka_res$logit$aucIn$target1$model$bests$best1$weight,
    berka_res$probit$aucIn$target1$model$bests$best1$weight
  ),
  auc_out = c(
    berka_res$logit$aucOut$target1$model$bests$best1$weight,
    berka_res$probit$aucOut$target1$model$bests$best1$weight
  )
)
rns <- c("Logit", "Probit")
cns <- c("In-Sample", "Out-Of-Sample")
rownames(data_cost) <- rns
rownames(data_auc) <- rns
colnames(data_cost) <- cns
colnames(data_auc) <- cns


angle <- c(0, 45)
density <- 20
col <- c("black", "red")
lwd <- 1

barplot(as.matrix(data_cost),
        beside = T, ylim = c(0, 1),
        col = col, lwd = lwd, angle = angle, density = density
)
legend("top",
       legend = colnames(data_cost), angle = angle, density = density,
       fill = col, bg = NULL
)

barplot(as.matrix(data_auc),
        beside = T, ylim = c(0, 1),
        col = col, lwd = lwd, angle = angle, density = density
)
legend("top",
       legend = colnames(data_auc), angle = angle, density = density,
       fill = col, bg = NULL
)

