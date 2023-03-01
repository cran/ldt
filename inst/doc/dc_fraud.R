## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "  >",
  warning = FALSE,
  message = FALSE,
  echo = TRUE,
  results = "hide",
  eval = FALSE
)

## ----setup--------------------------------------------------------------------
#  library(ldt)

## ----data---------------------------------------------------------------------
#  vestadata <- Data_VestaFraud(training = TRUE)

## ----save_data, echo=FALSE,eval=FALSE-----------------------------------------
#  vig_data = ldt::vig_data
#  vig_data$vesta = list()
#  vig_data$vesta$sum_data <- sum(lengths(vestadata$data))
#  vig_data$vesta$sum_data_na <- sum(is.na(vestadata$data))
#  vig_data$vesta$ncols <- ncol(vestadata$data)
#  #usethis::use_data(vig_data, overwrite = TRUE)

## -----------------------------------------------------------------------------
#  y <- as.matrix(vestadata$data[, c("isFraud")])
#  x <- as.matrix(vestadata$data[, 3:length(vestadata$data)])
#  weight <- as.matrix((y == 1) * (nrow(y) / sum(y == 1)) + (y == 0))

## -----------------------------------------------------------------------------
#  optimOptions <- GetNewtonOptions(maxIterations = 10, functionTol = 1e-2)

## -----------------------------------------------------------------------------
#  xSizes <- list(as.integer(c(1)), as.integer(c(2)), as.integer(c(3)), as.integer(c(4:10)))
#  xCounts <- c(NA, 20, 15, 10)

## -----------------------------------------------------------------------------
#  simFixSize <- 4

## ----estimation---------------------------------------------------------------
#  
#  vestaRes <- DcSearch_s(
#    x = x, y = y, w = weight,
#    xSizes = xSizes, counts = xCounts,
#    optimOptions = optimOptions,
#    searchItems = GetSearchItems(bestK = 20),
#    modelCheckItems = GetModelCheckItems(
#      maxConditionNumber = 1e15, minDof = 1e5, minOutSim = simFixSize / 2
#    ),
#    measureOptions = GetMeasureOptions(
#      typesIn = c("aucIn"),
#      typesOut = c("aucOut"),
#      simFixSize = 4,
#      trainRatio = 0.9,
#      seed = 340
#    ),
#    searchOptions = GetSearchOptions(printMsg = FALSE),
#    printMsg = FALSE,
#    savePre = "data/dc_vesta_"
#  )

## ----results='markup'---------------------------------------------------------
#  print(paste0("Best In-Sample AUC:     ", vestaRes$aucIn$target1$model$bests$best1$weight))
#  print(paste0("Best Out-Of-Sample AUC: ", vestaRes$aucOut$target1$model$bests$best1$weight))

