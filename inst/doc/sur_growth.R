## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "  >",
  warning = FALSE,
  message = FALSE,
  echo = TRUE,
  results = 'hide',
  eval = TRUE 
)

## ----setup--------------------------------------------------------------------
library(ldt)
library(kableExtra)

## ----split_year---------------------------------------------------------------
splitYear <- 2005

## ----create_data_1, eval=FALSE------------------------------------------------
#  wdi_lr_2005 <- Data_Wdi(
#    dirPath = path_to_wdi_directory,
#    minYear = 1960, maxYear = splitYear,
#    aggFunction = function(data, code, name, unit, definition, aggMethod) {
#      isPerc <- unit == "%" || grepl(".ZG", code)
#      if (isPerc) NA else LongrunGrowth(data, 20, 2, FALSE, TRUE, isPerc)
#    },
#    keepFunction = function(X) {
#      var(X, na.rm = TRUE) > 1e-12 && sum((is.na(X)) == FALSE) >= 50
#    }
#  )
#  

## ----get_create_data_2, eval=FALSE--------------------------------------------
#  wdi_lr_2006_21 <- Data_Wdi(
#    dirPath = path_to_wdi_directory,
#    minYear = splitYear + 1, maxYear = 2021,
#    aggFunction = function(data, code, name, unit, definition, aggMethod) {
#      isPerc <- unit == "%" || grepl(".ZG", code)
#      if (isPerc) NA else LongrunGrowth(data, 2, 4, FALSE, TRUE, isPerc)
#    },
#    keepFunction = function(X) {
#      var(X, na.rm = TRUE) > 1e-12 && sum((is.na(X)) == FALSE) >= 50
#    }
#  )
#  

## ----data_find_target, eval=FALSE---------------------------------------------
#  serCode <- Data_WdiSearchFor(wdi_lr_2005$series, c("GDP", "per capita", "constant", "us"))[[1]]$code
#  y <- wdi_lr_2006_21$data[, which(colnames(wdi_lr_2006_21$data) %in% c(serCode)), drop = FALSE]
#  x <- as.data.frame(wdi_lr_2005$data)

## ----data_remove_noncountry, eval=FALSE---------------------------------------
#  x <- x[, which(sapply(x, function(v) abs(cor(y, v, use = "complete.obs")) < 0.999)), drop = FALSE]
#  y <- y[wdi_lr_2005$countries$isCountry, , drop = FALSE]
#  x <- x[wdi_lr_2005$countries$isCountry, , drop = FALSE]
#  if (any(rownames(x) != names(y))) {
#    stop("Invalid operation.")
#  }

## ----data_rearrange, eval=FALSE-----------------------------------------------
#  ind <- which(colnames(x) %in% c(serCode))
#  x <- cbind(x[, ind, drop = F], x[, 1:(ind - 1), drop = F], x[, ((ind + 1):ncol(x)), drop = F])
#  x <- cbind(matrix(rep(1, nrow(x)), nrow(x), dimnames = list(NULL, "Intercept")), x)

## ----modelset_size, results='markup'------------------------------------------
n0 = vig_data$wdi$nxcol_orig
n0Choose6 <- sum(sapply(seq(1:6), function(i) choose(n0, i)))
print(prettyNum(n0Choose6, big.mark = ","))

## ----modelset_steps-----------------------------------------------------------
xSizes <- list(as.integer(c(2,3)), as.integer(c(4)))
xCounts <- c(NA, 5)

## ----modelset_measure_options-------------------------------------------------
measureOptions <- GetMeasureOptions(
  typesIn = c("aic", "sic"), typesOut = c("rmse", "crps"),
  simFixSize = 60, trainRatio = 0.75, seed = 340
)

## ----modelset_search_items----------------------------------------------------
searchItems <- GetSearchItems(model = TRUE, bestK = 50, inclusion = TRUE)

## ----modelset_check_items-----------------------------------------------------
checkItems <- GetModelCheckItems(maxConditionNumber = 1e15, minDof = 35, minOutSim = 50)

## ----data_load----------------------------------------------------------------
x = as.matrix(ldt::vig_data$wdi$x)
y = as.matrix(ldt::vig_data$wdi$y)

## ----estimation---------------------------------------------------------------
search_res <- SurSearch_s(
  x = x, y = y, numFixXPartitions = 2,
  xSizes = xSizes, counts = xCounts,
  searchItems = searchItems, modelCheckItems = checkItems, measureOptions = measureOptions,
  searchOptions = GetSearchOptions(parallel = TRUE, printMsg = FALSE)
)

## ----estimation_print, results='hold'-----------------------------------------
print(search_res)

## ----estimation_summary, results='markup'-------------------------------------
search_res_sum <- summary(search_res, y = y, x = x, test = FALSE, printMsg = FALSE)

## ----estimation_table_1-------------------------------------------------------
best_models <- list(
  aic = search_res_sum$aic$target1$model$bests$best1,
  rmse = search_res_sum$rmse$target1$model$bests$best1,
  crps = search_res_sum$crps$target1$model$bests$best1
)

## ----estimation_table_2-------------------------------------------------------
regInfo <- list(
  c("", ""), c("num_obs", "No. Obs."), c("num_x", "No. Exo."), c("sigma2", "S.E. of Reg."), 
  c("r2", "R Sq."), c("aic", "AIC"), c("sic", "SIC"), c("rmse", "RMSE"), c("crps", "CRPS")
)

## ----estimation_table_3-------------------------------------------------------
vnamefun_wdi <- function(x) {
  tryCatch(
    {
      Data_WdiSearchFor(wdi_lr_2005$series, keywords = c(x), searchName = FALSE, findOne = TRUE)$name
    },
    error = function(e) x
  )
}
hnameFun <- function(x) {
  paste0((if (x == "aic") "AIC" else if (x == "sic") "SIC" else if (x == "rmse") "RMSE" else if (x == "crps") "CRPS"), "-based")
}
replaces <- list(c("US$", "US\\$"))

## ----estimation_table_4-------------------------------------------------------
res <- CoefTable(best_models,
                 tableFun = "coef_star", hnameFun = hnameFun, vnamesFun = vnamefun_wdi, regInfo = regInfo,
                 vnamesFun_sub = replaces, vnamesFun_max = 40, formatNumFun = function(j,x)round(x,3),
                 formatLatex = FALSE
)

## ----estimation_table_kable, echo=FALSE, results='markup'---------------------
kableExtra::kable(res, "html", digits = 2,
                  booktabs = TRUE, label = "long-run-growth",
                  align = c("c", "c", "c"), caption = "Long-run GDP per capita growth: best regressions",
                  linesep = "", escape = FALSE
) %>%
  kableExtra::footnote(
    general = "<p>Dependent variable is the long-run yearly growth rates of GDP per capita of different countries after 2006. Regressors are selected automatically. Column headers indicate the criteria used to select the best regression. Data is from WDI. Parameters of the equations are reported at the top and other properties at the bottom. `***`, `**`, and `*` denotes significance level of 1%, 5% and 10%, respectively.</p>", escape = FALSE, fixed_small_size = TRUE, footnote_as_chunk = TRUE
  ) %>%
  kable_styling(full_width = NULL)

## ----pca_na_strategies--------------------------------------------------------
x_ <- x[, 3:ncol(x), drop = FALSE]
strategies <- RemoveNaStrategies(x_)

## ----pca_na_strategies_print, results='markup'--------------------------------
cat(unlist(sapply(strategies, function(s) s$nCols))[1:20], " ...\n")

## ----pca_na_strategies_select-------------------------------------------------
inds <- c(1, 2, 1, 2)
pcaCutoffRates <- c(0.95, 0.95, 0.6, 0.6)
xPcaOptions <- GetPcaOptions(ignoreFirst = 2, exactCount = 0, max = 8)

## ----pca_significance_options-------------------------------------------------
searchSigMaxIter <- 2
searchSigMaxProb <- 0.2

## ----pca_estimation-----------------------------------------------------------
pca_models <- list()
i <- 0
for (ind in inds) {
  i <- i + 1
  strat <- strategies[[ind]]
  x0 <- NULL
  y0 <- y[-strat$rowRemove, , drop = FALSE]
  if (strat$colFirst) {
    if (length(strat$colRemove)>0)
      x0 <- x_[, -strat$colRemove, drop = FALSE]
    x0 <- x0[-strat$rowRemove, , drop = FALSE]
  } else {
    x0 <- x_[-strat$rowRemove, , drop = FALSE]
    if (length(strat$colRemove)>0)
      x0 <- x0[, -strat$colRemove, drop = FALSE]
  }
  x0 <- cbind(x[, c(1, 2), drop = F][-strat$rowRemove, , drop = FALSE], x0)
  xPcaOptions$cutoffRate <- pcaCutoffRates[[i]]
  pca_models[[length(pca_models) + 1]] <- SurEstim(y0, x0,
                                                   addIntercept = FALSE,
                                                   searchSigMaxIter = searchSigMaxIter,
                                                   searchSigMaxProb = searchSigMaxProb,
                                                   pcaOptionsX = xPcaOptions,
                                                   simFixSize = measureOptions$simFixSize,
                                                   simTrainRatio = measureOptions$trainRatio, simSeed = abs(measureOptions$seed),
                                                   simMaxConditionNumber = checkItems$maxConditionNumber,
                                                   printMsg = FALSE
  )
}

## ----pca_prepare_table--------------------------------------------------------
regInfo <- append(regInfo, 
                  list(c("num_exo", "No. Exo. (orig.)"), 
                       c("pca_x_cutoff", "PCA Cutoff")), 2)
regInfo <- append(regInfo, list(c("", "[. . .]")), 0)
regInfo <- append(regInfo, list(c("num_rest", "No. Rest.")), 6)
res <- CoefTable(pca_models,
                 tableFun = "coef_star", hnameFun = function(x) x, vnamesFun_sub = replaces,
                 vnamesFun = vnamefun_wdi, vnamesFun_max = 16, regInfo = regInfo, 
                 numCoefs = 2, formatNumFun = function(j,x)round(x,3),
                 formatLatex = FALSE
)
colnames(res) <- paste0("mod.", seq(1, length(pca_models))) 

## ----pca_table_kable, echo=FALSE, results='markup'----------------------------
kableExtra::kable(res, "html",
                  booktabs = TRUE, label = "long-run-growth-pca",
                  align = c(rep("c", 6)), caption = "Long-run GDP per capita growth: regressions with PCA",
                  linesep = "", escape = FALSE
) %>%
  kableExtra::footnote(
    general = "<p>Dependent variable is the long-run yearly growth rates of GDP per capita of different countries after 2006. Regressors are the PC of different combinations of the variables. Each column belongs to a specific strategy for removing the `NA`s and selecting the number of PCs. The intercept and the lag of the dependent variable are excluded from PCA. Other coefficients are not reported. Data is from WDI. `***`, `**`, and `*` denotes significance level of 1%, 5% and 10%, respectively. `(r)` means the coefficient is restricted.</p>", escape = FALSE, fixed_small_size = TRUE, footnote_as_chunk = TRUE
  ) %>%
  kable_styling(full_width = NULL)

