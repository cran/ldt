## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ldt)

## -----------------------------------------------------------------------------
data <- data.pcp$data[,1:5]

## ----datatail-----------------------------------------------------------------
tail(data)

## ----datasummary--------------------------------------------------------------
sapply(data, summary)

## ----columndesc, echo=FALSE, results='asis'-----------------------------------
for (c in colnames(data)){
  cat(paste0("- ", c, ": ", data.pcp$descriptions[c]), "\n\n")
}

## ----search-------------------------------------------------------------------

search_res <- search.varma(data = get.data(data, endogenous = 5),
                           combinations = get.combinations(sizes = c(1,2,3),
                                                           numTargets = 1),
                           maxParams = c(2,0,0),
                           metric <- get.search.metrics(typesIn = c(), 
                                                        typesOut = c("mape"),
                                                        simFixSize = 6),
                           maxHorizon = 5)
print(search_res)

## ----summary------------------------------------------------------------------
search_sum <- summary(search_res)

## ----plot, fig.width=6, fig.height=5------------------------------------------
best_model <- search_sum$results[[1]]$value
pred <- predict(best_model, 
                actualCount = 10, 
                startFrequency = tdata::f.monthly(data.pcp$start,1))
plot(pred, simMetric = "mape")

