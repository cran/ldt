## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(kableExtra)
library(ldt)

## -----------------------------------------------------------------------------
data <- cbind(data.wdi$y, data.wdi$x[,1:5])
colnames(data)[2] <- paste0(colnames(data)[2],".lag")

## ----datatail-----------------------------------------------------------------
tail(data)

## ----datasummary--------------------------------------------------------------
sapply(as.data.frame(data), summary)

## ----columndesc, echo=FALSE, results='asis'-----------------------------------
for (c in colnames(data)){
  if (endsWith(c, ".lag"))
    next()
  cat(paste0("- ", c, ": ", data.wdi$names[which(sapply(data.wdi$names,function(d)d$code==c))][[1]]$name), "\n\n")
}

## ----search-------------------------------------------------------------------

search_res <- search.sur(data = get.data(data, endogenous = 1),
                         combinations = get.combinations(sizes = c(1,2,3),
                                                         numTargets = 1,
                                                         numFixPartitions = 2), 
                         metric <- get.search.metrics(typesIn = c("aic")),
                         items = get.search.items(bestK = 4))
print(search_res)

## ----summary, warning=FALSE---------------------------------------------------
search_sum <- summary(search_res)

## ----tableprepare-------------------------------------------------------------
models <- lapply(0:3, function(i)
  search_sum$results[which(sapply(search_sum$results, function(d)
    d$info==i && d$typeName=="best model"))][[1]]$value)
names(models) <- paste("Best",c(1:4))
table <- coefs.table(models, latex = FALSE, 
                     regInfo = c("obs", "aic", "sic"))

## ----table, echo=FALSE--------------------------------------------------------

kb <- kable(table, "html", escape = FALSE,
                  caption = "(Automatically Selected) Determinants of long-run GDP per capita growth")
row_spec(kb, 0, bold = TRUE)


