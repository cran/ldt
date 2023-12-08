## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ldt)

## -----------------------------------------------------------------------------
data <- cbind(data.berka$y, data.berka$x[,1:7])

## ----datatail-----------------------------------------------------------------
tail(data)

## ----datasummary--------------------------------------------------------------
sapply(as.data.frame(data), summary)

## ----columndesc, echo=FALSE, results='asis'-----------------------------------
for (c in colnames(data)){
  cat(paste0("- ", c, ": ", data.berka$descriptions[which(sapply(data.berka$descriptions,function(d)d$id==c))][[1]]$description), "\n\n")
}

## ----search-------------------------------------------------------------------

search_res <- search.bin(data = get.data(data, endogenous = 1, weights = data.berka$w),
                         combinations = get.combinations(sizes = c(1,2,3),
                                                         numTargets = 1),
                         metric <- get.search.metrics(typesIn = c(), 
                                                      typesOut = c("auc"),
                                                      simFixSize = 20,
                                                      trainRatio = 0.8,
                                                      seed = 123),
                         items = get.search.items(bestK = 0,
                                                  inclusion = TRUE, 
                                                  type1 = TRUE, 
                                                  mixture4 = TRUE))
print(search_res)

## ----summary------------------------------------------------------------------
search_sum <- summary(search_res)

## ----prepareplot--------------------------------------------------------------

inclusion_mat <- search_res$results[sapply(search_res$results, function(a)a$typeName == "inclusion")][[1]]$value
inclusion_mat <- inclusion_mat[!(rownames(inclusion_mat) %in% c("label", "(Intercept)")), ]
sorted_inclusion_mat <- inclusion_mat[order(inclusion_mat[,1], decreasing = TRUE),]
selected_vars <- rownames(sorted_inclusion_mat)[1:4]

mixture_mat <- search_sum$results[sapply(search_sum$results, function(a)a$typeName == "mixture")][[1]]$value
moments <- lapply(selected_vars, function(v)mixture_mat[rownames(mixture_mat)==v,])
gld_parms <- lapply(moments, function(c) s.gld.from.moments(c[[1]], c[[2]],
                        c[[3]], c[[4]],
                        start = c(0.25, 0.25),
                        type = 4,
                        nelderMeadOptions = get.options.neldermead(100000,1e-6)))


## ----plot, fig.height=4, fig.subcap=selected_vars, fig.cap="Determinants of Loan Default: Variables with the highest inclusion weights, automatically selected. Each plot presents a combined distribution of all estimated coefficients, estimated by generalized lambda distribution.", out.width='.40\\linewidth', fig.ncol=2----
 
# plots
probs <- seq(0.01,0.99,0.01)
i <- 0
for (gld in gld_parms){
  i <- i + 1
  x <- s.gld.quantile(probs, gld[1],gld[2],gld[3],gld[4])
  y <- s.gld.density.quantile(probs, gld[1],gld[2],gld[3],gld[4])

  plot(x, y, type = "l", xaxt = "n", xlab = NA, ylab = NA, col = "blue", lwd = 2, 
       main = selected_vars[i])
  lower <- x[abs(probs - 0.05)<1e-10]
  upper <- x[abs(probs - 0.95)<1e-10] 
  axis(1, at = c(lower, 0, upper), labels = c(round(lower, 2), 0, round(upper, 2)))
  xleft <- x[x <= lower]
  xright <- x[x >= upper]
  yleft <- y[x <= lower]
  yright <- y[x >= upper]
  polygon(c(min(x), xleft, lower), c(0, yleft, 0), col = "gray", density = 30, angle = 45)
  polygon(c(max(x), upper, xright), c(0, 0, yright), col = "gray", density = 30, angle = -45)
  text(mean(x), mean(y), "90%", col = "gray")
}

