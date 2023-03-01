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

## -----------------------------------------------------------------------------
measureOptions <- GetMeasureOptions(
  simFixSize = 24,
  typesIn = NULL, typesOut = c("direction", "scaledRmse", "crps"),
  horizons = as.integer(c(1))
)

## ----data, eval=FALSE---------------------------------------------------------
#  pcpdata <- Data_Pcp()

## -----------------------------------------------------------------------------
seasonsCount <- 12

## ---- eval=FALSE--------------------------------------------------------------
#  numTargets <- ncol(pcpdata$data) - 2

## ---- eval=FALSE--------------------------------------------------------------
#  y <- sapply(pcpdata$data, function(v) {
#    r <- log(v)
#    r[r <= 0] <- NA
#    r
#  })

## ----eval=FALSE---------------------------------------------------------------
#  n <- nrow(y)
#  x <- as.matrix(data.frame(Intercept = rep.int(1, n), Trend = c(1:n)))
#  newX <- as.matrix(data.frame(Intercept = rep(1, 24), Trend = c((n + 1):(n + 24))))

## ----save_data, echo=FALSE,eval=FALSE-----------------------------------------
#  vig_data = ldt::vig_data
#  vig_data$pcp = list()
#  vig_data$pcp$y <- y[,1:5]
#  vig_data$pcp$desc <- pcpdata$desc[1:5]
#  vig_data$pcp$x <- x
#  vig_data$pcp$newX <- newX
#  vig_data$pcp$ncols <- ncol(pcpdata$data)
#  #usethis::use_data(vig_data, overwrite = TRUE)

## ----data_load----------------------------------------------------------------
y = ldt::vig_data$pcp$y
x = ldt::vig_data$pcp$x
newX = ldt::vig_data$pcp$newX
numTargets <- 3

## -----------------------------------------------------------------------------
ySizes <- as.integer(c(1:2))

## -----------------------------------------------------------------------------
xGroups <- list(as.integer(c(1)), as.integer(c(1, 2)))

## -----------------------------------------------------------------------------
maxParams <- c(2, 1, 0, 2, 1, 0)

## -----------------------------------------------------------------------------
checkItems <- GetModelCheckItems(
  estimation = TRUE, prediction = TRUE,
  predictionBoundMultiplier = 4
)

## ----estimation---------------------------------------------------------------

pcpRes <- VarmaSearch(
  y = y, x = x, numTargets = numTargets,
  ySizes = ySizes, xGroups = xGroups, maxParams = maxParams,
  seasonsCount = seasonsCount, maxHorizon = 1, newX = newX,
  interpolate = TRUE,
  measureOptions = measureOptions,
  modelCheckItems = checkItems,
  searchItems = GetSearchItems(model = TRUE, bestK = 1),
  searchOptions = GetSearchOptions(printMsg = FALSE)
)



## ----pcp_table, echo=FALSE, results='markup'----------------------------------
max_desc_length <- 25

pcpranks <- data.frame(sapply(c(1:3), function(mi) {
  targs <- sapply(c(1:numTargets), function(ti) pcpRes[[1 + mi]][[ti]]$model$bests$best1$weight)
  names(targs) <- sapply(c(1:numTargets), function(ti) pcpRes[[1 + mi]][[ti]]$name)
  targs <- sort(targs, decreasing = TRUE, index.return = TRUE)
  nams <- colnames(vig_data$pcp$y)[targs$ix]
  desc <- vig_data$pcp$desc[targs$ix]
  targs$labels <- sapply(targs$ix, function(i) {
    d <- desc[[i]]
    if (nchar(d) > max_desc_length) {
      d <- paste0(substr(d, 1, max_desc_length), "...")
    }
    # nm = paste0(nams[[i]],": ", d)
    nm <- gsub("%", "\\%", d, fixed = TRUE)
    nm
  })
  targs
}))
mea_names <- names(pcpRes)[2:4]
names(pcpranks) <- mea_names

# a table with the names and ranks
take_Each_count <- 3
nms <- unique(c(sapply(c(1:3), function(i) c(sapply(c(1:take_Each_count), function(j) pcpranks[[i]]$labels[[j]])))))
res <- list()
for (i in c(1:3)) {
  r <- pcpranks[[i]]
  mea <- mea_names[[i]]
  res[[length(res) + 1]] <- sapply(nms, function(n) {
    ij <- which(r$labels == n)[[1]]
    v <- GetMeasureFromWeight(r$x[[ij]], measureName = mea)
    if (ij > 3) {
      paste0(round(v, 4), " <sup>(", ij, ")</sup>")
    } else {
      paste0(round(v, 4), " <b><sup>(", ij, ")</sup></b>")
    }
  })
}
res <- as.data.frame(res)
colnames(res) <- mea_names


kableExtra::kable(res, "html",
                  booktabs = TRUE, digits = 2,
                  align = c("l", "l", "l"), caption = "Psuedo out-of-sample scores and ranks",
                  linesep = "", escape = FALSE
) %>%
  kableExtra::footnote(
    general = "<p>Each value is the best score that is found for the variables in the row. The numbers in the parentheses indicate the rank of the values.</p>",
    escape = FALSE, fixed_small_size = TRUE, footnote_as_chunk = TRUE
  ) %>%
  kable_styling(full_width = NULL)

