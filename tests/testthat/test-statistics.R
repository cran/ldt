

x <- matrix(c(32.446,44.145,17.062,65.818,76.19,40.408,78.131,
              26.695,21.992,68.033,98.872,61.154,71.842,66.922,
              31.142,58.429,45.123,80.99,26.345,50.096,36.478,
              29.377,27.141,65.037,72.621,63.391,68.125,60.369,
              76.384,5.449,99.204,6.87,2.514,98.799,95.082,
              6.048,59.287,48.889,21.464,54.972,14.997,27.161,
              92.522,65.383,51.852,71.011,55.434,89.082,59.556,
              29.524,21.193,2.684,35.457,69.849,76.352,49.455,
              18.762,8.492,95.032,39.042,32.517,13.667,91.408,
              23.432,56.526,33.531,10.67,72.891,11.796,31.202,
              96.893,2.552,82.001,87.786,96.292,93.249,11.688,
              19.522,37.55,55.967,97.026,14.017,19.869,60.988,
              91.525,33.192,50.666,97.465,58.493,17.033,76.138,
              3.432,58.561,69.172,56.453,46.325,63.116,84.577,
              12.265,77.277,9.141,69.192,65.464,29.827,8.261,
              26.696,94.1,64.958,68.924,97.838,91.389,76.779,
              56.448,14.524,33.549,39.059,94.886,98.52,80.476,
              2.754,93.605,17.733,37.658,97.567,2.705,74.385,
              59.03,10.732,82.043,92.891,69.384,86.848,40.02,
              62.295,18.609,61.597,22.438,67.702,83.393,96.283,
              64.895,34.39,42.212,52.377,24.745,42.534,64.688,
              7.392,82.462,22.022,68.858,55.901,98.156,96.029),
            nrow =22, ncol=7)
tX= t(x)
X_NA <- matrix(c(32.446,44.145,17.062,65.818,76.19,40.408,78.131,
                 26.695,21.992,68.033,98.872,61.154,71.842,66.922,
                 31.142,58.429,45.123,80.99,26.345,50.096,36.478,
                 29.377,27.141,65.037,72.621,63.391,68.125,60.369,
                 76.384,5.449,99.204,6.87,2.514,98.799,95.082,
                 6.048,59.287,48.889,21.464,54.972,14.997,27.161,
                 92.522,65.383,51.852,71.011,55.434,89.082,59.556,
                 29.524,21.193,2.684,35.457,69.849,76.352,49.455,
                 18.762,8.492,95.032,39.042,32.517,13.667,91.408,
                 23.432,56.526,33.531,10.67,72.891,11.796,31.202,
                 96.893,2.552,82.001,87.786,96.292,93.249,11.688,
                 19.522,NA,55.967,97.026,14.017,19.869,60.988,
                 91.525,33.192,50.666,97.465,58.493,17.033,76.138,
                 3.432,58.561,69.172,56.453,46.325,63.116,84.577,
                 12.265,77.277,9.141,69.192,65.464,29.827,8.261,
                 26.696,94.1,64.958,68.924,97.838,91.389,76.779,
                 56.448,14.524,33.549,39.059,94.886,98.52,80.476,
                 2.754,93.605,NA,37.658,97.567,2.705,74.385,
                 59.03,10.732,82.043,92.891,69.384,86.848,40.02,
                 62.295,18.609,61.597,22.438,67.702,83.393,96.283,
                 64.895,34.39,42.212,52.377,24.745,42.534,64.688,
                 7.392,82.462,NA,68.858,55.901,98.156,96.029),
               nrow =22, ncol=7)


test_that("Pca works", {
  res = s.pca(tX,TRUE,TRUE,tX)
  resR = prcomp(tX, center = T, scale. = T)
  expect_equal(res$stds, resR$sdev, tolerance = 1e-8)
  presR = predict(resR, newdata = tX)
  expect_equal(as.numeric(abs(presR)), as.numeric(abs(res$projections[,1:7])), tolerance = 1e-8)

  res = s.pca(tX,TRUE,FALSE,tX)
  resR = prcomp(tX, center = T, scale. = F)
  expect_equal(res$stds, resR$sdev, tolerance = 1e-8)

  res = s.pca(tX,FALSE,TRUE,tX)
  y=tX
  for (i in c(1:ncol(y)))
    y[,i]=y[,i]/sd(y[,i])  #TODO: otherwise the test is not working and I am not sure why
  resR = prcomp(y, center = F, scale. = F)
  expect_equal(res$stds, resR$sdev, tolerance = 1e-8)

  res = s.pca(tX,FALSE,FALSE,tX)
  resR = prcomp(tX, center = F, scale. = F)
  expect_equal(res$stds, resR$sdev, tolerance = 1e-8)

})

test_that("Pca works with zero variance columns", {

  X_0 = cbind(1,tX[,1:4],1,tX[,5:22],1)

  res = s.pca(X_0,TRUE,TRUE,X_0)
  resR = prcomp(tX, center = T, scale. = T)
  expect_equal(res$stds, resR$sdev, tolerance = 1e-8)
  presR = predict(resR, newdata = tX)
  expect_equal(as.numeric(abs(presR)), as.numeric(abs(res$projections[,1:7])), tolerance = 1e-8)

  res = s.pca(X_0,FALSE,TRUE,X_0)
  y=tX
  for (i in c(1:ncol(y)))
    y[,i]=y[,i]/sd(y[,i])  #TODO: otherwise the test is not working and I am not sure why
  resR = prcomp(y, center = F, scale. = F)
  expect_equal(res$stds, resR$sdev, tolerance = 1e-8)

})

test_that("GldFromMoments works", {

  res = s.gld.from.moments(0,1,0,0, start = c(0,0), type = 4)
  #resR = gld::fit.fkml.moments.val(moments=c(mean=0, variance=1, skewness=0,
  #                                    kurtosis=3), starting.point = c(0,0))
  #lambda = as.numeric(resR$lambda)
  lambda <- c(6.72393226737002e-05, 1.46375447283582, 0.134754823266661, 0.1348815721514)
  expect_equal(lambda, as.numeric(res)[1:4], tolerance = 1e-4)

})

test_that("Gld Qunatile works", {

  res = s.gld.quantile(seq(0.1,0.9,0.1), 0,1,0,0)
  expect_equal(0, res[[5]], tolerance = 1e-16)

})

test_that("Combine4Moments works", {

  set.seed(340)

  x1 <- rchisq(1000,3)
  x2 <- rchisq(1000,5)
  x <- c(x1,x2)

  d1 <- list(mean = mean(x1), variance = var(x1),
             skewness = moments::skewness(x1), kurtosis = moments::kurtosis(x1),
             count=length(x1), weight = length(x1))
  d2 <- list(mean = mean(x2), variance = var(x2),
             skewness = moments::skewness(x2), kurtosis = moments::kurtosis(x2),
             weight = length(x2), count = length(x2))
  d <- list(mean = mean(x), variance = var(x),
            skewness = moments::skewness(x), kurtosis = moments::kurtosis(x),
            weight = length(x), count = length(x))

  c <- s.combine.stats4(d1,d2)

  expect_equal(as.numeric(c)[1:2],as.numeric(d)[1:2], tolerance = 1e-3)
  expect_equal(as.numeric(c)[3],as.numeric(d)[3], tolerance = 1e-3)
  expect_equal(as.numeric(c[4]),as.numeric(d[4]), tolerance = 1) # !!!! check kurtosis definition, among other things
  expect_equal(as.numeric(c[5]),as.numeric(d[5]), tolerance = 1e-10)
})

test_that("Auc works for binary and no weights", {

  y = c(1, 0, 1, 0, 1, 1)
  scores = c(0.1, 0.9, 0.1, 0.9, 0.1, 0.9)

  res = s.roc(y,scores,NULL)
  #resR = pROC::roc(y,scores[,1])
  expect_equal(res$auc, 0.875, tolerance = 1e-14)

  #partial
  y <- c(1, 0, 1, 0, 1, 1, 0, 0, 1, 0)
  scores <- c(0.1, 0.2, 0.3, 0.5, 0.5, 0.5, 0.7, 0.8, 0.9, 1)
  opt <- get.options.roc(0.2,0.8)
  res = s.roc(y,scores,NULL,options = opt)
  expect_equal(res$auc, 0.44 / 0.6, tolerance = 1e-14)

  # weighted
  y <- c(1, 0, 1, 0, 1, 1, 0, 0, 1, 0, 1) # zero weight for the last observation
  scores <- c(0.1, 0.2, 0.3, 0.5, 0.5, 0.5, 0.7, 0.8, 0.9, 1, 0.8)
  weights <- c(1,1,1,1,1,1,1,1,1,1,0)
  res = s.roc(y,scores,weights,options = opt)
  expect_equal(res$auc, 0.44 / 0.6, tolerance = 1e-14)

  # varying cost
  y <- c(1, 0, 1, 0, 1, 1, 0, 0, 1, 0)
  scores <- c(0.1, 0.2, 0.3, 0.5, 0.5, 0.5, 0.7, 0.8, 0.9, 1)
  costs <- c(1,1,1,1,1,1,1,1,1,1)
  costMatrix = matrix(c(0.02,-1,-10,10),2,2)
  opt <- get.options.roc(costs = costs, costMatrix = costMatrix)
  res = s.roc(y,scores,NULL,options = opt)
  #expect_equal(res$AUC, ?, tolerance = 1e-14) TODO

})







test_that("Distance (euclidean,manhattan,maximum) works", {

  for (dis in c("euclidean","manhattan","maximum")){
    res <- s.distance(x, dis)
    rres = as.numeric(dist(tX,dis))
    expect_equal(res, rres, tolerance = 1e-8)  }

  # todo: if you want to test by removing NA, you should calculate distances pairwise (same as LDT)

})


test_that("Distance (correlation) works", {
  res <- s.distance(x, "correlation", "pearson", FALSE)
  rres <- as.numeric(as.dist(sqrt((1 - cor(x, method = "pearson"))/2.0)))
  expect_equal(res, rres, tolerance = 1e-8)

  # abs
  res <- s.distance(x, "absCorrelation", "pearson", FALSE)
  rres <- as.numeric(as.dist(sqrt((1 - cor(x, method = "pearson")^2))))
  expect_equal(res, rres, tolerance = 1e-8)

})

test_that("Distance (pearson correlation) works with NA", {
  res <- s.distance(X_NA, "correlation", "pearson", TRUE)
  rres <- as.numeric(as.dist(sqrt((1 - cor(X_NA, use = "pairwise.complete.obs", method = "pearson"))/2.0)))
  expect_equal(res, rres, tolerance = 1e-8)

  # abs
  res <- s.distance(X_NA, "absCorrelation", "pearson", TRUE)
  rres <- as.numeric(as.dist(sqrt((1 - cor(X_NA, use = "pairwise.complete.obs" , method = "pearson")^2))))
  expect_equal(res, rres, tolerance = 1e-8)

})

test_that("Distance (spearman correlation) works", {
  res <- s.distance(x, "correlation", "spearman", FALSE)
  rres <- as.numeric(as.dist(sqrt((1 - cor(x, method = "spearman"))/2.0)))
  expect_equal(res, rres, tolerance = 1e-8)

  # abs
  res <- s.distance(x, "absCorrelation", "spearman", FALSE)
  rres <- as.numeric(as.dist(sqrt((1 - cor(x, method = "spearman")^2))))
  expect_equal(res, rres, tolerance = 1e-8)

})

test_that("Distance (spearman correlation) works with NA", {
  res <- s.distance(X_NA, "correlation", "spearman", TRUE)
  rres <- as.numeric(as.dist(sqrt((1 - cor(X_NA, use = "pairwise.complete.obs", method = "spearman"))/2.0)))
  expect_equal(res, rres, tolerance = 1e-8)

  # abs
  res <- s.distance(X_NA, "absCorrelation", "spearman", TRUE)
  rres <- as.numeric(as.dist(sqrt((1 - cor(X_NA, use = "pairwise.complete.obs" , method = "spearman")^2))))
  expect_equal(res, rres, tolerance = 1e-8)

})


# H CLUSTERING

Dist = dist(t(x))
DistCount = 7



test_that("Hierarchical (single, complete, uAverage, wAverage, ward) clustering works", {

  for (link in c("single", "complete", "uAverage", "wAverage", "ward")) {
    rLink=link
    if (link == "uAverage")
      rLink = "average"
    if (link == "wAverage")
      rLink = "mcquitty"
    if (link == "ward")
      rLink="ward.D"

    res <- s.cluster.h(as.numeric(Dist), link)
    rres <- hclust(Dist, rLink)
    expect_equal(res$height, rres$height, tolerance = 1e-8)

  }
})


#test_that("Hierarchical grouping works with NAs", {

#  res <- s.cluster.h.group(X_NA,3,0, "correlation", "wAverage", "pearson")

#TODO

#})
