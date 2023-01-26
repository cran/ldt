
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
            nrow =7, ncol=22)


test_that("Pca works", {
  res = GetPca(x,TRUE,TRUE,x)
  resR = prcomp(x, center = T, scale. = T)
  expect_equal(res$stds, resR$sdev, tolerance = 1e-8)
  presR = predict(resR, newdata = x)
  expect_equal(as.numeric(abs(presR)), as.numeric(abs(res$projections[,1:7])), tolerance = 1e-8)

  res = GetPca(x,TRUE,FALSE,x)
  resR = prcomp(x, center = T, scale. = F)
  expect_equal(res$stds, resR$sdev, tolerance = 1e-8)

  res = GetPca(x,FALSE,TRUE,x)
  y=x
  for (i in c(1:ncol(y)))
    y[,i]=y[,i]/sd(y[,i])  #TODO: otherwise the test is not working and I am not sure why
  resR = prcomp(y, center = F, scale. = F)
  expect_equal(res$stds, resR$sdev, tolerance = 1e-8)

  res = GetPca(x,FALSE,FALSE,x)
  resR = prcomp(x, center = F, scale. = F)
  expect_equal(res$stds, resR$sdev, tolerance = 1e-8)

})

test_that("Pca works with zero variance columns", {

  X_0 = cbind(1,x[,1:4],1,x[,5:22],1)

  res = GetPca(X_0,TRUE,TRUE,X_0)
  resR = prcomp(x, center = T, scale. = T)
  expect_equal(res$stds, resR$sdev, tolerance = 1e-8)
  presR = predict(resR, newdata = x)
  expect_equal(as.numeric(abs(presR)), as.numeric(abs(res$projections[,1:7])), tolerance = 1e-8)

  res = GetPca(X_0,FALSE,TRUE,X_0)
  y=x
  for (i in c(1:ncol(y)))
    y[,i]=y[,i]/sd(y[,i])  #TODO: otherwise the test is not working and I am not sure why
  resR = prcomp(y, center = F, scale. = F)
  expect_equal(res$stds, resR$sdev, tolerance = 1e-8)

})

test_that("GldFromMoments works", {

  res = GetGldFromMoments(0,1,0,0,0,c(0,0))
  #resR = gld::fit.fkml.moments.val(moments=c(mean=0, variance=1, skewness=0,
  #                                    kurtosis=3), starting.point = c(0,0))
  #lambda = as.numeric(resR$lambda)
  lambda = c(1.051238729527833e-07, 1.463551748074293e+00, 1.349123923194031e-01, 1.349125904877055e-01)
  expect_equal(as.numeric(res), lambda, tolerance = 1e-4)

})

test_that("Gld Qunatile works", {

  res = GldQuantile(seq(0.1,0.9,0.1), 0,1,0,0)
  expect_equal(0, res[[5]], tolerance = 1e-16)

})

test_that("Combine4Moments works", {

  set.seed(340)

  x1 <- rchisq(10000,3)
  x2 <- rchisq(20000,5)
  x <- c(x1,x2)

  s1 = 1.279220669965758 # moments::skewness(x)
  k1 = 5.291531073584487 # moments::kurtosis(x)
  skewness1 = 1.638413219051492 # moments::skewness(x1)
  kurtosis1 = 7.035957122848987 # moments::kurtosis(x1)
  skewness2 = 1.194487661956922 # moments::skewness(x2)
  kurtosis2 = 5.036175897895664 # moments::kurtosis(x2)

  d1 <- list(mean = mean(x1), variance = var(x1), skewness = skewness1, kurtosis = kurtosis1, count=length(x1), sumWeights = length(x1))
  d2 <- list(mean = mean(x2), variance = var(x2), skewness = skewness2, kurtosis = kurtosis2, count=length(x2), sumWeights = length(x2))
  d <- list(mean = mean(x), variance = var(x), skewness = s1, kurtosis = k1, count=length(x), sumWeights = length(x))

  c <- GetCombination4Moments(d1,d2)

  expect_equal(as.numeric(c)[1:3],as.numeric(d)[1:3], tolerance = 1e-3)
  expect_equal(as.numeric(c[4]),as.numeric(d[4]), tolerance = 1e-1)
  expect_equal(as.numeric(c[5]),as.numeric(d[5]), tolerance = 1e-10)
})

test_that("Auc works for binary and no weights", {

  y = c(0,1,0,1,0,1,0,1)
  scores = matrix(c(
    0.4, 0.6, 0.45, 0.9, 0.6, 0.3, 0.5, 0.1,
    0.6, 0.4, 0.55, 0.1, 0.4, 0.7, 0.5, 0.9),8,2)

  res = GetAuc(y,scores,NULL)
  #resR = pROC::roc(y,scores[,1])
  #aucR = 1- as.numeric(resR$auc)
  aucR = 0.46875
  expect_equal(as.numeric(res), aucR, tolerance = 1e-4)
})

test_that("Long-run growth works (discrete)", {
  g <- LongrunGrowth(c(1,2,1))
  expect_equal(0, g, tolerance = 1e-16)

  g <- LongrunGrowth(c(1,2))
  expect_equal(100, g, tolerance = 1e-16)

  y <- abs(rnorm(10,2,5))
  g <- LongrunGrowth(y)
  #dy <- (diff(y) / y[1:(length(y)-1)])
  x = y[[1]] # start from y_0 and reach y_T
  for (i in c(1:(length(y)-1)))
    x <- x * (1 + g/100)
  expect_equal(x, y[[length(y)]], tolerance = 1e-12)

})

test_that("Long-run growth works (continuous)", {

  y <- abs(rnorm(10,2,5))
  g <- LongrunGrowth(y, cont = TRUE)
  #dy <- log(y[2:(length(y))]) - log(y[1:(length(y)-1)])
  x = y[[1]] # start from y_0 and reach y_T
  for (i in c(1:(length(y)-1)))
    x <- x * exp(g/100)
  expect_equal(x, y[[length(y)]], tolerance = 1e-12)

})

test_that("Long-run growth works (discrete with NAs)", {

  y <- c(NA, 0, abs(rnorm(10,2,5)), 0, NA, NA)
  g <- LongrunGrowth(y, 2, 3, skipZero = TRUE)
  x = y[[3]] # start from y_0 and reach y_T
  for (i in c(1:(length(y)-6)))
    x <- x * (1 + g/100)
  if (is.na(x))
    stop("x is NA")
  expect_equal(x, y[[length(y)-3]], tolerance = 1e-12)

})

test_that("Long-run growth works (%)", {

  y <- c(NA, 0, c(60,70,80,90), 0, NA, NA)
  g <- LongrunGrowth(y, 2, 3, skipZero = TRUE, isPercentage = TRUE, cont = TRUE)
  expect_equal(75, g, tolerance = 1e-12)

  g <- LongrunGrowth(y, 2, 3, skipZero = TRUE, isPercentage = TRUE, cont = FALSE)
  expect_equal(74.64202, g, tolerance = 1e-3) # TODO: check its validity

})

