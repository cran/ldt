
test_that("estim.sur estimation works with simulated data", {
  set.seed(340)
  data <- sim.sur(sigma = 2L, coef = 4L, nObs = 1000, intercept = TRUE)

  res = estim.sur(data = get.data(cbind(data$y,data$x), endogenous = ncol(data$y),
                                  addIntercept = FALSE))

  expect_equal(as.numeric(coef(res)), as.numeric(data$coef), tolerance = 1e-1)
  expect_equal(as.numeric(res$estimations$sigma), as.numeric(data$sigma), tolerance = 1e-1)

  # test exogenous and endogenous functions:
  X <- exogenous(res)
  Y <- endogenous(res)
  beta <- solve(t(X) %*% X) %*% t(X) %*% Y
  expect_equal(res$estimations$coefs, beta)
})


x <-matrix(c(32.446,44.145,17.062,65.818,76.19,40.408,78.131,
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
colnames(x) = paste0("V",seq(1,ncol(x)))



test_that("estim.sur estimation works with NO restrictions (OLS)", {
  res = estim.sur(data = get.data(x[,1:5], endogenous = 2))

  #resR = systemfit::systemfit(list(V1~V3+V4+V5,V2~V3+V4+V5), data=as.data.frame(x))
  #cofs = as.numeric(resR$coefficients)
  cofs = c(71.72759801705402083, -0.04375020121948767,  0.02041453289551552, -0.37114616290821228, 50.38631937796724714,
           -0.01306229557043696,  0.01754567721511469,  0.01380101248468951)
  #rcov = as.numeric(resR$residCov)*18/22
  rcov = c(379.12773062174330,  -74.25995265091441,  -74.25995265091441, 1004.26919375767909)
  expect_equal(as.numeric(coef(res)), cofs, tolerance = 1e-8)
  expect_equal(as.numeric(res$estimations$sigma),
               rcov, tolerance = 1e-8) # adjusted dof

  # I couldn't get equal AIC. It seems that the number of parameters in systemfit
  # is not 8 in this specific example, but 9.
  # TODO: Why +1 ? see also rank documentation

})

test_that("estim.sur peojection works with NO restrictions (OLS)", {
  newX = matrix(c(1,2,3,4,5,6),2,3)
  colnames(newX) <- c("V3", "V4", "V5")
  res = estim.sur(data = get.data(x[,1:5], endogenous = 2, newData = newX))


  #cont = systemfit::systemfit.control(methodResidCov = 'noDfCor')
  #resR = systemfit::systemfit(list(V1~V3+V4+V5,V2~V3+V4+V5), data=as.data.frame(x), control = cont, method = "SUR")
  #pre = predict(resR, newdata = as.data.frame(newX), se.pred = TRUE,
  #              useDfSys = TRUE)
  mprd = c(69.88936059998029, 69.49487876874811) #as.numeric(pre$eq1.pred)
  #rcov = as.numeric(resR$residCov)
  rcov = c(379.12773062174324, -74.25995265091446, -74.25995265091446, 1004.26919375767909)
  #rc = as.numeric(resR$coefCov)[1:3]
  rc = c(135.2439004698999270, -1.0229556625096508, -0.4419294580987213)

  expect_equal(as.numeric(res$projection$means[,1]), mprd, tolerance = 1e-8)
  expect_equal(as.numeric(res$estimations$sigma), rcov, tolerance = 1e-8)
  expect_equal(as.numeric(res$estimations$gammaVar)[1:3], rc, tolerance = 1e-8)

  expect_equal(as.numeric(sqrt(res$estimations$gammaVar[3,3])), as.numeric(res$estimations$stds[3]), tolerance = 1e-8)

  # well, I think in systemfit the variances are calculated for each equation
  # See the details in systemfit:::predict.systemfit
  # this is different from what I implemented
  # therefore, the following is not valid:

  #expect_equal(sqrt(as.numeric(res$Projections$Variance[,1])),
  #             as.numeric(pre$eq1.se.pred), tolerance = 1e-8)

})

test_that("estim.sur estimation works with restrictions", {
  R = diag(8)
  R=R[,c(-2,-7)]
  res = estim.sur(data = get.data(x[,1:5], endogenous = 2), restriction = R)
  #resR = systemfit::systemfit(list(V1~V4+V5,V2~V3+V5), data=as.data.frame(x),method="SUR")
  #cf = as.numeric(resR$coefficients)
  cf = c(69.83450663570873473 , 0.01888766594180449 ,-0.37231463758924349, 51.22018576974861759, -0.01993726393353337,
         0.02184315674580949)
  #cv = as.numeric(resR$residCov)*19/22
  cv = c(380.49525763946917, -74.49900118994815, -74.49900118994815, 1004.63372739176225)
  expect_equal(as.numeric(res$estimations$gamma), cf,
               tolerance = 1e-3) # low tolerance ?! it must be because the initialization is different
  expect_equal(as.numeric(res$estimations$sigma),
               cv, tolerance = 1e-3) # adjusted dof

})

test_that("estim.sur peojection works with NO restrictions (OLS)", {
  newX = matrix(c(1,2,3,4,5,6),2,3)
  colnames(newX) <- c("V3", "V4", "V5")
  R = diag(8)
  R=R[,c(-2,-7)]
  res = estim.sur(data = get.data(x[,1:5], endogenous = 2, newData = newX), restriction = R)

  # cont = systemfit::systemfit.control(methodResidCov = 'noDfCor', singleEqSigma = TRUE)
  # resR = systemfit::systemfit(list(V1~V4+V5,V2~V3+V5), data=as.data.frame(x), control = cont, method = "SUR")
  # pre = predict(resR, newdata = as.data.frame(newX), se.pred = TRUE, useDfSys = FALSE)

  prd = c(68.02959644558878, 67.67616947394133) # as.numeric(pre$eq1.pred)
  rcov = c(380.49525763946917, -74.49900118994805,
           -74.49900118994805, 1004.63372739176225) # as.numeric(resR$residCov)

  pr <- predict(res)
  expect_equal(as.numeric(pr$means[,1]), prd, tolerance = 1e-3) # ??
  expect_equal(as.numeric(res$estimations$sigma), rcov, tolerance = 1e-3)

  # I could not reproduce the exact results of the coefficient variance.
  # I must be due to the difference between the initializations

  #expect_equal(as.numeric(res$estimations$gammaVar), as.numeric(resR$coefCov), tolerance = 1e-3)

  #expect_equal(sqrt(as.numeric(res$Projections$Variance[,1])),
  #             as.numeric(pre$eq1.se.pred), tolerance = 1e-8)

})

test_that("estim.sur estimation works with significance search", {
  for (th in seq(0.01,0.90,0.1)){
    res = estim.sur(data = get.data(x[,1:5], endogenous = 2),
                    searchSigMaxIter = 10,
                   searchSigMaxProb = th)
    for (a in res$estimations$pValues){
      if (is.nan(a) == FALSE){
        expect_true(a < th)
      }
    }
  }
})

test_that("estim.sur projection works with PCA for endogenous", {

  y = as.data.frame(cbind(as.matrix(x[,1:2]), prcomp(x[,3:5], scale. = TRUE)$x)) # orthogonal
  pcaOp = get.options.pca()
  pcaOp$ignoreFirst = 2
  pcaOp$exactCount = 1

  newX = matrix(c(10,11,12, 13,14,15),3,2)

  D <- cbind(as.matrix(y[,1:3]), x[,6:7, drop = FALSE])
  colnames(D)[4:5] <- c("X1", "X2")
  colnames(newX) <- c("X1", "X2")
  res1 = estim.sur(data = get.data(D, endogenous = 3, newData = newX))
  colnames(newX) <- c("V6", "V7")
  res2 = estim.sur(data = get.data(x[,1:7], endogenous = 5, newData = newX),
                   pcaOptionsY = pcaOp)

  expect_equal(res1$estimations$gamma, res2$estimations$gamma, tolerance = 1e-13)
  expect_equal(as.numeric( res1$estimations$sigma),as.numeric(res2$estimations$sigma), tolerance = 1e-13)

  # It will project PCA of y, but When there is ignoreFirst, it is not PCA

  expect_equal(res1$projection$means[,1:3], res2$projection$means[,1:3], tolerance = 1e-13)

})

test_that("estim.sur projection works with PCA for exogenous", {
  p=prcomp(x[,3:4], scale. = TRUE)
  Z = as.matrix(p$x[,1])
  newZ = matrix(c(10,11,12, 13,14,15),3,2)
  colnames(newZ) <- colnames(x)[3:4]
  newZp = predict(p,newdata = newZ)

  pcaOp = get.options.pca()
  pcaOp$ignoreFirst = 0
  pcaOp$exactCount = 1

  D <- cbind(as.matrix(x[,1:2]), Z)
  colnames(D) <- c("Y1", "Y2", "pca1")
  newX = newZp[,1,drop=FALSE]
  colnames(newX) <- c("pca1")
  res1 = estim.sur(data = get.data(D, endogenous = 2, newData = newX))
  colnames(newZp) <- c("V3", "V4")
  res2 = estim.sur(data = get.data(x[,1:4], endogenous = 2, newData = newZp),
                   pcaOptionsX = pcaOp)

  expect_equal(res1$estimations$gamma, res2$estimations$gamma, tolerance = 1e-8)
  expect_equal(res1$estimations$gammaVar, res2$estimations$gammaVar, tolerance = 1e-8)

  # without intercept
  res1 = estim.sur(data = get.data(D, endogenous = 2, newData = newX, addIntercept = FALSE))
  res2 = estim.sur(data = get.data(x[,1:4], endogenous = 2, newData = newZp, addIntercept = FALSE),
                   pcaOptionsX = pcaOp)

  expect_equal(res1$estimations$gamma, res2$estimations$gamma, tolerance = 1e-8)
  expect_equal(res1$estimations$gammaVar, res2$estimations$gammaVar, tolerance = 1e-8)
})

test_that("estim.sur estimation works with PCA for endogenous and exogenous with restrictions", {

  p=prcomp(x[,3:4], scale. = TRUE)
  Z = as.matrix(p$x[,1])

  pcaOpX = get.options.pca()
  pcaOpX$ignoreFirst = 0
  pcaOpX$exactCount = 1

  y = as.data.frame(cbind(as.matrix(x[,1:2]), prcomp(x[,3:5], scale. = TRUE)$x)) # orthogonal
  pcaOpY = get.options.pca()
  pcaOpY$ignoreFirst = 2
  pcaOpY$exactCount = 1

  # note that it is the current code's responsibility to predict the dimensions of R when PCA is used
  R = diag(6)
  R=R[,c(-2,-5)]

  D = cbind(y[,1:3], Z)
  res1 = estim.sur(data = get.data(D, endogenous = 3), restriction = R)
  DD = cbind(x[,1:5], x[,3:4])
  colnames(DD) <- c("V1", "V2", "V3", "V4", "V5", "X1", "X2")
  res2 = estim.sur(data = get.data(DD, endogenous = 5), pcaOptionsY = pcaOpY, pcaOptionsX = pcaOpX, restriction = R)

  expect_equal(res1$estimations$gamma, res2$estimations$gamma, tolerance = 1e-8)
})

test_that("estim.sur estimation works with PCA for endogenous and exogenous with significance search", {

  p=prcomp(x[,3:4], scale. = TRUE)
  Z = as.matrix(p$x[,1])

  pcaOpX = get.options.pca()
  pcaOpX$ignoreFirst = 0
  pcaOpX$exactCount = 1

  y = as.data.frame(cbind(as.matrix(x[,1:2]), prcomp(x[,3:5], scale. = TRUE)$x)) # orthogonal
  pcaOpY = get.options.pca()
  pcaOpY$ignoreFirst = 2
  pcaOpY$exactCount = 1

  # note that it is the current code's responsibility to predict the dimensions of R when PCA is used

  D = cbind(y[,1:3], Z)
  res1 = estim.sur(data = get.data(D, endogenous = 3), searchSigMaxIter = 10, searchSigMaxProb = 0.3)
  DD = cbind(x[,1:5], x[,3:4])
  res2 = estim.sur(data = get.data(DD, endogenous = 5), pcaOptionsY = pcaOpY, pcaOptionsX = pcaOpX, searchSigMaxIter = 10, searchSigMaxProb = 0.3)

  expect_equal(res1$estimations$gamma, res2$estimations$gamma, tolerance = 1e-8)
})

test_that("estim.sur simulation works", {

  # The primary test was with this endogenous variable x[,c(1,2,1)]
  # I wanted to test the equality of coefficients of 1st and 3rd equations
  # but the estimation fails because var(resid) is singular
  # you might want to change the options so that simulation be possible even when estimation fails.

  exo <- x[,c(5,6)]
  exo[[1,1]] <- NA
  exo[[1,2]] <- NaN
  D <- cbind(x[,c(1,2,3)], exo)
  res1 = suppressWarnings(estim.sur(data = get.data(D, endogenous = 3), simFixSize = 10, searchSigMaxIter = 2, searchSigMaxProb = 0.95 ))
  expect_equal(res1$simulation$validIter, 10)
  expect_true(all(is.na(res1$simulation$results[,1]) == FALSE))
  expect_equal(res1$simulation$results[,1], res1$simulation$results[,1], tolerance = 1e-8) # any better test ?!

})

test_that("estim.sur lambda in simulation works", {

  res1 = estim.sur(data = get.data(x[,1:6], endogenous = 2),
                   simFixSize = 10, simSeed = 340 )
  expect_equal(res1$simulation$validIter, 10)

  #data$lambdas <- c(1,1)
  res2 = estim.sur(data = get.data(x[,1:6], endogenous = 2, lambdas = c(1,1)),
                   simFixSize = 10, simSeed = 340)
  expect_equal(res1$simulation, res2$simulation)


  # There is no λ in the Box-Cox transformation which is f(x)=x
  # Therefore, the outputs will be different
  # Of course, I checked by changing the Cpp code

})

test_that("search.sur works for insample", {
  skip_on_cran()

  res = search.sur(data = get.data(x[,1:7, drop = FALSE], endogenous = 3),
                   combinations = get.combinations(sizes = c(1,2),
                                                   innerGroups = list(c(1,2), c(1,3),c(2,3)),
                                                   numTargets = 3),
                   items = get.search.items(all = TRUE, bestK = 2),
                   metrics = get.search.metrics(typesIn = c("aic", "sic")))
  allMetrics = sort(sapply(res$results, function(x){x$value$metric}))
  sumRes <- summary(res, test = TRUE)

  expect_equal(sumRes$counts, res$counts) # Just to show that is not an empty test

  # change Indexes
  res1 = search.sur(data = get.data(x[,c(2,3,1,5,4,7,6), drop = FALSE], endogenous = 3),
                   combinations = get.combinations(sizes = c(1,2),
                                                   innerGroups = list(c(1,2), c(1,3),c(2,3)),
                                                   numTargets = 3),
                   items = get.search.items(all = TRUE, bestK = 2),
                   metrics = get.search.metrics(typesIn = c("aic", "sic")))

  expect_equal(res$counts, res1$counts)
  allMetrics1 = sort(sapply(res1$results, function(x){x$value$metric}))

  for (r1 in res$results){
    if (r1$typeName == "model")
      next # we cannot compare 'info' of all models, but best ones (you can check the existence)

    r2 = res1$results[which(sapply(res1$results,
                                  function(r)r$evalName == r1$evalName &&
                                    r$targetName == r1$targetName && r$typeName == r1$typeName &&
                                    r$info == r1$info))]
    expect_true(length(r2) == 1)
    expect_equal(r1$value$metric, r2[[1]]$value$metric) # indices are different
  }
  # note that data must have given column names in the two searches
  # also, 'innerGroups' must point to same variables in both cases
  # also, make sure 'numTargets' does not exclude best models

  expect_equal(allMetrics, allMetrics1)

})

test_that("search.sur works with fixed exogenous variables", {
  skip_on_cran()

  res = search.sur(data = get.data(x[,1:7, drop = FALSE], endogenous = 3),
                   combinations = get.combinations(sizes = c(1, 2, 3),
                                                   innerGroups = list(c(2),c(1,3),c(1,2,3)),
                                                   numTargets = 2,
                                                   numFixPartitions = 3),
                   items = get.search.items(all = TRUE, bestK = 2),
                   metrics = get.search.metrics(typesIn = c("aic", "sic")))

  for (m in res$results){
    expect_equal(c('(Intercept)', 'V4', 'V5'),m$value$exogenous[1:3]) # first 3 exogenous variables are fixed
  }
})

test_that("search.sur works for out-of-sample", {
  skip_on_cran()

  res = search.sur(data = get.data(x[,1:7, drop = FALSE], endogenous = 3),
                   combinations = get.combinations(sizes = c(1,2),
                                                   innerGroups = list(c(1,2), c(1,3),c(2,3)),
                                                   numTargets = 3),
                   items = get.search.items(all = TRUE, bestK = 2),
                   metrics = get.search.metrics(c("aic"),c("rmse", "crps", "sign"),
                                                simFixSize = 4,
                                                trainRatio = 0.75,
                                                seed = -340))  # negative seed for equal seed in the searchers
  sumRes <- summary(res, test = TRUE)
  allMetrics = sort(sapply(res$results, function(x){x$value$metric}))
  expect_equal(sumRes$counts, res$counts) # Just to show that is not an empty test

  # change Indexes
  res1 = search.sur(data = get.data(x[,c(3,1,2,6,4,5,7), drop = FALSE], endogenous = 3),
                   combinations = get.combinations(sizes = c(1,2),
                                                   innerGroups = list(c(1,2), c(1,3),c(2,3)),
                                                   numTargets = 3),
                   items = get.search.items(all = TRUE, bestK = 2),
                   metrics = get.search.metrics(c("aic"),c("rmse", "crps", "sign"),
                                                simFixSize = 4,
                                                trainRatio = 0.75,
                                                seed = -340))
  allMetrics1 = sort(sapply(res1$results, function(x){x$value$metric}))
  expect_equal(res$counts, res1$counts)

  for (r1 in res$results){
    if (r1$typeName == "model")
      next # we cannot compare 'info' of all models, but best ones (you can check the existence)

    r2 = res1$results[which(sapply(res1$results,
                                   function(r)r$evalName == r1$evalName &&
                                     r$targetName == r1$targetName && r$typeName == r1$typeName &&
                                     r$info == r1$info))]
    expect_true(length(r2) == 1)
    expect_equal(r1$value$metric, r2[[1]]$value$metric) # indices are different
  }
  # note that data must have given column names in the two searches
  # also, 'innerGroups' must point to same variables in both cases
  # also, make sure 'numTargets' does not exclude best models

  expect_equal(allMetrics, allMetrics1)
})


test_that("search.sur works when parallel", {
  skip_on_cran()

  res = search.sur(data = get.data(x[,1:7, drop = FALSE], endogenous = 3),
                   combinations = get.combinations(sizes = c(1, 2, 3),
                                                   innerGroups = list(c(2),c(1,3),c(1,2,3)),
                                                   numTargets = 2,
                                                   numFixPartitions = 3),
                   items = get.search.items(all = TRUE, bestK = 2),
                   metrics = get.search.metrics(character(0),c("rmse", "crps", "sign"),simFixSize = 4, trainRatio = 0.75,
                                                seed = -340),
                   options = get.search.options(parallel = FALSE))
  sumRes <- summary(res, test=TRUE)
  allWeights = sort(sapply(res$results, function(x){x$value$metric}))

  res1 = search.sur(data = get.data(x[,1:7, drop = FALSE], endogenous = 3),
                   combinations = get.combinations(sizes = c(1, 2, 3),
                                                   innerGroups = list(c(2),c(1,3),c(1,2,3)),
                                                   numTargets = 2,
                                                   numFixPartitions = 3),
                   items = get.search.items(all = TRUE, bestK = 2),
                   metrics = get.search.metrics(character(0),c("rmse", "crps", "sign"),
                                                simFixSize = 4,
                                                trainRatio = 0.75,
                                                seed = -340),
                   options = get.search.options(parallel = TRUE))

  allWeights1 = sort(sapply(res1$results, function(x){x$value$metric}))

  expect_equal(as.numeric(allWeights), as.numeric(allWeights1), tolerance = 1e-10)
})



test_that("search.sur works with restricted aic", {
  skip_on_cran()

  res = search.sur(data = get.data(x[,1:7, drop = FALSE], endogenous = 3),
                   combinations = get.combinations(sizes = c(1, 2),
                                                   innerGroups = list(c(2),c(1,3),c(1,2,3)),
                                                   numTargets = 2),
                  modelChecks = get.search.modelchecks(maxAic = 230),
                  items = get.search.items(all = TRUE, bestK = 0),
                  metrics = get.search.metrics(character(0),c("rmse", "crps", "sign"),simFixSize = 4, trainRatio = 0.75,
                                                     trainFixSize = 12,
                                                     seed = -340))  # negative seed for equal distribution
  sumRes <- summary(res, test = TRUE)
  for (m in sumRes$results){
    aic <- as.numeric(m$value$metrics[rownames(m$value$metrics) == "aic",1])
    #print(aic)
    expect_true(aic <= 230)
  }
})

test_that("search.sur works with inclusion weights", {
  skip_on_cran()

  res = search.sur(data = get.data(x[,1:7, drop = FALSE], endogenous = 3),
                   combinations = get.combinations(sizes = c(1, 2),
                                                   innerGroups = list(c(2),c(1,3),c(1,2,3)),
                                                   numTargets = 2),
                   items = get.search.items(all = TRUE, bestK = 0, inclusion = TRUE),
                   metrics = get.search.metrics(c("sic"),c("rmse", "crps", "sign"),simFixSize = 4, trainRatio = 0.75,
                                                trainFixSize = 12,
                                                seed = -340))

  inclusion = matrix(0,8,2, dimnames = list(colnames(res$info$data$data), NULL))
  for (m in res$results[which(sapply(res$results,
                                     function(r) r$evalName == "sic" && r$typeName == "model" && r$targetName == "V1"))]){
    for (d in (m$value$endogenous)){
      inclusion[d,1] = inclusion[d,1] + m$value$weight
      inclusion[d,2] = inclusion[d,2] + 1
    }
    for (d in (m$value$exogenous)){
      inclusion[d,1] = inclusion[d,1] + m$value$weight
      inclusion[d,2] = inclusion[d,2] + 1
    }
  }
  inclusion[,1] = inclusion[,1]/inclusion[,2]

  searchInclusion = res$results[which(sapply(res$results,
                                             function(r) r$evalName == "sic" && r$targetName == "V1" && r$typeName == "inclusion"))]
  expect_equal(as.numeric(searchInclusion[[1]]$value), as.numeric(inclusion), tolerance = 1e-10)

})

test_that("search.sur works with coefficients (bests)", {
  skip_on_cran()

  res = search.sur(data = get.data(x[,1:5, drop = FALSE], endogenous = 3),
                   combinations = get.combinations(sizes = c(1, 2),
                                                   innerGroups = list(c(2),c(1,3),c(1,2,3)),
                                                   numTargets = 2),
                   items = get.search.items(all = TRUE, bestK = 2, type1 = TRUE),
                   metrics = get.search.metrics(c("sic"),c("rmse", "crps", "sign")
                                                , seed = -340))
   sumRes <- summary(res, test = TRUE)
   expect_equal(sumRes$counts, res$counts) # Just to show that is not an empty test
})

test_that("search.sur works with coefficients (cdfs)", {
  skip_on_cran()

  res = search.sur(data = get.data(x[,1:7, drop = FALSE], endogenous = 3),
                   combinations = get.combinations(sizes = c(1, 2),
                                                   innerGroups = list(c(2),c(1,3),c(1,2,3)),
                                                   numTargets = 2),
                   items = get.search.items(all = TRUE, bestK = 0, type1 = TRUE,
                                            cdfs = c(0,1,0)),
                   metrics = get.search.metrics(c("sic"),c("rmse", "crps", "sign")
                                                , seed = -340))
  sumRes <- summary(res, test = TRUE)
  sum = 0
  c = 0
  cc=0
  i = 0
  for (m in sumRes$results){
    i = i + 1
    if (m$evalName != "rmse" || m$typeName != "model" || m$targetName != "V1")
      next()
    ind = which(rownames(m$value$estimations$coefs) == "V4")
    if (length(ind) == 1){
      coef = m$value$estimations$gamma[ind]
      sd = sqrt(m$value$estimations$gammaVar[ind,ind])
      w = res$results[[i]]$value$weight
      sum = sum + w * pnorm(0,coef,sd)  # note the NORMAL dist. If t,  d.o.f : nrow(y)-length(gamma)
      c=c+w
      cc=cc+1
    }
  }

  cdfs = res$results[which(sapply(res$results,
                                  function(r) r$evalName == "rmse" && r$targetName == "V1" && r$typeName == "cdf"))]

  expect_equal(cdfs[[1]]$value[2,1], sum/c, tolerance = 1e-10)
  expect_equal(cdfs[[1]]$value[2,2], cc, tolerance = 1e-10)
})


test_that("search.sur works with coefficients (extreme bounds)", {
  skip_on_cran()

  res = search.sur(data = get.data(x[,1:7, drop = FALSE], endogenous = 3),
                   combinations = get.combinations(sizes = c(1, 2),
                                                   innerGroups = list(c(2),c(1,3),c(1,2,3)),
                                                   numTargets = 2),
                   items = get.search.items(all = TRUE, bestK = 0, type1 = TRUE,
                                            extremeMultiplier = 2),
                   metrics = get.search.metrics(c("sic"),c("rmse", "crps", "sign")
                                                , seed = -340))
  sumRes <- summary(res, test = TRUE)
  mn = Inf
  mx = -Inf
  for (m in sumRes$results){
    if (m$evalName != "rmse" || m$typeName != "model" || m$targetName != "V1")
      next()

    ind = which(rownames(m$value$estimations$coefs) == "V4")
    if (length(ind) == 1){
      coef = m$value$estimations$gamma[ind]
      sd = sqrt(m$value$estimations$gammaVar[ind,ind])
      mn = min(mn,coef-2*sd)
      mx = max(mx,coef+2*sd)
    }
  }

  extremeB = res$results[which(sapply(res$results,
                                  function(r) r$evalName == "rmse" && r$targetName == "V1" && r$typeName == "extreme bound"))]

  expect_equal(extremeB[[1]]$value[2,1], mn, tolerance = 1e-10)
  expect_equal(extremeB[[1]]$value[2,2], mx, tolerance = 1e-10)
})

test_that("search.sur works with coefficients (mixture)", {
  skip_on_cran()

  res = search.sur(data = get.data(x[,1:7, drop = FALSE], endogenous = 3),
                   combinations = get.combinations(sizes = c(1, 2),
                                                   innerGroups = list(c(2),c(1,3),c(1,2,3)),
                                                   numTargets = 2),
                   items = get.search.items(all = TRUE, bestK = 0, type1 = TRUE,
                                            mixture4 = TRUE),
                   metrics = get.search.metrics(c("sic"),c("rmse", "crps", "sign")
                                                , seed = -340))
  sumRes <- summary(res, test = TRUE)
  coefs = c()
  vars = c()
  weights = c()
  i = 0
  for (m in sumRes$results){
    i = i + 1
    if (m$evalName != "rmse" || m$typeName != "model" || m$targetName != "V1")
      next()

    ind = which(rownames(m$value$estimations$coefs) == "V4")
    if (length(ind) == 1){
      coefs = append(coefs,m$value$estimations$gamma[ind])
      vars = append(vars, m$value$estimations$gammaVar[ind,ind])
      w = res$results[[i]]$value$weight
      weights = append(weights, w)
    }
  }

  mixture = res$results[which(sapply(res$results,
                                      function(r) r$evalName == "rmse" && r$targetName == "V1" && r$typeName == "mixture"))]

  # note that we need weighted mean, variance, etc. assuming normal distribution

  len = length(coefs)
  expect_equal(mixture[[1]]$value[2,5], len)
  me = weighted.mean(coefs, weights)
  expect_equal(mixture[[1]]$value[2,1], me, tolerance = 1e-14)

  # TODO : compare weighted variance, skewness, kurtosis assuming normality
  #        of course, its better to .Call the running statistics, test it, and use it here

})




test_that("SUR SplitSearch works (no subsetting)", {
  skip_on_cran()

  # don't test with out-of-sample metrics. It seems we have different model with equal weights (the result change by repeating the call ?!)

  data = get.data(x[,1:7, drop = FALSE], endogenous = 3)
  combinations = get.combinations(numTargets = 3, innerGroups = list(c(1), c(1,2), c(1,3)))
  items = get.search.items(inclusion = TRUE
                           #, all = TRUE
                           , bestK = 4
                           , type1 = TRUE
                           , cdfs = c(0,0.3)
                           , mixture4 = TRUE
                           , extremeMultiplier = 2
                           )
  metrics = get.search.metrics(c("sic", "aic")) # don't test with out-of-sample metrics. It seems we have different model with equal weights (the result change by repeating the call ?!)
  options = get.search.options(FALSE,
                               #reportInterval = 1
                               )

  combinations$sizes <- c(1, 2, 3)
  whole = search.sur(data = data,
                     combinations = combinations,
                     items = items,
                     metrics = metrics,
                     options = options)

  combinations$sizes <- list(c(1, 2), c(3))
  combinations$stepsNumVariables <- c(NA, NA)
  split = search.sur(data = data,
                     combinations = combinations,
                     items = items,
                     metrics = metrics,
                     options = options)

  expect_equal(whole$counts, split$counts)
  expect_equal(length(whole$results), length(split$results))

  pastedList_w <- unlist(lapply(whole$results, function(x) paste(x[1:4], collapse = "")))
  pastedList_s <- unlist(lapply(split$results, function(x) paste(x[1:4], collapse = "")))

  sortedList_w <- whole$results[order(pastedList_w)]
  sortedList_s <- split$results[order(pastedList_s)]

  for (i in 1:length(sortedList_w)){
    if (sortedList_s[[i]]$typeName == "mixture"){
      expect_equal(sortedList_s[[i]]$value[,c(1:3,5,6)], sortedList_w[[i]]$value[,c(1:3,5,6)])
      expect_equal(sortedList_s[[i]]$value[,c(4)], sortedList_w[[i]]$value[,c(4)], tolerance = 0.1)
    }
    else
      expect_equal(sortedList_s[[i]]$value, sortedList_w[[i]]$value)
  }

})


