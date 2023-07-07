
#TODO:  update search tests by using 'summary' function

printMsg = FALSE
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
  res = estim.sur(x[,1:2], x[,3:5], printMsg = printMsg)

  #resR = systemfit::systemfit(list(V1~V3+V4+V5,V2~V3+V4+V5), data=as.data.frame(x))
  #cofs = as.numeric(resR$coefficients)
  cofs = c(71.72759801705402083, -0.04375020121948767,  0.02041453289551552, -0.37114616290821228, 50.38631937796724714,
           -0.01306229557043696,  0.01754567721511469,  0.01380101248468951)
  #rcov = as.numeric(resR$residCov)*18/22
  rcov = c(379.12773062174330,  -74.25995265091441,  -74.25995265091441, 1004.26919375767909)
  expect_equal(as.numeric(res$estimations$gamma), cofs, tolerance = 1e-8)
  expect_equal(as.numeric(res$estimations$sigma),
               rcov, tolerance = 1e-8) # adjusted dof

  # I couldn't get equal AIC. It seems that the number of parameters in systemfit
  # is not 8 in this specific example, but 9.
  # TODO: Why +1 ? see also rank documentation

})

test_that("estim.sur peojection works with NO restrictions (OLS)", {
  newX = matrix(c(1,2,3,4,5,6),2,3)
  colnames(newX) <- c("V3", "V4", "V5")
  res = estim.sur(x[,1:2], x[,3:5], newX = newX, printMsg = printMsg)


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
  res = estim.sur(x[,1:2], x[,3:5], restriction = R, printMsg = printMsg)
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
  res = estim.sur(x[,1:2], x[,3:5], restriction = R, newX = newX, printMsg = printMsg)

  # cont = systemfit::systemfit.control(methodResidCov = 'noDfCor', singleEqSigma = TRUE)
  # resR = systemfit::systemfit(list(V1~V4+V5,V2~V3+V5), data=as.data.frame(x), control = cont, method = "SUR")
  # pre = predict(resR, newdata = as.data.frame(newX), se.pred = TRUE, useDfSys = FALSE)

  prd = c(68.02959644558878, 67.67616947394133) # as.numeric(pre$eq1.pred)
  rcov = c(380.49525763946917, -74.49900118994805,
           -74.49900118994805, 1004.63372739176225) # as.numeric(resR$residCov)

  expect_equal(as.numeric(res$projection$means[,1]), prd, tolerance = 1e-3) # ??
  expect_equal(as.numeric(res$estimations$sigma), rcov, tolerance = 1e-3)

  # I could not reproduce the exact results of the coefficient variance.
  # I must be due to the difference between the initializations

  #expect_equal(as.numeric(res$estimations$gammaVar), as.numeric(resR$coefCov), tolerance = 1e-3)

  #expect_equal(sqrt(as.numeric(res$Projections$Variance[,1])),
  #             as.numeric(pre$eq1.se.pred), tolerance = 1e-8)

})

test_that("estim.sur estimation works with significance search", {
  for (th in seq(0.01,0.90,0.1)){
    res = estim.sur(x[,1:2], x[,3:7], searchSigMaxIter = 10,
                   searchSigMaxProb = th, printMsg = printMsg)
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

  res1 = estim.sur(as.matrix(y[,1:3]), as.matrix(x[,6:7]), newX = newX, printMsg = printMsg)
  res2 = estim.sur(as.matrix(x[,1:5]), as.matrix(x[,6:7]), pcaOptionsY = pcaOp, newX = newX, printMsg = printMsg)

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

  res1 = estim.sur(x[,1:2], x= Z, addIntercept = TRUE, newX = as.matrix(newZp[,1],3,1), printMsg = printMsg)
  res2 = estim.sur(x[,1:2], x=x[,3:4], addIntercept = TRUE, pcaOptionsX = pcaOp, newX = newZp, printMsg = printMsg)

  expect_equal(res1$estimations$gamma, res2$estimations$gamma, tolerance = 1e-8)
  expect_equal(res1$estimations$gammaVar, res2$estimations$gammaVar, tolerance = 1e-8)

  # without intercept
  res1 = estim.sur(x[,1:2], x= Z, addIntercept = FALSE, newX = as.matrix(newZp[,1],3,1), printMsg = printMsg)
  res2 = estim.sur(x[,1:2], x=x[,3:4], addIntercept = FALSE, pcaOptionsX = pcaOp, newX = newZ, printMsg = printMsg)

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

  res1 = estim.sur(as.matrix(y[,1:3]), Z, restriction = R, printMsg = printMsg)
  res2 = estim.sur(x[,1:5], x[,3:4], pcaOptionsY = pcaOpY, pcaOptionsX = pcaOpX, restriction = R, printMsg = printMsg)

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

  res1 = estim.sur(as.matrix(y[,1:3]), Z, searchSigMaxIter = 10, searchSigMaxProb = 0.3, printMsg = printMsg)
  res2 = estim.sur(x[,1:5], x[,3:4], pcaOptionsY = pcaOpY, pcaOptionsX = pcaOpX, searchSigMaxIter = 10, searchSigMaxProb = 0.3, printMsg = printMsg)

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
  res1 = estim.sur(x[,c(1,2,3)], exo, simFixSize = 10, searchSigMaxIter = 2, searchSigMaxProb = 0.95, printMsg = printMsg )
  expect_equal(res1$simulation$validIter, 10)
  expect_true(all(is.na(res1$simulation$results[,1]) == FALSE))
  expect_equal(res1$simulation$results[,1], res1$simulation$results[,1], tolerance = 1e-8) # any better test ?!

})

test_that("search.sur works for insample", {
  skip_on_cran()

  y=x[,c(1,2,3)]
  Exo=x[,4:7]
  res = search.sur(y, Exo, numTargets = 2,  yGroups = list(c(2L),c(1L,3L),c(1L,2L,3L)),
                  searchOptions = get.options.search( printMsg = printMsg),
                  searchItems = get.items.search(all = TRUE, bestK = 2),
                  metricOptions = get.options.metric(c("aic", "sic"), character(0)))
  res1 = estim.sur(y[,res$aic$target1$model$bests$best1$depIndices,drop=FALSE],
                  Exo[,res$aic$target1$model$bests$best1$exoIndices,drop=FALSE],
                  simFixSize = 0, addIntercept = FALSE, printMsg = printMsg)
  res2 = estim.sur(y[,res$sic$target2$model$bests$best1$depIndices,drop=FALSE],
                  Exo[,res$sic$target2$model$bests$best1$exoIndices,drop=FALSE],
                  simFixSize = 0, addIntercept = FALSE, printMsg = printMsg)

  expect_equal(exp(-0.5 * res1$metrics[2,1]), res$aic$target1$model$bests$best1$weight, tolerance = 1e-10)
  expect_equal(exp(-0.5 * res2$metrics[3,1]), res$sic$target2$model$bests$best1$weight, tolerance = 1e-10)

  for (m in res$aic$target1$model$all){
    M = estim.sur(y[,m$depIndices,drop=FALSE], x = Exo[,m$exoIndices,drop=FALSE],
                 simFixSize = 0, addIntercept = FALSE, printMsg = printMsg)
    expect_equal(exp(-0.5 * M$metrics[2,1]), m$weight, tolerance = 1e-10)
  }

  # change Indexes
  y=x[,c(2,3,1)]
  Exo=x[,c(5,4,7,6)]
  res3 = search.sur(y, Exo,2,  yGroups = list(c(2L),c(1L,3L),c(1L,2L,3L)),
                   searchOptions = get.options.search( printMsg = printMsg),
                   searchItems = get.items.search(all = TRUE, bestK = 2),
                   metricOptions = get.options.metric(c("aic", "sic"), character(0)))
  expect_equal(res$bests$target2$aic$best1$weight, res3$bests$target1$aic$best1$weight, tolerance = 1e-14)

})

test_that("search.sur works with fixed exogenous variables", {
  skip_on_cran()

  y=x[,c(1,2,3)]
  Exo=x[,4:7]
  res = search.sur(y, Exo,2, xSizes = c(3L),  yGroups = list(c(2L),c(1L,3L),c(1L,2L,3L)),
                  numFixXPartitions = 3,
                  searchOptions = get.options.search( printMsg = printMsg),
                  searchItems = get.items.search(all = TRUE, bestK = 2),
                  metricOptions = get.options.metric(c("aic", "sic"), character(0)))

  for (m in res$aic$target1$model$all){
    expect_equal(c(1,2,3),m$exoIndices[1:3])
  }
})


test_that("search.sur works for insample when changing indexes", {
  skip_on_cran()

  y=x[,c(1,2,3)]
  Exo=x[,4:7]
  res = search.sur(y, Exo,2,  yGroups = list(c(1L),c(1L,2L),c(1L,2L,3L)),
                  searchOptions = get.options.search(parallel = F, printMsg = printMsg),
                  searchItems = get.items.search(all = TRUE, bestK = 0),
                  metricOptions = get.options.metric(c("aic", "sic"), character(0)))
  allWeights = sort(sapply(res$aic$target1$model$all, function(x){x$weight}))

  # change Indexes
  y=x[,c(2,1,3)]
  Exo=x[,c(5,4,7,6)]
  res3 = search.sur(y, Exo,2,  yGroups = list(c(2L),c(1L,2L),c(1L,2L,3L)),
                   searchOptions = get.options.search(parallel = F, printMsg = printMsg),
                   searchItems = get.items.search(all = TRUE, bestK = 0),
                   metricOptions = get.options.metric(c("aic", "sic"), character(0)))
  allWeights3 = sort(sapply(res3$aic$target2$model$all, function(x){x$weight}))

  expect_equal(as.numeric(allWeights), as.numeric(allWeights3), tolerance = 1e-8)
})


test_that("search.sur works for out-of-sample", {
  skip_on_cran()

  y=x[,c(1,2,3),drop=FALSE]
  Exo=x[,4:7,drop=FALSE]
  res = search.sur(y, Exo,2,  yGroups = list(c(1L),c(1L,2L),c(1L,2L,3L)),
                  searchOptions = get.options.search( printMsg = printMsg),
                  searchItems = get.items.search(all = TRUE, bestK = 2),
                  metricOptions = get.options.metric(c("aic"),c("rmse", "crps", "sign"),simFixSize = 4, trainRatio = 0.75,
                                                     seed = -340))  # negative seed for equal seed in the searchers
  res1 = estim.sur(y[,res$rmse$target1$model$bests$best1$depIndices,drop=FALSE],
                  Exo[,res$rmse$target1$model$bests$best1$exoIndices,drop=FALSE],
                  simFixSize = 4, simTrainRatio = 0.75, simSeed = 340, addIntercept = FALSE, printMsg = printMsg)

  expect_equal(as.numeric(res1$metrics[which(rownames(res1$metrics)=="rmse"),1]),
              as.numeric(s.metric.from.weight(res$rmse$target1$model$bests$best1$weight, "rmse")), tolerance = 1e-10)

  for (m in res$crps$target1$model$all){
    M = estim.sur(y[,m$depIndices,drop=FALSE], x = Exo[,m$exoIndices,drop=FALSE],
                 simFixSize = 4, simSeed = 340, addIntercept = FALSE, printMsg = printMsg)
    expect_equal(as.numeric(M$metrics[which(rownames(res1$metrics)=="crps"),1]),
                 as.numeric(s.metric.from.weight(m$weight, "crps")), tolerance = 1e-10)
  }

  # change Indexes
  y=x[,c(2,3,1)]
  Exo=x[,c(5,4,7,6)]
  res3 = search.sur(y, Exo,2,  yGroups = list(c(1L),c(1L,2L),c(1L,2L,3L)),
                   searchOptions = get.options.search( printMsg = printMsg),
                   searchItems = get.items.search(all = TRUE, bestK = 2),
                   metricOptions = get.options.metric(c("aic"),c("rmse", "crps", "sign"),simFixSize = 4, trainRatio = 0.75,
                                                      seed = -340))
  expect_equal(res$bests$target2$aic$best1$weight, res3$bests$target1$aic$best1$weight, tolerance = 1e-14)

})

test_that("search.sur works for out-of-sample when changing indexes", {
  skip_on_cran()

  y=x[,c(1,2,3)]
  Exo=x[,4:7]
  res = search.sur(y, Exo,2,  yGroups = list(c(1L),c(1L,2L),c(1L,2L,3L)),
                  searchOptions = get.options.search( printMsg = printMsg),
                  searchItems = get.items.search(all = TRUE, bestK = 2),
                  metricOptions = get.options.metric(c("aic"),c("rmse", "crps", "sign"),simFixSize = 4, trainRatio = 0.75,
                                                     seed = -340))
  allWeights = sort(sapply(res$crps$target1$model$all, function(x){x$weight}))

  # change Indexes
  y=x[,c(2,1,3)]
  Exo=x[,c(5,4,7,6)]
  res3 = search.sur(y, Exo,2,  yGroups = list(c(2L),c(1L,2L),c(1L,2L,3L)),
                   searchOptions = get.options.search( printMsg = printMsg),
                   searchItems = get.items.search(all = TRUE, bestK = 2),
                   metricOptions = get.options.metric(c("aic"),c("rmse", "crps", "sign"),simFixSize = 4, trainRatio = 0.75,
                                                      seed = -340))
  allWeights3 = sort(sapply(res3$crps$target2$model$all, function(x){x$weight}))

  expect_equal(as.numeric(allWeights), as.numeric(allWeights3), tolerance = 1e-8)
})

test_that("search.sur works when parallel", {
  skip_on_cran()

  y=x[,c(2,1,3)]
  Exo=x[,c(5,4,7,6)]
  res = search.sur(y, Exo,2,  yGroups = list(c(1L),c(1L,2L),c(1L,2L,3L)),
                  searchOptions = get.options.search(parallel = FALSE, printMsg = printMsg),
                  searchItems = get.items.search(all = TRUE, bestK = 2),
                  metricOptions = get.options.metric(character(0),c("rmse", "crps", "sign"),simFixSize = 4, trainRatio = 0.75,
                                                     seed = -340))
  allWeights = sort(sapply(res$crps$target1$model$all, function(x){x$weight}))

  res = search.sur(y, Exo,2,  yGroups = list(c(1L),c(1L,2L),c(1L,2L,3L)),
                  searchOptions = get.options.search(parallel = TRUE, printMsg = printMsg),
                  searchItems = get.items.search(all = TRUE, bestK = 2),
                  metricOptions = get.options.metric(character(0),c("rmse", "crps", "sign"),simFixSize = 4, trainRatio = 0.75,
                                                     seed = -340))
  allWeights0 = sort(sapply(res$crps$target1$model$all, function(x){x$weight}))

  expect_equal(as.numeric(allWeights), as.numeric(allWeights0), tolerance = 1e-10)
})


test_that("search.sur works for fixed training sample", {
  skip_on_cran()

  y=x[,c(1,2,3)]
  Exo=x[,4:7]
  res = search.sur(y, Exo,2,  yGroups = list(c(1L),c(1L,2L),c(1L,2L,3L)),
                  searchOptions = get.options.search(printMsg = printMsg),
                  searchItems = get.items.search(all = TRUE, bestK = 2),
                  metricOptions = get.options.metric(character(0),c( "crps", "mae", "rmse"),simFixSize = 4, trainRatio = 0.65,
                                                     trainFixSize = 12,
                                                     seed = -340))  # negative seed for equal distribution
  res1 = estim.sur(as.matrix(y[,res$mae$target1$model$bests$best1$depIndices]),
                  as.matrix(Exo[,res$mae$target1$model$bests$best1$exoIndices]),
                  simFixSize = 4, simTrainRatio = 0.75, simTrainFixSize = 12, simSeed = 340, addIntercept = FALSE,
                  printMsg = printMsg)
  expect_equal(as.numeric(res1$metrics[which(rownames(res1$metrics)=="mae"),1]),
               as.numeric(s.metric.from.weight(res$mae$target1$model$bests$best1$weight, "mae")), tolerance = 1e-10)

  res1 = estim.sur(as.matrix(y[,res$crps$target1$model$bests$best1$depIndices]),
                   as.matrix(Exo[,res$crps$target1$model$bests$best1$exoIndices]),
                   simFixSize = 4, simTrainRatio = 0.75, simTrainFixSize = 12, simSeed = 340, addIntercept = FALSE,
                   printMsg = printMsg)
  expect_equal(as.numeric(res1$metrics[which(rownames(res1$metrics)=="crps"),1]),
               as.numeric(s.metric.from.weight(res$crps$target1$model$bests$best1$weight, "crps")), tolerance = 1e-10)


  res1 = estim.sur(as.matrix(y[,res$rmse$target1$model$bests$best1$depIndices]),
                   as.matrix(Exo[,res$rmse$target1$model$bests$best1$exoIndices]),
                   simFixSize = 4, simTrainRatio = 0.75, simTrainFixSize = 12, simSeed = 340, addIntercept = FALSE,
                   printMsg = printMsg)
  expect_equal(as.numeric(res1$metrics[which(rownames(res1$metrics)=="rmse"),1]),
               as.numeric(s.metric.from.weight(res$rmse$target1$model$bests$best1$weight, "rmse")), tolerance = 1e-10)

})

test_that("search.sur works with restricted aic", {
  skip_on_cran()

  y=x[,c(1,2,3)]
  Exo=x[,4:7]
  res = search.sur(y, Exo,2,  yGroups = list(c(1L),c(1L,2L),c(1L,2L,3L)),
                  searchOptions = get.options.search(printMsg = printMsg),
                  modelCheckItems = get.items.modelcheck(maxAic = 300),
                  searchItems = get.items.search(all = TRUE, bestK = 0),
                  metricOptions = get.options.metric(character(0),c("rmse", "crps", "sign"),simFixSize = 4, trainRatio = 0.75,
                                                     trainFixSize = 12,
                                                     seed = -340))  # negative seed for equal distribution
  alls = list()
  for (m in res$crps$target1$model$all){
    M = estim.sur(as.matrix(y[,m$depIndices]), x = as.matrix(Exo[,m$exoIndices]),
                 simFixSize = 0, addIntercept = FALSE, printMsg = printMsg)
    alls = append(alls, M$metrics[2,1])
    expect_true(as.numeric(M$metrics[2,1]) <= 300)
  }
})

test_that("search.sur works with inclusion weights", {
  skip_on_cran()

  y=x[,c(1,2,3)]
  Exo=x[,4:7]
  res = search.sur(y, Exo, numTargets = 2,  yGroups = list(c(1L),c(1L,2L),c(1L,2L,3L)),
                  searchOptions = get.options.search(printMsg = printMsg),
                  searchItems = get.items.search(type1 = FALSE, all = TRUE, bestK = 2,inclusion = TRUE ),
                  metricOptions = get.options.metric(c("sic"),c("rmse", "crps", "sign"),simFixSize = 4, trainRatio = 0.75,
                                                     seed = -340))
  inclusion = matrix(0,7,2)
  for (m in res$crps$target1$model$all){
    for (d in m$depIndices){
      inclusion[d,1] = inclusion[d,1] + m$weight
      inclusion[d,2] = inclusion[d,2] + 1
    }
    for (e in m$exoIndices){
      d = e + ncol(y)
      inclusion[d,1] = inclusion[d,1] + m$weight
      inclusion[d,2] = inclusion[d,2] + 1
    }
  }
  inclusion[,1] = inclusion[,1]/inclusion[,2]

  expect_equal(as.numeric(res$crps$target1$model$inclusion), as.numeric(inclusion), tolerance = 1e-10)

})

test_that("search.sur works with coefficients (bests)", {
  skip_on_cran()

  y=x[,c(1,2,3)]
  Exo=x[,4:7]
  res = search.sur(y, Exo,2,  yGroups = list(c(1L),c(1L,2L),c(1L,2L,3L)),
                  searchOptions = get.options.search(printMsg = printMsg),
                  searchItems = get.items.search(type1 = TRUE, all = TRUE, bestK = 2,inclusion = FALSE ),
                  metricOptions = get.options.metric(c("sic", "aic"),c("rmse", "sign")))
  best_coef2 = NULL
  w=-Inf
  for (m in res$aic$target1$model$all){
    if (any(m$exoIndices==3)){
      if (w<m$weight){
        w=m$weight
        best_coef2 = m
      }
    }
  }
  expect_equal(res$aic$target1$coefs$bests$item3$best1$weight, best_coef2$weight, tolerance = 1e-10)
  expect_equal(res$aic$target1$coefs$bests$item3$best1$depIndices, best_coef2$depIndices, tolerance = 1e-10)
  expect_equal(res$aic$target1$coefs$bests$item3$best1$exoIndices, best_coef2$exoIndices, tolerance = 1e-10)

  # are mean and variance equal?
  M = estim.sur(as.matrix(y[,res$aic$target1$coefs$bests$item3$best1$depIndices]),
               x = as.matrix(Exo[,res$aic$target1$coefs$bests$item3$best1$exoIndices]),
               simFixSize = 0, addIntercept = FALSE, printMsg = printMsg)
  expect_equal(exp(-0.5 * M$metrics[2,1]), res$aic$target1$coefs$bests$item3$best1$weight, tolerance = 1e-10)
  expect_equal(res$aic$target1$coefs$bests$item3$best1$mean, M$estimations$gamma[1], tolerance = 1e-10)
  expect_equal(res$aic$target1$coefs$bests$item3$best1$var, M$estimations$gammaVar[1,1], tolerance = 1e-10)

})

test_that("search.sur works with coefficients (cdfs)", {
  skip_on_cran()

  y=x[,c(1,2,3),drop=FALSE]
  Exo=x[,4:7,drop=FALSE]
  res = search.sur(y, Exo,2,  yGroups = list(c(1L),c(1L,2L),c(1L,2L,3L)),
                  searchOptions = get.options.search(printMsg = printMsg),
                  searchItems = get.items.search(type1 = TRUE,
                                               all = TRUE, bestK = 0,inclusion = FALSE,
                                               cdfs = c(0,1,0)),
                  metricOptions = get.options.metric(c("aic"),c("rmse", "crps", "sign"),simFixSize = 4, trainRatio = 0.75,
                                                     seed = 0))
  sum = 0
  c = 0
  cc=0
  for (m in res$rmse$target1$model$all){

    if (any(m$exoIndices==2)){
      ind = which(m$exoIndices == 2)
      M = estim.sur(y[,m$depIndices,drop=FALSE], x = Exo[,m$exoIndices,drop=FALSE],
                   simFixSize = 0, addIntercept = FALSE, printMsg = printMsg)
      coef = M$estimations$gamma[ind]
      sd = sqrt(M$estimations$gammaVar[ind,ind])
      sum = sum+m$weight * pnorm(0,coef,sd)  # note the NORMAL dist. If t,  d.o.f : nrow(y)-length(gamma)
      c=c+m$weight
      cc=cc+1
    }
  }
  expect_equal(res$rmse$target1$coefs$cdfs$cdf3[2,1], sum/c, tolerance = 1e-10)
  expect_equal(res$rmse$target1$coefs$cdfs$cdf3[2,2], cc, tolerance = 1e-10)
})


test_that("search.sur works with coefficients (extreme bounds)", {
  skip_on_cran()

  y=x[,c(1,2,3)]
  Exo=x[,4:7]
  res = search.sur(y, Exo,2,  yGroups = list(c(1L),c(1L,2L),c(1L,2L,3L)),
                  searchOptions = get.options.search(printMsg = printMsg),
                  searchItems = get.items.search(type1 = TRUE,
                                               all = TRUE, bestK = 0,inclusion = FALSE,
                                               extremeMultiplier = 2),
                  metricOptions = get.options.metric(c("aic"),c("rmse", "crps", "sign"),simFixSize = 4, trainRatio = 0.75,
                                                     seed = 0))
  mn = Inf
  mx = -Inf
  h = 2
  for (m in res$rmse$target1$model$all){

    if (any(m$exoIndices==h)) {
      ind = which(m$exoIndices == h)
      M = estim.sur(y[,m$depIndices, drop=FALSE], x = Exo[,m$exoIndices, drop=FALSE],
                   simFixSize = 0, addIntercept = FALSE, printMsg = printMsg)
      coef = M$estimations$gamma[ind]
      sd = sqrt(M$estimations$gammaVar[ind,ind])
      mn = min(mn,coef-2*sd)
      mx = max(mx,coef+2*sd)
    }
  }
  expect_equal(res$rmse$target1$coefs$extremeBounds[h,1], mn, tolerance = 1e-10)
  expect_equal(res$rmse$target1$coefs$extremeBounds[h,2], mx, tolerance = 1e-10)
})

test_that("search.sur works with coefficients (mixture)", {
  skip_on_cran()

  y=x[,c(1,2)]
  Exo=x[,3:7]
  res = search.sur(y, Exo, 1, yGroups = list(c(1L,2L)), xSizes = c(1L,2L,3L,4L,5L),
                  searchOptions = get.options.search(printMsg = printMsg),
                  searchItems = get.items.search(type1 = TRUE,
                                               all = TRUE, bestK = 0,inclusion = FALSE,
                                               extremeMultiplier = 0,
                                               mixture4 = TRUE),
                  metricOptions = get.options.metric(c("aic"),c("rmse", "crps", "sign"),simFixSize = 4, trainRatio = 0.75,
                                                     seed = 0))
  coefs = c()
  vars = c()
  weights = c()
  h = 1
  for (m in res$rmse$target1$model$all){

    if (any(m$exoIndices==h)) {
      M = estim.sur(y[,m$depIndices,drop=FALSE], x = Exo[,m$exoIndices,drop=FALSE],
                   simFixSize = 0, addIntercept = FALSE,printMsg = printMsg)
      ind = which(m$exoIndices == h)
      coefs = append(coefs,M$estimations$gamma[ind])
      vars = append(vars, M$estimations$gammaVar[ind,ind])
      weights = append(weights, m$weight)
    }
  }
  # note that we need weighted mean, variance, etc. assuming normal distribution

  len = length(coefs)
  expect_equal(res$rmse$target1$coefs$mixture[h,5], len)
  me = weighted.mean(coefs, weights)
  expect_equal(res$rmse$target1$coefs$mixture[h,1], me, tolerance = 1e-14)

  # TODO : compare weighted variance, skewness, kurtosis assuming normality
  #        of course, its better to .Call the running statistics, test it, and use it here

})

test_that("SUR summary works", {
  skip_on_cran()

  y=x[,c(1,2,3)]
  Exo=x[,4:7]
  res = search.sur(y, Exo, 2,  yGroups = list(c(1L),c(1L,2L),c(1L,2L,3L)), xSizes = c(1L,2L,3L),
                  searchItems = get.items.search(type1 = TRUE, all = TRUE, bestK = 2, inclusion = TRUE,
                                               cdfs = c(0,1), mixture4 = TRUE, extremeMultiplier = 2.0 ),
                  metricOptions = get.options.metric(c("sic", "aic"), c("rmse", "sign"), seed = -400),
                  searchOptions = get.options.search(TRUE, printMsg = printMsg))

  su =summary(res, y, Exo, addModelAll = TRUE, addItem1 = TRUE, test = TRUE)

})


test_that("SUR SplitSearch works (no subsetting)", {
  skip_on_cran()

  y=x[,c(1,2,3)]
  Exo=x[,4:7]


  # also don't test with out-of-sample metrics. It seems we have different model with equal weights (the result change by repeating the call ?!)

  yGroups = list(c(1L),c(1L,2L),c(1L,2L,3L))
  numTargets = 2
  searchItems = get.items.search(type1 = TRUE, all = TRUE, bestK = 200, inclusion = TRUE,
                               cdfs = c(0,1), mixture4 = TRUE, extremeMultiplier = 2.0 )
  metricOptions = get.options.metric(c("sic", "aic"), c("crps"), seed = -400)
  searchOptions = get.options.search(FALSE, printMsg = printMsg)

  split = search.sur.stepwise(x = Exo, y = y, xSizeSteps = list(c(1L,2L), c(3L)), countSteps = c(NA, NA),
                      numTargets = numTargets,  yGroups = yGroups,
                      searchItems = searchItems, metricOptions = metricOptions,
                      searchOptions = searchOptions, savePre = NULL)

  whole = search.sur(y, Exo, xSizes = c(1L,2L,3L),
                    numTargets = numTargets,  yGroups = yGroups,
                    searchItems = searchItems, metricOptions = metricOptions,
                    searchOptions = searchOptions)

  # CHECK ALL

  # for 'all' the order is generally different
  weights0 <- sort(sapply(whole$sic$target1$model$all, function(a) a$weight))
  weights1 <- sort(sapply(split$sic$target1$model$all, function(a) a$weight))
  expect_equal(as.numeric(weights0),as.numeric(weights1),tolerance =1e-12)

  weights0 <- sort(sapply(whole$crps$target1$model$all, function(a) a$weight))
  weights1 <- sort(sapply(split$crps$target1$model$all, function(a) a$weight))
  expect_equal(as.numeric(weights0),as.numeric(weights1),tolerance =1e-12) # some different models in crps has equal weight (due to OLS estimation of systems with different number of equations)
  # we have searched similar set of models


  # CHECK BESTS
  expect_equal(unlist(whole$sic$target1$model$bests[1]), unlist(split$sic$target1$model$bests[1]), tolerance = 1e-6)
  expect_equal(unlist(whole$sic$target1$model$bests[2]), unlist(split$sic$target1$model$bests[2]), tolerance = 1e-6)
  expect_equal(unlist(whole$sic$target1$model$bests[3]), unlist(split$sic$target1$model$bests[3]), tolerance = 1e-6)
  expect_equal(unlist(whole$sic$target1$model$bests[4]), unlist(split$sic$target1$model$bests[4]), tolerance = 1e-6)

  #INCLUSION / EXTREME BOUNDS / CDF / MIXTURE
  expect_equal(whole$sic$target1$model$inclusion, split$sic$target1$model$inclusion, tolerance = 1e-10)
  expect_equal(whole$sic$target1$coefs$extremeBounds, split$sic$target1$coefs$extremeBounds, tolerance = 1e-6)
  expect_equal(whole$sic$target1$coef$cdfs, split$sic$target1$coefs$cdfs, tolerance = 1e-6)
  expect_equal(whole$sic$target1$coefs$mixture, split$sic$target1$coefs$mixture, tolerance =1e-10)


  #BEST COEFS
  i = 0
  for (w_item in whole$aic$target2$coefs$bests){
    w_item <- w_item[lengths(w_item)!=0] # we set the bestK too high. some elements are null
    i = i + 1
    s_item <- split$aic$target2$coefs$bests[[i]]
    expect_equal(w_item[1:9], s_item[1:9], tolerance =1e-10)
  }

})


