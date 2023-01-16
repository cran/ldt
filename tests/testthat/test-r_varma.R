
#TODO: update search tests by using 'summary' function
printMsg = FALSE
x <- as.data.frame(matrix(c(32.446,44.145,17.062,65.818,76.19,40.408,78.131,
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
            nrow =22, ncol=7))
colnames(x) = paste0("W", c(1:ncol(x)))

test_that("VAR estimation works", {
  res = VarmaEstim(x[,1:2], params = c(2,0,0,0,0,0), printMsg = printMsg)
  #resR = MTS::VAR(x[,1:2],2)
  rc1 = 47.01986294876104 # resR$coef[[1]]
  rsg = c(498.5024107703099, -56.4870364060901,
          -56.4870364060901, 754.8641908858656) # as.numeric(resR$Sigma)

  expect_equal(res$estimations$coefs[1,5], rc1, tolerance = 1e-8)
  expect_equal(as.numeric(res$estimations$sigma), rsg, tolerance = 1e-8)

  #change indexes
  res0 = VarmaEstim(x[,c(2,1)], params = c(2,0,0,0,0,0), printMsg = printMsg)
  expect_equal(res$estimations$coefs[1,1], res0$estimations$coefs[2,2], tolerance = 1e-13) # ??1e-16 fails
  expect_equal(res$estimations$sigma[1,1], res0$estimations$sigma[2,2], tolerance = 1e-14)
  expect_equal(res$measures[2,1], res0$measures[2,1], tolerance = 1e-16)
})

test_that("VAR forecast works", {
  res = VarmaEstim(x[,1:2], params = c(2,0,0,0,0,0), maxHorizon = 3, printMsg = printMsg)
  #resR = MTS::VAR(x[,1:2],2)
  #forR <- MTS::VARpred(resR, 3);
  prd = c(59.71207367716434, 27.85724814539860, 53.92058456057188,
          40.35663646224699, 49.31001542183052, 65.52994498334375) #as.numeric(t(forR$pred))

  prs = c(22.32716754920583, 27.47479191706219, 22.37005566097166,
          28.55614239805317, 22.59875116704332, 32.08105495664848) #as.numeric(t(forR$se.err))
  expect_equal(as.numeric(res$prediction$means[,3:5]), prd, tolerance = 1e-8)
  #variance
  expect_equal(as.numeric(sqrt(res$prediction$vars[,3:5])), prs, tolerance = 1e-8)
})

test_that("VAR forecast works with NA", {
  y = x[,1:2]
  y = rbind(c(NA,2),y, c(2,NA))
  res = VarmaEstim(y[,1:2], params = c(2,0,0,0,0,0), maxHorizon = 3, printMsg = printMsg)
  #resR = MTS::VAR(x[,1:2],2)
  #forR <- MTS::VARpred(resR, 3)

  ms1 = c(59.71207367716434, 27.85724814539860, 53.92058456057188,
          40.35663646224699, 49.31001542183052, 65.52994498334375) # as.numeric(t(forR$pred))
  vs1 = c(22.32716754920583, 27.47479191706219, 22.37005566097166,
        28.55614239805317, 22.59875116704332, 32.08105495664848) #as.numeric(t(forR$se.err))
  expect_equal(as.numeric(res$prediction$means[,3:5]), ms1, tolerance = 1e-8)
  #variance
  expect_equal(as.numeric(sqrt(res$prediction$vars[,3:5])), vs1, tolerance = 1e-8)
})

test_that("MA estimation works", {
  res = VarmaEstim(x[,1], params = c(0,0,2,0,0,0), addIntercept = TRUE, printMsg = printMsg)
  resR = arima(x[,1], c(0,0,2), method = "CSS", optim.method = "L-BFGS-B",
               transform.pars = FALSE, include.mean = TRUE)
  expect_equal(res$estimations$coefs[2:3], as.numeric(resR$coef[1:2]), tolerance = 1e-4)
  expect_equal(res$estimations$sigma[[1]], resR$sigma2, tolerance = 1e-8)
})

test_that("MA forecast works", {
  res = VarmaEstim(x[,1], params = c(0,0,2,0,0,0), addIntercept = TRUE, maxHorizon = 3, printMsg = printMsg)
  resR = arima(x[,1], c(0,0,2), method = "CSS", optim.method = "L-BFGS-B",
               transform.pars = FALSE, include.mean = TRUE)
  forR <- predict(resR, n.ahead = 3)
  expect_equal(as.numeric(forR$pred), as.numeric(res$prediction$means), tolerance = 1e-6)
  #variance
  expect_equal(as.numeric(forR$se), sqrt(as.numeric(res$prediction$vars)), tolerance = 1e-6)
})

test_that("ARMA forecast works", {
  res = VarmaEstim(x[,2], params = c(1,0,1,0,0,0), addIntercept = TRUE, maxHorizon = 3, printMsg = printMsg)
  resR = arima(x[,2], c(1,0,1), method = "CSS", optim.method = "L-BFGS-B",
               transform.pars = FALSE, include.mean = TRUE)
  forR <- predict(resR, n.ahead = 3)
  expect_equal(as.numeric(forR$pred), as.numeric(res$prediction$means[2:4]), tolerance = 1e-3)
  #variance
  expect_equal(as.numeric(forR$se), sqrt(as.numeric(res$prediction$vars[2:4])), tolerance = 1e-3)
})

test_that("ARIMA forecast works", {
  res = VarmaEstim(x[,5], params = c(1,1,1,0,0,0), addIntercept = FALSE, maxHorizon = 3, printMsg = printMsg)
  resR = arima(x[,5], c(1,1,1), method = "CSS", optim.method = "L-BFGS-B",
               transform.pars = TRUE, include.mean = TRUE)
  forR <- predict(resR, n.ahead = 3)
  expect_equal(as.numeric(forR$pred), as.numeric(res$prediction$means[3:5]), tolerance = 1e-3)
  #variance
  expect_equal(as.numeric(forR$se), sqrt(as.numeric(res$prediction$vars[3:5])), tolerance = 1e-3)
})

test_that("VAR forecast works with PCA for endogenous", {

  y = as.data.frame(cbind(as.matrix(x[,1:2]), prcomp(x[,3:ncol(x)], scale. = TRUE)$x)) # orthogonal
  pcaOp = GetPcaOptions()
  pcaOp$ignoreFirst = 2
  pcaOp$exactCount = 1

  res1 = VarmaEstim(y[,1:3], params = c(1,0,0,0,0,0), maxHorizon = 3, printMsg = printMsg)
  res2 = VarmaEstim(x, params = c(1,0,0,0,0,0), maxHorizon = 3, pcaOptionsY = pcaOp, printMsg = printMsg)

  expect_equal(res1$gamma, res2$gamma, tolerance = 1e-8)
  expect_equal(as.numeric(res1$prediction$means), as.numeric(res2$prediction$means), tolerance = 1e-8)
  expect_equal(as.numeric(res1$prediction$vars), as.numeric(res2$prediction$vars), tolerance = 1e-8)
})

test_that("VAR forecast works with PCA for exogenous", {
  p=prcomp(x[,3:4], scale. = TRUE)
  Z = as.matrix(p$x[,1])
  newZ = matrix(c(10,11,12, 13,14,15),3,2)
  colnames(newZ) = colnames(x[,3:4])
  newZp = predict(p,newdata = newZ)

  pcaOp = GetPcaOptions()
  pcaOp$ignoreFirst = 0
  pcaOp$exactCount = 1

  res1 = VarmaEstim(x[,1:2], params = c(1,0,0,0,0,0), x=Z, newX = matrix(newZp[,1],3,1), maxHorizon = 3, printMsg = printMsg)
  res2 = VarmaEstim(x[,1:2], params = c(1,0,0,0,0,0), x=x[,3:4], newX = newZ, maxHorizon = 3, pcaOptionsX = pcaOp, printMsg = printMsg)

  expect_equal(res1$gamma, res2$gamma, tolerance = 1e-8)
  expect_equal(res1$prediction$means, res2$prediction$means, tolerance = 1e-8)
  expect_equal(res1$prediction$vars, res2$prediction$vars, tolerance = 1e-8)
})

test_that("VAR simulation works", {
  Z = x[,1:2]
  res = VarmaEstim(Z, params = c(1,0,0,0,0,0), maxHorizon = 2, simFixSize = 2, printMsg = printMsg)

  T=nrow(Z)
  f1 = VarmaEstim(Z[1:(T-1),], params = c(1,0,0,0,0,0), maxHorizon = 1, printMsg = printMsg)
  e1=(abs(f1$prediction$means[,2] - Z[T,])/Z[T,])^2

  f2 = VarmaEstim(Z[1:(T-2),], params = c(1,0,0,0,0,0), maxHorizon = 2, printMsg = printMsg)
  e2=(abs(f2$prediction$means[,2] - Z[T-1,])/Z[T-1,])^2
  e3=(abs(f2$prediction$means[,3] - Z[T,])/Z[T,])^2

  expect_equal(as.numeric(sqrt((e1+e2+e3)/3)), as.numeric(res$measures[9,]), tolerance = 1e-14)

  # change indexes
  Z = x[,c(2,1)]
  res0 = VarmaEstim(Z, params = c(1,0,0,0,0,0), maxHorizon = 2, simFixSize = 2, printMsg = printMsg)
  expect_equal(res$measures[,1], res0$measures[,2], tolerance = 1e-13)

})

test_that("VARMA simulation works", {
  Z = x[,1:2]
  res = VarmaEstim(Z, params = c(1,0,1,0,0,0), maxHorizon = 3, simFixSize = 2, simUsePreviousEstim = FALSE, printMsg = printMsg)

  T=nrow(Z)
  f1 = VarmaEstim(Z[1:(T-1),], params = c(1,0,1,0,0,0), maxHorizon = 1, printMsg = printMsg)
  e1=(abs(f1$prediction$means[,2] - Z[T,])/Z[T,])^2

  f2 = VarmaEstim(Z[1:(T-2),],params =  c(1,0,1,0,0,0), maxHorizon = 2, printMsg = printMsg)
  e2=(abs(f2$prediction$means[,2] - Z[T-1,])/Z[T-1,])^2
  e3=(abs(f2$prediction$means[,3] - Z[T,])/Z[T,])^2

  expect_equal(as.numeric(sqrt((e1+e2+e3)/3)), as.numeric(res$measures[9,]), tolerance = 1e-14)

  # change indexes
  Z = x[,c(2,1)]
  res0 = VarmaEstim(Z, params = c(1,0,1,0,0,0), maxHorizon = 3, simFixSize = 2, simUsePreviousEstim = FALSE, printMsg = printMsg)
  expect_equal(res$measures[,2], res0$measures[,1], tolerance = 1e-5)

})

test_that("VARMA estimation works", {
  p1=matrix(c(0.2,-0.6,0.3,1.1),2,2)
  sig=matrix(c(4,0.8,0.8,1),2,2)
  th1=matrix(c(-0.5,0,0,-0.6),2,2)
  set.seed(340)
  #m1=MTS::VARMAsim(3000,arlags=c(1),malags=c(1),phi=p1,theta=th1,sigma=sig)
  #Z=m1$series
  #res = VarmaEstim(Z, params = c(1,0,1,0,0,0), printMsg = printMsg)
  #expect_equal(as.numeric(res$estimations$sigma), as.numeric(sig), tolerance = 1e-1)

})


test_that("ARMA search works for In-Sample", {
  skip_on_cran()

  Z=x[,1]
  res = VarmaSearch(Z, maxParams = c(2,2,2,0,0,0),
                    searchOptions = GetSearchOptions(printMsg = printMsg),
                    measureOptions = GetMeasureOptions(c("aic"),c()))
  res1 = VarmaEstim(Z, params = res$aic$target1$model$bests$best1$parameters, simFixSize = 0,
                    addIntercept = FALSE, printMsg = printMsg)
  expect_equal(exp(-0.5 * res1$measures[2,1]), res$aic$target1$model$bests$best1$weight, tolerance = 1e-10)
})

test_that("ARMA search works for In-Sample with exogenous", {
  skip_on_cran()

  Z=x[,1]
  Exo=x[,3:6]
  res = VarmaSearch(Z, Exo, maxParams = c(2,2,2,0,0,0), xGroups = list(c(2),c(2,3)),
                    searchOptions = GetSearchOptions(printMsg = printMsg),
                    measureOptions = GetMeasureOptions(c("aic"),c()),
                    modelCheckItems = GetModelCheckItems(prediction = FALSE))
  res1 = VarmaEstim(Z, x = Exo[,res$aic$target1$model$bests$best1$exoIndices],
                     params = res$aic$target1$model$bests$best1$parameters, addIntercept = FALSE, printMsg = printMsg)
  expect_equal(exp(-0.5 * res1$measures[2,1]), res$aic$target1$model$bests$best1$weight, tolerance = 1e-10)
})

test_that("ARMA search works for In-Sample with Interpolation", {
  skip_on_cran()

  Z=x[,1]
  Z[[2]]=(Z[[1]]+Z[[3]])/2
  res0 = VarmaSearch(Z, maxParams = c(2,2,2,0,0,0),  searchOptions = GetSearchOptions(printMsg = printMsg),
                     measureOptions = GetMeasureOptions(c("aic"),c()))
  Z[[2]] = NA
  res1 = VarmaSearch(Z, maxParams = c(2,2,2,0,0,0),  searchOptions = GetSearchOptions(printMsg = printMsg),
                     measureOptions = GetMeasureOptions(c("aic"),c()))

  expect_equal(res0$aic$target1$model$bests$best1$weight, res1$aic$target1$model$bests$best1$weight, tolerance = 1e-10)
})

test_that("VARMA search works for In-Sample with exogenous", {
  skip_on_cran()

  Endo = x[,1:3]
  Exo=x[,4:7]
  res = VarmaSearch(Endo, Exo, numTargets = 3, ySizes = c(1,2),
                    maxParams = c(2,2,2,0,0,0), xGroups = list(c(3,4)),
                    searchOptions = GetSearchOptions(printMsg = printMsg),
                    measureOptions = GetMeasureOptions(c("aic", "sic"),c()),
                    searchItems = GetSearchItems(all = TRUE),
                    modelCheckItems = GetModelCheckItems(prediction = FALSE))

  allWeights = sort(sapply(res$aic$target1$model$all, function(x){x$weight}))
  for (m in res$aic$target1$model$all){
    M = VarmaEstim(Endo[,m$depIndices], x = Exo[,m$exoIndices],
              params = m$parameters, simFixSize = 0, addIntercept = FALSE, printMsg = printMsg)
    expect_equal(exp(-0.5 * M$measures[2,1]), m$weight, tolerance = 1e-10)
  }


  res1 = VarmaEstim(Endo[,res$sic$target1$model$bests$best1$depIndices], x = Exo[,res$sic$target1$model$bests$best1$exoIndices],
                     params = res$sic$target1$model$bests$best1$parameters, simFixSize = 0, addIntercept = FALSE, printMsg = printMsg)
  expect_equal(exp(-0.5 * res1$measures[3,1]), res$sic$target1$model$bests$best1$weight, tolerance = 1e-10)
  # find best in all
  best=-Inf
  bestM=NULL
  for (m in res$aic$target1$model$all){
    if (m$weight > best){
      best = m$weight
      bestM = m
    }
  }
  expect_equal(bestM$weight, res$aic$target1$model$bests$best1$weight, tolerance = 1e-10)
 })


test_that("VARMA search works when changing Indexes NO exogenous", {
  skip_on_cran()

  Endo = x[,1:3]
  res = VarmaSearch(Endo, NULL, 2, ySizes = c(1,2),
                    maxParams = c(2,2,2,0,0,0),
                    searchOptions = GetSearchOptions(printMsg = printMsg),
                    measureOptions = GetMeasureOptions(c("aic", "sic"),c()),
                    searchItems = GetSearchItems(all = TRUE))
  allWeights = sort(sapply(res$aic$target1$model$all, function(x){x$weight}))

  Endo = x[,c(2,1,3)]
  res0 = VarmaSearch(Endo, NULL, 2, ySizes = c(1,2),
                     maxParams = c(2,2,2,0,0,0),  searchOptions = GetSearchOptions(printMsg = printMsg),
                     measureOptions = GetMeasureOptions(c("aic", "sic"),c()),
                     searchItems = GetSearchItems(all = TRUE))

  allWeights0 = sort(sapply(res0$aic$target2$model$all, function(x){x$weight}))
  expect_equal(as.numeric(allWeights), as.numeric(allWeights0), tolerance = 1e-8)
})

test_that("VARMA search works when changing Indexes WITH exogenous", {
  skip_on_cran()

  Endo = x[,1:2]
  Exo=x[,4:5]
  res = VarmaSearch(Endo, NULL, 2, ySizes = c(1,2),
                    maxParams = c(0,2,3,0,0,0),  searchOptions = GetSearchOptions(printMsg = printMsg),
                    measureOptions = GetMeasureOptions(c("aic", "sic"),c()),
                    searchItems = GetSearchItems(all = TRUE))
  allWeights = sort(sapply(res$aic$target1$model$all, function(x){x$weight}))

  Endo = x[,c(2,1)]
  Exo = x[,c(5,4)]
  res0 = VarmaSearch(Endo, NULL, 2, ySizes = c(1,2),
                     maxParams = c(0,2,3,0,0,0),  searchOptions = GetSearchOptions(printMsg = printMsg),
                     measureOptions = GetMeasureOptions(c("aic", "sic"),c()),
                     searchItems = GetSearchItems(all = TRUE))

  allWeights0 = sort(sapply(res0$aic$target2$model$all, function(x){x$weight}))
  expect_equal(as.numeric(allWeights), as.numeric(allWeights0), tolerance = 1e-4)  # ?????!!!!!

  # it seems that one element (the first one) is different
  # see: as.numeric(allWeights) - as.numeric(allWeights0)
  # Maybe the models with exogenous variables (and MA restrictions) are not identifiable ??!!!
  # or maybe it is just about the convergence of the optimization
})


test_that("V-ARMA search works for Out-Sample", {
  skip_on_cran()


  Endo = x[,1:3]
  Exo = x[,4:7]
  newX = x[c(1,2),4:7]
  res = VarmaSearch(Endo, Exo, 2,
                    ySizes = c(1,2), maxParams = c(2,1,2,0,0,0), xGroups = list(c(1,2)),
                    simUsePreviousEstim = FALSE, maxHorizon = 2,
                    searchOptions = GetSearchOptions(printMsg = printMsg),
                    measureOptions = GetMeasureOptions(c(), c("crps", "mae", "direction"),
                                                       horizons = c(1,2), simFixSize = 2),
                    searchItems = GetSearchItems(all = TRUE), newX = newX,
                    modelCheckItems = GetModelCheckItems(estimation = TRUE, prediction = TRUE,
                                               predictionBoundMultiplier = 200))

  for (m in res$crps$target1$model$all){
    M = VarmaEstim(Endo[,m$depIndices], x = Exo[,m$exoIndices], maxHorizon = 0, simHorizons = c(1,2),
              params =  m$parameters, simFixSize = 2, addIntercept = FALSE, simUsePreviousEstim = FALSE, printMsg = printMsg)
     expect_equal(1/m$weight, as.numeric(M$measures[10,1]), tolerance = 1e-10)
  }

  # replace indexes
  Endo0 = x[,c(2,1,3)]
  res0 = VarmaSearch(Endo0, Exo, 2,
                     ySizes = c(1,2), maxParams = c(2,2,2,0,0,0), xGroups = list(c(1,2)),
                     simUsePreviousEstim = FALSE, newX = newX, maxHorizon = 2,
                     searchOptions = GetSearchOptions(printMsg = printMsg),
                     measureOptions = GetMeasureOptions(c(), c("crps", "mae", "direction"),
                                                        horizons = c(1,2), simFixSize = 2),
                     searchItems = GetSearchItems(all = TRUE),
                     modelCheckItems = GetModelCheckItems(estimation = TRUE, prediction = TRUE,
                                                predictionBoundMultiplier = 200))
  expect_equal(res$crps$target1$model$bests$best1$weight, res0$crps$target2$model$bests$best1$weight, tolerance = 1e-7)

})


test_that("VARMA search works when parallel", {
  skip_on_cran()


  res = VarmaSearch(x[,1:3], NULL, 2,
                    ySizes = c(1,2), maxParams = c(2,2,2,0,0,0),
                    simUsePreviousEstim = FALSE,
                    measureOptions = GetMeasureOptions(c("aic"), c("crps", "mae", "direction"),
                                                       horizons = c(1,2), simFixSize = 2),
                    searchItems = GetSearchItems(all = TRUE),
                    modelCheckItems = GetModelCheckItems(estimation = FALSE, prediction = FALSE),
                    searchOptions = GetSearchOptions(parallel = FALSE, printMsg = printMsg))
  allWeights = sort(sapply(res$aic$target1$model$all, function(x){x$weight}))

  res0 = VarmaSearch(x[,1:3], NULL, 2,
                     ySizes = c(1,2), maxParams = c(2,2,2,0,0,0),
                     simUsePreviousEstim = FALSE,
                     measureOptions = GetMeasureOptions(c("aic"), c("crps", "mae", "direction"),
                                                        horizons = c(1,2), simFixSize = 2),
                     searchItems = GetSearchItems(all = TRUE),
                     modelCheckItems = GetModelCheckItems(estimation = FALSE, prediction = FALSE),
                     searchOptions = GetSearchOptions(parallel = TRUE, printMsg = printMsg))

  allWeights0 = sort(sapply(res0$aic$target1$model$all, function(x){x$weight}))
  expect_equal(as.numeric(allWeights), as.numeric(allWeights0), tolerance = 1e-16)
})

test_that("VARMA search works with restricted aic", {
  skip_on_cran()

  Endo = x[,1:3]
  Exo = x[,4:7]
  res = VarmaSearch(Endo, Exo, 2,
                    ySizes = c(1,2), maxParams = c(2,2,2,0,0,0), xGroups = list(c(1,2)),
                    simUsePreviousEstim = FALSE,
                    searchOptions = GetSearchOptions(printMsg = printMsg),
                    measureOptions = GetMeasureOptions(c(), c("crps", "mae", "direction"),
                                                       horizons = c(1,2), simFixSize = 2),
                    searchItems = GetSearchItems(all = TRUE),
                    modelCheckItems = GetModelCheckItems(estimation = FALSE, prediction = FALSE,
                                                    maxAic = 20))

  alls = list()
  for (m in res$crps$target1$model$all){
    M = VarmaEstim(Endo[,m$depIndices], x = Exo[,m$exoIndices], params = m$parameters,
            addIntercept = FALSE, printMsg = printMsg)
    alls = append(alls, M$measures[2,1])
    expect_true(as.numeric(M$measures[2,1]) <= 20)
  }
})

test_that("VARMA search works with inclusion weights", {
  skip_on_cran()

  Endo = x[,1:3]
  Exo = x[,4:7]
  res = VarmaSearch(Endo, Exo, 2,
                    ySizes = c(1,2), maxParams = c(2,2,2,0,0,0), xGroups = list(c(1,2), c(3)),
                    simUsePreviousEstim = FALSE,
                    searchOptions = GetSearchOptions(printMsg = printMsg),
                    measureOptions = GetMeasureOptions(c(), c("crps", "mae", "direction"),
                                                       horizons = c(1,2), simFixSize = 2),
                    searchItems = GetSearchItems(all = TRUE, inclusion = TRUE),
                    modelCheckItems = GetModelCheckItems(estimation = FALSE, prediction = FALSE))

  inclusion = matrix(0,7,2)
  for (m in res$crps$target1$model$all){
    for (d in m$depIndices){
      inclusion[d,1] = inclusion[d,1] + m$weight
      inclusion[d,2] = inclusion[d,2] + 1
    }
    for (e in m$exoIndices){
      d = e + ncol(Endo)
      inclusion[d,1] = inclusion[d,1] + m$weight
      inclusion[d,2] = inclusion[d,2] + 1
    }
  }
  inclusion[,1] = inclusion[,1]/inclusion[,2]

  expect_equal(as.numeric(res$crps$target1$model$inclusion), as.numeric(inclusion), tolerance = 1e-10)

})

test_that("VARMA search works with predictions (bests)", {
  skip_on_cran()

  Endo = x[,1:3]
  Exo = x[,4:7]
  newX = matrix(c(7,8,5,6,7,8,5,6,7,5,6,7),3,4)
  res = VarmaSearch(Endo, Exo, 2, maxHorizon = 3, newX = newX,
                    ySizes = c(1,2), maxParams = c(2,2,2,0,0,0), xGroups = list(c(1,2), c(3)),
                    simUsePreviousEstim = FALSE,
                    searchOptions = GetSearchOptions(printMsg = printMsg),
                    measureOptions = GetMeasureOptions(c("aic"), c("crps", "mae"),
                                                       horizons = c(1,2), simFixSize = 2),
                    searchItems = GetSearchItems(all = TRUE, type1 = TRUE),
                    modelCheckItems = GetModelCheckItems(estimation = FALSE, prediction = FALSE, predictionBoundMultiplier = 0))

  best_pred2 = NULL
  w=-Inf
  for (m in res$aic$target1$model$all){
    if (w<m$weight){
      w=m$weight
      best_pred2 = m
    }
  }
  expect_equal(res$aic$target1$predictions$bests$horizon1$best1$weight, best_pred2$weight, tolerance = 1e-10)
  expect_equal(res$aic$target1$predictions$bests$horizon1$best1$depIndices, best_pred2$depIndices, tolerance = 1e-10)
  expect_equal(res$aic$target1$predictions$bests$horizon1$best1$exoIndices, best_pred2$exoIndices, tolerance = 1e-10)

  # are mean and variance equal?
  M = VarmaEstim( y= as.matrix(Endo[,res$aic$target1$predictions$bests$horizon1$best1$depIndices]),
            params = res$aic$target1$predictions$bests$horizon1$best1$parameters,
          x = as.matrix(Exo[,res$aic$target1$predictions$bests$horizon1$best1$exoIndices]),
          newX = as.matrix(newX[,res$aic$target1$predictions$bests$horizon1$best1$exoIndices]), maxHorizon = 3,
          simFixSize = 0, addIntercept = FALSE, printMsg = printMsg)
  expect_equal(exp(-0.5 * M$measures[2,1]), res$aic$target1$predictions$bests$horizon1$best1$weight, tolerance = 1e-10)
  expect_equal(res$aic$target1$predictions$bests$horizon1$best1$mean, M$prediction$means[M$prediction$startIndex], tolerance = 1e-9)
  expect_equal(res$aic$target1$predictions$bests$horizon1$best1$var, M$prediction$vars[M$prediction$startIndex], tolerance = 1e-9)

})

test_that("VARMA search works with predictions (cdfs)", {
  skip_on_cran()

  Endo = x[,1:3]
  Exo = x[,4:7]
  newX = matrix(c(7,8,5,6,7,8,5,6,7,5,6,7),3,4)
  res = VarmaSearch(Endo, Exo, 2, maxHorizon = 3, newX = newX,
                    ySizes = c(2,3), maxParams = c(2,1,2,0,0,0), xGroups = list(c(1,2), c(3)),
                    simUsePreviousEstim = FALSE,
                    searchOptions = GetSearchOptions(printMsg = printMsg),
                    measureOptions = GetMeasureOptions(c("aic"), c("crps", "mae"),
                                                       horizons = c(1,2), simFixSize = 2),
                    searchItems = GetSearchItems(all = TRUE, type1 = TRUE, cdfs = c(0,1,0)),
                    modelCheckItems = GetModelCheckItems(estimation = FALSE, prediction = FALSE, predictionBoundMultiplier = 0))
  h=2
  v = 1
  sum = 0
  c = 0
  cc = 0
  for (m in res$aic$target1$model$all){

    M = VarmaEstim( y= as.matrix(Endo[,m$depIndices]),
               params = m$parameters,
               x = as.matrix(Exo[,m$exoIndices]),
               newX = as.matrix(newX[,m$exoIndices]), maxHorizon = 3,
               simFixSize = 0, addIntercept = FALSE, printMsg = printMsg)

    hh=h+M$prediction$startIndex - 1
    coef = M$prediction$means[v,hh]
    sd = sqrt(M$prediction$vars[v,hh])
    sum = sum+ m$weight* pnorm(0,coef,sd)  # note the NORMAL dist. If t,  d.o.f : nrow(y)-length(gamma)
    c=c+m$weight
    cc=cc+1
  }
  expect_equal(res$aic$target1$predictions$cdfs$cdf3[2,1], sum/c, tolerance = 1e-10)
  expect_equal(res$aic$target1$predictions$cdfs$cdf3[2,2], cc, tolerance = 1e-10)
})

test_that("VARMA search works with predictions (extreme bounds)", {
  skip_on_cran()

  Endo = x[,1:3]
  Exo = x[,4:7]
  newX = matrix(c(7,8,5,6,7,8,5,6,7,5,6,7),3,4)
  res = VarmaSearch(Endo, Exo, 2, maxHorizon = 3, newX = newX,
                    ySizes = c(2,3), maxParams = c(2,1,2,0,0,0), xGroups = list(c(1,2), c(3)),
                    simUsePreviousEstim = FALSE,
                    searchOptions = GetSearchOptions(printMsg = printMsg),
                    measureOptions = GetMeasureOptions(c("aic"), c("crps", "mae"),
                                                       horizons = c(1,2), simFixSize = 2),
                    searchItems = GetSearchItems(all = TRUE, type1 = TRUE, cdfs = c(0,1,0), extremeMultiplier = 2),
                    modelCheckItems = GetModelCheckItems(estimation = FALSE, prediction = FALSE, predictionBoundMultiplier = 0))
  mn = Inf
  mx = -Inf
  v = 1
  h = 2
  for (m in res$aic$target1$model$all){

    M = VarmaEstim( y= as.matrix(Endo[,m$depIndices]),
               params = m$parameters,
               x = as.matrix(Exo[,m$exoIndices]),
               newX = as.matrix(newX[,m$exoIndices]), maxHorizon = 3,
               simFixSize = 0, addIntercept = FALSE, printMsg = printMsg)

    hh=h+M$prediction$startIndex - 1
    coef = M$prediction$means[v,hh]
    sd = sqrt(M$prediction$vars[v,hh])
    mn = min(mn,coef-2*sd)
    mx = max(mx,coef+2*sd)
  }
  expect_equal(res$aic$target1$predictions$extremeBounds[h,1], mn, tolerance = 1e-10)
  expect_equal(res$aic$target1$predictions$extremeBounds[h,2], mx, tolerance = 1e-10)
})

test_that("VARMA search works with predictions (mixture)", {
  skip_on_cran()

  Endo = x[,1:3]
  Exo = x[,4:7]
  newX = matrix(c(7,8,5,6,7,8,5,6,7,5,6,7),3,4)
  res = VarmaSearch(Endo, Exo, 1, maxHorizon = 3, newX = newX,
                    ySizes = c(1), maxParams = c(1,1,1,0,0,0), xGroups = list(c(1),c(2)),
                    simUsePreviousEstim = FALSE,
                    searchOptions = GetSearchOptions(printMsg = printMsg),
                    measureOptions = GetMeasureOptions(c("aic"), c("crps", "mae"),
                                                       horizons = c(1,2), simFixSize = 2),
                    searchItems = GetSearchItems(all = TRUE, type1 = TRUE, cdfs = c(0,1,0), extremeMultiplier = 2,
                                                 mixture4 = TRUE),
                    modelCheckItems = GetModelCheckItems(estimation = FALSE, prediction = FALSE, predictionBoundMultiplier = 0))

  coefs = c()
  vars = c()
  weights = c()
  v = 1
  h = 1
  for (m in res$aic$target1$model$all){
    M = VarmaEstim( y= as.matrix(Endo[,m$depIndices]),
               params = m$parameters,
               x = as.matrix(Exo[,m$exoIndices]),
               newX = as.matrix(newX[,m$exoIndices]), maxHorizon = 3,
               simFixSize = 0, addIntercept = FALSE, printMsg = printMsg)

    hh=h+M$prediction$startIndex - 1
    coefs = append(coefs, M$prediction$means[v,hh])
    vars = append(vars, M$prediction$vars[v,hh])
    weights = append(weights, m$weight)
  }
  # note that we need weighted mean, variance, etc. assuming normal distribution

  len = length(coefs)
  expect_equal(res$aic$target1$predictions$mixture[h,5], len)
  me = weighted.mean(coefs, weights)
  expect_equal(res$aic$target1$predictions$mixture[h,1], me, tolerance = 1e-14)

  # TODO : compare weighted variance, skewness, kurtosis assuming normality
  #        of course, its better to .Call the running statistics, test it, and use it here

})



test_that("VARMA summary works", {
  skip_on_cran()

  Endo = x[,1:3]
  Exo = x[,4:7]
  newX = matrix(c(7,8,5,6,7,8,5,6,7,5,6,7),3,4)
  res = VarmaSearch(Endo, Exo, 2, newX = newX,
                    ySizes = c(2), maxParams = c(2,2,2,0,0,0), xGroups = list(c(1,2)),
                    maxHorizon = 3,
                    simUsePreviousEstim = FALSE,
                    searchOptions = GetSearchOptions(printMsg = printMsg),
                    measureOptions = GetMeasureOptions(c("aic", "sic"), c("crps", "rmse", "direction"),
                                                       horizons = c(1,2), simFixSize = 2),
                    searchItems = GetSearchItems(all = TRUE, type1 = TRUE),
                    modelCheckItems = GetModelCheckItems(estimation = FALSE, prediction = FALSE, predictionBoundMultiplier = 0))
  su =summary(res, Endo, Exo, addModelAll = TRUE, addItem1 = TRUE, newX = newX, test = TRUE)

})




test_that("SurEstim SplitSearch works (no subsetting)", {
  skip_on_cran()

  Endo = x[,1:3]
  Exo = x[,4:7]
  newX = matrix(c(7,8,5,6,7,8,5,6,7,5,6,7),3,4)


  # also don't test with out-of-sample measures. It seems we have different model with equal weights (the result change by repeating the call ?!)

  xGroups = list(c(1), c(2),c(1,2),c(3),c(1,2,3))
  numTargets = 2
  maxHorizon = 3;
  searchItems = GetSearchItems(type1 = TRUE, all = TRUE, bestK = 200, inclusion = TRUE,
                               cdfs = c(0,1), mixture4 = TRUE, extremeMultiplier = 2.0 )
  measureOptions = GetMeasureOptions(c("sic", "aic"), c("crps"), seed = -400)
  searchOptions = GetSearchOptions(FALSE, printMsg = FALSE)
  modelCheckItems = GetModelCheckItems(prediction = FALSE, predictionBoundMultiplier = 0)

  split = VarmaSearch_s(y = Endo, x = Exo, ySizes = list(c(1,2), c(3)), counts = c(NA, NA),
                      numTargets = numTargets,  xGroups = xGroups,
                      searchItems = searchItems, measureOptions = measureOptions,
                      searchOptions = searchOptions, modelCheckItems = modelCheckItems,
                      newX = newX, maxHorizon = maxHorizon, savePre = NULL, printMsg = printMsg)

  whole = VarmaSearch(y = Endo, x = Exo, ySizes = c(1,2,3),
                    numTargets = numTargets,  xGroups = xGroups,
                    searchItems = searchItems, measureOptions = measureOptions,
                    searchOptions = searchOptions,modelCheckItems = modelCheckItems,
                    newX = newX, maxHorizon = maxHorizon)

  # CHECK ALL

  # for 'all' the order is generally different
  weights0 <- sort(sapply(whole$sic$target1$model$all, function(a) a$weight))
  weights1 <- sort(sapply(split$sic$target1$model$all, function(a) a$weight))
  expect_equal(as.numeric(weights0),as.numeric(weights1), tolerance =  1e-12)

  weights0 <- sort(sapply(whole$crps$target1$model$all, function(a) a$weight))
  weights1 <- sort(sapply(split$crps$target1$model$all, function(a) a$weight))
  expect_equal(as.numeric(weights0),as.numeric(weights1),tolerance = 1e-12) # some different models in crps has equal weight (due to OLS estimation of systems with different number of equations)
  # we have searched similar set of models


  # CHECK BESTS
  expect_equal(unlist(whole$sic$target1$model$bests[1]), unlist(split$sic$target1$model$bests[1]), tolerance = 1e-6)
  expect_equal(unlist(whole$sic$target1$model$bests[2]), unlist(split$sic$target1$model$bests[2]), tolerance = 1e-6)
  expect_equal(unlist(whole$sic$target1$model$bests[3]), unlist(split$sic$target1$model$bests[3]), tolerance = 1e-6)
  expect_equal(unlist(whole$sic$target1$model$bests[4]), unlist(split$sic$target1$model$bests[4]), tolerance = 1e-6)

  #INCLUSION / EXTREME BOUNDS / CDF / MIXTURE
  expect_equal(whole$sic$target1$model$inclusion, split$sic$target1$model$inclusion, tolerance = 1e-10)
  expect_equal(whole$sic$target1$predictions$extremeBounds, split$sic$target1$predictions$extremeBounds, tolerance = 1e-6)
  expect_equal(whole$sic$target1$predictions$cdfs, split$sic$target1$predictions$cdfs, tolerance = 1e-6)
  expect_equal(whole$sic$target1$predictions$mixture, split$sic$target1$predictions$mixture, tolerance =1e-10)


  #BEST COEFS
  i = 0
  for (w_item in whole$aic$target2$predictions$bests){
    w_item <- w_item[lengths(w_item)!=0] # we set the bestK too high. some elements are null
    i = i + 1
    s_item <- split$aic$target2$predictions$bests[[i]]
    expect_equal(w_item[1:9], s_item[1:9], tolerance =1e-10)
  }

})
