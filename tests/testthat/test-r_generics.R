
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
colnames(x) <- paste0("V", c(1:ncol(x)))

RES = SurSearch(x[,c(1,2)], x[,3:7], 2, yGroups = list(as.integer(c(1,2))), xSizes = as.integer(c(1,2,3,4,5)),
                searchOptions = GetSearchOptions(printMsg = FALSE),
                searchItems = GetSearchItems(type1 = TRUE, cdfs = c(0,1),
                                             all = TRUE, bestK = 3,inclusion = TRUE,
                                             extremeMultiplier = 2,
                                             mixture4 = TRUE),
                measureOptions = GetMeasureOptions(c("aic"),c("rmse", "crps", "sign"),simFixSize = 4, trainRatio = 0.75,
                                                   seed = 0))

test_that("to.data.frame works", {

  #best
  a = to.data.frame(RES, types = "bestweights")
  expect_equal(a$V1.best2[[2]],RES$rmse$target1$model$bests$best2$weight)
  expect_equal(a$V2.best3[[1]],RES$aic$target2$model$bests$best3$weight)

  a = to.data.frame(RES, types = "bestwei", rowContent = "target")
  expect_equal(a$rmse.best1[[2]],RES$rmse$target2$model$bests$best1$weight)
  expect_equal(a$aic.best3[[1]],RES$aic$target1$model$bests$best3$weight)

  a = to.data.frame(RES, types = "bestwei", rowContent = "item")
  expect_equal(a$rmse.V2[[2]],RES$rmse$target2$model$bests$best2$weight)
  expect_equal(a$crps.V1[[1]],RES$crps$target1$model$bests$best1$weight)

  #all
  a = to.data.frame(RES, types = c("allw", "bestwe"))
  expect_equal(a$bw.V1.best2[[2]],RES$rmse$target1$model$bests$best2$weight)
  expect_equal(a$bw.V2.best3[[1]],RES$aic$target2$model$bests$best3$weight)
  expect_equal(a$aw.V1.model2[[2]],RES$rmse$target1$model$all$model2$weight)
  expect_equal(a$aw.V2.model6[[3]],RES$crps$target2$model$all$model6$weight)

  a = to.data.frame(RES, types = c("allw", "bestwe"), rowContent = "target")
  expect_equal(a$aw.rmse.model1[[2]],RES$rmse$target2$model$all$model1$weight)
  expect_equal(a$aw.aic.model10[[1]],RES$aic$target1$model$all$model10$weight)

  a = to.data.frame(RES, types = c("allw"), rowContent = "item")
  expect_equal(a$crps.V1[[20]],RES$crps$target1$model$all$model20$weight)
  expect_equal(a$rmse.V2[[10]],RES$rmse$target2$model$all$model10$weight)


  # type1 bests
  a = to.data.frame(RES, types = "type1bests")
  expect_equal(a$V2.mean.V4.best1[[1]],RES$aic$target2$coefs$bests$item2$best1$mean)
  expect_equal(a$V1.var.V7.best3[[3]],RES$crps$target1$coefs$bests$item5$best3$var)

  a = to.data.frame(RES, types = "type1bests", rowContent = "target")
  expect_equal(a$aic.weight.V3.best1[[2]],RES$aic$target2$coefs$bests$item1$best1$weight)
  expect_equal(a$rmse.var.V4.best2[[1]],RES$rmse$target1$coefs$bests$item2$best2$var)

  a = to.data.frame(RES, types = "type1bests", rowContent = "item")
  expect_equal(a$V1.mean.V3.aic[[2]],RES$aic$target1$coefs$bests$item1$best2$mean)
  expect_equal(a$V2.var.V5.aic[[1]],RES$aic$target2$coefs$bests$item3$best1$var)

  a = to.data.frame(RES, types = "type1bests", rowContent = "row")
  expect_equal(a$V1.weight.aic.best3[[2]],RES$aic$target1$coefs$bests$item2$best3$weight)
  expect_equal(a$V1.var.sign.best3[[1]],RES$sign$target1$coefs$bests$item1$best3$var)

  a = to.data.frame(RES, types = "type1bests", rowContent = "column")
  expect_equal(a$V1.aic.V3.best1[[2]],RES$aic$target1$coefs$bests$item1$best1$mean)
  expect_equal(a$V2.crps.V4.best2[[3]],RES$crps$target2$coefs$bests$item2$best2$var)

  # inclusion
  a = to.data.frame(RES, types = c("inclu"))
  expect_equal(a$V1.Mean.V3[[2]],RES$rmse$target1$model$inclusion[[3,1]])
  expect_equal(a$V2.Count.V7[[2]],RES$rmse$target2$model$inclusion[[7,2]])

  a = to.data.frame(RES, types = c("inclu"), rowContent = "targ")
  expect_equal(a$aic.Count.V1[[2]],RES$aic$target2$model$inclusion[[1,2]])
  expect_equal(a$crps.Mean.V4[[1]],RES$crps$target1$model$inclusion[[4,1]])

  a = to.data.frame(RES, types = c("inclu"), rowContent = "row")
  expect_equal(a$V2.Mean.aic[[1]],RES$aic$target2$model$inclusion[[1,1]])
  expect_equal(a$V2.Mean.crps[[4]],RES$crps$target2$model$inclusion[[4,1]])

  a = to.data.frame(RES, types = c("inclu"), rowContent = "column")
  expect_equal(a$V1.rmse.V1[[1]],RES$rmse$target1$model$inclusion[[1,1]])
  expect_equal(a$V1.rmse.V1[[2]],RES$rmse$target1$model$inclusion[[1,2]])

  # cdfs
  a = to.data.frame(RES, types = c("cdf"), cdfIndex = 2)
  expect_equal(a$V1.Mean.V3[[2]],RES$rmse$target1$coefs$cdfs$cdf2[[1,1]])
  expect_equal(a$V2.SumWeights.V6[[2]],RES$rmse$target2$coefs$cdfs$cdf2[[4,3]])

  # mixture
  #a = to.data.frame(RES, types = c("mixture"))

})




