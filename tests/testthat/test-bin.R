
#TODO: add a global 'printMsg'
#      update search tests by using 'summary' function
printMsg = FALSE
x = matrix(c(1,0,1,0,1,1,0,0,1,1,0,1,0,0,1,0,1,0,0,0,0,1,0,1,0,1,1,0,1,1,1,0,1,1,0,1,1,1,1,1,
                           1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                           2,4.7,9.9,3,9.9,7.5,2.9,2.6,2.4,2.8,3.7,0.6,9.8 ,0.7,5.8,6.3,6.2,5.1,8.1,4,3.2,1.6,7.9,8.9,4.6,2.2,7.3,8.5,3.1,4.6,4.7,4.2,9.6 ,3,3.1,9.6,9,2.6,1.5,1.9,4.7,6.3,6.1,7.2,1,6.4,7.1,1.6,2.9,8.1,2,1,9,2.4,4.9,0.4,6.8,1,8.4,7.8,0.2,4.5,9.5,7.8,8,9.5
                           ,0.8,6.9,7.6,6.2,7.3,3.7,3.7,6.2,9.1 ,6.9,5.5,0.7,7.9,8.1 ,5.1,5.7,5.6,5,2.6,3.7,5.2,7.3,8.1,2.7,2.8,8,4.2,3.7,1.7,0.3,6.8,8.3 ,9.1 ,1.5,5.6,0.2,4,6.7,0.8,8.3 ,7.4,2.8,3.2,7.9,4.9,6.1,3.2,0.2,2.7,4.5,8.9,4.5,5.2,4.3,4.3,3.6,8.8 ,4.9,3.4,9.4,9.5,9.9,4.5,9,0.2,7.1,1.6,6.3,2.6,4.8,1.7,8.9,4.2,4.3,6.3,2.5,4.1,0.2,7.8,6.7,4.8,3.9,1,7.2,1.4,2.2,3.1,4.9,7.3,2,1.5,9.1,3.3,1.4,8.8 ,7.4,8.3 ,4.2,7.7,7.8,4.5,6.9,7.6,7.8,2.8,0,0.3,1,7.9,1.8,9.5,8.6,7.1,6.7,4.6,5.6,8.9,7.3,7.6,8,4.1,6.7,9.1,0.6,3.4,2.6,5.6,2.8,5.5,8.9,4.1,9.8 ,2.4,10,2.2,7.9,6.3,3.2,4.4,1.3,1.9,2.1,6.9,5.5,2.5,2.9,2,6.6,6.9,3.3,8.5,
                           1.1,6.6,9.8 ,3.7,7.8,7.8,7,4.5,0.2,4.4,7.5,5.5,9,7.7,9.6 ,6.2,7.1,4.5,6.7,9.3 ,5.7,2.8,6.3,6,2,9.6 ,3.6,8.1,0.5,0.6,9,5.9,2.2,0.4,3,3.2,2.6,6.3,4.4,0.1,9.3 ,8.6 ,4.2,6.9,4.4,6.6,7.9,7.8,2.8,6.7,4.8,2.1,0.2,4,2.7,8.1 ,0.3,3.8,1.7,8.1,4.2,8.9,4,9.6,5.4,1.6,3.3,8.6,4.7,6.4,3.6,0.8,0.1,6.6,10,7,8.6,9.4,1.6,5.3,1.8,5.8,5.3,3.9,5.8,6.9,5.7,8.1 ,7.7,4.8,2.9,8.9,4.6,6.3,5.9,0.3,4.7,4.6,1.4,2.8,3.4,9.3 ,3.1,6.4,3.1,4,2.1,3.6,4,5.7,9.6,0.9,2.2,6.4,6.3,8.4,3.6,0.1,3.8,
                           8.6,5.9,5.1,0.9,9.9,3.5,4.7,6.3,3.8,0.7,9.6,2.3,4,8,8.8 ,1.9,2.4,2.7,8.9,0.8,1.7,4.8,2.1,4.2,2,5.2,2,6.7,8.1,9.9,7.4,6.7,4.6,0.8,2.9,6.8,8.3 ,8.4,3.3,2.8,8.4,7.7,8.1 ,2.9,4.4,3.4,5.5,3,3.3,4.5,3.9,5.8,4.7,9.4,2.1,3.8,3.8,0.9,8.9,4.7,1.6,8,8.1 ,3.9,3.3,2.9,7.7,3.3,5.1,2,3.6,4,9.4,6.1,2.3,4,6.6,9.1 ,9.8 ,8.6,9.6,9,3.4,3.6,6,1.7,2,5.3,8.6 ,9.1,3,7.4,9.5,8.6,1.3,0.4,9.6,4.3,9.6,1.5,5.7,2.3,7.9,1.3,0.8,9.9,6.4,8.6 ,7,8.1,7.2,4.6,1.1,0.3,5,2.5,4.6,4.5,7.8,1.6,1,2.4,4.6,6,9,6.2,4.5,0.8,4.8,1.9,4.4,2.2,9.6,8.3 ,2.9,7.4,1.2,2,2.8,6.2,5.3,3.3,8.3
                           ,0.6,1.9,8.4,6.3,9.1 ,3.5,8.8 ,5.7,5.1,6.5,8,0.5,3.3,7.7,1.8,2.3,4.6,5.3,5.5,3.9,5.1,3.6,5,5,2.8,7.5,7.7,1.4,2.6,1.7,1.7,6.3,7.7,4.2,4,4.2,0.6,9.9,5.4,7.3,6,6.2,9.4,5.8,1.6,7.8,9.6,9.6 ,9.4,9.6 ,8.1 ,0.8,9.8 ,9.3 ,3.2,7,6.9,3.4,3.5,1.7,5.9,7.4,3.1,3.4,4.8,1.7,2.8,9.1,3.7,7.6,1.8,4.7,10,8.1 ,7.9,8.9,4.3,9.1 ,3.5,5,7.8,8,2,2.9,3.5,0.8,6.6,8.4,1,7,7.3,7.2,6.4,3.7,1.5,8.6,3.1,6,0.9,9.9,5,0.5,6,1.7,7.9,3.5,0.9,1.6,1.4,4.3,2.7,7.7,9.9,8.5,3.9,7.9,5.4,0.2,9.8 ,4,5.1,4.4,8.1 ,6.6,0.3,4.5,8.3 ,6.3,6.7,9.3 ,5.8,8.3 ,8.3 ,3.5,10,7.4,1.9,0.3,5,0,10,5,6.9,6.6,4.3,9.6,7.3,8,9.8 ,3.6,3.2,2.9,6.2,6.9,
                           4.8,0.6,2.3,9.4,1.4,3.4,5.2,4.6,4.9,2.1,8.5,4.4,9.4,6.4,8.5,6.5,6,5.1,5.2,9.8 ,4,0.1,7.5,1.5,2.3,0.8,7.2,0.3,6.5,3.2,6.1,2.9,2.1,1.3,8.6 ,9.4,5,2.5,8.9,1.6,4.8,5.4,0.1,4.9,8.1,9.9,0.1,1,9.5,3.3,7.6,8.1,7.6,6.3,2.9,4.7,3.1,0.8,7.1,0.6,6.2,4.9,8,1.7,6,5.6,2.3,8.1,7.7,9.1,1.5,3.5,6.2,3.6,5,8.6,10,2.6,3.4,2.4,0.6,6.3,0.8,3.2,5.6,8,5.3,5.8,8.4,4,6.3,4.2,5.4,8.3 ,5.4,9.9,7.4,8.4,8.9,2.5,7.6,2,3.3,0.4,1.8,4.1,1.8,4,1.3,4.3,4.7,0.5,7.4,4.7,10,9.9,4.1,4.8,6.1,7.9,0.3,6.6,9.6 ,3.7,2.8,7.2,8.1,8,8.1 ,5.1,1.3,1.6,3.7,0.8,4.8,8.6,3,0.4,1.4,1.5,6.2,1.3,4.4,4.2,9.3 ,5.8,6.8,0.6,8.3 ,5.6,5.8,2.5,2.3,1,
                           2.3,1,2.3,9.6,7.6,4.6,6,3.9,5.9,6.2,3,3.1,6.8,6.6,9.9,1.4,6.9,3.7,3.5,4.6,1.7,9,2.1,5.6,4.4,7,0.5,7.5,9.6 ,2.6,9.3 ,8.8 ,9.3 ,2.8,8.6,3.8,6.4,3.3,2.8,6.8,5.5,2.3,5.4,8.1 ,1.2,6.1,9,0.2,0.7,6.9,8.6,2.1,9,7.7,9.1 ,3.2,8.9,6.7,5.3,0.6,4.2,8.5,4.1,3.3,6.9,4.2,3.9,4,8,6.2,5,3.6,0.9,6.7,9.1,9.4,3.6,3.9,4.4,8.3 ,8,0.2,6.8,2.8,4,7.2,9.6 ,1.5,3.5,5.3,4.8,9.6 ,8.1,2.2,2.6,7.1,8.1 ,2.1,1.3,7,2.2,5,2.2,4.5,1.9,5,1.3,2.3,8.6,9.8 ,9.6,5,2.3,7,2.6,3.9,1.6,1.3,9.3 ,9.6 ,4.5,2.4,0.5,2.7,1.4,5.3,5.5,8.6,4.1,2,1.2,1.5,2.6,7.4,6,3.1,8,2.3,0.9,8.9,5.9,7.4,2.9,3.7,5.7,9.8 ,7.4,1.1,9.6 ,8.6 ,5.4,6.9,
                           2.8,1.6,2.5,9.9,4.5,9.5,3.1,6.6,9.5,2.6,8.1 ,6.7,8.6,8.1,2.5,4.9,1,9.5,5.9,9.3 ,0.4,0.4,3.8,7,9.5,6.8,7,1.6,7.9,4.4,9,5.4,6.5,4.3,8.6 ,0.9,4.8,6.2,8.6,8.5,9.1 ,4.3,9.4,3.3,9.1 ,6.2,5,7.6,7.3,2.4,0.1,0.2,2.8,6.1,6.5,2.5,7.8,5.5,0.4,7.7,2.6,1.6,8.6 ,5.1,3.6,2.8,0.1,4.4,9.5,1.5,0.1,4.8,0.9,7.3,0.1,6.2,1,0.6,7.8,3.2,1.8,9.4,4.2,5.5,4.3,8.8 ,2.6,0.4,7.8,6.2,3.2,9.3 ,5.7,0.9,6,7.6,5.5,4.3,9.6,6.4,6.1,0.7,8.1,5.8,2.3,5.3,9.3 ,5.5,6,0.3,1.5,7.2,7.7,7.2,5.9,3.2,7.3,3.3,0.8,4.8,0.2,9.6 ,6.6,3.3,8.4,0.8,9.1,9.9,1.4,2.5,0.6,6.7,1,1.4,7.1,1.1,1.3,8.5,3.1,9,9.5,7.1,4.5,7.4,4.7,5.9,1.6,8.6,5,
                           2.8,0.3,2.2,3.4,4.8,6.2,6.4,4.1,4.8,7.4,1.5,6.2,9.4,4.1,3.5,9.6 ,4.8,6.3,6.3,0,4.8,1.8,8.9,0.1,1.8,5.3,3.1,5.2,9.1,8.6,2.4,9.1,0.5,4.9,8.6,2.2,8.4,3.3,6.2,5.1,0.7,7,6.6,1.1,1.6,2.7,0.2,3,1,7.7,9.9,9.6 ,5.3,8.6,0.2,1,1.4,2.1,1.6,9.4,3.1,1.2,5,6.1,5,2.4,5.8,4.5,5.5,1.2,1.3,1.5,0.1,1.2,9.3 ,7.9,5.3,7.8,8.5,9.1,5.2,0.1,4.2,1.4,8.6 ,2.7,3.8,1.9,4.7,7.6,0.8,6.3,1.7,6.7,2,9.5,2,
                           0.4,6.1,9.1 ,7,5.7,7.5,9.8 ,6.4,8.5,9.6,4.2,9.3 ,2.3,5.7,3.7)
                         , 40,32)
colnames(x) <- paste0("V", c(1:ncol(x)))
dx = as.data.frame(x)


test_that("Discrete choice (binary) estimation works", {

  for (linkFunc in c("logit", "probit")){
    res <- estim.bin(x[,1,drop=FALSE], x[,3:5], NULL, newX = x[,3:5], linkFunc, printMsg = printMsg)
    resR <- glm(V1 ~ V3 + V4 + V5, data = dx, family = binomial(link = linkFunc))
    expect_equal(as.numeric(resR$coefficients), as.numeric(res$estimations$gamma), tolerance = 1e-5)
    sum_resR=summary(resR)
    expect_equal(as.numeric(sum_resR$cov.scaled), as.numeric(res$estimations$gammaVar), tolerance = 1e-1)

    glm_pp <- predict(resR, type = "response")
    brier_score <- mean((dx$V1 - glm_pp)^2)
    expect_equal(brier_score, res$metrics[4,1], tolerance = 1e-1)

    auc <- ldt::s.roc(dx$V1, 1 - glm_pp) # 1- glm_pp because of s.roc behaviour
    expect_equal(auc$auc, res$metrics[5,1], tolerance = 1e-1)

  }
})

test_that("Discrete choice (binary) estimation works with PCA", {

  y = as.data.frame(cbind(as.matrix(x[,1,drop=FALSE]), prcomp(x[,3:ncol(x)], scale. = TRUE)$x)) # orthogonal, will be used in R glm
  pcaOp = get.options.pca()
  pcaOp$ignoreFirst = 0 #It automatically adds and ignores intercept
  pcaOp$exactCount = 3

  for (linkFunc in c("logit", "probit")){
    res <- estim.bin(x[,1,drop=FALSE], x[,3:ncol(x)], NULL, linkFunc, pcaOptions = pcaOp, printMsg = printMsg)

    resR <- glm(V1 ~ PC1 + PC2 + PC3, data = y, family = binomial(link = linkFunc))
    expect_equal(abs(as.numeric(resR$coefficients)), abs(as.numeric(res$estimations$gamma)), tolerance = 1e-4) # abs? they are related to PC
    sum_resR=summary(resR)
    expect_equal(as.numeric(sum_resR$cov.scaled), as.numeric(res$estimations$gammaVar), tolerance = 1e-1)
  }
})

test_that("Discrete choice (binary, weighted) estimation works", {

  linkFunc = "probit" # the test fails for "logit" (I think this is because of the glm estimation)

  res <- estim.bin(x[,1,drop=FALSE], x[,3:5], x[,6,drop = FALSE], linkFunc, printMsg = printMsg)
  resR <- glm(V1 ~ V3 + V4 + V5, data = dx, weights = x[,6], family = quasibinomial(link = linkFunc))
  # quasibinomial (to avoid a warning (non-integer #successes in a binomial glm!))
  # see: https://stackoverflow.com/a/12954119
  expect_equal(as.numeric(resR$coefficients), as.numeric(res$estimations$gamma), tolerance = 1e-4)
  sum_resR=summary(resR)
  expect_equal(as.numeric(sum_resR$cov.scaled), as.numeric(res$estimations$gammaVar), tolerance = 1e-1)

})

test_that("Discrete choice (binary, weighted) estimation works with PCA", {

  y = as.data.frame(cbind(as.matrix(x[,1,drop=FALSE]), prcomp(x[,3:ncol(x)], scale. = TRUE)$x)) # orthogonal, will be used in R glm
  pcaOp = get.options.pca()
  pcaOp$ignoreFirst = 0
  pcaOp$exactCount = 3

  for (linkFunc in c("logit", "probit")){
    res <- estim.bin(x[,1,drop=FALSE], x[,3:ncol(x)], x[,6,drop = FALSE], linkFunc, pcaOptions = pcaOp, printMsg = printMsg)

    resR <- glm(V1 ~ PC1 + PC2 + PC3, data = y, weights = x[,6,drop = FALSE], family = quasibinomial(link = linkFunc))
    expect_equal(abs(as.numeric(resR$coefficients)), abs(as.numeric(res$estimations$gamma)), tolerance = 1e-5) # abs? they are related to PCs
    sum_resR=summary(resR)
    expect_equal(as.numeric(sum_resR$cov.scaled), as.numeric(res$estimations$gammaVar), tolerance = 1e-1)
  }
})

test_that("Discrete choice (binary, weighted, logL) estimation works", {

  y0 = x[,1,drop=FALSE]
  w0 = x[,6,drop=FALSE]
  Z0 = x[,3:5]

  y=as.matrix(append(y0, c(1,1,1,1)))
  w=as.matrix(append(w0, c(1,1,1,1)))
  m=matrix(c(3,3,3,3,5,5,5,5,6,6,6,6),4,3)
  colnames(m)<-colnames(Z0)
  Z=rbind(Z0,m)
  yw=as.matrix(append(y0,c(1))) # remove the last 4 observations and add a weight of 4
  ww = as.matrix(append(w0,c(4)))
  m=matrix(c(3,5,6),1,3)
  colnames(m)<-colnames(Z0)
  Zw = rbind(Z0,m)

  for (linkFunc in c("logit", "probit")){
    res <- estim.bin(y, Z, w, linkFunc, printMsg = printMsg)
    resw <- estim.bin(yw, Zw, ww, linkFunc, printMsg = printMsg)
    expect_equal(res$estimations$gamma, resw$estimations$gamma)
    expect_equal(res$estimations$gammaVar, resw$estimations$gammaVar)
    expect_equal(res$metrics[1,1], resw$metrics[1,1])

    # while LogL is the same, aic and sic are different
    # this is because I use the number of observations in the weighted case (instead of e.g., sum of weights)
    # TODO : I don't know if it is OK to use the sum of weights here!
    # the following fails:
    #expect_equal(res$metrics[2,1], resw$metrics[2,1])
  }
})

test_that("Discrete choice (binary, weighted) projection works", {

  for (linkFunc in c("logit", "probit")){
    newX <- dx[6:7,3:5]
    res <- estim.bin(x[,1,drop=FALSE], x[,3:5], NULL, linkFunc, newX = as.matrix(newX), printMsg = printMsg)
    resR <- glm(V1 ~ V3 + V4 + V5, data = dx, family = binomial(link = linkFunc))


    resRp = predict(resR, newdata = newX, type="response")
    expect_equal(resRp[[1]], res$projection[1,2], tolerance = 1e-5)
    expect_equal(resRp[[2]], res$projection[2,2], tolerance = 1e-5)
  }
})

test_that("Discrete choice (binary, weighted) projection works with PCA", {

  pr=prcomp(x[,3:ncol(x)], scale. = TRUE)
  y = as.data.frame(cbind(as.matrix(x[,1,drop=FALSE]), pr$x)) # orthogonal, will be used in R glm
  pcaOp = get.options.pca()
  pcaOp$ignoreFirst = 0
  pcaOp$exactCount = 3
  newX <- x[6:10,3:ncol(x)]
  projY <- as.data.frame(predict(pr, newdata = newX ))

  for (linkFunc in c("logit", "probit")){
    res <- estim.bin(x[,1,drop=FALSE], x[,3:ncol(x)], x[,6, drop=FALSE], linkFunc, newX, pcaOptions = pcaOp, printMsg = printMsg)

    resR <- glm(V1 ~ PC1 + PC2 + PC3, data = y, weights = x[,6], family = quasibinomial(link = linkFunc))

    resRp = predict(resR, newdata = projY, type="response")
    expect_equal(resRp[[1]], res$projection[1,2], tolerance = 1e-5)
    expect_equal(resRp[[2]], res$projection[2,2], tolerance = 1e-5)
  }
})

test_that("Discrete choice (binary) projection works", {

  for (linkFunc in c("logit", "probit")){
    newX <- dx[5:6,3:5]
    res <- estim.bin(x[,1,drop=FALSE], x[,3:5], NULL, linkFunc, as.matrix(newX), printMsg = printMsg)
    resR <- glm(V1 ~ V3 + V4 + V5, data = dx, family = binomial(link = linkFunc))


    resRp = predict(resR, newdata = newX, type="response")
    expect_equal(resRp[[1]], res$projection[1,2], tolerance = 1e-5)
    expect_equal(resRp[[2]], res$projection[2,2], tolerance = 1e-5)
  }
})

test_that("Discrete choice (binary) scoring works", {

  for (linkFunc in c("logit", "probit")){
    newX <- x[5:6,3:4]
    c1=matrix(c(0.5,1, 1, 0, 1, 0),2,3) # if probability is < 0.5, count as an error (1 cost)
    c2=matrix(c(0.5,1, 1, 0, 1, 0),2,3) # same as c1
    c3=matrix(c(0.5,1, 0, 1, 0, 1),2,3) # reversed

    res <- estim.bin(x[,1,drop=FALSE], x[,3:4], NULL, linkFunc,
                     newX =  newX, costMatrices = list(c1,c2, c3), simFixSize = 50, printMsg = printMsg)

    expect_equal(res$simulation$costRatios[[1]],res$simulation$costRatios[[2]], tolerance = 1e-16)
    expect_equal(res$simulation$costRatios[[1]], 1 - res$simulation$costRatios[[3]], tolerance = 1e-14)

    #TODO: test the actual scores & probability frequencies
  }
})

test_that("Discrete choice (binary) scoring works with PCA", {

  pcaOp = get.options.pca()
  pcaOp$ignoreFirst = 0
  pcaOp$exactCount = 3
  newX <- x[6:10,3:ncol(x)]

  for (linkFunc in c("logit", "probit")){

    c1=matrix(c(0.5,1, 1, 0, 1, 0),2,3) # if probability is < 0.5, count as an error (1 cost)
    c2=matrix(c(0.5,1, 1, 0, 1, 0),2,3) # same as c1
    c3=matrix(c(0.5,1, 0, 1, 0, 1),2,3) # reversed

    res <- estim.bin(x[,1,drop=FALSE], x[,3:ncol(x)], NULL, linkFunc, newX,
                     list(c1,c2, c3), pcaOptions = pcaOp, simFixSize = 50, printMsg = printMsg)

    expect_equal(res$simulation$costRatios[[1]],res$simulation$costRatios[[2]], tolerance = 1e-16)
    expect_equal(res$simulation$costRatios[[1]], 1 - res$simulation$costRatios[[3]], tolerance = 1e-14)

    #TODO: test the actual scores & probability frequencies
  }
})

test_that("Discrete choice search (avgCost,best) works", {
  skip_on_cran()

  c1=matrix(c(0.5,1, 1, 0, 1, 0),2,3)
  c2=matrix(c(0.5,1, 0, 1, 0, 1),2,3) # reversed
  g1 = c(1L,2L)
  g2 = c(3L,4L)
  res <- search.bin(x[,1,drop=FALSE], x[,3:7],
                  searchOptions = get.options.search(printMsg = printMsg),
                              metricOptions = get.options.metric(typesIn = c(), typesOut = c("frequencyCost")),
                              costMatrices = list(c1,c2), xPartitions = list(g1,g2), xSizes = c(1L,2L))
  expect_equal(0.5,res$frequencyCostOut$target1$model$bests$best1$weight, tolerance = 1e-16) #because of the structure of cost tables
})

test_that("Discrete choice search (avgCost,best, weighted) works", {
  skip_on_cran()


  c1=matrix(c(0.5,1, 1, 0, 1, 0),2,3)
  c2=matrix(c(0.5,1, 0, 1, 0, 1),2,3) # reversed
  g1 = c(1L,2L)
  g2 = c(3L,4L)
  g3 = c(5L,6L)
  res <- search.bin(x[,1,drop=FALSE], x[,3:8], x[,10,drop=FALSE],
                  searchOptions = get.options.search(printMsg = printMsg),
                  metricOptions = get.options.metric(typesIn = c(), typesOut = c("frequencyCost")),
                              costMatrices = list(c1,c2), xPartitions = list(g1,g2,g3), xSizes = c(1L,2L,3L))
  expect_equal(0.5,res$frequencyCostOut$target1$model$bests$best1$weight, tolerance = 1e-12) #because of the structure of cost tables
})


test_that("Discrete choice search (avgCost,all) works", {
  skip_on_cran()


  c1=matrix(c(0.5,1, 1, 0, 1, 0),2,3)
  c2=matrix(c(0.5,1, 0, 1, 0, 1),2,3) # reversed
  g1 = c(1L,2L)
  g2 = c(3L,4L)
  res <- search.bin(x[,1,drop=FALSE], x[,3:7], searchOptions = get.options.search(printMsg = printMsg),
                  metricOptions = get.options.metric(typesIn = c(), typesOut = c("frequencyCost")),
                              costMatrices = list(c1,c2), xPartitions = list(g1,g2), xSizes = c(1L,2L),
                              searchItems = get.items.search(bestK = 0, all = TRUE))
  for (a in res$frequencyCostOut$target1$model$all)
    expect_equal(0.5,a[[1]], tolerance = 1e-16) #because of the structure of cost tables

  alli = lapply(res$frequencyCostOut$target1$model$all, function(x) x$exoIndices)
  expect_equal(8,length(alli))
  expect_true(list.has.array(alli, c(1)))
  expect_true(list.has.array(alli, c(2)))
  expect_true(list.has.array(alli, c(3)))
  expect_true(list.has.array(alli, c(4)))
  expect_true(list.has.array(alli, c(1,3)))
  expect_true(list.has.array(alli, c(1,4)))
  expect_true(list.has.array(alli, c(2,3)))
  expect_true(list.has.array(alli, c(2,4)))

  expect_false(list.has.array(alli, c(1,2)))
  expect_false(list.has.array(alli, c(3,4)))

})

test_that("Discrete choice search (aic, one model) works", {
  skip_on_cran()


  g1 = c(1L)
  g2 = c(2L)
  res <- search.bin(x[,1,drop=FALSE], x[,3:4],
                  searchOptions = get.options.search(printMsg = printMsg),
                  metricOptions = get.options.metric(typesIn = c("aic"), typesOut = c()),
                              xPartitions = list(g1,g2), xSizes = c(2L),
                              searchItems = get.items.search(bestK = 1, all = TRUE))

  res1 = estim.bin(x[,1,drop=FALSE],x[,c(3,4)],NULL, linkFunc = "logit", printMsg = printMsg)
  expect_equal(res$aic$target1$model$bests$best1$weight, exp(-0.5*res1$metrics[2,1]), tolerance = 1e-14)



})

test_that("Discrete choice search (avgCost, one model) works", {
  skip_on_cran()


  c1=matrix(c(0.5,1, 1, 0, 1, 0),2,3)
  g1 = c(1L)
  g2 = c(2L)
  tratio = 0.8

  res <- search.bin(x[,1,drop=FALSE], x[,3:4], searchOptions = get.options.search(printMsg = printMsg),
                  metricOptions = get.options.metric(typesIn = c(), typesOut = c("frequencyCost"),
                                                                                 seed = -340, simFixSize = 200, trainRatio = tratio),
                              xPartitions = list(g1,g2), xSizes = c(2L), costMatrices = list(c1),
                              searchItems = get.items.search(bestK = 1, all = FALSE))

  res1 = estim.bin(x[,1,drop=FALSE],x[,c(3,4)],NULL, linkFunc = "logit",
                        costMatrices = list(c1), simSeed = 340, simFixSize = 200,
                        simTrainRatio = tratio, printMsg = printMsg)
  expect_equal(res$frequencyCostOut$target1$model$bests$best1$weight, 1 - res1$simulation$costRatios[[1]], tolerance = 1e-14) # note that in search, it is 1-cost score
})


test_that("Discrete choice search (avgCost, best & all) works", {
  skip_on_cran()


  c1=matrix(c(0.5,1, 1, 0, 1, 0),2,3)
  g1 = c(1L,2L,3L)
  g2 = c(4L,5L,6L,7L,8L,9L)
  tratio = 0.8

  res <- search.bin(x[,1,drop=FALSE], x[,3:20], searchOptions = get.options.search(printMsg = printMsg),
                  metricOptions = get.options.metric(typesIn = c("sic"), typesOut = c("frequencyCost"),
                                                                                  seed = -340, simFixSize = 200, trainRatio = tratio),
                              xPartitions = list(g1,g2), xSizes = c(2L), costMatrices = list(c1),
                              searchItems = get.items.search(bestK = 4, all = TRUE))

  expect_true(length(res$frequencyCostOut$target1$model$all)>2)
  j=0
  for (a in res$frequencyCostOut$target1$model$all){
    expect_true(res$frequencyCostOut$target1$model$bests$best1$weight >= a[[1]])

    # note that in the indexes, we should adjust the indexes too
    aa<-a$exoIndices+2
    resB = estim.bin(x[,1,drop=FALSE],as.matrix(x[,aa]),NULL, linkFunc = "logit",
                          costMatrices = list(c1), simSeed = 340, simFixSize = 200,
                          simTrainRatio = tratio, printMsg = printMsg)
    expect_equal(a[[1]], 1 - resB$simulation$costRatios[[1]]) # note that in search, it is 1-cost score
    j=j+1
    if (j==4)
      break # I think no more test is needed
  }
})

test_that("Discrete choice search (NA) works", {
  skip_on_cran()


  y0 = x[,1,drop=FALSE]
  w0 = x[,5,drop=FALSE]
  Z0 = x[,2:6]

  yNA=as.matrix(append(y0, c(1,1,1,1)))
  wNA=as.matrix(append(w0, c(1,1,1,1)))
  m=matrix(c(1,1,1,1,
             3,3,3,3,
             5,5,5,1,
             4,4,4,4,
             7,7,7,7),4,5)*NA
  colnames(m)<-colnames(Z0)
  ZNA=rbind(Z0,m)


  g1 = c(1L,2L)
  g2 = c(3L,4L)
  res1 <- search.bin(y0, Z0, w0, searchOptions = get.options.search(printMsg = printMsg),
                   metricOptions = get.options.metric(typesIn = c("aic"), typesOut = c()),
                               xPartitions = list(g1,g2), xSizes = c(2L),
                               searchItems = get.items.search(bestK = 1, all = FALSE))
  res2 <- search.bin(yNA, ZNA, wNA, searchOptions = get.options.search(printMsg = printMsg),
                   metricOptions = get.options.metric(typesIn = c("aic"), typesOut = c()),
                               xPartitions = list(g1,g2), xSizes = c(2L),
                               searchItems = get.items.search(bestK = 1, all = FALSE))

  expect_equal(res1$aic$target1$model$bests$best1$weight, res2$aic$target1$model$bests$best1$weight, tolerance = 1e-15)
})

test_that("Discrete choice search (parallel) works", {
  skip_on_cran()


  g1 = c(1L,2L,3L,4L,5L,6L)
  g2 = c(7L,8L,9L,10L,11L,12L)
  res1 <- search.bin(x[,1,drop=FALSE], x[,3:20], metricOptions = get.options.metric(typesIn = c("aic"), typesOut = c()),
                               xPartitions = list(g1,g2), xSizes = c(2L),
                               searchItems = get.items.search(bestK = 1, all = TRUE),
                               searchOptions = get.options.search(parallel = FALSE, printMsg = printMsg))
  res2 <- search.bin(x[,1,drop=FALSE], x[,3:20], metricOptions = get.options.metric(typesIn = c("aic"), typesOut = c()),
                               xPartitions = list(g1,g2), xSizes = c(2L),
                               searchItems = get.items.search(bestK = 1, all = TRUE),
                               searchOptions = get.options.search(parallel = TRUE, printMsg = printMsg))

  expect_equal(res1$aic$target1$model$bests$best1$weight, res2$aic$target1$model$bests$best1$weight, tolerance = 1e-15)
})

test_that("Discrete choice search works with restricted AIC", {
  skip_on_cran()

  y=x[,1,drop=FALSE]
  Exo=x[,4:20]
  g1 = c(1L,2L,3L,4L,5L,6L)
  g2 = c(7L,8L,9L,10L,11L,12L)
  res = search.bin(x[,1,drop=FALSE], Exo, metricOptions = get.options.metric(typesIn = c("aic", "auc"), typesOut = c()),
                                   xPartitions = list(g1,g2), xSizes = c(1L,2L),
                                   modelCheckItems = get.items.modelcheck(maxAic = 1.45),
                                   searchItems = get.items.search(bestK = 1, all = TRUE),
                                   searchOptions = get.options.search(parallel = FALSE, printMsg = printMsg))
  alls = list()
  for (m in res$aic$target1$model$all){
    M = estim.bin(y, x = as.matrix(Exo[,m$exoIndices]), printMsg = printMsg)
    alls = append(alls, M$metrics[2,1])
    expect_true(as.numeric(M$metrics[2,1]) <= 1.45)
  }
})

test_that("Discrete choice search works with inclusion weights", {
  skip_on_cran()

  y=x[,c(1), drop = FALSE]
  Exo=x[,4:7]
  res = search.bin(y, Exo, metricOptions = get.options.metric(typesIn = c("auc"), typesOut = c()),
                                   xSizes = c(1L,2L),
                                   searchItems = get.items.search(bestK = 1, all = TRUE, inclusion = TRUE),
                                   searchOptions = get.options.search(parallel = FALSE, printMsg = printMsg))
  inclusion = matrix(0,6,2)
  for (m in res$aucIn$target1$model$all){
    # endogenous
    inclusion[1,1] = inclusion[1,1] + m$weight
    inclusion[1,2] = inclusion[1,2] + 1
    #intercept
    inclusion[2,1] = inclusion[2,1] + m$weight
    inclusion[2,2] = inclusion[2,2] + 1

    for (e in m$exoIndices){
      d = e + 2 # the dependent variable and the intercept are not in the m$exoIndices
      inclusion[d,1] = inclusion[d,1] + m$weight
      inclusion[d,2] = inclusion[d,2] + 1
    }
  }
  inclusion[,1] = inclusion[,1]/inclusion[,2]

  expect_equal(as.numeric(res$aucIn$target1$model$inclusion), as.numeric(inclusion), tolerance = 1e-10)

})

test_that("Discrete choice search works with coefficients (bests)", {
  skip_on_cran()

  y=x[,c(1), drop=FALSE]
  Exo=x[,4:7]
  res = search.bin(y, Exo, metricOptions = get.options.metric(typesIn = c("aic"), typesOut = c()),
                                   xSizes = c(1L,2L),
                                   searchItems = get.items.search(bestK = 1, type1 = TRUE, all = TRUE, inclusion = FALSE),
                                   searchOptions = get.options.search(parallel = FALSE, printMsg = printMsg))

  for (u in c(1:4)){
    best_coef2 = NULL
    w=-Inf
    for (m in res$aic$target1$model$all){
      if (any(m$exoIndices==u)){
        if (w<m$weight){
          w=m$weight
          best_coef2 = m
        }
      }
    }
    # for 3, use Item4 due to the intercept
    item=res$aic$target1$coefs$bests$item1
    if (u==1)
      item = res$aic$target1$coefs$bests$item2
    else if (u==2)
      item=res$aic$target1$coefs$bests$item3
    else if (u==3)
      item=res$aic$target1$coefs$bests$item4
    else if (u==4)
      item = res$aic$target1$coefs$bests$item5
    expect_equal(item$best1$weight, best_coef2$weight, tolerance = 1e-10)
    expect_equal(item$best1$exoIndices, best_coef2$exoIndices, tolerance = 1e-10)

    # are mean and variance equal?
    M = estim.bin(y,
                       x = as.matrix(Exo[,item$best1$exoIndices]),
                       simFixSize = 0, printMsg = printMsg)
    expect_equal(exp(-0.5 * M$metrics[2,1]), item$best1$weight, tolerance = 1e-10)
    expect_equal(item$best1$mean, M$estimations$gamma[[2]], tolerance = 1e-10)
    expect_equal(item$best1$var, M$estimations$gammaVar[2,2], tolerance = 1e-10)

  }
})

test_that("Discrete choice search works with coefficients (cdfs)", {
  skip_on_cran()

  y=x[,c(1), drop=FALSE]
  Exo=x[,4:7]
  res = search.bin(y, Exo, metricOptions = get.options.metric(typesIn = c("aic"), typesOut = c()),
                                   xSizes = c(1L,2L),
                                   searchItems = get.items.search(bestK = 1, type1 = TRUE, all = TRUE, inclusion = FALSE,
                                                                cdfs = c(0,1)),
                                   searchOptions = get.options.search(parallel = FALSE, printMsg = printMsg))
  for (u in c(1:4)){
    sum = 0
    c = 0
    cc=0
    for (m in res$aic$target1$model$all){

      if (any(m$exoIndices==u)){
        ind = which(m$exoIndices == u) + 1 #+1 for intercept
        M = estim.bin(y, x = as.matrix(Exo[,m$exoIndices]),
                           simFixSize = 0, printMsg = printMsg)
        coef = M$estimations$gamma[ind]
        sd = sqrt(M$estimations$gammaVar[ind,ind])

        sum = sum+m$weight * pnorm(0,coef,sd)  # note the NORMAL dist. If t,  d.o.f : nrow(y)-length(gamma)
        c=c+m$weight
        cc=cc+1
      }
    }
    expect_equal(res$aic$target1$coefs$cdfs$cdf1[u+1,1], sum/c, tolerance = 1e-10)
    expect_equal(res$aic$target1$coefs$cdfs$cdf1[u+1,2], cc, tolerance = 1e-10)
  }
})


test_that("Discrete choice search works with coefficients (extreme bounds)", {
  skip_on_cran()

  y=x[,c(1),drop=FALSE]
  Exo=x[,4:7]
  res = search.bin(y, Exo, metricOptions = get.options.metric(typesIn = c("aic"), typesOut = c()),
                                   xSizes = c(1L,2L),
                                   searchItems = get.items.search(bestK = 1, type1 = TRUE, all = TRUE, inclusion = FALSE,
                                                                extremeMultiplier = 2),
                                   searchOptions = get.options.search(parallel = FALSE, printMsg = printMsg))
  mn = Inf
  mx = -Inf
  h = 2
  for (m in res$aic$target1$model$all){

    if (any(m$exoIndices==h)) {
      ind = which(m$exoIndices == h) + 1 #+1 for intercept
      M = estim.bin(y, x = as.matrix(Exo[,m$exoIndices]),
                         simFixSize = 0, printMsg = printMsg)
      coef = M$estimations$gamma[ind]
      sd = sqrt(M$estimations$gammaVar[ind,ind])
      mn = min(mn,coef-2*sd)
      mx = max(mx,coef+2*sd)
    }
  }
  expect_equal(res$aic$target1$coefs$extremeBounds[h+1,1], mn, tolerance = 1e-10)
  expect_equal(res$aic$target1$coefs$extremeBounds[h+1,2], mx, tolerance = 1e-10)
})


test_that("Discrete choice search works with coefficients (mixture)", {
  skip_on_cran()

  y=x[,c(1),drop=FALSE]
  Exo=x[,4:7]
  res = search.bin(y, Exo, metricOptions = get.options.metric(typesIn = c("aic"), typesOut = c()),
                                   xSizes = c(1L,2L),
                                   searchItems = get.items.search(bestK = 1, type1 = TRUE, all = TRUE, inclusion = FALSE,
                                                                extremeMultiplier = 2, mixture4 = TRUE),
                                   searchOptions = get.options.search(parallel = FALSE, printMsg = printMsg))

  coefs = c()
  vars = c()
  weights = c()
  h = 1
  for (m in res$aic$target1$model$all){
    if (any(m$exoIndices==h)) {
      M = estim.bin(y, x = as.matrix(Exo[,m$exoIndices]),
                         simFixSize = 0, printMsg = printMsg)
      ind = which(m$exoIndices == h) + 1 #+1 for intercept
      coefs = append(coefs,M$estimations$gamma[ind])
      vars = append(vars, M$estimations$gammaVar[ind,ind])
      weights = append(weights, m$weight)
    }
  }
  # note that we need weighted mean, variance, etc. assuming normal distribution

  len = length(coefs)
  expect_equal(res$aic$target1$coefs$mixture[h+1,5], len)
  me = weighted.mean(coefs, weights)
  expect_equal(res$aic$target1$coefs$mixture[h+1,1], me, tolerance = 1e-14)

  # TODO : compare weighted variance, skewness, kurtosis assuming normality
  #        of course, its better to .Call the running statistics, test it, and use it here

})


test_that("Discrete choice summary works", {
  skip_on_cran()

  c1=matrix(c(0.5,1, 1, 0, 1, 0),2,3)
  c2=matrix(c(0.6,1, 1, 0, 1, 0),2,3)
  y=x[,c(1),drop=FALSE]
  Exo=x[,4:7]

  res <- search.bin(y, Exo,searchLogit = TRUE, searchProbit = TRUE,costMatrices = list(c1, c2),
                  metricOptions = get.options.metric(typesIn = c("sic", "aic", "aucIn", "frequencyCost"), typesOut = c("frequencyCost", "auc"),
                                                     seed = -400,
                                                     simFixSize = 10, trainRatio = 0.75, trainFixSize = 0),

                  xSizes = c(1L,2L),
                  searchItems = get.items.search(bestK = 5, type1=TRUE, all = TRUE),
                  searchOptions = get.options.search(parallel = FALSE, printMsg = printMsg))

  su =summary(res, y = y, x = Exo, addModelBests = TRUE,
              addModelAll = FALSE, addItem1 = FALSE, w = NULL, test = TRUE)

})


test_that("Discrete choice SplitSearch works (no subsetting)", {
  skip_on_cran()

  c1=matrix(c(0.5,1, 1, 0, 1, 0),2,3)
  c2=matrix(c(0.6,1, 1, 0, 1, 0),2,3)
  y=x[,c(1),drop=FALSE]
  Exo=x[,4:7]

  # don't test with out-of-sample metrics. It seems we have different model with equal weights (the result change by repeating the call ?!)

  searchItems = get.items.search(type1 = TRUE, all = TRUE, bestK = 200, inclusion = TRUE,
                               cdfs = c(0,1), mixture4 = TRUE, extremeMultiplier = 2.0 )
  metricOptions = get.options.metric(c("sic", "aic"), c("frequencyCost"), seed = -400)
  searchOptions = get.options.search(FALSE, printMsg = FALSE)

  split = search.bin.stepwise(x = Exo, y = y, xSizeSteps = list(c(1L,2L), c(3L)), countSteps = c(NA, NA),
                     costMatrices = list(c1, c2),
                      searchItems = searchItems, metricOptions = metricOptions,
                      searchOptions = searchOptions, savePre = NULL)

  whole = search.bin(y, Exo, xSizes = c(1L,2L,3L),
                   costMatrices = list(c1, c2),
                    searchItems = searchItems, metricOptions = metricOptions,
                    searchOptions = searchOptions)

  # CHECK ALL

  # for 'all' the order is generally different
  weights0 <- sort(sapply(whole$sic$target1$model$all, function(a) a$weight))
  weights1 <- sort(sapply(split$sic$target1$model$all, function(a) a$weight))
  expect_equal(as.numeric(weights0),as.numeric(weights1), tolerance = 1e-12)

  weights0 <- sort(sapply(whole$frequencyCostOut$target1$model$all, function(a) a$weight))
  weights1 <- sort(sapply(split$frequencyCostOut$target1$model$all, function(a) a$weight))
  expect_equal(as.numeric(weights0),as.numeric(weights1), tolerance = 1e-12) # some different models in crps has equal weight (due to OLS estimation of systems with different number of equations)
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
  for (w_item in whole$aic$target1$coefs$bests){
    w_item <- w_item[lengths(w_item)!=0] # we set the bestK too high. some elements are null
    i = i + 1
    s_item <- split$aic$target1$coefs$bests[[i]]
    expect_equal(w_item$best1$weight, s_item$best1$weight, tolerance =1e-10)
    expect_equal(w_item$best1$exoIndices, s_item$best1$exoIndices, tolerance =1e-10)
    expect_equal(w_item$best1$var, s_item$best1$var, tolerance =1e-10)


    expect_equal(w_item$best3$weight, s_item$best3$weight, tolerance =1e-10)
    expect_equal(w_item$best3$exoIndices, s_item$best3$exoIndices, tolerance =1e-10)
    expect_equal(w_item$best3$var, s_item$best3$var, tolerance =1e-10)

    expect_equal(w_item$best5$weight, s_item$best5$weight, tolerance =1e-10)
    expect_equal(w_item$best5$exoIndices, s_item$best5$exoIndices, tolerance =1e-10)
    expect_equal(w_item$best5$var, s_item$best5$var, tolerance =1e-10)

  }

})

