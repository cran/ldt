
x = matrix(c(1,0,1,0,1,1,0,0,1,1,0,1,0,0,1,0,1,0,0,0,0,1,0,1,0,1,1,0,1,1,1,0,1,1,0,1,1,1,1,1,
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
           , 40,31)
colnames(x) <- paste0("V", c(1:ncol(x)))
dx <- as.data.frame(x)


test_that("Discrete choice (binary) estimation works", {

  for (linkFunc in c("logit", "probit")){
    res <- estim.bin(data = get.data(x[,1:4]),
                     linkFunc = linkFunc)
    resR <- glm(V1 ~ V2 + V3 + V4, data = dx, family = binomial(link = linkFunc))
    expect_equal(as.numeric(resR$coefficients), as.numeric(res$estimations$gamma), tolerance = 1e-5)
    sum_resR=summary(resR)
    expect_equal(as.numeric(sum_resR$cov.scaled), as.numeric(res$estimations$gammaVar), tolerance = 1e-1)
    expect_equal(as.numeric(resid(res, pearson = FALSE)), as.numeric(resid(resR, type="response")), tolerance = 1e-5)
    expect_equal(as.numeric(resid(res, pearson = TRUE)), as.numeric(resid(resR, type="pearson")), tolerance = 1e-5)

    glm_pp <- predict(resR, type = "response")
    brier_score <- mean((dx$V1 - glm_pp)^2)
    expect_equal(brier_score, res$metrics[4,1], tolerance = 1e-1)

    auc <- ldt::s.roc(dx$V1, 1 - glm_pp) # 1- glm_pp because of s.roc behaviour
    expect_equal(auc$auc, res$metrics[5,1], tolerance = 1e-1)

  }
})

test_that("Discrete choice (binary) estimation works with PCA", {

  y = as.data.frame(cbind(as.matrix(x[,1,drop=FALSE]), prcomp(x[,2:ncol(x)], scale. = TRUE)$x)) # orthogonal, will be used in R glm
  pcaOp = get.options.pca()
  pcaOp$ignoreFirst = 0 #It automatically adds and ignores intercept
  pcaOp$exactCount = 3

  for (linkFunc in c("logit", "probit")){
    res <- estim.bin(data = get.data(x[,1:ncol(x)]),
                     linkFunc = linkFunc, pcaOptions = pcaOp)

    resR <- glm(V1 ~ PC1 + PC2 + PC3, data = y, family = binomial(link = linkFunc))
    expect_equal(abs(as.numeric(resR$coefficients)), abs(as.numeric(res$estimations$gamma)), tolerance = 1e-4) # abs? they are related to PC
    sum_resR=summary(resR)
    expect_equal(as.numeric(sum_resR$cov.scaled), as.numeric(res$estimations$gammaVar), tolerance = 1e-1)
  }
})

test_that("Discrete choice (binary, weighted) estimation works", {

  for (linkFunc in c("logit", "probit")){

    res <- estim.bin(data = get.data(x[,1:4], weights = x[,7]),
                     linkFunc = linkFunc)
    resR <- glm(V1 ~ V2 + V3 + V4, data = dx, weights = x[,7], family = quasibinomial(link = linkFunc))
    # quasibinomial (to avoid a warning (non-integer #successes in a binomial glm!))
    # see: https://stackoverflow.com/a/12954119
    expect_equal(as.numeric(resR$coefficients), as.numeric(res$estimations$gamma), tolerance = 1e-4)
    sum_resR=summary(resR)
    expect_equal(as.numeric(sum_resR$cov.scaled), as.numeric(res$estimations$gammaVar), tolerance = 1e-1)
  }

})

test_that("Discrete choice (binary, weighted) estimation works with PCA", {

  y = as.data.frame(cbind(as.matrix(x[,1,drop=FALSE]), prcomp(x[,2:ncol(x)], scale. = TRUE)$x)) # orthogonal, will be used in R glm
  pcaOp = get.options.pca()
  pcaOp$ignoreFirst = 0
  pcaOp$exactCount = 3

  for (linkFunc in c("logit", "probit")){
    res <- estim.bin(data = get.data(x[,1:ncol(x)], weights = x[,7]),
                     linkFunc, pcaOptions = pcaOp)

    resR <- glm(V1 ~ PC1 + PC2 + PC3, data = y, weights = x[,7,drop = FALSE], family = quasibinomial(link = linkFunc))
    expect_equal(abs(as.numeric(resR$coefficients)), abs(as.numeric(res$estimations$gamma)), tolerance = 1e-5) # abs? they are related to PCs
    sum_resR=summary(resR)
    expect_equal(as.numeric(sum_resR$cov.scaled), as.numeric(res$estimations$gammaVar), tolerance = 1e-1)
  }
})

test_that("Discrete choice (binary, weighted, logL) estimation works", {

  y0 = x[,1,drop=FALSE]
  w0 = x[,7,drop=FALSE]
  Z0 = x[,3:5]

  y=as.matrix(append(y0, c(1,1,1,1)))
  colnames(y) <- "V1"
  w=as.matrix(append(w0, c(1,1,1,1)))
  m=matrix(c(3,3,3,3,5,5,5,5,6,6,6,6),4,3)
  colnames(m)<-colnames(Z0)
  Z=rbind(Z0,m)
  yw=as.matrix(append(y0,c(1))) # remove the last 4 observations and add a weight of 4
  colnames(yw) <- "V1"
  ww = as.matrix(append(w0,c(4)))
  m=matrix(c(3,5,6),1,3)
  colnames(m)<-colnames(Z0)
  Zw = rbind(Z0,m)


  for (linkFunc in c("logit", "probit")){
    res <- estim.bin(data = get.data(cbind(y, Z), weights = w), linkFunc)
    resw <- estim.bin(data = get.data(cbind(yw, Zw), weights = ww), linkFunc)
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

    res <- estim.bin(data = get.data(x[,1:4], newData = x[c(6,7),]), linkFunc = linkFunc)
    resR <- glm(V1 ~ V2 + V3 + V4, data = dx, family = binomial(link = linkFunc))

    resRp = predict(resR, newdata = as.data.frame(x[c(6,7),]), type="response")
    expect_equal(resRp[[1]], res$projection[[1,2]], tolerance = 1e-5)
    expect_equal(resRp[[2]], res$projection[[2,2]], tolerance = 1e-5)
  }
})

test_that("Discrete choice (binary, weighted) projection works with PCA", {

  pr=prcomp(x[,2:ncol(x)], scale. = TRUE)
  y = as.data.frame(cbind(as.matrix(x[,1,drop=FALSE]), pr$x)) # orthogonal, will be used in R glm
  pcaOp = get.options.pca()
  pcaOp$ignoreFirst = 0
  pcaOp$exactCount = 3
  newX <- x[6:10,2:ncol(x)]
  projY <- as.data.frame(predict(pr, newdata = newX ))

  for (linkFunc in c("logit", "probit")){
    res <- estim.bin(data = get.data(x[,1:ncol(x)],
                                     weights = x[,7, drop=FALSE],
                                     newData = newX),
                     linkFunc, pcaOptions = pcaOp)

    resR <- glm(V1 ~ PC1 + PC2 + PC3, data = y, weights = x[,7],
                family = quasibinomial(link = linkFunc))

    resRp = predict(resR, newdata = projY, type="response")
    expect_equal(resRp[[1]], res$projection[[1,2]], tolerance = 1e-5)
    expect_equal(resRp[[2]], res$projection[[2,2]], tolerance = 1e-5)
  }
})

test_that("Discrete choice (binary) projection works", {

  for (linkFunc in c("logit", "probit")){
    newX <- dx[5:6,]
    res <- estim.bin(data = get.data(x[,1:4], newData = newX), linkFunc = linkFunc)
    resR <- glm(V1 ~ V2 + V3 + V4, data = dx, family = binomial(link = linkFunc))


    resRp = predict(resR, newdata = newX, type="response")
    expect_equal(resRp[[1]], res$projection[[1,2]], tolerance = 1e-5)
    expect_equal(resRp[[2]], res$projection[[2,2]], tolerance = 1e-5)
  }
})

test_that("Discrete choice (binary) scoring works", {

  for (linkFunc in c("logit", "probit")){
    c1=matrix(c(0.5,1, 1, 0, 1, 0),2,3) # if probability is < 0.5, count as an error (1 cost)
    c2=matrix(c(0.5,1, 1, 0, 1, 0),2,3) # same as c1
    c3=matrix(c(0.5,1, 0, 1, 0, 1),2,3) # reversed

    res <- estim.bin(data = get.data(x[,1:4], newData = x[c(5,6),]),
                     linkFunc, costMatrices = list(c1,c2, c3), simFixSize = 50)

    expect_equal(res$simulation$costRatios[[1]],res$simulation$costRatios[[2]], tolerance = 1e-16)
    expect_equal(res$simulation$costRatios[[1]], 1 - res$simulation$costRatios[[3]], tolerance = 1e-14)

    #TODO: test the actual scores & probability frequencies
  }
})

test_that("Discrete choice (binary) scoring works with PCA", {

  pcaOp = get.options.pca()
  pcaOp$ignoreFirst = 0
  pcaOp$exactCount = 3
  newX <- x[6:10,2:ncol(x)]

  for (linkFunc in c("logit", "probit")){

    c1=matrix(c(0.5,1, 1, 0, 1, 0),2,3) # if probability is < 0.5, count as an error (1 cost)
    c2=matrix(c(0.5,1, 1, 0, 1, 0),2,3) # same as c1
    c3=matrix(c(0.5,1, 0, 1, 0, 1),2,3) # reversed

    res <- estim.bin(data = get.data(x[,1:4], newData = x[c(5,6),]),
                     linkFunc, list(c1,c2, c3), pcaOptions = pcaOp, simFixSize = 50)

    expect_equal(res$simulation$costRatios[[1]],res$simulation$costRatios[[2]], tolerance = 1e-16)
    expect_equal(res$simulation$costRatios[[1]], 1 - res$simulation$costRatios[[3]], tolerance = 1e-14)

    #TODO: test the actual scores & probability frequencies
  }
})


test_that("Discrete choice search (avgCost,all) works", {
  skip_on_cran()

  c1=matrix(c(0.5,1, 1, 0, 1, 0),2,3)
  c2=matrix(c(0.5,1, 0, 1, 0, 1),2,3) # reversed
  g1 = c(1,2)
  g2 = c(3,4)
  res <- search.bin(data = get.data(x[,1:6], newData = x[c(5,6),], weights = x[,7]),
                    combinations = get.combinations(sizes = c(1,2)),
                    metrics = get.search.metrics(typesIn = c("aic"), typesOut = c("frequencyCost")),
                    costMatrices = list(c1,c2),
                    items = get.search.items(bestK = 4, all = TRUE))
  sapply(res$results[which(sapply(res$results, function(r)r$typeName=="model" && r$evalName == "aic"))],
         function(d)d$value$metric)
  sumRes <- summary(res, test = TRUE)
  for (a in res$results[which(sapply(res$results,
                                     function(r)r$typeName=="model" && r$evalName == "frequencyCostOut"))])
    expect_equal(0.5,a$value$weight, tolerance = 1e-16) #because of the structure of cost tables
})


test_that("Discrete choice search (NA) works", {
  skip_on_cran()


  y0 = x[,1,drop=FALSE]
  w0 = x[,5,drop=FALSE]
  Z0 = x[,2:6]

  yNA=as.matrix(append(y0, c(1,1,1,1)))
  colnames(yNA) <- c("yNA")
  wNA=as.matrix(append(w0, c(1,1,1,1)))
  m=matrix(c(1,1,1,1,
             3,3,3,3,
             5,5,5,1,
             4,4,4,4,
             7,7,7,7),4,5)*NA
  colnames(m)<-colnames(Z0)
  ZNA=rbind(Z0,m)


  g1 = c(2)
  g2 = c(3,4)
  res1 <- search.bin(data = get.data(cbind(y0,Z0), weights = w0),
                     combinations = get.combinations(sizes = c(2),
                                                     partitions = list(c(1),g1,g2)),
                     metrics = get.search.metrics(typesIn = c("aic"), typesOut = NULL),
                     items = get.search.items(bestK = 0, all = TRUE))
  res2 <- search.bin(data = get.data(cbind(yNA,ZNA), weights = wNA),
                     combinations = get.combinations(sizes = c(2),
                                                     partitions = list(c(1),g1,g2)),
                     metrics = get.search.metrics(typesIn = c("aic"), typesOut = c()),
                     items = get.search.items(bestK = 0, all = TRUE))

  allWeights1 = sort(sapply(res1$results, function(x){x$value$metric}))
  allWeights2 = sort(sapply(res2$results, function(x){x$value$metric}))

  expect_equal(allWeights1, allWeights2)
})

test_that("Discrete choice search works with inclusion weights", {
  skip_on_cran()

  res <- search.bin(data = get.data(x[,1:7], weights = x[,7]),
                    combinations = get.combinations(sizes = c(1,2,3)),
                    metrics = get.search.metrics(typesIn = c("aic"), typesOut = NULL),
                    items = get.search.items(all = TRUE, inclusion = TRUE))

  inclusion = matrix(0,8,2, dimnames = list(colnames(res$info$data$data)[-2], NULL))
  for (m in res$results[which(sapply(res$results,
                                     function(r) r$evalName == "aic" && r$typeName == "model" && r$targetName == "V1"))]){
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
                                             function(r) r$evalName == "aic" && r$targetName == "V1" && r$typeName == "inclusion"))]
  expect_equal(as.numeric(searchInclusion[[1]]$value), as.numeric(inclusion), tolerance = 1e-10)


})

test_that("Discrete choice search works with coefficients (bests)", {
  skip_on_cran()

  res <- search.bin(data = get.data(x[,1:7]),
                    combinations = get.combinations(sizes = c(1,2,3)),
                    metrics = get.search.metrics(typesIn = c("aic"), typesOut = NULL),
                    items = get.search.items(all = TRUE, type1 = TRUE, bestK = 2))
  sumRes <- summary(res, test = TRUE)
  expect_equal(res$counts, sumRes$counts)

})

test_that("Discrete choice search works with coefficients (cdfs)", {
  skip_on_cran()

  res <- search.bin(data = get.data(x[,1:7]),
                    combinations = get.combinations(sizes = c(1,2,3)),
                    metrics = get.search.metrics(typesIn = c("aic"), typesOut = NULL),
                    items = get.search.items(all = TRUE, type1 = TRUE, cdfs = c(0,1)))

  sumRes <- summary(res, test = TRUE)
  sum = 0
  c = 0
  cc=0
  i = 0
  for (m in sumRes$results){
    i = i + 1
    if (m$evalName != "aic" || m$typeName != "model" || m$targetName != "V1")
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
                                  function(r) r$evalName == "aic" && r$targetName == "V1" && r$typeName == "cdf"))]

  expect_equal(cdfs[[1]]$value[4,1], sum/c, tolerance = 1e-10)
  expect_equal(cdfs[[1]]$value[4,2], cc, tolerance = 1e-10)

})


test_that("Discrete choice search works with coefficients (extreme bounds)", {
  skip_on_cran()

  res <- search.bin(data = get.data(x[,1:7]),
                    combinations = get.combinations(sizes = c(1,2,3)),
                    metrics = get.search.metrics(typesIn = c("aic"), typesOut = NULL),
                    items = get.search.items(all = TRUE, type1 = TRUE, extremeMultiplier = 2))
  sumRes <- summary(res, test = TRUE)
  mn = Inf
  mx = -Inf
  for (m in sumRes$results){
    if (m$evalName != "aic" || m$typeName != "model" || m$targetName != "V1")
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
                                      function(r) r$evalName == "aic" && r$targetName == "V1" && r$typeName == "extreme bound"))]

  expect_equal(extremeB[[1]]$value[4,1], mn, tolerance = 1e-10)
  expect_equal(extremeB[[1]]$value[4,2], mx, tolerance = 1e-10)
})


test_that("Discrete choice search works with coefficients (mixture)", {
  skip_on_cran()

  res <- search.bin(data = get.data(x[,1:7], weights = x[,7]),
                    combinations = get.combinations(sizes = c(1,2,3)),
                    metrics = get.search.metrics(typesIn = c("aic"), typesOut = NULL),
                    items = get.search.items(all = TRUE, type1 = TRUE, mixture4 = TRUE))

  sumRes <- summary(res, test = TRUE)
  coefs = c()
  vars = c()
  weights = c()
  i = 0
  for (m in sumRes$results){
    i = i + 1
    if (m$evalName != "aic" || m$typeName != "model" || m$targetName != "V1")
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
                                     function(r) r$evalName == "aic" && r$targetName == "V1" && r$typeName == "mixture"))]

  # note that we need weighted mean, variance, etc. assuming normal distribution

  len = length(coefs)
  expect_equal(mixture[[1]]$value[4,5], len)
  me = weighted.mean(coefs, weights)
  expect_equal(mixture[[1]]$value[4,1], me, tolerance = 1e-14)

})


test_that("Discrete choice SplitSearch works (no subsetting)", {
  skip_on_cran()

  data = get.data(x[,1:7, drop = FALSE])
  combinations = get.combinations()
  items = get.search.items(inclusion = TRUE
                           #, all = TRUE
                           , bestK = 4   # TODO: Test fails for bestK > 1. best coefficients for info>0 are not included!
                           , type1 = TRUE
                           , cdfs = c(0,0.3)
                           , mixture4 = TRUE
                           , extremeMultiplier = 2
  )
  metrics = get.search.metrics(c("sic", "aic")) # don't test with out-of-sample metrics. It seems we have different model with equal weights (the result change by repeating the call ?!)
  options = get.search.options(FALSE,
                               reportInterval = 0
  )

  combinations$sizes <- c(1, 2, 3)
  whole = search.bin(data = data,
                     combinations = combinations,
                     items = items,
                     metrics = metrics,
                     options = options)

  combinations$sizes <- list(c(1, 2), c(3))
  combinations$stepsNumVariables <- c(NA, NA)
  split = search.bin(data = data,
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

