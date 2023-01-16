
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


test_that("Distance (euclidean,manhattan,maximum) works", {

  for (dis in c("euclidean","manhattan","maximum")){
    res <- GetDistance(x, dis)
    rres = as.numeric(dist(tX,dis))
    expect_equal(res, rres, tolerance = 1e-8)  }

  # todo: if you want to test by removing NA, you should calculate distances pairwise (same as LDT)

})


test_that("Distance (correlation) works", {
  res <- GetDistance(x, "correlation", "pearson", FALSE)
  rres <- as.numeric(as.dist(sqrt((1 - cor(x, method = "pearson"))/2.0)))
  expect_equal(res, rres, tolerance = 1e-8)

  # abs
  res <- GetDistance(x, "absCorrelation", "pearson", FALSE)
  rres <- as.numeric(as.dist(sqrt((1 - cor(x, method = "pearson")^2))))
  expect_equal(res, rres, tolerance = 1e-8)

})

test_that("Distance (pearson correlation) works with NA", {
  res <- GetDistance(X_NA, "correlation", "pearson", TRUE)
  rres <- as.numeric(as.dist(sqrt((1 - cor(X_NA, use = "pairwise.complete.obs", method = "pearson"))/2.0)))
  expect_equal(res, rres, tolerance = 1e-8)

  # abs
  res <- GetDistance(X_NA, "absCorrelation", "pearson", TRUE)
  rres <- as.numeric(as.dist(sqrt((1 - cor(X_NA, use = "pairwise.complete.obs" , method = "pearson")^2))))
  expect_equal(res, rres, tolerance = 1e-8)

})

test_that("Distance (spearman correlation) works", {
  res <- GetDistance(x, "correlation", "spearman", FALSE)
  rres <- as.numeric(as.dist(sqrt((1 - cor(x, method = "spearman"))/2.0)))
  expect_equal(res, rres, tolerance = 1e-8)

  # abs
  res <- GetDistance(x, "absCorrelation", "spearman", FALSE)
  rres <- as.numeric(as.dist(sqrt((1 - cor(x, method = "spearman")^2))))
  expect_equal(res, rres, tolerance = 1e-8)

})

test_that("Distance (spearman correlation) works with NA", {
  res <- GetDistance(X_NA, "correlation", "spearman", TRUE)
  rres <- as.numeric(as.dist(sqrt((1 - cor(X_NA, use = "pairwise.complete.obs", method = "spearman"))/2.0)))
  expect_equal(res, rres, tolerance = 1e-8)

  # abs
  res <- GetDistance(X_NA, "absCorrelation", "spearman", TRUE)
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

    res <- ClusterH(as.numeric(Dist), 7, link)
    rres <- hclust(Dist, rLink)
    expect_equal(res$height, rres$height, tolerance = 1e-8)

  }
})


#test_that("Hierarchical grouping works with NAs", {

#  res <- ClusterHGroup(X_NA,3,0, "correlation", "wAverage", "pearson")

  #TODO

#})

