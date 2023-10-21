
#' Transform and Prepare Data for Analysis
#'
#' This function prepares a data matrix for analysis. It applies a Box-Cox transformation to the endogenous variables, adds an intercept column, and optionally includes new rows with exogenous data.
#'
#' @param data A data.frame or a numeric matrix that serves as the primary data source.
#' @param endogenous A single number indicating the number of endogenous variables in the first columns, or a list of names specifying the endogenous variables. The remaining variables will be treated as exogenous.
#' @param equations A formula or a list of formula objects that represent the equations to be used instead of \code{endogenous}. If provided, the final data will be a matrix where the response variables are in the first columns and the predictor variables are in the subsequent columns.
#' @param lambdas A numeric vector, a single number, NA, or NULL indicating the lambda parameter(s) for the Box-Cox transformation. Use \code{NULL} for no transformation, \code{NA} for estimating the lambda parameter for each variable, a single number for an equal lambda parameter for all variables, and a numeric vector for distinct lambda parameters for corresponding variables.
#' @param newData A data.frame or a numeric matrix representing new data for exogenous variables. It should have a structure similar to \code{data}, excluding endogenous or response variables.
#' @param addIntercept A logical value indicating whether to add an intercept column to the final matrix.
#' @param weights A numeric vector or a column matrix representing weights of observations. Not all applications implement this parameter.
#' @param ... Additional parameters for the \code{MASS::boxcox} function.
#'
#' @return A list suitable for use in \code{ldt::search.?} functions. The list contains:
#' \item{data}{The final data matrix. Endogenous variables are in the first columns, followed by weights (if provided), then the intercept (if added), and finally the exogenous variables.}
#' \item{numEndo}{The number of endogenous variables in the data.}
#' \item{numExo}{The number of exogenous variables in the data (including 'intercept' if it is added).}
#' \item{obsCount}{The number of observations in the original data.}
#' \item{newX}{The matrix of new observations for exogenous variables.}
#' \item{newObsCount}{The number of observations in the new data.}
#' \item{lambdas}{The lambda parameters used in the Box-Cox transformation.}
#' \item{hasIntercept}{Indicates whether an intercept column is added to the final matrix.}
#' \item{hasWeight}{Indicates whether there is a weight column in the final matrix.}
#' \item{startFrequency}{Frequency of the first observation, extracted from \code{ldtf} attribute of \code{data}, if available. This will be used in time-series analysis such as VARMA estimation.}
#'
#' @details
#' This function is designed to prepare a data matrix for model search (or screening) analysis. It performs several operations to transform and structure the data appropriately.
#'
#' The function first checks if the input data is a matrix or a data frame. If new data is provided, it also checks its type. It then extracts the frequency of the first observation from the \code{ldtf} attribute of the data, if available.
#'
#' If no equations are provided, the function assumes that the endogenous variables are in the first columns of the data. It checks if an intercept is already present and throws an error if one is found and \code{addIntercept} is set to TRUE. It then validates the number of endogenous variables and converts the data to a numeric matrix.
#'
#' If column names are missing, they are added based on the number of endogenous and exogenous variables. If new data is provided, it checks its structure and matches it with the exogenous part of the original data.
#'
#' If equations are provided, they are used to transform the original data into a matrix where response variables are in the first columns and predictor variables in subsequent columns. The new data is also transformed accordingly.
#'
#' The function then applies a Box-Cox transformation to the endogenous variables if lambda parameters are provided. Weights are added if provided, and an intercept column is added if \code{addIntercept} is set to TRUE.
#'
#' Finally, the function returns a list containing all relevant information for further analysis. This includes the final data matrix, number of endogenous and exogenous variables, number of observations in original and new data, lambda parameters used in Box-Cox transformation, and flags indicating whether an intercept or weights were added.
#'
#'
#' @examples
#' # Example 1:
#' data <- matrix(1:24, ncol = 6)
#' result <- get.data(data, endogenous = 1)
#' print(result$data)
#'
#' # Example 2:
#' data <- matrix(1:24, ncol = 6,
#'                dimnames = list(NULL,c("V1", "V2", "V3", "V4", "V5", "V6")))
#' result <- get.data(data, endogenous = c("V6", "V1"))
#' print(result$data)
#'
#' # Example 3:
#' data <- data.frame(matrix(1:24, ncol = 6))
#' colnames(data) <- c("X1", "X2", "Y2", "X3", "Y1", "X4")
#' equations <- list(
#'    Y1 ~ X2 + X1,
#'    Y2 ~ X4 + X3)
#' result <- get.data(data, equations = equations)
#' print(result$data)
#'
#' @export
get.data <- function(data, endogenous = 1, equations = NULL,
                     weights = NULL, lambdas = NULL, newData = NULL,
                     addIntercept = TRUE, ...) {

  stopifnot(is.matrix(data) || is.data.frame(data))
  if (!is.null(newData))
    stopifnot(is.matrix(newData) || is.data.frame(newData))
  startFreq <- attr(data, "ldtf")
  if (is.null(equations)) { # use endogenous
    data <- as.matrix(data)
    if (ncol(data) > 2 && get.data.check.intercept(data[,2:ncol(data)]) != -1 && addIntercept)
      stop("Data already contains intercept. 'addIntercept' argument cannot be TRUE.")

    stopifnot(is.number(endogenous) || is.character(endogenous))
    numEndo <- ifelse(is.numeric(endogenous), as.integer(endogenous), length(endogenous))
    if (numEndo == 0)
      stop("Number of endogenous variables cannot be zero.")
    if (numEndo > ncol(data))
      stop("Invalid number of endogenous variables. It is larger than the number of columns in data.")
    numExo <- ncol(data) - numEndo

    # make sure it is a numeric matrix:
    data <- matrix(as.numeric(data), ncol = ncol(data), dimnames = list(NULL, colnames(data)))

    # column names might be missing:
    if (any(nchar(colnames(data)) == 0))
      stop("data has columns with empty names.")
    if (is.null(colnames(data))) {
      if (is.character(endogenous))
        stop("data has no column name while endogenous is a list of names.")

      colnames(data) <- c(paste0("Y", 1:numEndo), paste0("X", 1:numExo))
      if (numExo > 0){
        if (!is.null(newData)){
          if (ncol(newData) != numExo)
            stop("newData does not have the same number of columns as the exogenous part of data.")
          colnames(newData) <- paste0("X", 1:numExo) # override column names (?!)
        } # else, newData is null
      } # there is no exogenous variable
    } # else, data has column names
    else{
      if (is.null(newData) == FALSE && is.null(colnames(newData)))
        stop("data has column names, while column names in newData is missing")
    }
    if (is.character(endogenous)){ # reorder columns
      idx <- match(endogenous, colnames(data))
      data <- data[, c(idx, setdiff(seq_len(ncol(data)), idx))]
    }

    if (addIntercept){
      if (numEndo == ncol(data))
        data <- cbind(data[,1:numEndo,drop=FALSE],rep(1,nrow(data)))
      else
        data <- cbind(data[,1:numEndo,drop=FALSE],rep(1,nrow(data)), data[,(numEndo+1):ncol(data),drop=FALSE])
      colnames(data)[numEndo + 1] <- "(Intercept)"
      if (!is.null(newData)){
        newData <- cbind(rep(1,nrow(newData)), newData)
        colnames(newData)[1] <- "(Intercept)"
      }
      numExo = numExo + 1
    }

  }
  else{

    data0 <- eqList2Matrix(equations, data, addIntercept)
    data <- data0$result
    numEndo <- data0$numResponse
    if (numEndo == 0)
      stop("Number of endogenous variables cannot be zero.")
    numExo <- ncol(data) - numEndo # it counts the intercept

    # deal with newData
    if (!is.null(newData)){
      newData <- as.data.frame(newData)
      # add endogenous data if missing
      responseNames <- colnames(data)[1:numEndo]
      for (name in responseNames) {
        if (!name %in% names(newData))
          newData[[name]] <- 1.0 # we will ignore this part
      }
      newData <- tryCatch(eqList2Matrix(equations, newData, addIntercept)$result,
                          error = function(err){
                            stop("Compared with the exogenous variables, newData either lacks some necessary columns or does not have the correct column names. Additional information: ", err)
                          })
    }
  }

  # make sure data and newData are numeric matrices
  data <- matrix(as.numeric(as.matrix(data)), ncol = ncol(data), dimnames = list(NULL, colnames(data)))
  if (!is.null(newData))
    newData <- matrix(as.numeric(as.matrix(newData)), ncol = ncol(newData), dimnames = list(NULL, colnames(newData)))
  obsCount <- nrow(data)

  if (!is.null(lambdas)) {
    bcRes <- boxCoxTransform(data[, 1:numEndo,drop=FALSE], lambdas, ...)
    data[, 1:numEndo] <- bcRes$data
    lambdas <- bcRes$lambda
  }

  if(!is.null(weights)){
    if ("(Weights)" %in% colnames(data))
      stop("Invalid column name in 'data'. '(Weights)' is reserved.")
    stopifnot(all(weights != 0))
    stopifnot(is.numeric(weights) || (is.matrix(weights) && ncol(weights) == 1))
    stopifnot(length(weights) == nrow(data))
    data <- cbind(data[,1:numEndo,drop=FALSE], weights, data[,(numEndo+1):ncol(data),drop=FALSE])
    colnames(data)[numEndo+1] <- "(Weights)"
  }

  if (!is.null(newData)) {
    strt <- (numEndo+1+ifelse(is.null(weights),0,1))
    if (strt <= ncol(data)){
      exoNames <- colnames(data[, strt:ncol(data),drop=FALSE])
      if (is.null(colnames(newData)) ||
          !all(exoNames %in% colnames(newData)))
        stop("Error:Compared with the exogenous variables, newData either lacks some necessary columns or does not have the correct column names.")
      newData <- newData[,exoNames, drop=FALSE] # reorder and filter columns
    }
    else
      newData <- NULL
  }

  res <- list(data = data,
              numEndo = numEndo,
              numExo = numExo,
              obsCount = obsCount,
              newX = newData, # it contains just exogenous data, if not NULL
              newObsCount = ifelse(is.null(newData),0, nrow(newData)),
              lambdas = lambdas,
              hasIntercept = addIntercept,
              hasWeight = !is.null(weights),
              startFrequency = startFreq)
  class(res) <- c("ldt.search.data", "ldt.list", "list")
  return(res)
}

#' Append \code{newX} to \code{data$data} matrix.
#'
#' Use it for VARMA estimation
#'
#' @param data The output of [get.data] function.
#' @param maxHorizon Number of expected new data
#'
#' @return The input \code{data} with updated data matrix
get.data.append.newX <- function(data, maxHorizon = NA){
  added <- attr(data, "ldt.new.appended")
  if (!is.null(added) && added)
    return(data)

  if (is.null(data$newX)){
    if (is.na(maxHorizon)){ }
    else if (data$hasIntercept && data$numExo == 1){
      new_rows <- cbind(matrix(NA,
                               ncol = data$numEndo + ifelse(data$hasWeight,1,0),
                               nrow = maxHorizon), rep(1,maxHorizon))
      colnames(new_rows) <- colnames(data$data)
      data$data <- rbind(data$data, new_rows)
    }
    else if (maxHorizon > 0 && data$numExo != 0)
      stop("Number of new data points (=0) is less than the required maximum horizon (=", maxHorizon,").")
  }
  else{
    if (!is.na(maxHorizon) && maxHorizon > 0 && nrow(data$newX) < maxHorizon)
      stop("Number of new data points (=", nrow(data$newX)
           ,") is less than the required maximum horizon (=", maxHorizon,").")

    new_rows <- cbind(matrix(NA,
                             ncol = data$numEndo + ifelse(data$hasWeight,1,0),
                             nrow = nrow(data$newX)), data$newX)
    colnames(new_rows) <- colnames(data$data)
    data$data <- rbind(data$data, new_rows)
  }

  attr(data, "ldt.new.appended") <- TRUE
  return(data)
}

#' Remove Rows with Missing Observations from Data
#'
#' @param data Output of [get.data] function
#' @param warn If true,  warning message about the indices of the removed rows is shown
#'
#' @return The input \code{data} but with updated \code{data$data} and \code{data$obsCount}
get.data.keep.complete <- function(data, warn = TRUE){
  incomplete_rows <- which(apply(data$data, 1, function(x) any(is.na(x) | is.nan(x))))

  if (length(incomplete_rows) > 0) {
    if (warn)
      warning(paste("Removed rows with missing observations: ", paste(incomplete_rows, collapse = ", ")))
    data$data <- data$data[-incomplete_rows, ]
    data$obsCount <- nrow(data$data)
  }
  data
}

#' Check if a column is discrete
#'
#' For example, it checks if the endogenous variable in binary model is 0 and 1 (number of choices is 2)
#'
#' @param data Output of [get.data] function
#' @param colIndex The index of column to be checked.
#'
#' @return Number of choices in the model, if no error occured.
#' This means, the maximum value for the discrete data will be the output minus one.
get.data.check.discrete <- function(data, colIndex = 1){
  column <- data$data[, colIndex]
  k <- as.integer(max(column))
  if (all(floor(column) == column)) {
    if (all(column >= 0)) {
      return(k+1)
    }
  }
  stop("First column of data must contain discrete numbers with minimum of zero.")
}

#' Check for an intercept in a matrix
#'
#' This function checks if any column in the matrix is intercept.
#' @param matrix data matrix
#'
#' @return The index of the intercept. '-1' in intercept is not found.
#'
get.data.check.intercept <- function(matrix) {
  for (i in seq_len(ncol(matrix))) {
    if (all(matrix[, i] == 1))
      return(i)
  }
  return(-1)
}


#' Convert a List of Equations to a Matrix
#'
#' This function takes a list of equations and a data frame, and returns a matrix where the response variables are in the first columns, and the predictor variables are in the subsequent columns.
#'
#' The function checks for duplicate response variables across equations and throws an error if any are found. It also ensures that predictor variables are not duplicated in the final matrix.
#'
#' @param equations A formula or a list of formula objects representing the equations.
#' @param data A matrix or a data frame containing the variables in the equations.
#' @param addIntercept Logical. If \code{TRUE}, an intercept column is added after the response variables. Default is \code{FALSE}.
#'
#' @return
#' \item{result}{A matrix with response variables in the first columns, predictor variables in subsequent columns, and optionally an intercept column. The matrix does not contain duplicate columns.}
#' \item{numResponse}{Number of response variables in the first columns.}
#'
#' @examples
#' data <- data.frame(income = c(50000, 60000, 80000, 85000, 65000),
#'                    age = c(25, 32, 47, 51, 36),
#'                    education = c(16, 18, 20, 20, 16),
#'                    savings = c(20000, 25000, 30000, 35000, 40000))
#' equations <- list(as.formula("income ~ age + education"),
#'                   as.formula("savings ~ age + education"))
#' matrix_data <- ldt:::eqList2Matrix(equations, data, addIntercept = TRUE)
#' print(matrix_data)
#'
#' @importFrom stats model.matrix model.response model.frame
eqList2Matrix <- function(equations, data, addIntercept = FALSE) {

  stopifnot(inherits(equations, "formula") || is.list(equations))
  if (inherits(equations, "formula"))
    equations <- list(equations)
  for(eq. in equations)
    stopifnot(inherits(eq., "formula"))
  stopifnot(is.data.frame(data) || is.matrix(data))
  stopifnot(is.logical(addIntercept))

  data <- as.data.frame(data)

  res <- data.frame() # storage
  response_vars <- c() # names of response variables

  for (eq in equations) {
    rv <- all.vars(eq)[1] # response variable
    if (rv %in% response_vars)
      stop("Duplicate response variable '", rv, "' found in equations.")
    response_vars <- c(response_vars, rv)

    model_mat <- model.matrix(eq, data)
    if ("(Intercept)" %in% colnames(model_mat)) # remove intercept
      model_mat <- model_mat[, colnames(model_mat) != "(Intercept)", drop = FALSE]

    # Add the response variable as a column of the matrix
    rvdata <- model.response(model.frame(eq, data))
    model_mat <- cbind(rvdata, model_mat)
    colnames(model_mat)[1] <- rv

    # Add the predictor variables to the matrix, excluding those already present
    newvars <- setdiff(colnames(model_mat), colnames(res))
    if (nrow(res) == 0) {
      res <- model_mat
    } else {
      res <- cbind(res, model_mat[, newvars, drop = FALSE])
    }
  }

  res <- res[, c(response_vars, setdiff(colnames(res), response_vars))]

  if (addIntercept) {
    intercept <- rep(1, nrow(res))
    res <- cbind(res[,1:length(response_vars), drop = FALSE], intercept, res[,-(1:length(response_vars)), drop = FALSE])
    colnames(res)[length(response_vars)+1] <- "(Intercept)"
  }

  return(list(result = res, numResponse = length(response_vars)))
}

#' Box-Cox Transformation of Numeric Matrix
#'
#' This function applies the Box-Cox transformation to the columns of a numeric matrix.
#'
#' @param data A numeric matrix to be transformed.
#' @param lambda A numeric vector, a single number, NA, or NULL indicating the lambda parameter(s) for the Box-Cox transformation.
#' Use \code{NULL} for no transformation, \code{NA} for estimating the lambda parameter for each variable,
#' a single number for equal lambda parameter for all variables, and a numeric vector for distinct lambda parameters for the corresponding variables.
#' @param ... additional parameters for \code{MASS::boxcox} function.
#'
#' @return
#' \item{data}{transformed data}
#' \item{lambda}{final lambda vector used in the calculations.}
#'
#' @examples
#' data <- matrix(rnorm(40), ncol = 2)
#' result <- ldt:::boxCoxTransform(data, c(0.5, 0.5))
#'
#' @importFrom MASS boxcox
boxCoxTransform <- function(data, lambda, ...) {

  if (!is.matrix(data) || !is.numeric(data))
    stop("Data should be a numeric matrix")

  if (!is.null(lambda) && !is.numeric(lambda) && length(lambda) != 1 && length(lambda) != ncol(data))
    stop("lambda should be NULL, NA, a single number, or a vector of numbers with length equal to 'ncol(data)'.")

  if (is.null(lambda))
    return(list(data = data, lambda = NULL))

  lambda_used <- numeric(ncol(data))
  res <- data

  for (i in 1:ncol(data)) {
    if ((length(lambda) == 1 && is.na(lambda)) ||
        (length(lambda) == ncol(data) && is.na(lambda[i]))) { # estimate
      model <- boxcox(data[,i]~1, plotit = FALSE, ...)
      li <- model$x[which.max(model$y)]
    }
    else if (length(lambda) == 1)
      li <- lambda
    else if (length(lambda) == ncol(data))
      li <- lambda[i]
    else
      stop("Invalid lambda argument")

    lambda_used[i] <- li

    if (li == 0)
      res[,i] <- log(data[,i])
    else
      res[,i] <- (data[,i]^li - 1) / li
  }

  return(list(data = res, lambda = lambda_used))
}



#' Define Combinations for Search Process
#'
#' This function defines a structure for a two-level nested loop used in a model search (or screening) process. The outer loop is defined by a vector of sizes and all the combinations of the variables are generated automatically. The inner loop is defined by a list of predefined combinations of the variables. Each variable can belong to either endogenous or exogenous variables based on their usage.
#'
#' @param sizes A numeric vector or a list of numeric vectors that determines the sizes of outer loop combinations. For example, if the outer loop belongs to the endogenous variables, \code{c(1, 2)} means all models with 1 and 2 equations. If the outer loop belongs to exogenous variables, \code{c(1,2)} means all regressions with 1 and 2 exogenous variables. It can also be a list of numeric vectors for step-wise search. Each vector determines the size of the models in a step. In the next step, a subset of potential variables is selected by using \code{stepsNumVariables} argument.
#' @param partitions A list of numeric vectors or character vectors that partitions the outer loop variables. No model is estimated with two variables from the same partition.
#' @param numFixPartitions A single number that determines the number of partitions at the beginning of \code{partitions} to be included in all models.
#' @param innerGroups A list of numeric vectors or character vectors that determines different combinations of the variables for the inner loop. For example, if the inner loop belongs to exogenous data, \code{list(c(1), c(1, 2))} means estimating all models with just the first exogenous variable and all models with both first and second exogenous variables.
#' @param numTargets An integer for the number of target variables at the first columns of the data matrix. Results of a search process are specific to these variables. A model is not estimated if it does not contain a target variable.
#' @param stepsNumVariables A numeric vector. If \code{sizes} is a list (i.e., a step-wise search), this vector must be of equal length and determines the number of variables (with best performance) in each step.
#' @param stepsFixedNames A character vector. If \code{sizes} is a list (i.e., a step-wise search), this vector determines the name of variables to be included in all steps.
#' @param stepsSavePre A name for saving and loading progress, if \code{sizes} is a list. Each step's result is saved in a file (name=\code{paste0(stepsSavePre,i)}) where \code{i} is the index of the step.
#'
#' @return A list suitable for use in \code{ldt::search.?} functions. The list contains:
#' \item{sizes}{The sizes of outer loop combinations.}
#' \item{partitions}{The partitions of outer loop variables.}
#' \item{numFixPartitions}{The number of fixed partitions at the beginning.}
#' \item{innerGroups}{Different combinations of variables for inner loop.}
#' \item{numTargets}{The number of target variables at first columns.}
#' \item{stepsNumVariables}{The number of variables in each step for step-wise search.}
#' \item{stepsFixedNames}{The names of fixed variables in each step for step-wise search.}
#' \item{stepsSavePre}{The name for saving and loading progress for step-wise search.}
#'
#' @details
#' The \code{get.combinations} function in the \code{ldt} package uses a two-level nested loop to iterate over different combinations of endogenous and exogenous variables. This is similar to running the following code:
#' \preformatted{
#' for (endo in list(c(1), c(1, 2)))
#'   for (exo in list(c(1), c(1, 2)))
#'     Estimate a model using \code{endo} and \code{exo} indexation
#' }
#' However, predefining both loops is not memory efficient. Therefore, \code{ldt} uses a running algorithm to define the outer loop. It asks for the desired size of endogenous or exogenous variables in the model (i.e., \code{sizes}) and creates the outer groups using all possible combinations of the variables. The \code{partitions} and \code{numFixPartitions} parameters can be used to restrict this set.
#'
#' For the inner loop, you must provide the desired combination of variables (endogenous or exogenous). Given \code{m} as the number of variables, you can generate all possible combinations using the following code:
#' \preformatted{
#' m <- 4
#' combinations <- unlist(lapply(1:m, function(i) {
#'  t(combn(1:m, i, simplify = FALSE))
#' }), recursive = FALSE)
#' }
#' You can use this as the \code{innerGroups} argument. However, this might result in a large model set.
#'
#' Note that in \code{ldt}, if the data matrix does not have column names, default names for the endogenous variables are \code{Y1, Y2, ...}, and default names for the exogenous variables are \code{X1, X2, ...}. See [get.data()] function for more details.
#'
#' Also note that \code{ldt} ensure that all possible models can be estimated with the given number of partitions and sizes. If it's not possible, it will stop with an error message.
#'
#' @examples
#' # Some basic examples are given in this section. However, more practical examples are available
#' # for the \code{search.?} functions.
#'
#' # Example 1:
#' combinations1 <- get.combinations(sizes = c(1, 2))
#' # The function will generate all possible combinations of sizes 1 and 2.
#'
#' # Example 2: Using partitions
#' combinations2 <- get.combinations(sizes = c(1, 2), partitions = list(c(1, 2), c(3, 4)))
#'
#' # Here, we're specifying partitions for the variables.
#' # The function will generate combinations such that no model is estimated with two variables
#' # from the same partition.
#'
#' # Example 3: Specifying inner groups
#' combinations3 <- get.combinations(sizes = c(1, 2), innerGroups = list(c(1), c(1, 2)))
#'
#' # In this example, we're specifying different combinations of variables for the inner loop.
#' # For instance, \code{list(c(1), c(1, 2))} means estimating all models with just the first
#' # variable and all models with both first and second variables.
#'
#' # Example 4: Step-wise search
#' combinations4 <- get.combinations(sizes = list(c(1), c(1, 2)), stepsNumVariables = c(NA, 1))
#'
#' # This example demonstrates a step-wise search. In the first step (\code{sizes = c(1)}), all
#' # models with one variable are estimated.
#' # In the next step (\code{sizes = c(1, 2)}), a subset of potential variables is selected based
#' # on their performance in the previous step and all models with both first and second variables
#' # are estimated.
#'
#' @export
get.combinations <- function(sizes = c(1),
                             partitions = NULL,
                             numFixPartitions = 0,
                             innerGroups = list(c(1)),
                             numTargets = 1,
                             stepsNumVariables = c(NA),
                             stepsFixedNames = NULL,
                             stepsSavePre = NULL){
  stopifnot(is.numeric(sizes) || is.list(sizes))
  if (is.list(sizes)){
    for (size in sizes)
      if (!is.numeric(size) )
        stop("An element in 'sizes' argument is not numeric.")
    stopifnot(is.numeric(stepsNumVariables))
    stopifnot(all(is.na(stepsNumVariables) | stepsNumVariables >= 1))
    stopifnot(is.null(stepsSavePre) || is.character(stepsSavePre))
    if (is.character(stepsSavePre))
      stopifnot(length(stepsSavePre) != 1)
    stopifnot(length(sizes) == length(stepsNumVariables))

    stopifnot(is.null(stepsFixedNames) || is.character(stepsFixedNames))
  }
  stopifnot(is.numeric(numTargets) && length(numTargets) == 1 && numTargets >= 0)

  stopifnot(is.null(partitions) || is.list(partitions))
  if (!is.null(partitions))
    for (partition in partitions)
      stopifnot(is.numeric(partition) || is.character(partition))

  stopifnot(is.numeric(numFixPartitions) && length(numFixPartitions) == 1)

  if (is.null(innerGroups))
    innerGroups = list()
  stopifnot(is.list(innerGroups))
  for (innerGroup in innerGroups)
    stopifnot(is.numeric(innerGroup) || is.character(innerGroup))

  res <- list(sizes = sizes,
              partitions = partitions,
              numFixPartitions = numFixPartitions,
              innerGroups = innerGroups,
              numTargets = numTargets,
              stepsNumVariables = stepsNumVariables,
              stepsFixedNames = stepsFixedNames,
              stepsSavePre = stepsSavePre)
  class(res) <- c("ldt.search.combinations", "ldt.list", "list")
  return(res)
}


#' Get Numeric Indices in a Combination
#'
#' This function takes the output of the [get.combinations] function and a numeric matrix with given column names.
#' It converts all character vectors in \code{innerGroups} or \code{partitions} to numeric vectors based on the index of the columns.
#'
#' @param combinations A list returned by the [get.combinations] function.
#' @param data A list returned by [get.data] function.
#' @param isInnerExogenous Use \code{TRUE} if outer loop is defined over the endogenous variables and \code{FALSE} if it is for exogenous.
#'
#' @return A list similar to the input \code{combinations}, but with all character vectors in \code{innerGroups} or \code{partitions} converted to numeric vectors based on the index of the columns in the \code{data} matrix.
#' It sums the exogenous indexes with the number of endogenous variables and returns zero-based indexation for C code.
#'
get.indexation <- function(combinations, data, isInnerExogenous) {

  stopifnot(is.logical(isInnerExogenous))
  stopifnot(!is.null(data))
  if (!(inherits(data, "ldt.search.data")))
    stop("Invalid class. Use 'get.data()' function to generate 'data'.")
  stopifnot(!is.null(combinations))
  if (!(inherits(combinations, "ldt.search.combinations")))
    stop("Invalid class. Use 'get.combinations()' function to generate 'combinations'.")
  stopifnot(is.matrix(data$data) && is.numeric(data$data) && !is.null(colnames(data$data)))

  dup_cols <- anyDuplicated(colnames(data$data))
  if (dup_cols > 0)
    stop("'data' has duplicate column names.")

  if (isInnerExogenous == FALSE && data$numExo == 0)
    stop("Number of exogenous variables cannot be zero.") #TODO: check for intercept
  # we check for non-zero number of endogenous variables in 'get.data'

  if (combinations$numTargets > data$numEndo)
    stop("Invalid number of target variables (=",combinations$numTargets,
         "). It cannot be larger than the number of endogenous variables (=", data$numEndo, ").")

  colnames_to_index <- function(names, colnames) {
    indices <- match(names, colnames)
    if (any(is.na(indices))) {
      stop("Some column names are not found in the data matrix.")
    }
    return(sort(as.integer(indices)))
  }

  if (is.list(combinations$sizes)) {
    combinations$sizes <- lapply(combinations$sizes, function(x){
      j <- sort(as.integer(x))
      if (any(j < 1))
        stop("Zero or negative index in sizes.")
      if (isInnerExogenous == FALSE && any(j > data$numExo))
        stop("Invalid value in sizes. Make sure they are less than the number of exogenous variables.")
      if (isInnerExogenous && any(j > data$numEndo))
        stop("Invalid index in sizes. Make sure they are less than the number of endogenous variables.")
      j
    })

    combinations$stepsNumVariables <- as.integer(combinations$stepsNumVariables)
    if (any(!is.na(combinations$stepsNumVariables) & combinations$stepsNumVariables < 1))
      stop("Zero or negative value in stepsNumVariables.")
    if (any(!is.na(combinations$stepsNumVariables) & combinations$stepsNumVariables > data$numEndo + data$numExo))
      stop("Invalid value in stepsNumVariables. Make sure they are whether 'NA' or less than the number of variables.")

    if (!is.null(combinations$stepsFixedNames) && length(combinations$stepsFixedNames) != 0){
      if (!any(combinations$stepsFixedNames %in% colnames(data$data)))
        stop("Invalid item in 'stepsFixedNames'. Make sure they are valid names of variables.")
    }
    else
      combinations$stepsFixedNames = character(0)
  }
  else{
    combinations$sizes = sort(as.integer(combinations$sizes))
    if (any(combinations$sizes < 1))
      stop("Zero or negative index in sizes.")
    if (isInnerExogenous == FALSE && any(combinations$sizes > data$numExo))
      stop("Invalid value in sizes. Make sure they are less than the number of exogenous variables.")
    if (isInnerExogenous && any(combinations$sizes > data$numEndo))
      stop("Invalid index in sizes. Make sure they are less than the number of endogenous variables.")
  }


  if (!is.null(combinations$partitions)) {
    combinations$partitions <-
      lapply(combinations$partitions,
             function(x){
               j <- if (is.character(x)) {colnames_to_index(x, colnames(data$data))}
               else {sort(as.integer(x))}
               if (any(j) < 1)
                 stop("Zero or negative index in partitions.")
               if (isInnerExogenous && any(j > data$numEndo))
                 stop("Invalid index in partitions(=", j,"). Make sure they are less than the number of endogenous variables (=", data$numEndo, ").")
               if (isInnerExogenous == FALSE && any(j > data$numExo))
                 stop("Invalid index in partitions (=", j,"). Make sure they are less than the number of exogenous variables (=", data$numExo, ").")
               j
             })
  }
  else{ # put each variable in its own partition
    combinations$partitions <-
      lapply(1:ifelse(isInnerExogenous, data$numEndo, data$numExo),
             function(x)c(x))
  }

  combinations$innerGroups <-
    lapply(combinations$innerGroups,
           function(x){
             j <- if (is.character(x))
             {colnames_to_index(x, colnames(data$data))}
             else
             {sort(as.integer(x))}
             if (any(j < 1))
               stop("Zero or negative index in innerGroups.")
             if (isInnerExogenous && any(j > data$numExo))
               stop("Invalid index in innerGroups. Make sure they are less than the number of exogenous variables.")
             if (isInnerExogenous  == FALSE && any(j > data$numEndo))
               stop("Invalid index in innerGroups. Make sure they are less than the number of endogenous variables.")
             j
           })


  combinations$innerGroups <- unique(combinations$innerGroups)

  if (isInnerExogenous == FALSE){ # inner is endogenous
    inner_group_values <- unique(unlist(combinations$innerGroups))
    for (t in 1:combinations$numTargets)
      if (!(t %in% inner_group_values))
        stop("Invalid 'innerGroups' argument in 'combinations'. A target variable (index=",t,") is not reported in this list.")
  }


  # zero-based indexation and moving exogenous indices:
  for (i in seq_along(combinations$partitions)){
    combinations$partitions[[i]] <- combinations$partitions[[i]] - 1
    if (isInnerExogenous == FALSE)
      combinations$partitions[[i]] <- combinations$partitions[[i]] + data$numEndo
  }
  for (i in seq_along(combinations$innerGroups)){
    combinations$innerGroups[[i]] <- combinations$innerGroups[[i]] - 1
    if (isInnerExogenous)
      combinations$innerGroups[[i]] <- combinations$innerGroups[[i]] + data$numEndo
  }

  # finally, sort and check for intersection values
  for(i in 1:length(combinations$partitions)) {
    for(j in i:length(combinations$partitions)) {
      if(i != j) {
        commons <- intersect(combinations$partitions[[i]], combinations$partitions[[j]])
        if(length(commons) > 0)
          stop("At least two members of 'combinations$partitions' have common elements.")
      }
    }
  }
  combinations$partitions <- lapply(combinations$partitions, sort)

  # sort inner groups
  # (it's OK for inner groups to have common elements, unless some partitions are fixed (TODO))
  combinations$innerGroups <- lapply(combinations$innerGroups, sort)
  if (isInnerExogenous == FALSE)
    for(i in combinations$innerGroups){
      if (i[1] >= combinations$numTargets)
        stop("At least one item in an inner group (index=",i,") contains no target variable.")
    }

  for (size in unlist(combinations$sizes))
    if (size > length(combinations$partitions))
      stop("Invalid number of partitions. With the given number of partitions (=", length(combinations$partitions), "), it is not possible to estimate a model with size =",size,".")

  return(combinations)
}

