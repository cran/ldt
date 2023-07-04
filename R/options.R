
#' Get Options for ROC and AUC Calculations
#'
#' Use this function to get the required options for [search.bin], [estim.bin], or [s.roc] functions.
#'
#' @param lowerThreshold A number representing the lower bound for calculating partial AUC.
#' @param upperThreshold A number representing the upper bound for calculating partial AUC.
#' @param epsilon A small number used to ignore small floating point differences when comparing scores.
#' @param pessimistic If \code{TRUE}, sequences of equally scored instances are treated differently and a pessimistic metric is calculated (see Fawcett (2006) An introduction to ROC analysis, fig. 6).
#' @param costs The cost of each observation. If \code{NULL}, the cost of all observations will be 1.
#' @param costMatrix A \code{2x2} cost matrix in which: (1,1) is the cost of TN,
#' (2,2) is the cost of TP, (1,2) is the cost of FP and (2,1) is the cost of FN. The first
#' column is multiplied by the corresponding value in the costs vector (see
#' Fawcett (2006), ROC graphs with instance-varying costs).
#'
#' @details
#' See details of [s.roc] function.
#'
#' @return A list with the given options.
#' @examples
#' # See 's.roc' function.
#'
#'
#' @export
#' @seealso [search.bin], [estim.bin], [s.roc]
get.options.roc <- function(lowerThreshold = 0, upperThreshold = 1, epsilon = 1e-12,
                            pessimistic = FALSE, costs = NULL, costMatrix = NULL){

  O = list(lowerThreshold = lowerThreshold, upperThreshold = upperThreshold,
           epsilon=epsilon, pessimistic = pessimistic, costs=costs,
           costMatrix = costMatrix)
  O = CheckRocOptions(O)
  O
}

CheckRocOptions <- function(O){
  O$lowerThreshold = as.numeric(O$lowerThreshold);
  O$upperThreshold = as.numeric(O$upperThreshold);
  O$epsilon = as.numeric(O$epsilon);
  O$pessimistic = as.logical(O$pessimistic);

  if (O$lowerThreshold < 0)
    stop("Invalid ROC option. 'lowerThreshold' cannot be negative.")
  if (O$upperThreshold > 1)
    stop("Invalid ROC option. 'upperThreshold' cannot be larger than 1.")
  if (O$epsilon < 0 || O$epsilon > 1)
    stop("Invalid ROC option. 'epsilon' must be positive and less than 1.")

  if (is.null(O$costs) == FALSE) {
    O$costs = as.numeric(O$costs)
    O$costMatrix = as.matrix(O$costMatrix)

    if (is.null(O["costMatrix"]))
      stop("Invalid ROC option. 'costMatrix' cannot be NULL when there is a cost vector.")
    if (nrow(O$costMatrix) != 2 || ncol(O$costMatrix) != 2)
      stop("Invalid ROC option. 'costMatrix' must be a 2x2 matrix.")
  }
  O
}

#' Options for Nelder-Mead Optimization
#'
#' Use this function to get the required options when Nelder-Mead optimization is needed such as [s.gld.from.moments] function.
#'
#' @param maxIterations (int) Maximum number of iterations.
#' @param tolerance A small number to determine the convergence.
#' The algorithm terminates when the difference between the best and worst points in the simplex is less than this value.
#' @param reflection A number for reflection coefficient.
#' It controls how far the worst point is reflected through the centroid of the remaining points.
#' @param expansion A number that determines the expansion coefficient.
#' It controls how far the reflected point is expanded along the line connecting it to the centroid.
#' @param contraction A number that determines the contraction coefficient.
#' It controls how far the worst point is contracted towards the centroid.
#' @param shrink A number that determines the shrink coefficient.
#' It controls how much the simplex is shrunk towards the best point when all other moves are rejected.
#'
#' @return A list with the given options.
#' @export
get.options.neldermead <- function(maxIterations = 100, tolerance = 1e-6,
                                   reflection = 1, expansion = 2, contraction = 0.5,
                                   shrink = 1){
  O = list(maxIterations = maxIterations,
           tolerance = tolerance, reflection = reflection,
           expansion = expansion, contraction = contraction,
           shrink = shrink)
  O = CheckNelderMeadOptions(O)
  O
}


CheckNelderMeadOptions <- function(O){
  O$maxIterations <- as.integer(O$maxIterations)
  O$tolerance <- as.numeric(O$tolerance)
  O$reflection <- as.numeric(O$reflection)
  O$expansion <- as.numeric(O$expansion)
  O$contraction <- as.numeric(O$contraction)
  O$shrink <- as.numeric(O$shrink)


  if (O$maxIterations <= 0)
    stop("Invalid Nelder-Mead option: 'maxIterations' must be positive.")

  if (O$tolerance < 0)
    stop("Invalid Nelder-Mead option: 'tolerance' cannot be negative.")

  if (O$reflection <= 0)
    stop("Invalid Nelder-Mead option: 'reflection' must be positive.")

  if (O$expansion <= 0)
    stop("Invalid Nelder-Mead option: 'expansion' must be positive.")

  if (O$contraction <= 0)
    stop("Invalid Nelder-Mead option: 'contraction' must be positive.")

  if (O$shrink <= 0)
    stop("Invalid Nelder-Mead option: 'shrink' must be positive.")

  O
}



#' Get Options for PCA
#'
#' Use this function to get PCA options in [estim.bin], [estim.sur], [estim.varma], or [s.pca] functions.
#'
#' @param ignoreFirst A number representing the number of variables to exclude at the beginning of data matrices (such as intercept) from PCA.
#' @param exactCount A number that determines the number of components to be used. If zero, the number of components is determined by the \code{cutoffRate}.
#' @param cutoffRate A number between 0 and 1 that determines the cutoff rate for the cumulative variance ratio in order to determine the number of PCA components. It is not used if \code{exactCount} is positive.
#' @param max A number representing the maximum number of components when \code{cutoffRate} is used.
#'
#' @details
#' See details of [s.pca] function.
#'
#' @return A list with the given options.
#' @examples
#' # See 's.pca' function.
#'
#' @export
#' @seealso [estim.bin], [estim.sur], [estim.varma], [s.pca]
get.options.pca <- function(ignoreFirst = 1, exactCount = 0, cutoffRate = 0.8, max = 1000){
  O = list(
    ignoreFirst = ignoreFirst, exactCount = exactCount,
    cutoffRate = cutoffRate, max = max)
  O = CheckPcaOptions(O)
  O
}

CheckPcaOptions <- function(O){
  O$ignoreFirst <- as.integer(O$ignoreFirst)
  O$exactCount <- as.integer(O$exactCount)
  O$cutoffRate <- as.numeric(O$cutoffRate)
  O$max <- as.integer(O$max)

  if (O$ignoreFirst < 0)
    stop("Invalid Pca option. 'ignoreFirst' cannot be negative.")

  if (O$exactCount < 0)
    stop("Invalid Pca option. 'exactCount' cannot be negative.")

  if (O$cutoffRate <= 0 || O$cutoffRate > 1)
    stop("Invalid Pca option. 'cutoffRate' must be positive and less than 1.")

  if (O$max <= 0)
    stop("Invalid Pca option. 'max' must be positive.")

  O
}


#' Get Options for L-BFGS Optimization
#'
#' Use this function to get optimization options in [estim.varma] or [search.varma] functions.
#'
#' @param maxIterations A positive integer representing the maximum number of iterations.
#' @param factor A number that determines the condition for stopping the iterations. Use, for example, 1e12 for low accuracy, 1e7 (default) for moderate accuracy, and 1e1 for extremely high accuracy. The default is 1e7.
#' @param projectedGradientTol A number used to stop the iteration using the projected gradient. The default is zero.
#' @param maxCorrections The maximum number of variable metric corrections allowed in the limited memory matrix. The default is 5.
#'
#' @return A list with the given options.
#'
#' @export
get.options.lbfgs <- function(maxIterations = 100, factor = 1e7,
                               projectedGradientTol = 0, maxCorrections = 5){
  O = list(maxIterations = maxIterations,
           factor = factor,
           projectedGradientTol = projectedGradientTol,
           maxCorrections = maxCorrections)
  O = CheckLbfgsOptions(O)
  O
}


CheckLbfgsOptions <- function(O){
  O$maxIterations <- as.integer(O$maxIterations)
  O$factor <- as.numeric(O$factor)
  O$projectedGradientTol <- as.numeric(O$projectedGradientTol)
  O$maxCorrections <- as.integer(O$maxCorrections)


  if (O$maxIterations <= 0)
    stop("Invalid L-BFGS option. 'maxIterations' must be positive.")

  if (O$factor <= 0)
    stop("Invalid L-BFGS option. 'factor' must be positive.")

  if (O$projectedGradientTol < 0)
    stop("Invalid L-BFGS option. 'projectedGradientTol' cannot be negative.")

  if (O$maxCorrections <= 0)
    stop("Invalid L-BFGS option. 'maxCorrections' must be positive.")

  O
}


#' Get Options for Newton Optimization
#'
#' Use this function to get optimization options in [estim.bin] or [search.bin] functions.
#'
#' @param maxIterations An integer representing the maximum number of iterations.
#' @param functionTol A small value used to test the convergence of the objective function.
#' @param gradientTol A small value used to test the convergence of the gradient.
#' @param useLineSearch If \code{TRUE}, line search is used.
#'
#' @return A list with the given options.
#'
#' @export
get.options.newton <- function(maxIterations = 100, functionTol = 1e-4,
                               gradientTol = 0, useLineSearch = TRUE){
  O = list(maxIterations = maxIterations,
           functionTol = functionTol,
           gradientTol = gradientTol,
           useLineSearch = useLineSearch)
  O = CheckNewtonOptions(O)
  O
}


CheckNewtonOptions <- function(O){
  O$maxIterations <- as.integer(O$maxIterations)
  O$functionTol <- as.numeric(O$functionTol)
  O$gradientTol <- as.numeric(O$gradientTol)
  O$useLineSearch <- as.logical(O$useLineSearch)

  if (O$maxIterations <= 0)
    stop("Invalid Newton option. 'maxIterations' must be positive.")

  if (O$functionTol < 0)
    stop("Invalid Newton option. 'functionTol' cannot be negative.")

  if (O$gradientTol < 0)
    stop("Invalid Newton option. 'gradientTol' cannot be negative.")

  O
}


#' Specify the Purpose of the Model Search Process
#'
#' Use this function to list the required items and information that should be saved and retrieved from the model set search process in \code{search.?} functions.
#'
#' @param model If \code{TRUE}, some information about the models is saved.
#' @param type1 If \code{TRUE} and implemented, extra information is saved. This can be the coefficients in the SUR search or predictions in the VARMA search.
#' @param type2 If \code{TRUE} and implemented, extra information is saved. This is similar to \code{type1}. **It is reserved for future updates.**
#' @param bestK The number of best items to be saved in \code{model}, \code{type1}, or \code{type2} information.
#' @param all If \code{TRUE}, all models' information is saved.
#' @param inclusion If \code{TRUE}, inclusion weights are saved.
#' @param cdfs Weighted average of the CDFs at each given point is calculated (for \code{type1} and \code{type2} cases).
#' @param extremeMultiplier A number that determines the multiplier in the extreme bound analysis (for \code{type1} and \code{type2} cases). Use zero to disable it.
#' @param mixture4 If \code{TRUE}, the first four moments of the average distributions are calculated in \code{type1} and \code{type2} cases.
#'
#' @return A list with the given options.
#'
#' @export
get.items.search <- function(model = TRUE, type1 = FALSE, type2 = FALSE,
                             bestK = 1, all = FALSE, inclusion = FALSE,
                             cdfs = numeric(0), extremeMultiplier = 0,
                             mixture4 = FALSE){
  O = list(model = model, type1 = type1,
           type2 = type2, bestK = bestK,
           all = all, inclusion = inclusion,
           cdfs = cdfs,
           extremeMultiplier = extremeMultiplier,
           mixture4 = mixture4)
  O = CheckSearchItems(O)
  O
}


CheckSearchItems <- function(O){
  O$model <- as.logical(O$model)
  O$type1 <- as.logical(O$type1)
  O$bestK <- as.integer(O$bestK)
  O$all <- as.logical(O$all)
  O$inclusion <- as.logical(O$inclusion)
  O$cdfs <- as.numeric(O$cdfs)
  O$extremeMultiplier <- as.numeric(O$extremeMultiplier)
  O$mixture4 <- as.logical(O$mixture4)

  if (O$bestK < 0)
    stop("Invalid Search item. 'bestK' cannot be negative.")

  if (O$extremeMultiplier < 0)
    stop("Invalid Search item. 'extremeMultiplier' cannot be negative.")

  O
}


#' Get Extra Options for Model Search Process
#'
#' Use this function to determine how the model search is performed.
#'
#' @param parallel If \code{TRUE}, a parallel search algorithm is used. This generally changes the speed and memory usage.
#' @param reportInterval An integer representing the time interval (in seconds) for reporting progress (if any significant change has occurred). Set to zero to disable reporting.
#' @param printMsg Set to \code{TRUE} to enable printing details.
#'
#' @return A list with the given options.

#'
#' @export
get.options.search <- function(parallel = FALSE, reportInterval = 2,
                               printMsg = FALSE){
  O = list(parallel = parallel,
           reportInterval = reportInterval,
           printMsg = printMsg)
  O = CheckSearchOptions(O)
  O
}

CheckSearchOptions <- function(O){
  if (O$parallel && .SupportsParallel() == FALSE) {
    O$parallel = FALSE
    warning("Warning: 'parallel' option is not available.")
  }
  O$reportInterval = as.integer(O$reportInterval)

  if (O$reportInterval < 0)
    stop("Invalid Search option. 'reportInterval' cannot be negative.")

  O
}


#' Set Options to Exclude a Model Subset
#'
#' Use this function to determine which models should be skipped in the search process.
#'
#' @param estimation If \code{TRUE}, the model is estimated with all data and is ignored if this estimation fails. If \code{FALSE}, you might get a 'best model' that cannot be estimated.
#' @param maxConditionNumber A number used to ignore an estimation that has a high condition number (if implemented in the search).
#' @param minObsCount An integer used to ignore an estimation where the number of observations (after dealing with \code{NA}) is low. Use 0 to disable this check.
#' @param minDof An integer used to ignore an estimation with low degrees of freedom (equation-wise). Use 0 to disable this check.
#' @param minOutSim An integer used to ignore estimations with a low number of out-of-sample simulations (if implemented in the search).
#' @param minR2 A number used to ignore estimations with a low value for 'R2' (if implemented in the search).
#' @param maxAic A number used to ignore estimations with a high 'AIC' (if implemented in the search).
#' @param maxSic A number used to ignore estimations with a high 'SIC' (if implemented in the search).
#' @param prediction If \code{TRUE}, model data is predicted given all data and is ignored if this process fails. If \code{FALSE}, you might get a 'best model' that cannot be used for prediction.
#' @param predictionBoundMultiplier A positive number used to create a bound and check predictions.
#' The bound is created by multiplying this value by the average growth rate of the data.
#' A model is ignored if its prediction lies outside of this bound. Use zero to disable this check.
#'
#' @return A list with the given options.
#'
#' @export
get.items.modelcheck <- function( estimation = TRUE, maxConditionNumber = Inf,
                                  minObsCount = 0, minDof = 0, minOutSim = 0,
                                  minR2 = -Inf, maxAic = Inf, maxSic = Inf,
                                  prediction = FALSE, predictionBoundMultiplier = 4){

  O = list(
    estimation = estimation,
    maxConditionNumber = maxConditionNumber,
    minObsCount = minObsCount, minDof = minDof,
    minOutSim = minOutSim, maxSic = maxSic,
    minR2 = minR2, maxAic = maxAic,
    maxSic = maxSic, prediction = prediction,
    predictionBoundMultiplier = predictionBoundMultiplier)
  O = CheckModelCheckItems(O)
  O
}


CheckModelCheckItems <- function(O){

  O$estimation = as.logical(O$estimation)
  O$maxConditionNumber = as.numeric(O$maxConditionNumber)
  O$minObsCount = as.integer(O$minObsCount)
  O$minDof = as.integer(O$minDof)
  O$minOutSim = as.integer(O$minOutSim)
  O$maxSic = as.numeric(O$maxSic)
  O$minR2 = as.numeric(O$minR2)
  O$maxAic = as.numeric(O$maxAic)
  O$maxSic = as.numeric(O$maxSic)
  O$prediction = as.logical(O$prediction)
  O$predictionBoundMultiplier = as.numeric(O$predictionBoundMultiplier)

  if (O$minObsCount < 0)
    stop("Invalid model-Check option. 'minObsCount' cannot be negative.")

  if (O$minDof < 0)
    stop("Invalid model-Check option. 'minDof' cannot be negative.")

  if (O$minOutSim < 0)
    stop("Invalid model-Check option. 'minOutSim' cannot be negative.")

  if (O$maxConditionNumber < 0)
    stop("Invalid model-Check option. 'maxConditionNumber' cannot be negative.")

  if (O$predictionBoundMultiplier < 0)
    stop("Invalid model-Check option. 'predictionBoundMultiplier' cannot be negative.")

  O
}

#' Get Options for Measuring Performance
#'
#' Use this function to get measuring options in \code{search.?} functions.
#'
#' @param typesIn A list of evaluation metrics when the model is estimated using all available data. It can be \code{aic}, \code{sic}, \code{frequencyCostIn}, \code{brierIn}, or \code{aucIn}. \code{NULL} means no metric.
#' @param typesOut A list of evaluation metrics in a out-of-sample simulation. It can be \code{sign}, \code{direction}, \code{rmse}, \code{rmspe}, \code{mae}, \code{mape}, \code{crps}, \code{frequencyCostOut}, \code{brierOut}, or \code{aucOut}. Null means no metric.
#' @param simFixSize An integer that determines the number of out-of-sample simulations. Use zero to disable the simulation.
#' @param trainFixSize An integer representing the number of data points in the training sample in the out-of-sample simulation. If zero, \code{trainRatio} will be used.
#' @param trainRatio A number representing the size of the training sample relative to the available size, in the out-of-sample simulation. It is effective if \code{trainFixSize} is zero.
#' @param seed A seed for the random number generator. Use zero for a random value. It can be negative to get reproducible results between the \code{search.?} function and the \code{estim.?} function.
#' @param horizons An array of integers representing the prediction horizons to be used in out-of-sample simulations, if the model supports time-series prediction. If \code{NULL}, \code{c(1)} is used.
#' @param weightedEval If \code{TRUE}, weights are used in evaluating discrete-choice models.
#'
#' @details
#' The following list describe the details of calculating the metrics:
#' \itemize{
#' \item **\code{AIC}**: Akaike Information Criterion which is an estimator of the relative quality of statistical model for a given set of data.
#' Given a statistical model, if \eqn{k} is the number of estimated parameters and \eqn{L} is the maximized value of the likelihood function, then \eqn{AIC = 2k - 2\ln{L}}.
#' Smaller value for AIC means higher quality of the model.
#' We can convert AIC to model weight by calculating relative likelihood which is \eqn{rL = e^{0.5(AIC - min_{aic})}} where \eqn{min_{aic}} is minimum AIC value among all the candidate models. Model weight is \eqn{\frac{rL}{\sum rL}}. Since \eqn{exp(-0.5 * x)} transformation is invariant to translation, we can ignore the \eqn{min_{aic}} part.
#'
#' \item **\code{SIC}**: Schwarz Information Criterion which is similar to AIC.
#' The penalty term is larger in BIC than in AIC for sample sizes greater than 7. This means that BIC tends to favor simpler models than AIC.
#' Its formula as a function of the number of observations (\eqn{n}), number of parameters of the model (\eqn{k}) and the value of maximized likelihood (\eqn{L}) is \eqn{SIC = ln(n)k - 2\ln{L}}.
#' Smaller value for AIC means higher quality of the model.
#' The AIC weight discussion is valid here.
#'
#' \item **\code{brier}**: Brier Score which measures the accuracy of probabilistic predictions for binary outcomes.
#' It is calculated as the mean squared difference between the predicted probabilities and the actual outcomes.
#' Its formula is \eqn{B_{in} = \frac{\sum (y_i-\hat{p}_i)^2}{n}}, where \eqn{y_i} are the actual values, \eqn{\hat{p}_i} is the predicted probability that \eqn{y_i} is equal to 1, and \eqn{n} is the number of observations.
#' Its values are between 0 and 1. Lower values mean better predictions.
#' In this package, \eqn{e^{-0.5*Brier}} formula is used to convert it to weight.
#'
#' \item **\code{auc}**: Area Under the Receiver Operating Characteristic Curve (AUC-ROC) which measures the performance of a binary classification model.
#' In ROC we plot the true positive rate (sensitivity) against the false positive rate (1-specificity) at different classification thresholds. AUC is the area under the resulting curve.
#' Its value is between 0 and 1.
#' An AUC of 0 indicates that the model is perfectly incorrect, meaning that it always predicts the opposite of the true class.
#' An AUC of 0.5 indicates that the model is no better than random chance at distinguishing between the two classes.
#' An AUC of 1 indicates that the model is able to perfectly distinguish between the two classes.
#' In general, the higher the AUC-ROC, the better the model is at distinguishing between the two classes.
#' In this package, the weight of AUC is its value.
#'
#' \item **\code{sign}**: Sign Prediction Accuracy which reports the proportion of predictions that have the same sign as the actual values.
#' It is calculated as the number of correct sign predictions divided by the total number of predictions.
#' It is between 0 and 1, with higher values indicating better performance of the model.
#' Its value is used as the wight of a model.
#'
#' \item **\code{direction}**: Directional Prediction Accuracy which is calculated as the proportion of predictions that correctly predict the direction of change, relative to the previous observation.
#' It is between 0 and 1, with higher values indicating better performance of the model.
#' Its value is used as the wight of a model.
#'
#' \item **\code{rmse}**: Root Mean Squared Error which is based on the differences between the predicted values and the actual values.
#' Its formula is \eqn{\sqrt{\frac{1}{n}\sum_{i=1}^{n}(\hat{y}_i - y_i)^2}} where \eqn{n} is the number of observations, \eqn{y_i} is the actual values and \eqn{\hat{y}_i} is the prediction.
#' Lower values of RMSE indicate better model fit, with an RMSE of 0 indicating a perfect fit.
#' In this package, \eqn{e^{-0.5*RMSE}} formula is used to convert it to weight.
#'
#' \item **\code{mae}**: Mean Absolute Error which is based on the differences between the predicted values and the actual values.
#' Its formula is \eqn{\frac{1}{n}\sum_{i=1}^{n}|\hat{y}_i - y_i|} where \eqn{n} is the number of observations, \eqn{y_i} is the actual values and \eqn{\hat{y}_i} is the prediction.
#' Lower values of MAE indicate better model fit, with an MAE of 0 indicating a perfect fit.
#' MAE gives less weight to large errors than RMSE. This means that RMSE will be more sensitive to large errors than MAE.
#'
#' \item **\code{mape}**: Mean Absolute Percentage Error which is similar to MAE, but expressed as a percentage of the actual values.
#' Its formula is \eqn{\frac{1}{n}\sum_{i=1}^{n}|\frac{\hat{y}_i - y_i}{y_i}|\times 100} where \eqn{n} is the number of observations, \eqn{y_i} is the actual values and \eqn{\hat{y}_i} is the prediction.
#' MAPE can be used to compare the accuracy of forecasts across different scales, while MAE cannot.
#' Also note that MAE is always well-defined while MAPE is undefined if actual values are zero or negative.
#'
#' \item **\code{rmspe}**: Root Mean Squared Percentage Error which is similar to RMAE, but expressed as a percentage of the actual values.
#' Its formula is \eqn{\sqrt{\frac{1}{n}\sum_{i=1}^{n}(\frac{\hat{y}_i - y_i}{y_i})^2}\times 100} where \eqn{n} is the number of observations, \eqn{y_i} is the actual values and \eqn{\hat{y}_i} is the prediction.
#' RMSPE can be used to compare the accuracy of forecasts across different scales, while RMSE cannot.
#' Also note that RMSE is always well-defined while RMSPE is undefined if actual values are zero or negative.
#' Unlike MAPE, RMSPE gives more weight to large errors.
#'
#' \item **\code{crps}**: Continuous Ranked Probability Score which calculates the accuracy of probabilistic forecasts.
#' It is calculated as the mean squared difference between the predicted CDF and the empirical CDF of the observation.
#' Its formula is \eqn{\int_{-\infty}^{\infty}(F(y) - \mathbb{1}\{y \geq x\})^2dy} where \eqn{F(y)} is the predicted CDF and \eqn{\mathbb{1}\{y \geq x\}} is the empirical CDF.
#' Under normality assumption (which is mainly the case in this package), the formula is \eqn{\frac{\sigma}{\sqrt{\pi}}\left(1 - 2\Phi\left(\frac{x - \mu}{\sigma}\right) + 2\frac{x - \mu}{\sigma}\phi\left(\frac{x - \mu}{\sigma}\right)\right)}.
#' Unlike MAE, CRPS takes into account the entire distribution of the prediction, rather than focusing on a specific point of the probability distribution.
#'
#'}
#'
#' Note that \code{In} at the end of the name shows that the actual values are the observations used in the estimation process.
#' \code{Out} shows that out-of-sample actual values is used.
#'
#' @return A list with the given options.
#'
#' @export
get.options.metric <- function(typesIn = character(0), typesOut = character(0),
                                simFixSize = 10, trainRatio = 0.75,
                                trainFixSize = 0, seed = 0,
                                horizons = c(1L), weightedEval = FALSE){
  O = list(
    typesIn = typesIn, typesOut = typesOut,
    simFixSize = simFixSize, trainRatio = trainRatio,
    trainFixSize = trainFixSize, seed = seed,
    horizons = horizons, weightedEval = weightedEval)
  O = CheckmetricOptions(O)
  O
}


CheckmetricOptions <- function(O){

  O$typesIn = as.character(O$typesIn)
  O$typesOut = as.character(O$typesOut)
  O$weightedEval = as.logical(O$weightedEval)


  if (length(O$typesIn) == 0 && length(O$typesOut) == 0)
    stop("Invalid metric option. Both 'typesIn' and 'typesOut' are empty.")

  if (length(O$typesOut) > 0) {
    O$horizons = as.integer(O$horizons)
    O$simFixSize = as.integer(O$simFixSize)
    O$trainRatio = as.numeric(O$trainRatio)
    O$trainFixSize = as.integer(O$trainFixSize)
    O$seed = as.numeric(O$seed)

    for (h in O$horizons) {
      if (h <= 0)
        stop("Invalid metric option. zero or negative value in 'horizons'.")
    }

    if (O$simFixSize < 0)
      stop("Invalid metric option. 'simFixSize' cannot be negative.")

    if (O$trainRatio < 0 || O$trainRatio > 1)
      stop("Invalid metric option. 'trainRatio' cannot be negative or cannot be larger than 1.")

    if (O$trainFixSize < 0)
      stop("Invalid metric option. 'trainFixSize' cannot be negative.")

    # if (O$seed < 0)  It can be negative for similar distribution
    # of the seeds in the searchers

    if (O$trainRatio == 0 && O$trainFixSize == 0)
      stop("Invalid metric option. Both 'trainRatio' and 'trainFixSize' are zero.")
  }
  O
}
