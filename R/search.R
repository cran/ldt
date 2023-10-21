
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
get.search.items <- function(model = TRUE, type1 = FALSE, type2 = FALSE,
                             bestK = 1, all = FALSE, inclusion = FALSE,
                             cdfs = numeric(0), extremeMultiplier = 0,
                             mixture4 = FALSE){
  stopifnot(is.logical(model) && length(model) == 1)
  stopifnot(is.logical(type1) && length(type1) == 1)
  stopifnot(is.logical(type2) && length(type2) == 1)
  stopifnot(is.logical(all) && length(all) == 1)
  stopifnot(is.logical(inclusion) && length(inclusion) == 1)
  stopifnot(is.logical(mixture4) && length(mixture4) == 1)
  stopifnot(is.zero.or.positive.number(bestK))
  stopifnot(is.zero.or.positive.number(extremeMultiplier))
  stopifnot(is.numeric(cdfs))

  res = list(model = model, type1 = type1,
             type2 = type2, bestK = bestK,
             all = all, inclusion = inclusion,
             cdfs = cdfs,
             extremeMultiplier = extremeMultiplier,
             mixture4 = mixture4)
  class(res) <- c("ldt.list", "list")
  res
}


#' Get Extra Options for Model Search Process
#'
#' Use this function to determine how the model search is performed.
#'
#' @param parallel If \code{TRUE}, a parallel search algorithm is used. This generally changes the speed and memory usage.
#' @param reportInterval An integer representing the time interval (in seconds) for reporting progress (if any significant change has occurred). Set to zero to disable reporting.
#'
#' @return A list with the given options.
#'
#' @export
get.search.options <- function(parallel = FALSE, reportInterval = 0){
  stopifnot(is.logical(parallel) && length(parallel) == 1)
  stopifnot(is.zero.or.positive.number(reportInterval))

  res = list(parallel = parallel,
             reportInterval = reportInterval)
  class(res) <- c("ldt.list", "list")
  res
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
get.search.modelchecks <- function( estimation = TRUE, maxConditionNumber = Inf,
                                    minObsCount = 0, minDof = 0, minOutSim = 0,
                                    minR2 = -Inf, maxAic = Inf, maxSic = Inf,
                                    prediction = FALSE, predictionBoundMultiplier = 4){

  stopifnot(is.logical(estimation) && length(estimation) == 1)
  stopifnot(is.logical(prediction) && length(prediction) == 1)

  stopifnot(is.zero.or.positive.number(minObsCount))
  stopifnot(is.zero.or.positive.number(minDof))
  stopifnot(is.zero.or.positive.number(minOutSim))
  stopifnot(is.zero.or.positive.number(maxConditionNumber))
  stopifnot(is.zero.or.positive.number(predictionBoundMultiplier))
  stopifnot(is.number(minR2))
  stopifnot(is.number(maxAic))
  stopifnot(is.number(maxSic))


  res = list(
    estimation = estimation,
    maxConditionNumber = maxConditionNumber,
    minObsCount = minObsCount, minDof = minDof,
    minOutSim = minOutSim, maxSic = maxSic,
    minR2 = minR2, maxAic = maxAic,
    maxSic = maxSic, prediction = prediction,
    predictionBoundMultiplier = predictionBoundMultiplier)
  class(res) <- c("ldt.list", "list")
  res
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
#' @param minMetrics a list of minimum values for adjusting the weights when applying the AIC weight formula.
#' It can contain the following members: \code{aic}, \code{sic}, \code{brierIn}, \code{rmse}, \code{rmspe}, \code{mae}, \code{mape}, \code{crps}, \code{brierOut}.
#' Members can be numeric vectors for specifying a value for each target variable. See details.
#'
#' @details
#' An important aspect of \code{ldt} is model evaluation during the screening process. This involves considering both in-sample and out-of-sample evaluation metrics. In-sample metrics are computed using data that was used in the estimation process, while out-of-sample metrics are computed using new data. These metrics are well documented in the literature, and I will provide an overview of the main computational aspects and relevant references.
#'
#'
#' @section AIC and SIC:
#' According to \insertCite{burnham2002model;textual}{ldt} or \insertCite{greene2020econometric;textual}{ldt}, AIC and SIC are two commonly used metrics for comparing and choosing among different models with the same endogenous variable(s). Given \eqn{L^*} as the maximum value of the likelihood function in a regression analysis with \eqn{k} estimated parameters and \eqn{N} observations, AIC is calculated by \eqn{2k-2\ln L^*} and SIC is calculated by \eqn{k\ln N-2\ln L^*}. SIC includes a stronger penalty for increasing the number of estimated parameters in the model.
#'
#' These metrics can be converted into weights using the formula \eqn{w=\exp (-0.5x)}, where \eqn{x} is the value of the metric. When divided by the sum of all weights, \eqn{w} can be interpreted as the probability that a given model is the best model among all members of the model set (see section 2.9 in \insertCite{burnham2002model;textual}{ldt}). Compared to the \insertCite{burnham2002model;textual}{ldt} discussion and since \eqn{f(x)=exp(-0.5x)} transformation is invariant to translation, the minimum AIC part is removed in the screening process. This is an important property because it enables the use of running statistics and parallel computation.
#'
#' @section MSE, RMSE, MSPE, and RMSPE:
#' According to \insertCite{hyndman2018forecasting;textual}{ldt}, MSE and RMSE are two commonly used scale-dependent metrics, while MAPE is a commonly used unit-free metric. \code{ldt} also calculates the less common RMSPE metric. If there are \eqn{n} predictions and \eqn{e_i=y_i-\hat{y}_i} for \eqn{i=1\ldots n} is the prediction error, i.e., the distance between actual values (\eqn{y_i}) and predictions (\eqn{\hat{y}_i}), these metrics can be expressed analytically by the following formulas:
#'
#' \deqn{
#' \begin{aligned}
#' &\text{MAE} = \frac{1}{n}\sum_{i=1}^{n}|e_i|,
#' &&\text{MAPE} = \frac{1}{n}\sum_{i=1}^{n}\left|\frac{e_i}{y_i}\right|\times 100 \\
#' &\text{RMSE} = \sqrt{\frac{1}{n}\sum_{i=1}^{n}(e_i)^2},\quad\quad
#' &&\text{RMSPE} = \sqrt{\frac{1}{n}\sum_{i=1}^{n}\left(\frac{e_i}{y_i}\right)^2}\times 100.
#' \end{aligned}
#' }
#'
#' Note that, first MAPE and RMSPE are not defined if \eqn{y_i} is zero and may not be meaningful or useful if it is near zero or negative. Second, although these metrics cannot be directly interpreted as weights, they are treated in a manner similar to AIC in the \code{ldt} package.. Third, caution is required when target variables are transformed, for example to a logarithmic scale. \code{ldt} provides an option to transform the data back when calculating these metrics.
#'
#' @section Brier:
#' The Brier score measures the accuracy of probabilistic predictions for binary outcomes. It is calculated as the mean squared difference between the actual values (\eqn{y_i}) and the predicted probabilities (\eqn{p_i}). Assuming that there are \eqn{n} predictions, its formula is given by:
#'
#' \eqn{
#' \text{Brier} = \frac{\sum (y_i-\hat{p}_i)^2}{n},
#' }
#'
#' where \eqn{p_i} is the predicted probability that the \eqn{i}-th observation is positive. The value of this metric ranges from 0 to 1, with lower values indicating better predictions. In the screening process in \code{ldt}, both in-sample and out-of-sample observations can be used to calculate this metric. Although this metric cannot be directly interpreted as a weight, it is treated in a manner similar to AIC.
#'
#' @section AUC:
#' As described by \insertCite{fawcett2006introduction;textual}{ldt}, the receiver operating characteristic curve (ROC) plots the true positive rate (sensitivity) against the false positive rate (1-specificity) at different classification thresholds. The area under this curve is known as the AUC. Its value ranges from 0 to 1, with higher values indicating that the model is better at distinguishing between the two classes \insertCite{fawcett2006introduction,fawcett2006roc;textual}{ldt}. In the screening process in \code{ldt}, both in-sample and out-of-sample observations can be used to calculate this metric. There is also an option to calculate the pessimistic or an instance-varying costs version of this metric. Although this metric does not have a direct interpretation as weights, in \code{ldt} its value is considered as weight.
#'
#' @section CRPS:
#' According to \insertCite{gneiting2005calibrated;textual}{ldt}, the continuous ranked probability score (CRPS) is a metric used to measure the accuracy of probabilistic predictions. Unlike MAE, RMSE, etc., CRPS takes into account the entire distribution of the prediction, rather than focusing on a specific point of the probability distribution. For \eqn{n} normally distributed predictions with mean \eqn{\hat{y}_i} and variance \eqn{\operatorname{var}(\hat{y}_i)}, this metric can be expressed analytically as:
#'
#' \eqn{
#' \text{CRPS}=\sum_{i=1}^{n} \sigma \left(\frac{1}{\sqrt{\pi}} - 2\Phi(z_i) + z_i (2\phi(z_i)-1)\right),
#' }
#'
#' where \eqn{z_i=(y_i-\hat{y}_i)/\sqrt{\operatorname{var}(\hat{y}_i)}}, and \eqn{\Phi} and \eqn{\phi} are CDF and density functions of standard normal distribution. Although this metric cannot be directly interpreted as a weight, it is treated in a manner similar to AIC in the \code{ldt} package.
#'
#' @section Other metrics:
#' There are some other metrics in \code{ldt}. One is ``directional prediction accuracy'', which is calculated as the proportion of predictions that correctly predict the direction of change relative to the previous observation. Its value ranges from 0 to 1, with higher values indicating better performance of the model. Its value is used as the weight of a model. Note that this is applicable only to time-series data.
#'
#' Another similar metric is ``sign prediction accuracy'', which reports the proportion of predictions that have the same sign as the actual values. It is calculated as the number of correct sign predictions divided by the total number of predictions. Its value ranges from 0 to 1, with higher values indicating better performance of the model. Its value is used as the weight of a model.
#'
#' @references
#'   \insertAllCited{}
#' @importFrom Rdpack reprompt
#'
#' @return A list with the given options.
#'
#' @export
get.search.metrics <- function(typesIn = c("aic"),
                               typesOut = NULL,
                               simFixSize = 2,
                               trainRatio = 0.75,
                               trainFixSize = 0,
                               seed = 0,
                               horizons = c(1L),
                               weightedEval = FALSE,
                               minMetrics = list(aic = 0)){

  stopifnot(is.logical(weightedEval) && length(weightedEval) == 1)

  if (is.null(typesIn))
    typesIn = character(0)
  if (is.null(typesOut))
    typesOut = character(0)

  stopifnot(is.character(typesIn))
  stopifnot(is.character(typesOut))

  if (length(typesIn) == 0 && length(typesOut) == 0)
    stop("Invalid metric option. Both 'typesIn' and 'typesOut' are empty.")

  if (length(typesOut) > 0) {
    stopifnot(is.numeric(horizons) && all(horizons >= 1))
    horizons <- as.integer(horizons)
    stopifnot(is.positive.number(simFixSize))
    simFixSize <- as.integer(simFixSize)
    stopifnot(is.zero.or.positive.number(trainRatio) && trainRatio <= 1)
    stopifnot(is.zero.or.positive.number(trainFixSize))
    trainFixSize <- as.integer(trainFixSize)
    stopifnot(is.number(seed)) # It can be negative for similar distribution of the seeds in the searchers
    seed <- as.integer(seed)
    if (seed == 0)
      seed = runif(1,10,10e4) # set it here such that it is reported in the info section
    stopifnot(trainRatio != 0 || trainFixSize != 0)

    if (simFixSize < 1)
      stop("'simFixSize' must be larger than 1 when out-of-sample measures exists.")
  }

  # initialize adjustments:
  if (is.null(minMetrics))
    minMetrics = list()
  else
    stopifnot(is.list(minMetrics))
  ls <- c("aic", "sic", "rmse", "mae", "rmspe", "mape", "brierIn", "brierOut", "crps")
  for (m in ls) {
    if (!m %in% names(minMetrics)) {
      minMetrics[[m]] <- 0
    }
    else if (!is.number(minMetrics[[m]]))
      stop("An element in 'minMetrics' is not a number.")
  }

  res = list(
    typesIn = typesIn,
    typesOut = typesOut,
    simFixSize = simFixSize,
    trainRatio = trainRatio,
    trainFixSize = trainFixSize,
    seed = seed,
    horizons = c(horizons),
    weightedEval = weightedEval,
    minMetrics = minMetrics)
  class(res) <- c("ldt.list", "list")
  res
}

get.search.metrics.update <- function(metrics, numTargets){
  min_metrics <- list()
  nms <- names(metrics$minMetrics)
  for (n in nms){
    ln <- length(metrics$minMetrics[n])
    if (ln == 1)
      min_metrics[[n]] <- rep(metrics$minMetrics[[n]], numTargets)
    else if (ln != numTargets)
      stop("An element in 'metrics$minMetrics' has invalid length. name:", n)
    else
      min_metrics[n] <- metrics$minMetrics[n]
  }
  metrics$minMetrics <- min_metrics
  metrics
}

