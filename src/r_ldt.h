// [[Rcpp::depends(BH)]]
// [[Rcpp::plugins(cpp17)]]

#pragma once

#include "frequency.h"
#include "helpers.h"
#include "matrix.h"
#include "pca.h"
#include "searchers.h"
#include "variable.h"
#include <Rcpp.h>

#include <boost/date_time/posix_time/posix_time.hpp>

#include <chrono>
#include <cstring>
#include <exception>
#include <future>
#include <iostream>
#include <sstream>
#include <string>
#include <thread>

#ifdef _OPENMP
#include <omp.h>
// [[Rcpp::plugins(openmp)]]
#endif

// #include <Rinternals.h>

using namespace Rcpp;

std::unique_ptr<double[]>
CombineEndoExo(bool printMsg, ldt::Matrix<double> &result,
               std::vector<std::string> &colNames, ldt::Matrix<double> &my,
               ldt::Matrix<double> &mx, ldt::Matrix<double> &mw,
               ldt::Matrix<double> &mnewX, SEXP &y, SEXP &x, SEXP &w,
               SEXP &newX, bool removeNan, bool addIntercept, int minExpectedY,
               int minExpectedX, int minExpectedW, int minExpectedNewX = 0,
               bool appendNewX = false);

void GetSizes(bool printMsg, std::vector<int> &result, SEXP &sizes,
              int variableCount, bool isX);

void GetPartitions(bool printMsg, std::vector<std::vector<int>> &result,
                   SEXP &partitions, int variableCount, int adjustPos = 0,
                   bool isX = true);

void GetGroups(bool printMsg, std::vector<std::vector<int>> &result,
               SEXP &groups, int variableCount, int adjustPos, bool isX);

void UpdateOptions(bool printMsg, List &searchItems, List &measureOptions,
                   List &modelCheckItems,
                   ldt::SearchMeasureOptions &res_measure,
                   ldt::SearchItems &res_items,
                   ldt::SearchModelChecks &res_checks,
                   std::vector<std::string> &measuresNames, int length1,
                   int exoCount, int numTargets, int numDependents,
                   bool isTimeSeries = false, bool type1NeedsModelEstim = true,
                   const char *length1Informtion = "Coefficients",
                   bool isDc = false);

NumericMatrix insert_intercept(NumericMatrix a);

NumericMatrix cbind_matrix(NumericMatrix a, NumericMatrix b);

NumericMatrix cbind_vectormatrix(NumericVector a, NumericMatrix b,
                                 std::string vectorName);

NumericMatrix as_matrix(ldt::Matrix<double> &mat,
                        std::vector<std::string> *rowNames = nullptr,
                        std::vector<std::string> *colNames = nullptr);

NumericVector as_vector(ldt::Matrix<double> &vec,
                        std::vector<std::string> *names = nullptr);

IntegerMatrix as_imatrix(ldt::Matrix<int> &mat,
                         std::vector<std::string> *rowNames = nullptr,
                         std::vector<std::string> *colNames = nullptr);

IntegerVector as_ivector(ldt::Matrix<int> &vec,
                         std::vector<std::string> *names = nullptr);

std::unique_ptr<ldt::FrequencyWeekBased> GetFreqFromSEXP_week(List f);

std::unique_ptr<ldt::Frequency>
GetFreqFromSEXP(SEXP value, std::vector<std::string> &listItems,
                std::vector<boost::gregorian::date> &listItemsDate);

std::unique_ptr<ldt::Variable<double>> GetVariableFromSEXP(List w);

std::vector<std::string> GetDefaultColNames(std::string pre, int length);

// clang-format off

//' Options for ROC and AUC
//'
//' @param lowerThreshold (double) Lower bound for calculating partial AUC.
//' @param upperThreshold (double) Upper bound for calculating partial AUC.
//' @param epsilon (double) A value to ignore small floating point differences in comparing scores.
//' @param pessimistic (bool) If true, sequences of equally scored instances are treated differently and a pessimistic measure is calculated (see Fawcett (2006) An introduction to roc analysis, fig. 6).
//' @param costs (numeric vector) cost of each observations. If null, cost of all observations will be 1.
//' @param costMatrix (numeric matrix) a 2x2 cost matrix in which: (1,1) is cost of TN,
//' (2,2) is cost of TP, (1,2) is cost of FP and (2,1) is cost of FN. First
//' column is multiplied by the corresponding value in costs vector (see
//' Fawcett (2006), ROC graphs with instance-varying costs).
//'
//' @return A list with the given options.
//'
//' @export
// [[Rcpp::export]]
 List GetRocOptions(double lowerThreshold = 0, double upperThreshold = 1, double epsilon = 1e-12,
                    bool pessimistic = false, SEXP costs = R_NilValue, SEXP costMatrix = R_NilValue);

// clang-format on

void CheckRocOptions(List options);

void UpdateRocOptions(bool printMsg, List &rocOptionsR,
                      ldt::RocOptions &options, const char *startMsg);

// clang-format off

//' Options for Nelder-Mead Optimization
//'
//' @param maxIterations (int) Maximum number of iterations.
//' @param epsilon (double) A small value to test convergence.
//' @param alpha (double) the reflection coefficient.
//' @param beta (double) the contraction coefficient.
//' @param gamma (double) the expansion coefficient.
//' @param scale (double) A scale in initializing the simplex.
//'
//' @return A list with the given options.
//'
//' @export
// [[Rcpp::export]]
 List GetNelderMeadOptions(int maxIterations = 100, double epsilon = 1e-8,
                           double alpha = 1, double beta = 0.5, double gamma = 2,
                           double scale = 1);
// clang-format on

void CheckNelderMeadOptions(List options);

// clang-format off

//' Options for PCA
//'
//' @param ignoreFirst (int) Excludes variables at the beginning of data matrices (such as intercept) from PCA.
//' @param exactCount (int) Determines the number of components to be used. If zero, number of components are determined by the \code{cutoffRate}.
//' @param cutoffRate (double between 0 and 1) Determines the cutoff rate for cumulative variance ratio in order to determine the number of PCA components. It is not used if \code{exactCount} is positive.
//' @param max (int) Maximum number of components when \code{cutoffRate} is used.
//'
//' @return A list with the given options.
//'
//' @export
// [[Rcpp::export]]
List GetPcaOptions(int ignoreFirst = 1, int exactCount = 0,
                    double cutoffRate = 0.8, int max = 1000);

// clang-format on

void CheckPcaOptions(List options);

void UpdatePcaOptions(bool printMsg, List &pcaOptionsR, bool hasPca,
                      ldt::PcaAnalysisOptions &options, const char *startMsg);

// clang-format off

//' Options for LMBFGS Optimization
//'
//' @param maxIterations (int) A positive integer for maximum number of iterations.
//' @param factor (double) A condition for stopping the iterations. The iteration will stop when (f^k - f^{k+1})/max{|f^k|,|f^{k+1}|,1} < \code{factor}*epsmch where epsmch is the machine precision, which is automatically generated by the code. Use e.g., 1e12 for low accuracy, 1e7 (default) for moderate accuracy and 1e1 for extremely high accuracy. default is 1e7
//' @param projectedGradientTol (double) The iteration will stop when \code{max{|proj g_i | i = 1, ..., n} < projectedGradientTol} where \code{pg_i} is the ith component of the projected gradient. default is zero.
//' @param maxCorrections (int) Maximum number of variable metric corrections allowed in the limited memory Matrix. default is 5.
//'
//' @return A list with the given options.
//'
//' @export
// [[Rcpp::export]]
List GetLmbfgsOptions(int maxIterations = 100, double factor = 1e7,
                       double projectedGradientTol = 0, int maxCorrections = 5);
// clang-format on

void CheckLmbfgsOptions(List options);

void UpdateLmbfgsOptions(bool printMsg, List &lmbfgsOptions,
                         ldt::LimitedMemoryBfgsbOptions &options);

// clang-format off

//' Options for Newton Optimization
//'
//' @param maxIterations (int) Maximum number of iterations.
//' @param functionTol (double) A small value to test convergence of the objective function.
//' @param gradientTol (double) A small value to test convergence of the gradient.
//' @param useLineSearch (bool) If true, it uses line search.
//'
//' @return A list with the given options.
//'
//' @export
// [[Rcpp::export]]
List GetNewtonOptions(int maxIterations = 100, double functionTol = 1e-4,
                       double gradientTol = 0, bool useLineSearch = true);
// clang-format on

void CheckNewtonOptions(List options);

void UpdateNewtonOptions(bool printMsg, List &newtonR, ldt::Newton &newton);

// clang-format off

//' Options for 'Search Items'
//'
//' @description Creates a list with predefined items which determines the information to be saved and retrieved.
//'
//' @param model (bool) If true, information about the models is saved.
//' @param type1 (bool) If true and implemented, extra information is saved. This can be the coefficients in the SUR search or predictions in VARMA search.
//' @param type2 (bool) If true and implemented, extra information is saved. This is similar to \code{type1}. **It is reserved for future updates.**
//' @param bestK (int) Number of best items to be saved in \code{model}, \code{type1}, or \code{type2} information.
//' @param all (bool) If true, all available information is saved.
//' @param inclusion (bool) If true, inclusion weights are saved in \code{model}.
//' @param cdfs (nullable numeric vector) Weighted average of the CDFs at each given point is calculated (for \code{type1} and \code{type2}).
//' @param extremeMultiplier (double) Determined the multiplier in the extreme bound analysis (for \code{type1} and \code{type2}).
//' @param mixture4 (bool) If true, the first 4 moments of the average distributions are calculated in \code{type1} and \code{type2}.
//'
//' @return A list with the given options.
//'
//' @export
// [[Rcpp::export]]
List GetSearchItems(bool model = true, bool type1 = false, bool type2 = false,
                     int bestK = 1, bool all = false, bool inclusion = false,
                     SEXP cdfs = R_NilValue, double extremeMultiplier = 0,
                     bool mixture4 = false);
// clang-format on

void CheckSearchItems(List options);

void UpdateSearchItems(bool printMsg, List &searchItems,
                       ldt::SearchItems &items, Ti length1, Ti length2,
                       const char *length1Informtion,
                       const char *length2Informtion, bool type1NeedsModelEstim,
                       bool type2NeedsModelEstim);

// clang-format off

//' Options for 'Search Options'
//'
//' @description Creates a list with predefined Search options.
//'
//' @param parallel (bool) If true, it uses a parallel search. It generally changes the speed and memory usage.
//' @param reportInterval (int) Time interval (in seconds) for reporting the progress (if the change is significant). Set zero to disable.
//' @param printMsg (bool) Set false to disable printing the details.
//'
//' @return A list with the given options.
//'
//' @export
// [[Rcpp::export]]
List GetSearchOptions(bool parallel = false, int reportInterval = 2,
                       bool printMsg = false);
// clang-format on

void CheckSearchOptions(List options);

void UpdateSearchOptions(List &searchOptions, ldt::SearchOptions &options,
                         int &reportInterval, bool &printMsg);

// clang-format off

//' Options for 'Model Check Items'
//'
//' @param estimation (bool) If true, model is estimated with all data. If false, you might get a 'best model' that cannot be estimated.
//' @param maxConditionNumber (double) Maximum value for the condition number (if implemented in the search).
//' @param minObsCount (int) Minimum value for the number of observations. Use 0 to disable.
//' @param minDof (int) Minimum value for the degrees of freedom (equation-wise). Use 0 to disable.
//' @param minOutSim (int) Minimum value for the number of valid out-of-sample simulations (if implemented in the search).
//' @param minR2 (double) Minimum value for R2 (if implemented in the search).
//' @param maxAic (double) Maximum value for AIC (if implemented in the search).
//' @param maxSic (double) Maximum value for SIC (if implemented in the search).
//' @param prediction (bool) If true, model data is predicted given all data. If false, you might get a 'best model' that cannot be used in prediction.
//' @param predictionBoundMultiplier (double) If positive, a bound is created by multiplying this value to the average growth rate. A model is ignored, if its prediction lies outside of this bound.
//'
//' @return A list with the given options.
//'
//' @export
// [[Rcpp::export]]
List GetModelCheckItems(
     bool estimation = true,
     double maxConditionNumber =
       1.7e308, // TODO: INFINITY or R_PosInf throws warning
       int minObsCount = 0, int minDof = 0, int minOutSim = 0,
       double minR2 = -1.7e308, double maxAic = 1.7e308, double maxSic = 1.7e308,
       bool prediction = false, double predictionBoundMultiplier = 4);
// clang-format on

void CheckModelCheckItems(List options);

void UpdateModelCheckItems(bool printMsg, List &checkOptions,
                           ldt::SearchModelChecks &checks,
                           const ldt::SearchMeasureOptions &measures,
                           const ldt::SearchItems &items);

// clang-format off

//' Options for 'Measuring Performance'
//'
//' @param typesIn (nullable string vector) Evaluations when model is estimated using all available data. It can be \code{aic}, \code{sic}, \code{frequencyCostIn}, \code{aucIn}. Null means no measure.
//' @param typesOut (nullable string vector) Evaluations in an pseudo out-of-sample simulation. It can be \code{sign}, \code{direction}, \code{rmse}, \code{scaledRmse}, \code{mae}, \code{scaledMae}, \code{crps}, \code{frequencyCostOut}, \code{aucOut}. Null means no measure.
//' @param simFixSize (int) Number of pseudo out-of-sample simulations. Use zero to disable the simulation.
//' @param trainFixSize (int) Number of data-points in the training sample in the pseudo out-of-sample simulation. If zero, \code{trainRatio} will be used.
//' @param trainRatio (double) Number of data-points, as a ratio of the available size, in the training sample in the pseudo out-of-sample simulation.
//' @param seed (int) A seed for random number generator. Use zero for a random value.
//' @param horizons (nullable integer vector) prediction horizons to be used in pseudo out-of-sample simulations, if model supports time-series prediction. If null, c(1) is used.
//' @param weightedEval (bool) If true, weights are used in evaluationg discrete-choice models
//' @return A list with the given options.
//'
//' @export
// [[Rcpp::export]]
List GetMeasureOptions(SEXP typesIn = R_NilValue, SEXP typesOut = R_NilValue,
                        int simFixSize = 10, double trainRatio = 0.75,
                        int trainFixSize = 0, int seed = 0,
                        SEXP horizons = R_NilValue, bool weightedEval = false);
// clang-format on

void CheckMeasureOptions(List options);

void UpdateMeasureOptions(bool printMsg, List &measureOptions,
                          ldt::SearchMeasureOptions &measures,
                          std::vector<std::string> &measureNames,
                          bool isTimeSeries, bool isDc);

void ReportProgress(bool pringMsg, int reportInterval, ldt::ModelSet &model,
                    bool &estimating, ldt::SearchOptions &options);

List GetModelSetResults(ldt::ModelSet &model, ldt::SearchItems &searchItems,
                        std::vector<std::string> &measureNames, int length1,
                        const char *extra1Label,
                        std::vector<std::string> *extra1Names,
                        int exoIndexesPlus,
                        std::vector<std::string> &length1Names,
                        std::vector<std::string> &inclusionNames,
                        const char *length1Label,
                        const char *length1_itemlabel);
