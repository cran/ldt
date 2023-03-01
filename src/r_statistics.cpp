
#include "distributions.h"
#include "matrix.h"
#include "optimization.h"
#include "pca.h"
#include "r_ldt.h"
#include "scoring.h"
#include "searchers.h"

using namespace Rcpp;
using namespace ldt;

// clang-format off

//' Converts a Measure to Weight
//'
//' @param value (double) the measure
//' @param measureName (string) measure name
//'
//' @return the weight
//' @export
//'
//' @examples
//' weight <- GetWeightFromMeasure(-3.4, "sic")
// [[Rcpp::export]]
SEXP GetWeightFromMeasure(SEXP value, SEXP measureName)
// clang-format on
{

  double value0 = as<double>(value);
  std::string measureName0 = as<std::string>(measureName);
  boost::algorithm::to_lower(measureName0);

  double v = NAN;

  try {
    auto type = FromString_GoodnessOfFitType(measureName0.c_str());
    v = GoodnessOfFit::ToWeight(type, value0);
  } catch (...) {
    try {
      auto type1 = FromString_ScoringType(measureName0.c_str());
      v = Scoring::ToWeight(type1, value0);
    } catch (...) {
      throw std::logic_error(
          std::string(
              "An error occurred. Probably an invalid 'measureName': ") +
          measureName0);

      // Rethrow is not working ?!
    }
  }
  return wrap(v);
}

// clang-format off

//' Converts a Measure to Weight
//'
//' @param value (double) the measure
//' @param measureName (string) measure name
//'
//' @return the measure
//' @export
//'
//' @examples
//' weight <- GetWeightFromMeasure(-3.4, "sic")
//' measure <- GetMeasureFromWeight(weight, "sic")
// [[Rcpp::export]]
SEXP GetMeasureFromWeight(SEXP value, SEXP measureName)
// clang-format on
{
  double value0 = as<double>(value);
  std::string measureName0 = as<std::string>(measureName);
  boost::algorithm::to_lower(measureName0);

  double v = NAN;
  try {
    auto type = FromString_GoodnessOfFitType(measureName0.c_str());
    v = GoodnessOfFit::FromWeight(type, value0);
  } catch (...) {
    try {
      auto type1 = FromString_ScoringType(measureName0.c_str());
      v = Scoring::FromWeight(type1, value0);
    } catch (...) {
      throw std::logic_error(
          std::string(
              "An error occurred. Probably an invalid 'measureName': ") +
          measureName0);
    }
  }
  return wrap(v);
}

// clang-format off

//' ROC curve for a binary case
//'
//' It does not draw the ROC, but calculatates the required points. It also
//' Calculates the AUC with different options
//'
//' @param y (numeric vector, \code{Nx1}) Actual values
//' @param scores (numeric vector, \code{Nx1}) Calculated probabilities for the negative observations
//' @param weights (numeric vector, \code{Nx1}) Weights of the observations. Use \code{NULL} for equal weights.
//' @param options (list) More options. See [GetRocOptions()] function for details.
//' @param printMsg (bool) Set true to report some details.
//'
//' @return A list with the following items:
//' \item{N}{(integer) Number of observations}
//' \item{AUC}{(numeric) Value of AUC}
//' \item{Points}{(numeric matrix) Points for ploting ROC}
//'
//' @export
//'
//' @examples
//' y <- c(1, 0, 1, 0, 1, 1, 0, 0, 1, 0)
//' scores <- c(0.1, 0.2, 0.3, 0.5, 0.5, 0.5, 0.7, 0.8, 0.9, 1)
//' res1 = GetRoc(y,scores, printMsg = FALSE)
//' costs <- c(1,2,1,4,1,5,1,1,0.5,1)
//' costMatrix = matrix(c(0.02,-1,-3,3),2,2)
//' opt <- GetRocOptions(costs = costs, costMatrix = costMatrix)
//' res2 = GetRoc(y,scores,NULL,options = opt, printMsg = FALSE)
//' #plot(res1$Points)
//' #lines(res2$Points)
//'
// [[Rcpp::export]]
List GetRoc(SEXP y, SEXP scores, SEXP weights = R_NilValue, SEXP options = R_NilValue,
               bool printMsg = false)
// clang-format on
{
  if (y == R_NilValue || is<NumericVector>(y) == FALSE)
    throw std::logic_error("'y' should be a numeric vector.");
  if (scores == R_NilValue || is<NumericVector>(scores) == FALSE)
    throw std::logic_error("'scores' should be a numeric vector.");
  NumericVector y0 = as<NumericVector>(y);
  auto N = y0.length();
  if (printMsg)
    Rprintf("Number of observations = %i\n", N);

  NumericVector scores0 = as<NumericVector>(scores);
  if (N != scores0.length())
    throw std::logic_error(
        "Unequal number of observations in 'y' and 'scores'.");

  auto my = ldt::Matrix<double>(&y0[0], N, 1);
  auto mscores = ldt::Matrix<double>(&scores0[0], N, 1);

  NumericVector weights0;
  auto mweights = ldt::Matrix<double>(N, 1);
  auto hasWeight = weights != R_NilValue;
  if (hasWeight) {
    if (is<NumericVector>(weights) == FALSE)
      throw std::logic_error("'weights' should be a numeric vector.");
    weights0 = as<NumericVector>(weights);
    if (hasWeight && N != weights0.length())
      throw std::logic_error(
          "Unequal number of observations in 'y' and 'weights'.");
    mweights.SetData(&weights0[0]);
  }
  if (printMsg)
    Rprintf("Is Weighted = %s\n", hasWeight ? "TRUE" : "FALSE");

  auto min_y = min(y0);
  if (min_y != 0)
    throw std::logic_error("Invalid 'y' vector. Minimum must be 0.");
  auto max_y = max(y0);
  if (max_y != 1)
    throw std::logic_error("Invalid 'y' vector. Maximum must be 1.");

  List optionsL;
  if (options == R_NilValue)
    optionsL = GetRocOptions();
  else {
    if (is<List>(options) == FALSE)
      throw std::logic_error("Invalid 'options'. It should be list.");
    optionsL = as<List>(options);
    CheckRocOptions(options);
  }
  ldt::RocOptions options_;
  UpdateRocOptions(printMsg, optionsL, options_, "Options: ");

  std::unique_ptr<RocBase> auc0;
  if (hasWeight) {
    if (options_.Costs.Data) {
      auc0 = std::unique_ptr<RocBase>(new ROC<true, true>(N));
    } else {
      auc0 = std::unique_ptr<RocBase>(new ROC<true, false>(N));
    }
  } else {
    if (options_.Costs.Data) {
      auc0 = std::unique_ptr<RocBase>(new ROC<false, true>(N));
    } else {
      auc0 = std::unique_ptr<RocBase>(new ROC<false, false>(N));
    }
  }
  auto auc = auc0.get();
  auc->Calculate(my, mscores, hasWeight ? &mweights : nullptr, options_);

  auto points_d = std::unique_ptr<double[]>(new double[auc->Points.size() * 2]);
  auto points = ldt::Matrix<double>(points_d.get(), auc->Points.size(), 2);
  auto colnames = std::vector<std::string>({"FP Rate", "TP Rate"});
  for (auto i = 0; i < (int)auc->Points.size(); i++) {
    points.Set(i, 0, std::get<0>(auc0->Points.at(i)));
    points.Set(i, 1, std::get<1>(auc0->Points.at(i)));
  }

  List L = List::create(_["N"] = wrap(N), _["AUC"] = wrap(auc->Result),
                        _["Points"] = as_matrix(points, nullptr, &colnames));

  L.attr("class") = std::vector<std::string>({"ldtroc", "list"});

  return L;
}

// clang-format off

//' Gets the GLD-FKML Parameters from the moments
//'
//' @description Calculates the parameters of the generalized lambda distribution (FKML), given the first four moments of the distribution.
//'
//' @details
//' The type of the distribution is determined by one or two restrictions:
//' - **type 0:** general
//' - **type 1:** symmetric 'type 0'
//' - **type 2:** uni-modal continuous tail: L3<1 & L4<1
//' - **type 3:** symmetric 'type 2' L3==L4
//' - **type 4:** uni-modal continuous tail finite slope  L3<=0.5 &  L4<=5
//' - **type 5:** symmetric 'type 4' L3==L4
//' - **type 6:** uni-modal truncated density curves: L3>=2 & L4>=2 (includes uniform distribution)
//' - **type 7:** symmetric 'type 6' L3==L4
//' - **type 8:** S shaped L3>2 & 1<L4<2 or 1<L3<2 & L4>2
//' - **type 9:** U shaped 1<L3<=2 and 1<L4<=2
//' - **type 10:** symmetric 'type 9' L4==L4
//' - **type 11:** monotone L3>1 & L4<=1
//'
//' @param mean (double) mean of the distribution.
//' @param variance (double) variance of the distribution.
//' @param skewness (double) skewness of the distribution.
//' @param excessKurtosis (double) excess kurtosis of the distribution.
//' @param type (int) The type of the distribution.
//' @param start (numeric vector, length=2) starting value for L3 and L4. Use null for c(0,0).
//' @param nelderMeadOptions (list) The optimization parameters. Use null for default.
//' @param printMsg (bool) If \code{TRUE}, details are printed.
//'
//' @return a vector with the parameters of the GLD distribution.
//' @export
//'
//' @examples
//' res = GetGldFromMoments(0,1,0,0,0,c(0,0))
// [[Rcpp::export]]
NumericVector GetGldFromMoments(double mean = 0, double variance = 1,
                                 double skewness = 0, double excessKurtosis = 0,
                                 int type = 0, SEXP start = R_NilValue,
                                 SEXP nelderMeadOptions = R_NilValue,
                                 bool printMsg = false)
// clang-format on
{
  NumericVector start_ = {0, 0};
  if (start != R_NilValue) {
    if (is<NumericVector>(start) == false)
      throw std::logic_error("'start' must be a 'numeric vector'.");
    auto start_ = as<NumericVector>(start);
    if (start_.length() != 2)
      throw std::logic_error("Invalid length: 'start'.");
  }

  List nelderMeadOptions_;
  if (nelderMeadOptions != R_NilValue) {

    if (is<List>(nelderMeadOptions) == false)
      throw std::logic_error("'nelderMeadOptions' must be a 'List'.");
    auto nelderMeadOptions_ = as<List>(nelderMeadOptions);
    CheckNelderMeadOptions(nelderMeadOptions_);
  } else
    nelderMeadOptions_ = GetNelderMeadOptions();

  if (printMsg) {
    Rprintf("Moments=%f, %f, %f, %f\n", mean, variance, skewness,
            excessKurtosis);
    Rprintf("Type=%i\n", type); // TODO: convert to string
    Rprintf("Start (L3, L4)=(%f, %f)\n", start_[0], start_[1]);
  }

  auto optim = NelderMead(2);
  optim.Alpha = nelderMeadOptions_["alpha"];
  optim.Beta = nelderMeadOptions_["beta"];
  optim.Gamma = nelderMeadOptions_["gamma"];
  optim.MaxIterations = nelderMeadOptions_["maxIterations"];
  optim.Epsilon = nelderMeadOptions_["epsilon"];
  optim.Scale = nelderMeadOptions_["scale"];

  auto ps =
      DistributionGld::GetFromMoments(mean, variance, skewness, excessKurtosis,
                                      type, optim, start_[0], start_[1]);

  NumericVector result = {std::get<0>(ps), std::get<1>(ps), std::get<2>(ps),
                          std::get<3>(ps)};
  result.names() = std::vector<std::string>({"L1", "L2", "L3", "L4"});

  return result;
}

// clang-format off

//' Gets GLD Quantile
//'
//' @param data (numeric vector) data
//' @param L1 (double) First parameter
//' @param L2 (double) Second parameter
//' @param L3 (double) Third parameter
//' @param L4 (double) Fourth parameter
//'
//' @return (numeric vector) result
//' @export
// [[Rcpp::export]]
NumericVector GldQuantile(SEXP data, double L1, double L2, double L3,
                           double L4)
// clang-format on
{
  if (is<NumericVector>(data) == false)
    throw std::logic_error("'data' must be a 'numeric vector'.");
  NumericVector data0 = as<NumericVector>(data);
  NumericVector result(data0.length());
  for (int i = 0; i < data0.length(); i++)
    result[i] = DistributionGld::GetQuantile(data0[i], L1, L2, L3, L4);
  return result;
}

// clang-format off

//' Gets GLD Density Quantile
//'
//' @param data (numeric vector) data
//' @param L1 (double) First parameter
//' @param L2 (double) Second parameter
//' @param L3 (double) Third parameter
//' @param L4 (double) Fourth parameter
//'
//' @return (numeric vector) result
//' @export
// [[Rcpp::export]]
NumericVector GldDensityQuantile(SEXP data, double L1, double L2, double L3,
                                  double L4)
// clang-format on
{
  if (is<NumericVector>(data) == false)
    throw std::logic_error("'data' must be a 'numeric vector'.");
  NumericVector data0 = as<NumericVector>(data);

  NumericVector result(data0.length());
  for (int i = 0; i < data0.length(); i++)
    result[i] = DistributionGld::GetDensityQuantile(data0[i], L1, L2, L3, L4);
  return result;
}

// clang-format off

//' Combines Two Distributions Defined by their First 4 Moments
//'
//' @param mix1 (list) First distribution which is defined by a list with mean, variance, skewness, kurtosis, sumWeights, count
//' @param mix2 (list) Second distribution (similar to \code{mix1}).
//'
//' @return (list) A list similar to \code{mix1}
//' @export
//'
//' @examples
//' #see its \code{test_that} function
// [[Rcpp::export]]
List GetCombination4Moments(SEXP mix1, SEXP mix2)
// clang-format on
{

  if (is<List>(mix1) == false)
    throw std::logic_error("'mix1' must be a 'List'.");
  List mix1_ = as<List>(mix1);

  if (is<List>(mix2) == false)
    throw std::logic_error("'mix2' must be a 'List'.");
  List mix2_ = as<List>(mix2);

  auto r = RunningWeighted4();
  r.PushNewDistribution(mix1_["mean"], mix1_["variance"], mix1_["skewness"],
                        mix1_["kurtosis"], mix1_["sumWeights"], mix1_["count"]);
  r.PushNewDistribution(mix2_["mean"], mix2_["variance"], mix2_["skewness"],
                        mix2_["kurtosis"], mix2_["sumWeights"], mix2_["count"]);
  auto L = List::create(_["mean"] = r.GetMean(),
                        _["variance"] = r.GetVariancePopulation(),
                        _["skewness"] = r.GetSkewnessPopulation(),
                        _["kurtosis"] = r.GetKurtosisPopulation(),
                        _["sumWeights"] = r.Sum(), _["count"] = r.GetCount());
  return L;
}

// clang-format off

//' Principle Component Analysis
//'
//' @param x (numeric matrix) data with variables in columns.
//' @param center (bool) if \code{TRUE}, it demeans the variables.
//' @param scale (bool) if \code{TRUE}, it scales the variables to unit variance.
//' @param newX (numeric matrix) data to be used in projection. Its structure must be similar to the \code{x}.
//'
//' @return (list) results
//' \item{removed0Var}{(integer vector) Zero-based indices of removed columns with zero variances.}
//' \item{directions}{(numeric matrix) Directions}
//' \item{stds}{(integer vector) Standard deviation of the principle components}
//' \item{stds2Ratio}{(integer vector) stds^2/sum(stds^2)}
//' \item{projections}{(numeric matrix) Projections if \code{newX} is given.}
//'
//' @export
//'
// [[Rcpp::export]]
List GetPca(SEXP x, bool center = true, bool scale = true,
             SEXP newX = R_NilValue)
// clang-format on
{
  if (is<NumericMatrix>(x) == false)
    throw std::logic_error("'x' must be a 'numeric matrix'.");
  NumericMatrix x0 = as<NumericMatrix>(x);

  auto mx = ldt::Matrix<double>(&x0[0], x0.nrow(), x0.ncol());
  auto mnewX = ldt::Matrix<double>();
  bool hasNewX = newX != R_NilValue;
  if (hasNewX) {
    if (is<NumericMatrix>(newX) == false)
      throw std::logic_error("'newX' must be a 'numeric matrix'.");
    NumericMatrix newX_ = as<NumericMatrix>(newX);

    if (newX_.ncol() != x0.ncol())
      throw std::logic_error(
          "number of columns in 'newX' and 'x' are different.");
    mnewX.SetData(&newX_[0], newX_.nrow(), newX_.ncol());
  }

  auto model = PcaAnalysis(x0.nrow(), x0.ncol(), hasNewX ? mnewX.RowsCount : 0,
                           false, true, center, scale);
  auto W = std::unique_ptr<Tv[]>(new Tv[model.WorkSize]);
  auto S = std::unique_ptr<Tv[]>(new Tv[model.StorageSize]);
  model.Calculate(mx, W.get(), S.get(), hasNewX ? &mnewX : nullptr);
  // note that 'model.DataS' is null

  auto L = List::create(
      _["removed0Var"] = wrap(model.DataS.RemovedZeroVar),
      _["directions"] =
          NumericMatrix(model.Directions.RowsCount, model.Directions.ColsCount,
                        model.Directions.Data),
      _["stds"] =
          NumericVector(model.Stds.Data, model.Stds.Data + model.Stds.length()),
      _["stds2Ratio"] =
          NumericVector(model.Stds2Ratios.Data,
                        model.Stds2Ratios.Data + model.Stds2Ratios.length()),
      _["projections"] = hasNewX ? NumericMatrix(model.Forecasts.RowsCount,
                                                 model.Forecasts.ColsCount,
                                                 model.Forecasts.Data)
                                 : (NumericMatrix)R_NilValue);
  return L;
}
