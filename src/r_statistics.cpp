
#include "distributions.h"
#include "matrix.h"
#include "optimization.h"
#include "pca.h"
#include "r_ldt.h"
#include "scoring.h"
#include "searchers.h"

using namespace Rcpp;
using namespace ldt;

// [[Rcpp::export(.GetWeightFromMetric)]]
SEXP GetWeightFromMetric(SEXP value, SEXP metricName) {

  double value0 = as<double>(value);
  std::string metricName0 = as<std::string>(metricName);
  boost::algorithm::to_lower(metricName0);

  double v = NAN;

  try {
    auto type = FromString_GoodnessOfFitType(metricName0.c_str());
    v = GoodnessOfFit::ToWeight(type, value0);
  } catch (...) {
    try {
      auto type1 = FromString_ScoringType(metricName0.c_str());
      v = Scoring::ToWeight(type1, value0);
    } catch (...) {
      throw LdtException(
          ErrorType::kLogic, "R-statistics",
          std::string("An error occurred. Probably an invalid 'metricName': ") +
              metricName0);
    }
  }
  return wrap(v);
}

// [[Rcpp::export(.GetMetricFromWeight)]]
SEXP GetMetricFromWeight(SEXP value, SEXP metricName) {
  double value0 = as<double>(value);
  std::string metricName0 = as<std::string>(metricName);
  boost::algorithm::to_lower(metricName0);

  double v = NAN;
  try {
    auto type = FromString_GoodnessOfFitType(metricName0.c_str());
    v = GoodnessOfFit::FromWeight(type, value0);
  } catch (...) {
    try {
      auto type1 = FromString_ScoringType(metricName0.c_str());
      v = Scoring::FromWeight(type1, value0);
    } catch (...) {
      throw LdtException(
          ErrorType::kLogic, "R-statistics",
          std::string("An error occurred. Probably an invalid 'metricName': ") +
              metricName0);
    }
  }
  return wrap(v);
}

// [[Rcpp::export(.GetRoc)]]
List GetRoc(SEXP y, SEXP scores, SEXP weights, List options, bool printMsg) {
  if (y == R_NilValue || is<NumericVector>(y) == FALSE)
    throw LdtException(ErrorType::kLogic, "R-statistics",
                       "'y' should be a numeric vector");
  if (scores == R_NilValue || is<NumericVector>(scores) == FALSE)
    throw LdtException(ErrorType::kLogic, "R-statistics",
                       "'scores' should be a numeric vector");
  NumericVector y0 = as<NumericVector>(y);
  auto N = y0.length();
  if (printMsg)
    Rprintf("Number of observations = %i\n", N);

  NumericVector scores0 = as<NumericVector>(scores);
  if (N != scores0.length())
    throw LdtException(ErrorType::kLogic, "R-statistics",
                       "unequal number of observations in 'y' and 'scores'");

  auto my = ldt::Matrix<double>(&y0[0], N, 1);
  auto mscores = ldt::Matrix<double>(&scores0[0], N, 1);

  NumericVector weights0;
  auto mweights = ldt::Matrix<double>(N, 1);
  auto hasWeight = weights != R_NilValue;
  if (hasWeight) {
    if (is<NumericVector>(weights) == FALSE)
      throw LdtException(ErrorType::kLogic, "R-statistics",
                         "'weights' should be a numeric vector");
    weights0 = as<NumericVector>(weights);
    if (hasWeight && N != weights0.length())
      throw LdtException(ErrorType::kLogic, "R-statistics",
                         "unequal number of observations in 'y' and 'weights'");
    mweights.SetData(&weights0[0]);
  }
  if (printMsg)
    Rprintf("Is Weighted = %s\n", hasWeight ? "TRUE" : "FALSE");

  auto min_y = min(y0);
  if (min_y != 0)
    throw LdtException(ErrorType::kLogic, "R-statistics",
                       "minimum in 'y' vector must be 0");
  auto max_y = max(y0);
  if (max_y != 1)
    throw LdtException(ErrorType::kLogic, "R-statistics",
                       "maximum in 'y' vector must be 1");

  ldt::RocOptions options_;
  UpdateRocOptions(printMsg, options, options_, "Options: ");

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
    points.Set0(i, 0, std::get<0>(auc0->Points.at(i)));
    points.Set0(i, 1, std::get<1>(auc0->Points.at(i)));
  }

  List L = List::create(_["n"] = wrap(N), _["auc"] = wrap(auc->Result),
                        _["points"] = as_matrix(points, nullptr, &colnames));

  L.attr("class") = std::vector<std::string>({"ldtroc", "list"});

  return L;
}

// [[Rcpp::export(.GetGldFromMoments)]]
NumericVector GetGldFromMoments(double mean, double variance, double skewness,
                                double excessKurtosis, int type, double s1,
                                double s2, List nelderMeadOptions,
                                bool printMsg) {
  if (printMsg) {
    Rprintf("Moments=%f, %f, %f, %f\n", mean, variance, skewness,
            excessKurtosis);
    Rprintf("Type=%i\n", type); // TODO: convert to string
    Rprintf("Start (L3, L4)=(%f, %f)\n", s1, s2);
  }

  auto optim = NelderMead(2);
  optim.ParamContraction = nelderMeadOptions["contraction"];
  optim.ParamReflection = nelderMeadOptions["reflection"];
  optim.ParamShrink = nelderMeadOptions["shrink"];
  optim.ParamExpansion = nelderMeadOptions["expansion"];
  optim.Tolerance = nelderMeadOptions["tolerance"];
  optim.MaxIteration = nelderMeadOptions["maxIterations"];

  auto ps = DistributionGld::GetFromMoments(
      mean, variance, skewness, excessKurtosis, type, optim, s1, s2);

  if (optim.Iter == optim.MaxIteration)
    Rf_warning("Maximum number of iteration reached in GLD estimation");

  if (printMsg) {
    Rprintf("....\n");
    Rprintf("Iteration=%i\n", optim.Iter);
    Rprintf("Objective Minimum=%i\n", optim.Min);
    Rprintf("Parameters=%f, %f, %f, %f\n", std::get<0>(ps), std::get<1>(ps),
            std::get<2>(ps), std::get<3>(ps));
  }

  NumericVector result = {std::get<0>(ps), std::get<1>(ps), std::get<2>(ps),
                          std::get<3>(ps), (double)optim.Iter};
  result.names() = std::vector<std::string>({"L1", "L2", "L3", "L4", "Iter"});

  return result;
}

// [[Rcpp::export(.GldQuantile)]]
NumericVector GldQuantile(SEXP data, double L1, double L2, double L3,
                          double L4) {
  if (is<NumericVector>(data) == false)
    throw LdtException(ErrorType::kLogic, "R-statistics",
                       "'data' must be a 'numeric vector'");
  NumericVector data0 = as<NumericVector>(data);
  NumericVector result(data0.length());
  for (int i = 0; i < data0.length(); i++)
    result[i] = DistributionGld::GetQuantile(data0[i], L1, L2, L3, L4);
  return result;
}

// [[Rcpp::export(.GldDensityQuantile)]]
NumericVector GldDensityQuantile(SEXP data, double L1, double L2, double L3,
                                 double L4) {
  if (is<NumericVector>(data) == false)
    throw LdtException(ErrorType::kLogic, "R-statistics",
                       "'data' must be a 'numeric vector'");
  NumericVector data0 = as<NumericVector>(data);

  NumericVector result(data0.length());
  for (int i = 0; i < data0.length(); i++)
    result[i] = DistributionGld::GetDensityQuantile(data0[i], L1, L2, L3, L4);
  return result;
}

// [[Rcpp::export(.CombineStats4)]]
List CombineStats4(List list1, List list2)
// clang-format on
{
  auto r = RunningMoments<4, true, true, Tv>();
  r.Combine(list1["mean"], list1["variance"], list1["skewness"],
            list1["kurtosis"], list1["weight"], list1["count"]);
  r.Combine(list2["mean"], list2["variance"], list2["skewness"],
            list2["kurtosis"], list2["weight"], list2["count"]);
  auto L = List::create(
      _["mean"] = r.GetMean(), _["variance"] = r.GetVariance(),
      _["skewness"] = r.GetSkewness(), _["kurtosis"] = r.GetKurtosis(),
      _["weight"] = r.SumWeights, _["count"] = r.Count);
  return L;
}

// [[Rcpp::export(.GetPca)]]
List GetPca(NumericMatrix x, bool center, bool scale, SEXP newX) {

  auto mx = ldt::Matrix<double>(&x[0], x.nrow(), x.ncol());
  auto mnewX = ldt::Matrix<double>();
  bool hasNewX = newX != R_NilValue;
  if (hasNewX) {
    if (is<NumericMatrix>(newX) == false)
      throw LdtException(ErrorType::kLogic, "R-statistics",
                         "'newX' must be a 'numeric matrix'");
    NumericMatrix newX_ = as<NumericMatrix>(newX);
    mnewX.SetData(&newX_[0], newX_.nrow(), newX_.ncol());
  }

  auto model = PcaAnalysis(x.nrow(), x.ncol(), hasNewX ? mnewX.RowsCount : 0,
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
      _["projections"] = hasNewX
                             ? (SEXP)(NumericMatrix(model.Forecasts.RowsCount,
                                                    model.Forecasts.ColsCount,
                                                    model.Forecasts.Data))
                             : R_NilValue);
  return L;
}
