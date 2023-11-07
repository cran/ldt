#include "matrix_utils.h"
#include "r_ldt.h"
#include "varma.h"

using namespace Rcpp;
using namespace ldt;

// [[Rcpp::export(.SearchVarma)]]
SEXP SearchVarma(List data, List combinations, List metrics, List modelChecks,
                 List items, List options, IntegerVector maxParams,
                 int seasonsCount, int maxHorizon, bool simUsePreviousEstim,
                 double olsStdMultiplier, List lbfgsOptions) {

  auto options_ = SearchOptions();
  UpdateSearchOptions(options, options_);

  auto data_ = SearchData();
  UpdateSearchData(data, data_);

  // don't transpose and use R matrix data (It is used later in R)
  auto data_use_d = std::make_unique<double[]>(data_.Data.length());
  auto data_use =
      ldt::Matrix(data_use_d.get(), data_.Data.ColsCount, data_.Data.RowsCount);
  data_.Data.Transpose(data_use);
  auto dataset0 = std::make_unique<DatasetTs<true>>(
      data_use.RowsCount, data_use.ColsCount, true, true);
  dataset0->Data(data_use);
  if (dataset0->HasMissingData)
    throw LdtException(ErrorType::kLogic, "R-varma",
                       "missing observation exists");

  auto exoStart = data_.NumEndo;
  auto colNames = as<std::vector<std::string>>(colnames(data["data"]));
  auto exoNames =
      std::vector<std::string>(colNames.begin() + exoStart, colNames.end());

  auto combinations_ = SearchCombinations();
  UpdateSearchCombinations(combinations, combinations_);

  auto metrics_ = SearchMetricOptions();
  auto metricsNames = std::vector<std::string>();
  auto items_ = SearchItems();
  auto checks_ = SearchModelChecks();

  auto length1 = maxHorizon;

  UpdateOptions(items, metrics, modelChecks, metrics_, items_, checks_,
                metricsNames, length1, data_.NumExo, combinations_.NumTargets,
                data_.NumEndo, true, false, "Horizon", false);

  auto targetNames = std::vector<std::string>(
      colNames.begin(), colNames.begin() + items_.LengthTargets);

  LimitedMemoryBfgsbOptions optim;
  UpdateLbfgsOptions(lbfgsOptions, optim);

  std::vector<std::string> type1Names;
  if (items_.Length1 > 0) {
    for (int i = 0; i < items_.Length1; i++)
      type1Names.push_back(std::string("Horizon") + std::to_string(i + 1));
  }

  auto maxParams_ = as<std::vector<int>>(maxParams);

  // Modelset
  auto model =
      VarmaModelset(data_, combinations_, options_, items_, metrics_, checks_,
                    *dataset0, maxParams_, seasonsCount, simUsePreviousEstim,
                    &optim, olsStdMultiplier, maxHorizon);

  bool estimating = true;

  std::unique_ptr<double[]> W;
  try {
    W = std::make_unique<double[]>(model.Modelset.WorkSize);
  } catch (...) {
    throw LdtException(ErrorType::kLogic, "R-varma",
                       "more memory is required for running the project");
  }

  auto alli = model.Modelset.GetExpectedNumberOfModels();

  // handle unhandled exceptions in the async function
  // model.CheckStart();
  auto f = std::async(std::launch::async, [&] {
    model.Modelset.Start(W.get(), nullptr);
    estimating = false;
  });

  ReportProgress(model.Modelset, estimating, options_, alli);

  if (options_.RequestCancel)
    return R_NilValue;

  auto extraNames =
      std::vector<std::string>({"arP", "arD", "arQ", "maP", "maD", "maQ"});

  List L = GetModelSetResults(model.Modelset, items_, metricsNames, colNames,
                              targetNames, extraNames, type1Names, colNames,
                              std::string("predictions"),
                              options_.ReportInterval > 0);

  return L;
}

// [[Rcpp::export(.EstimVarma)]]
SEXP EstimVarma(List data, IntegerVector params, int seasonsCount,
                List lbfgsOptions, double olsStdMultiplier, SEXP pcaOptionsY,
                SEXP pcaOptionsX, int maxHorizon, int simFixSize,
                SEXP simHorizons, bool simUsePreviousEstim,
                double simMaxConditionNumber) {

  auto data_ = SearchData();
  UpdateSearchData(data, data_);

  auto colNames = as<std::vector<std::string>>(colnames(data["data"]));
  auto endoNames = std::vector<std::string>(colNames.begin(),
                                            colNames.begin() + data_.NumEndo);
  auto exoNames = std::vector<std::string>(colNames.begin() + data_.NumEndo,
                                           colNames.end());

  auto params_ = as<std::vector<int>>(params);

  auto pcaOptionsX0 = PcaAnalysisOptions();
  bool hasPcaX = pcaOptionsX != R_NilValue;
  if (hasPcaX) {
    List pcaOptionsX_ = as<List>(pcaOptionsX);
    UpdatePcaOptions(pcaOptionsX_, pcaOptionsX0);
    if (data_.HasIntercept)
      pcaOptionsX0.IgnoreFirstCount += 1; // intercept is added here. Ignore it
  }

  auto pcaOptionsY0 = PcaAnalysisOptions();
  bool hasPcaY = pcaOptionsY != R_NilValue;
  if (hasPcaY) {
    List pcaOptionsY_ = as<List>(pcaOptionsY);
    UpdatePcaOptions(pcaOptionsY_, pcaOptionsY0);
  }

  auto restriction = VarmaRestrictionType::kMaFinal; // TODO: as an option
  auto sizes =
      VarmaSizes(data_.Data.RowsCount, data_.NumEndo, data_.NumExo,
                 params_.at(0), params_.at(1), params_.at(2), params_.at(3),
                 params_.at(4), params_.at(5), seasonsCount);

  // L-BFGS
  LimitedMemoryBfgsbOptions optim;
  if (sizes.HasMa)
    UpdateLbfgsOptions(lbfgsOptions, optim);

  // Estimation

  auto model = VarmaExtended(sizes, restriction, true, true, true, maxHorizon,
                             hasPcaY ? &pcaOptionsY0 : nullptr,
                             hasPcaX ? &pcaOptionsX0 : nullptr, &optim);
  auto W = std::make_unique<double[]>(model.WorkSize);
  auto S = std::make_unique<double[]>(model.StorageSize);

  model.Calculate(data_.Data, S.get(), W.get(), false, maxHorizon, 0, -1,
                  olsStdMultiplier);

  // save isRestricted before running simulation because pR changes
  auto isRestricted = ldt::Matrix<double>();
  std::unique_ptr<double[]> isRestrictedD;
  if (model.Restriction.IsRestricted) {
    auto num_c = model.Restriction.R.RowsCount;
    isRestrictedD = std::make_unique<double[]>(num_c);
    isRestricted = ldt::Matrix<double>(isRestrictedD.get(),
                                       model.Model.Result.coef.RowsCount,
                                       model.Model.Result.coef.ColsCount);
    auto rowinds = std::vector<int>();
    model.Restriction.R.RowsSum(isRestricted, rowinds);
    for (int i = 0; i < isRestricted.length(); i++) {
      if (isRestricted.Data[i] != 0)
        isRestricted.Data[i] = 0;
      else
        isRestricted.Data[i] = 1;
    }
  }

  // Simulation

  std::vector<ScoringType> metrics;
  std::vector<std::string> metricNames;
  VarmaSimulation simModel;
  std::unique_ptr<double[]> S0;
  if (simFixSize > 0) {

    metrics = std::vector<ScoringType>(
        {ScoringType::kSign, ScoringType::kDirection, ScoringType::kMae,
         ScoringType::kMape, ScoringType::kRmse, ScoringType::kRmspe,
         ScoringType::kCrps});
    metricNames = std::vector<std::string>();
    for (auto &a : metrics)
      metricNames.push_back(ToString(a));

    // Simulation Horizons
    std::vector<int> simHorizons_;
    if (simHorizons == R_NilValue && maxHorizon > 0) {
      for (int i = 0; i < maxHorizon; i++)
        simHorizons_.push_back(i + 1);
    } else if (simHorizons != R_NilValue) {
      if (is<IntegerVector>(simHorizons) == false)
        throw LdtException(ErrorType::kLogic, "R-varma",
                           "'simHorizons' must be an 'integer vector'");
      auto hors = as<IntegerVector>(simHorizons);
      for (int i = 0; i < hors.length(); i++) {
        if (hors[i] <= 0)
          throw LdtException(ErrorType::kLogic, "R-varma",
                             "zero or negative value in 'simHorizons'");
        simHorizons_.push_back(hors[i]);
      }
    } else
      throw LdtException(ErrorType::kLogic, "R-varma",
                         "simulation horizons are missing");

    simModel =
        VarmaSimulation(sizes, simFixSize, simHorizons_, metrics, &optim, true,
                        restriction, true, hasPcaY ? &pcaOptionsY0 : nullptr,
                        hasPcaX ? &pcaOptionsX0 : nullptr);

    simModel.KeepDetails = true; // option?!

    auto W0 = std::make_unique<double[]>(simModel.WorkSize);
    S0 = std::make_unique<double[]>(simModel.StorageSize); // don't override S

    simModel.CalculateE(S0.get(), W0.get(), data_.Data, simMaxConditionNumber,
                        olsStdMultiplier, false, simUsePreviousEstim,
                        data_.Lambdas.size() > 0 ? &data_.Lambdas : nullptr);
  }

  // Simulation Details
  DataFrame simDetails;

  if (simFixSize > 0) {
    int n = simModel.Details.size();

    CharacterVector metric(n), target(n);
    IntegerVector sampleEnd(n), horizon(n);
    NumericVector last(n), actual(n), prediction(n), error(n), std(n);

    for (int i = 0; i < n; ++i) {

      // The items are: sample end, metric index, horizon, target index, last
      // value, actual value, prediction, prediction error, std

      sampleEnd[i] = std::get<0>(simModel.Details[i]);
      metric[i] = metricNames.at(std::get<1>(simModel.Details[i]));
      horizon[i] = std::get<2>(simModel.Details[i]);
      target[i] = endoNames.at(std::get<3>(simModel.Details[i]));
      last[i] = std::get<4>(simModel.Details[i]);
      actual[i] = std::get<5>(simModel.Details[i]);
      prediction[i] = std::get<6>(simModel.Details[i]);
      error[i] = std::get<7>(simModel.Details[i]);
      std[i] = std::get<8>(simModel.Details[i]);
    }

    simDetails = DataFrame::create(
        Named("metric") = metric, Named("target") = target,
        Named("sampleEnd") = sampleEnd, Named("horizon") = horizon,
        Named("last") = last, Named("actual") = actual,
        Named("prediction") = prediction, Named("error") = error,
        Named("std") = std);
  }

  // Simulation Failures
  List simFails;
  if (simFixSize > 0) { // Failures
    simFails = List(simModel.Errors.size());
    int fcount = 0;
    int h = 0;
    for (const auto &[k, v] : simModel.Errors) {
      h++;
      fcount += v;
      simFails[h - 1] =
          List::create(_["message"] = wrap(k), _["count"] = wrap(v));
    }
  }

  std::vector<std::string> exoNames_pca;
  if (hasPcaX) {
    for (int i = 0; i < model.Model.Sizes.ExoCount; i++) {
      if (i < pcaOptionsX0.IgnoreFirstCount)
        exoNames_pca.push_back(exoNames.at(i));
      else
        exoNames_pca.push_back(
            std::string("X_PC") +
            std::to_string(i - pcaOptionsX0.IgnoreFirstCount + 1));
    }
  } else
    exoNames_pca = exoNames;

  std::vector<std::string> endoNames_pca;
  if (hasPcaY) {
    for (int i = 0; i < model.Model.Result.y.RowsCount; i++) {
      if (i < pcaOptionsY0.IgnoreFirstCount)
        endoNames_pca.push_back(endoNames.at(i));
      else
        endoNames_pca.push_back(
            std::string("Y_PC") +
            std::to_string(i - pcaOptionsY0.IgnoreFirstCount + 1));
    }
  } else
    endoNames_pca = endoNames;

  // Metrics
  int metricCount = 3; // logL, aic, sic
  if (simFixSize > 0)
    metricCount += simModel.ResultAggs.RowsCount;
  auto metricsResD =
      std::unique_ptr<double[]>(new double[metricCount * data_.NumEndo]);
  auto metricsRes =
      ldt::Matrix<double>(metricsResD.get(), metricCount, endoNames_pca.size());
  auto metricsResRowNames = std::vector<std::string>({"logL", "aic", "sic"});
  metricsRes.SetRow(0, model.Model.Result.LogLikelihood);
  metricsRes.SetRow(1, model.Model.Result.Aic);
  metricsRes.SetRow(2, model.Model.Result.Sic);

  if (simFixSize > 0) {
    int k = 3;
    for (auto m : metricNames) {
      metricsResRowNames.push_back(m);
      metricsRes.SetRowFromRow(k, simModel.ResultAggs, k - 3);
      k++;
    }
  }

  auto expNames = std::vector<std::string>();
  VarmaStorage::GetExpNames(sizes, endoNames_pca, exoNames_pca, expNames);

  List L = List::create(
      _["estimations"] = List::create(
          _["Y"] = Rcpp::transpose(
              as_matrix(model.Model.Result.y, endoNames_pca,
                        std::vector<std::string>())), // might change due to PCA
          _["X"] = as_matrix(model.Model.Result.Xt, std::vector<std::string>(),
                             expNames),
          _["coefs"] = Rcpp::transpose(
              as_matrix(model.Model.Result.coef, endoNames_pca, expNames)),
          _["stds"] = Rcpp::transpose(
              as_matrix(model.Model.Result.coefstd, endoNames_pca, expNames)),
          _["tstats"] = Rcpp::transpose(
              as_matrix(model.Model.Result.coeft, endoNames_pca, expNames)),
          _["pValues"] = Rcpp::transpose(
              as_matrix(model.Model.Result.coefprob, endoNames_pca, expNames)),
          _["gamma"] = as_matrix(model.Model.Result.gamma),
          _["gammaVar"] = as_matrix(model.Model.Result.gammavar),
          _["resid"] =
              Rcpp::transpose(as_matrix(model.Model.Result.resid, endoNames_pca,
                                        std::vector<std::string>())),
          _["sigma"] = as_matrix(model.Model.Result.sigma2, endoNames_pca,
                                 endoNames_pca),
          _["isRestricted"] = model.Restriction.IsRestricted
                                  ? (SEXP)Rcpp::transpose(as_matrix(
                                        isRestricted, endoNames_pca, expNames))
                                  : R_NilValue),
      _["metrics"] = as_matrix(metricsRes, metricsResRowNames, endoNames_pca),
      _["prediction"] =
          maxHorizon == 0
              ? R_NilValue
              : (SEXP)List::create(
                    _["means"] =
                        as_matrix(model.Forecasts.Forecast, endoNames_pca),
                    _["vars"] = model.Forecasts.mDoVariance
                                    ? (SEXP)as_matrix(model.Forecasts.Variance,
                                                      endoNames_pca)
                                    : R_NilValue,
                    _["startIndex"] = wrap(model.Forecasts.StartIndex + 1)),
      _["simulation"] = simFixSize == 0
                            ? R_NilValue
                            : (SEXP)List::create(_["validCounts"] =
                                                     wrap(simModel.ValidCounts),
                                                 _["details"] = simDetails,
                                                 _["failed"] = simFails));

  return L;
}
