#include "matrix_utils.h"
#include "r_ldt.h"
#include "varma.h"

using namespace Rcpp;
using namespace ldt;

// [[Rcpp::export(.SearchVarma)]]
SEXP SearchVarma(SEXP y, SEXP x, int numTargets, SEXP ySizes, SEXP yPartitions,
                 SEXP xGroups, SEXP maxParams, int seasonsCount, int maxHorizon,
                 SEXP newX, bool simUsePreviousEstim, double olsStdMultiplier,
                 List lbfgsOptions, List metricOptions, List modelCheckItems,
                 List searchItems, List searchOptions) {

  if (y == R_NilValue)
    throw LdtException(ErrorType::kLogic, "R-varma",
                       "invalid data: 'y' is null");
  if (is<NumericMatrix>(y) == false)
    throw LdtException(ErrorType::kLogic, "R-varma",
                       "'y' must be a 'numeric matrix'");
  y = as<NumericMatrix>(y);

  if (numTargets < 1)
    throw LdtException(ErrorType::kLogic, "R-varma",
                       "number of targets must be positive");

  bool printMsg = false;
  auto options = SearchOptions();
  int reportInterval = 0;
  UpdateSearchOptions(searchOptions, options, reportInterval, printMsg);

  if (x != R_NilValue) {
    if (is<NumericMatrix>(x) == false)
      throw LdtException(ErrorType::kLogic, "R-varma",
                         "'x' must be a 'numeric matrix'");
    x = as<NumericMatrix>(x);
  }
  if (newX != R_NilValue) {
    if (is<NumericMatrix>(newX) == false)
      throw LdtException(ErrorType::kLogic, "R-varma",
                         "'newX' must be a 'numeric matrix'");
    newX = as<NumericMatrix>(newX);
  }

  ldt::Matrix<double> my;
  ldt::Matrix<double> mx;
  ldt::Matrix<double> mw;
  ldt::Matrix<double> mnewX;
  ldt::Matrix<double> mat;
  std::vector<std::string> colNames;
  auto d_re =
      CombineEndoExo(printMsg, mat, colNames, my, mx, mw, mnewX, y, x,
                     R_NilValue, newX, false, false, 1, 0, 0, maxHorizon, true);

  if (numTargets > my.ColsCount)
    throw LdtException(ErrorType::kLogic, "R-varma",
                       "'numTargets' cannot be larger than the number of "
                       "endogenous variables (i.e., columns of 'y')");

  mat.Transpose();

  auto dataset0 = new DatasetTs<true>(mat.RowsCount, mat.ColsCount, true, true);
  auto dataset = std::unique_ptr<ldt::DatasetTs<true>>(dataset0);
  dataset0->Data(mat);
  if (dataset0->HasMissingData)
    throw LdtException(ErrorType::kLogic, "R-varma",
                       "missing observation exists");

  std::vector<int> ySizes_;
  GetSizes(printMsg, ySizes_, ySizes, my.ColsCount, false);

  std::vector<std::vector<int>> yPartitions_;
  GetPartitions(printMsg, yPartitions_, yPartitions, my.ColsCount, 0, false);

  std::vector<std::vector<int>> xGroups_;
  GetGroups(printMsg, xGroups_, xGroups, mx.ColsCount, my.ColsCount, true);

  // Maximum Parameters
  if (maxParams == R_NilValue)
    maxParams = IntegerVector({1, 0, 0, 0, 0, 0});
  auto maxParams_ = as<std::vector<int>>(IntegerVector(maxParams));
  if (maxParams_.size() < 6)
    throw LdtException(ErrorType::kLogic, "R-varma",
                       "'maxParams_' must have 6 parameters");
  if (printMsg) {
    Rprintf("Max Parameters:%s(p,d,q)[P,D,Q]s=(%i,%i,%i)[%i,%i,%i]\n",
            my.ColsCount == 1 ? "ARMA" : "VARMA", maxParams_.at(0),
            maxParams_.at(1), maxParams_.at(2), maxParams_.at(3),
            maxParams_.at(4), maxParams_.at(5));
    Rprintf("Number of Seasons=%i\n", seasonsCount);
  }

  LimitedMemoryBfgsbOptions optim;
  UpdateLbfgsOptions(printMsg, lbfgsOptions, optim);

  // if (maxHorizon > 0 && items.Length1 > 0){ // model must provide predictions
  //   checks.Prediction = true;
  //   checks.Estimation = true;
  // }

  auto metrics = SearchMetricOptions();
  auto metricsNames = std::vector<std::string>();
  auto items = SearchItems();
  auto checks = SearchModelChecks();
  UpdateOptions(printMsg, searchItems, metricOptions, modelCheckItems, metrics,
                items, checks, metricsNames, maxHorizon, mx.ColsCount,
                numTargets, my.ColsCount, true, false, "Horizon", false);

  std::vector<std::string> type1Names;
  if (items.Length1 > 0) {
    if (maxHorizon == 0)
      throw LdtException(
          ErrorType::kLogic, "R-varma",
          "Inconsistent argument and option: If 'type1' is enabled in "
          "'searchItems', 'maxHorizon' cannot be zero");
    checks.Prediction = true;
    for (int i = 0; i < items.Length1; i++)
      type1Names.push_back(std::string("Horizon") + std::to_string(i + 1));
  }

  // Modelset
  auto model =
      VarmaModelset(options, items, metrics, checks, ySizes_, yPartitions_,
                    *dataset0, maxParams_, seasonsCount, xGroups_,
                    simUsePreviousEstim, &optim, olsStdMultiplier, maxHorizon);

  bool estimating = true;

  std::unique_ptr<double[]> W;
  try {
    W = std::unique_ptr<double[]>(new double[model.Modelset.WorkSize]);
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

  ReportProgress(printMsg, reportInterval, model.Modelset, estimating, options,
                 alli);

  if (options.RequestCancel)
    return R_NilValue;

  auto extraLabel = "parameters";
  auto extraNames = std::vector<std::string>(
      {"arP", "arD", "arQ", "maP", "maD", "maQ", "numSeasons"});

  List L = GetModelSetResults(model.Modelset, items, metricsNames,
                              (int)items.Length1, extraLabel, &extraNames,
                              -my.ColsCount + 1, type1Names, colNames,
                              "predictions", "horizon");

  L["info"] = List::create(
      _["yNames"] = colnames(y), _["xNames"] = colnames(x),
      _["seasonsCount"] = wrap(seasonsCount),
      _["olsStdMultiplier"] = wrap(olsStdMultiplier),
      _["simUsePreviousEstim"] = wrap(simUsePreviousEstim),
      _["maxHorizon"] = wrap(checks.Prediction ? maxHorizon : 0),
      _["lbfgsOptions"] = lbfgsOptions, _["metricOptions"] = metricOptions,
      _["modelCheckItems"] = modelCheckItems, _["searchItems"] = searchItems,
      _["searchOptions"] = searchOptions, _["numTargets"] = wrap(numTargets));

  L.attr("class") =
      std::vector<std::string>({"ldtsearchvarma", "ldtsearch", "list"});
  L.attr("method") = "varma";

  return L;
}

// [[Rcpp::export(.EstimVarma)]]
SEXP EstimVarma(SEXP y, SEXP x, SEXP params, int seasonsCount,
                bool addIntercept, List lbfgsOptions, double olsStdMultiplier,
                SEXP pcaOptionsY, SEXP pcaOptionsX, int maxHorizon, SEXP newX,
                int simFixSize, SEXP simHorizons, bool simUsePreviousEstim,
                double simMaxConditionNumber, SEXP simTransform,
                bool printMsg) {

  if (y == R_NilValue)
    throw LdtException(ErrorType::kLogic, "R-varma",
                       "invalid data: 'y' is null");
  if (is<NumericMatrix>(y) == false)
    throw LdtException(ErrorType::kLogic, "R-varma",
                       "'y' must be a 'numeric matrix'");
  y = as<NumericMatrix>(y);

  if (x != R_NilValue) {
    if (is<NumericMatrix>(x) == false)
      throw LdtException(ErrorType::kLogic, "R-varma",
                         "'x' must be a 'numeric matrix'");
    x = as<NumericMatrix>(x);
  }

  if (newX != R_NilValue) {
    if (is<NumericMatrix>(newX) == false)
      throw LdtException(ErrorType::kLogic, "R-varma",
                         "'newX' must be a 'numeric matrix'");
  }
  if (addIntercept && maxHorizon > 0) {
    if (newX == R_NilValue) {
      auto newX0 = NumericMatrix(maxHorizon, 0);
      newX = insert_intercept(newX0);
    } else {
      auto newX0 = as<NumericMatrix>(newX);
      newX = insert_intercept(newX0);
    }
  }

  ldt::Matrix<double> my;
  ldt::Matrix<double> mx;
  ldt::Matrix<double> mw;
  ldt::Matrix<double> mnewX;
  ldt::Matrix<double> mat;
  std::vector<std::string> colNames;
  auto d_re = CombineEndoExo(printMsg, mat, colNames, my, mx, mw, mnewX, y, x,
                             R_NilValue, newX, false, addIntercept, 1, 0, 0,
                             maxHorizon, true);

  int k = mx.ColsCount + (int)addIntercept;

  // Model Parameters
  if (params == R_NilValue)
    params = IntegerVector({1, 0, 0, 0, 0, 0});
  auto params_ = as<std::vector<int>>(IntegerVector(params));
  if (params_.size() < 6)
    throw LdtException(ErrorType::kLogic, "R-varma",
                       "'params' must have 6 parameters");
  if (printMsg) {
    Rprintf("Model:%s(p,d,q)[P,D,Q]s=(%i,%i,%i)[%i,%i,%i]\n",
            my.ColsCount == 1 ? "ARMA" : "VARMA", params_.at(0), params_.at(1),
            params_.at(2), params_.at(3), params_.at(4), params_.at(5));
    Rprintf("Number of Seasons=%i\n", seasonsCount);
  }
  if (maxHorizon < 0)
    throw LdtException(ErrorType::kLogic, "R-varma",
                       "'maxHorizon' cannot be negative");
  if (printMsg)
    Rprintf("Prediction Horizon=%i\n", maxHorizon);

  // PCA
  auto pcaOptionsX0 = PcaAnalysisOptions();
  bool hasPcaX = pcaOptionsX != R_NilValue;
  if (hasPcaX) {
    UpdatePcaOptions(printMsg, as<List>(pcaOptionsX), hasPcaX, pcaOptionsX0,
                     "Exogenous PCA options");
    if (addIntercept)
      pcaOptionsX0.IgnoreFirstCount += 1; // intercept is added here. Ignore it
  }

  auto pcaOptionsY0 = PcaAnalysisOptions();
  bool hasPcaY = pcaOptionsY != R_NilValue;
  if (hasPcaY)
    UpdatePcaOptions(printMsg, as<List>(pcaOptionsY), hasPcaY, pcaOptionsY0,
                     "Endogenous PCA options");

  auto restriction = VarmaRestrictionType::kMaFinal; // TODO: as an option
  auto sizes = VarmaSizes(my.RowsCount, my.ColsCount, k, params_.at(0),
                          params_.at(1), params_.at(2), params_.at(3),
                          params_.at(4), params_.at(5), seasonsCount);

  // L-BFGS
  LimitedMemoryBfgsbOptions optim;
  if (sizes.HasMa) {
    UpdateLbfgsOptions(printMsg, lbfgsOptions, optim);
  } else if (printMsg)
    Rprintf("L-BFGS (skipped).\n");

  // Estimation

  auto model = VarmaExtended(sizes, restriction, true, true, true, maxHorizon,
                             hasPcaY ? &pcaOptionsY0 : nullptr,
                             hasPcaX ? &pcaOptionsX0 : nullptr, &optim);
  auto W = std::unique_ptr<double[]>(new double[model.WorkSize]);
  auto S = std::unique_ptr<double[]>(new double[model.StorageSize]);

  if (printMsg)
    Rprintf("Starting Calculations ..");

  model.Calculate(mat, S.get(), W.get(), false, maxHorizon, 0, -1,
                  olsStdMultiplier);

  if (printMsg)
    Rprintf("Calculations Finished.\n");

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

    if (printMsg) {
      Rprintf("Simulation Horizons:%s\n",
              VectorToCsv(simHorizons_, ',').c_str());
      Rprintf("Maximum Condition Number=%f\n", simMaxConditionNumber);
    }

    std::function<void(double &)> transform = nullptr;
    if (printMsg)
      Rprintf("    - Metric Transform = ");

    if (simTransform == R_NilValue) {
      // do nothing
      if (printMsg)
        Rprintf("none");
    } else if (is<Function>(simTransform)) {

      auto F = as<Function>(simTransform);
      transform = [&F](double &x) { x = as<double>(F(wrap(x))); };
      if (printMsg)
        Rprintf("Custom function");

    } else if (TYPEOF(simTransform) == STRSXP) {

      auto funcType = FromString_FunctionType(as<const char *>(simTransform));

      if (printMsg)
        Rprintf(ToString(funcType));

      if (funcType == FunctionType::kId) { // for tests
        transform = [](double &x) { };
      } else if (funcType == FunctionType::kExp) {
        transform = [](double &x) { x = std::exp(x); };
      } else if (funcType == FunctionType::kPow2) {
        transform = [](double &x) { x = x * x; };
      } else {
        throw LdtException(ErrorType::kLogic, "R-varma",
                           "this type of transformation is not available");
      }
    } else {
      throw LdtException(
          ErrorType::kLogic, "R-varma",
          "invalid 'transform'. It can be null, string or function");
    }
    if (printMsg)
      Rprintf("\n");

    simModel =
        VarmaSimulation(sizes, simFixSize, simHorizons_, metrics, &optim, true,
                        restriction, true, hasPcaY ? &pcaOptionsY0 : nullptr,
                        hasPcaX ? &pcaOptionsX0 : nullptr);

    simModel.KeepDetails = true; // option?!

    auto W0 = std::unique_ptr<double[]>(new double[simModel.WorkSize]);
    S0 = std::unique_ptr<double[]>(
        new double[simModel.StorageSize]); // don't override S

    if (printMsg)
      Rprintf("Starting Simulation ..");

    simModel.CalculateE(S0.get(), W0.get(), mat, simMaxConditionNumber,
                        olsStdMultiplier, false, simUsePreviousEstim,
                        transform ? &transform : nullptr);

    if (printMsg)
      Rprintf("Simulation Finished.\n");
  }

  // Simulation Details
  NumericMatrix simDetails(0, 9);
  colnames(simDetails) = CharacterVector::create(
      "sampleEnd", "metricIndex", "horizon", "targetIndex", "last", "actual",
      "forecast", "error", "std");

  if (simFixSize > 0) {
    simDetails = NumericMatrix(simModel.Details.size(), 9);

    int h = -1;

    for (const auto &a : simModel.Details) {
      h++;
      simDetails(h, 0) = std::get<0>(a);
      simDetails(h, 1) = std::get<1>(a);
      simDetails(h, 2) = std::get<2>(a);
      simDetails(h, 3) = std::get<3>(a);
      simDetails(h, 4) = std::get<4>(a);
      simDetails(h, 5) = std::get<5>(a);
      simDetails(h, 6) = std::get<6>(a);
      simDetails(h, 7) = std::get<7>(a);
      simDetails(h, 8) = std::get<8>(a);
    }
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
    if (fcount > 0) {
      if (printMsg)
        Rprintf("    Failed Count in Simulation=%i\n", fcount);
      int i = -1;
      for (const auto &[k, v] : simModel.Errors) {
        i++;
        if (printMsg)
          Rprintf("        %i. (%i, %.2f) msg=%s\n", i, v,
                  (v / (double)fcount) * 100, k.c_str());
      }
    }
  }

  // Names
  std::vector<std::string> exoNames;
  std::vector<std::string> endoNames;
  for (int i = 0; i < (int)colNames.size(); i++) {
    if (i < my.ColsCount)
      endoNames.push_back(colNames.at(i));
    else
      exoNames.push_back(colNames.at(i));
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
      std::unique_ptr<double[]>(new double[metricCount * my.ColsCount]);
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
      _["counts"] = List::create(
          _["obs"] = wrap(model.Y.ColsCount), _["eq"] = wrap(model.Y.RowsCount),
          _["exoEq"] = wrap(model.X.ColsCount),
          _["expAll"] = wrap(model.Model.Result.gamma.length())),
      _["estimations"] = List::create(
          _["coefs"] =
              as_matrix(model.Model.Result.coef, &endoNames_pca, &expNames),
          _["stds"] =
              as_matrix(model.Model.Result.coefstd, &endoNames_pca, &expNames),
          _["tstats"] =
              as_matrix(model.Model.Result.coeft, &endoNames_pca, &expNames),
          _["pValues"] =
              as_matrix(model.Model.Result.coefprob, &endoNames_pca, &expNames),
          _["gamma"] = as_matrix(model.Model.Result.gamma),
          _["gammaVar"] = as_matrix(model.Model.Result.gammavar),
          _["sigma"] = as_matrix(model.Model.Result.sigma2, &endoNames_pca,
                                 &endoNames_pca)),
      _["metrics"] = as_matrix(metricsRes, &metricsResRowNames, &endoNames_pca),
      _["prediction"] =
          maxHorizon == 0
              ? R_NilValue
              : (SEXP)List::create(
                    _["means"] = as_matrix(model.Forecasts.Forecast,
                                           &endoNames_pca, nullptr),
                    _["vars"] = model.Forecasts.mDoVariance
                                    ? (SEXP)as_matrix(model.Forecasts.Variance,
                                                      &endoNames_pca, nullptr)
                                    : R_NilValue,
                    _["startIndex"] = wrap(model.Forecasts.StartIndex + 1)),
      _["simulation"] = simFixSize == 0
                            ? R_NilValue
                            : (SEXP)List::create(_["validCounts"] =
                                                     wrap(simModel.ValidCounts),
                                                 _["details"] = simDetails,
                                                 _["failed"] = simFails),
      _["info"] =
          List::create(_["y"] = y, _["x"] = x, _["pcaOptionsY"] = pcaOptionsY,
                       _["pcaOptionsX"] = pcaOptionsX));

  L.attr("class") =
      std::vector<std::string>({"ldtestimvarma", "ldtestim", "list"});
  L.attr("method") = "varma";

  return L;
}
