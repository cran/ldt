#include "r_ldt.h"
#include "sur.h"

using namespace Rcpp;
using namespace ldt;

// [[Rcpp::export(.SearchSur)]]
SEXP SearchSur(SEXP y, SEXP x, int numTargets, SEXP xSizes, SEXP xPartitions,
               int numFixXPartitions, SEXP yGroups, int searchSigMaxIter,
               double searchSigMaxProb, List metricOptions,
               List modelCheckItems, List searchItems, List searchOptions) {

  if (y == R_NilValue)
    throw std::logic_error("Invalid data: 'y' is null.");
  if (is<NumericMatrix>(y) == false)
    throw std::logic_error("'y' must be a 'numeric matrix'.");
  y = as<NumericMatrix>(y);

  if (numTargets < 1)
    throw std::logic_error("Number of targets must be positive.");
  if (numFixXPartitions < 0)
    throw std::logic_error(
        "Invalid 'numFixXPartitions'. It cannot be negative.");

  bool printMsg = false;
  auto options = SearchOptions();
  int reportInterval = 0;
  UpdateSearchOptions(searchOptions, options, reportInterval, printMsg);

  if (x != R_NilValue) {
    if (is<NumericMatrix>(x) == false)
      throw std::logic_error("'x' must be a 'numeric matrix'.");
    x = as<NumericMatrix>(x);
  }

  ldt::Matrix<double> my;
  ldt::Matrix<double> mx;
  ldt::Matrix<double> mw;
  ldt::Matrix<double> mnewX;
  ldt::Matrix<double> mat;
  std::vector<std::string> colNames;
  auto d_re = CombineEndoExo(printMsg, mat, colNames, my, mx, mw, mnewX, y, x,
                             R_NilValue, R_NilValue, false /*remove nan*/,
                             false /*add intercept*/, 1, 1, 0, 0,
                             false /*append new x*/);

  if (numTargets > my.ColsCount)
    throw std::logic_error("'numTargets' cannot be larger than the number of "
                           "endogenous variables (i.e., columns of 'y').");
  auto exoStart = my.ColsCount;
  int exoCount = mat.ColsCount - exoStart; // be careful with adding intercept

  std::vector<std::string> exoNames;
  for (auto i = 0; i < exoCount; i++)
    exoNames.push_back(colNames.at(exoStart + i));

  auto xSizes_ = std::vector<int>();
  GetSizes(printMsg, xSizes_, xSizes, mx.ColsCount, true);

  std::vector<std::vector<int>> xPartitions_;
  GetPartitions(printMsg, xPartitions_, xPartitions, mx.ColsCount, my.ColsCount,
                true);

  std::vector<std::vector<int>> yGroups_;
  GetGroups(printMsg, yGroups_, yGroups, my.ColsCount, 0, false);

  if (searchSigMaxIter < 0)
    throw std::logic_error("invalid 'searchSigMaxIter'.");
  if (searchSigMaxProb < 0 || searchSigMaxProb >= 1)
    throw std::logic_error("Invalid 'searchSigMaxProb'.");
  if (searchSigMaxIter > 0 && searchSigMaxProb == 0)
    throw std::logic_error(
        "'searchSigMaxProb' cannot be zero when search is enabled.");
  if (printMsg) {
    Rprintf("Search For Significant Coefficients:\n");
    if (searchSigMaxIter > 0) {
      Rprintf("    - Maximum Number of Iterations=%i\n", searchSigMaxIter);
      Rprintf("    - Maximum P-Value=%f\n", searchSigMaxProb);
    } else
      Rprintf("    - disabled\n");
  }

  auto metrics = SearchMetricOptions();
  auto metricsNames = std::vector<std::string>();
  auto items = SearchItems();
  auto checks = SearchModelChecks();
  UpdateOptions(printMsg, searchItems, metricOptions, modelCheckItems, metrics,
                items, checks, metricsNames, mx.ColsCount, mx.ColsCount,
                numTargets, my.ColsCount, false, true, "Coefficients", false);

  // Modelset
  auto model = SurModelset(options, items, metrics, checks, xSizes_,
                           xPartitions_, numFixXPartitions, mat, yGroups_,
                           searchSigMaxIter, searchSigMaxProb);

  bool estimating = true;

  std::unique_ptr<double[]> W;
  try {
    W = std::unique_ptr<double[]>(new double[model.Modelset.WorkSize]);
  } catch (...) {
    throw std::logic_error("More memory is required for running the project.");
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

  auto extraLabel = "";
  auto extraNames = std::vector<std::string>({"extra"});
  List L = GetModelSetResults(model.Modelset, items, metricsNames, exoCount,
                              extraLabel, &extraNames, -exoStart + 1, exoNames,
                              colNames, "coefs", "item");

  L["info"] = List::create(
      _["yNames"] = colnames(y), _["xNames"] = colnames(x),
      _["searchSigMaxIter"] = wrap(searchSigMaxIter),
      _["searchSigMaxProb"] = wrap(searchSigMaxProb),
      _["metricOptions"] = metricOptions,
      _["modelCheckItems"] = modelCheckItems, _["searchItems"] = searchItems,
      _["searchOptions"] = searchOptions, _["numTargets"] = wrap(numTargets));

  L.attr("class") =
      std::vector<std::string>({"ldtsearchsur", "ldtsearch", "list"});
  L.attr("method") = "sur";

  return L;
}

// [[Rcpp::export(.EstimSur)]]
SEXP EstimSur(SEXP y, SEXP x, bool addIntercept, int searchSigMaxIter,
              double searchSigMaxProb, SEXP restriction, SEXP newX,
              SEXP pcaOptionsY, SEXP pcaOptionsX, int simFixSize,
              double simTrainRatio, int simTrainFixSize, int simSeed,
              double simMaxConditionNumber, bool printMsg) {

  if (y == R_NilValue || x == R_NilValue)
    throw std::logic_error("Invalid data: 'y' or 'x' is null.");

  if (is<NumericMatrix>(y) == false)
    throw std::logic_error("'y' must be a 'numeric matrix'.");
  if (is<NumericMatrix>(x) == false)
    throw std::logic_error("'x' must be a 'numeric matrix'.");

  y = as<NumericMatrix>(y);
  x = as<NumericMatrix>(x);

  if (newX != R_NilValue && addIntercept) {
    if (is<NumericMatrix>(y) == false)
      throw std::logic_error("'newX' must be a 'numeric matrix'.");
    NumericMatrix newX0 = as<NumericMatrix>(newX);
    newX = insert_intercept(
        newX0); // Combine function does not handle adding intercept to newX
  }

  ldt::Matrix<double> my;
  ldt::Matrix<double> mx;
  ldt::Matrix<double> mw;
  ldt::Matrix<double> mnewX;
  ldt::Matrix<double> mat;
  std::vector<std::string> colNames;
  auto d_re = CombineEndoExo(printMsg, mat, colNames, my, mx, mw, mnewX, y, x,
                             R_NilValue /*weight*/, newX, true /*remove nan*/,
                             addIntercept /*add intercept*/, 1, 0 /*min x*/, 0,
                             0, false /*append new x*/);
  bool hasNewX = mnewX.ColsCount > 0;
  int m = my.ColsCount;
  int k = mx.ColsCount + (int)addIntercept;

  // Restrictions
  bool hasR = restriction != R_NilValue;
  ldt::Matrix<double> restriction_;
  if (hasR) {
    if (is<NumericMatrix>(restriction) == false)
      throw std::logic_error("'restriction' must be a 'numeric matrix'.");
    NumericMatrix rest0 = as<NumericMatrix>(restriction);
    restriction_.SetData(&rest0[0], rest0.nrow(), rest0.ncol());
    if (printMsg)
      Rprintf("Restriction Matrix Dimension=(%i, %i)\n", restriction_.RowsCount,
              restriction_.ColsCount);
  }

  // Significant Search
  searchSigMaxIter = std::max(searchSigMaxIter, 0);
  if (searchSigMaxIter > 0 && hasR)
    throw std::logic_error("Invalid model. 'restriction' must be null when "
                           "significant search is enabled. ");

  if (searchSigMaxProb < 0 || searchSigMaxProb >= 1)
    throw std::logic_error("Invalid 'searchSigMaxProb'. It must be in [0,1).");
  if (searchSigMaxIter > 0 && searchSigMaxProb == 0)
    throw std::logic_error(
        "'searchSigMaxProb' cannot be zero when search is enabled.");

  std::unique_ptr<double[]> R_d;
  if (printMsg)
    Rprintf("Search For Significant Coefficients:\n");
  if (searchSigMaxIter > 0) {
    if (printMsg) {
      Rprintf("    - Maximum Number of Iterations=%i\n", searchSigMaxIter);
      Rprintf("    - Maximum P-Value=%f\n", searchSigMaxProb);
    }

    // after check create the R for significant search
    int mk = k * m;
    hasR = true;
    R_d = std::unique_ptr<double[]>(new double[mk * mk]);
    restriction_.SetData(R_d.get(), mk, mk);
  } else {
    if (printMsg)
      Rprintf("    - disabled\n");
    searchSigMaxProb = 0;
  }

  // Simulation
  if (simFixSize > 0) {
    simTrainFixSize = std::max(0, simTrainFixSize);
    if (simTrainFixSize == 0) {
      if (simTrainRatio <= 0 || simTrainRatio >= 1)
        throw std::logic_error("Invalid 'simTrainRatio'. It must be in (0,1).");
    }
    if (simSeed < 0)
      throw std::logic_error("Invalid 'simSeed'. It cannot be negative.");
    if (printMsg)
      Rprintf("Number of Out-of-Sample Simulations=%i\n", simFixSize);
    if (printMsg) {
      if (simTrainFixSize > 0)
        Rprintf("    - Train Sample Size=%i\n", simTrainFixSize);
      else
        Rprintf("    - Train Sample Size (ratio)=%f\n", simTrainRatio);
      Rprintf("    - Maximum Condition Number=%.1e\n", simMaxConditionNumber);
      Rprintf("    - Seed=%i\n", simSeed);
    }
  } else if (printMsg) {
    Rprintf("Simulation: (skipped).\n");
  }

  // PCA
  auto pcaOptionsX0 = PcaAnalysisOptions();
  bool hasPcaX = pcaOptionsX != R_NilValue;
  if (hasPcaX) {
    if (is<List>(pcaOptionsX) == false)
      throw std::logic_error("'pcaOptionsX' must be a 'List'.");
    List pcaOptionsX_ = as<List>(pcaOptionsX);

    UpdatePcaOptions(printMsg, pcaOptionsX_, hasPcaX, pcaOptionsX0,
                     "Exogenous PCA options");
    if (addIntercept)
      pcaOptionsX0.IgnoreFirstCount += 1; // intercept is added here. Ignore it
  }

  auto pcaOptionsY0 = PcaAnalysisOptions();
  bool hasPcaY = pcaOptionsY != R_NilValue;
  if (hasPcaY) {
    if (is<List>(pcaOptionsY) == false)
      throw std::logic_error("'pcaOptionsY' must be a 'List'.");
    List pcaOptionsY_ = as<List>(pcaOptionsY);
    UpdatePcaOptions(printMsg, pcaOptionsY_, hasPcaY, pcaOptionsY0,
                     "Endogenous PCA options");
  }

  // Estimate

  auto model = SurExtended(
      mat.RowsCount, m, k, hasR, false, // I removed NAN
      true, hasNewX ? mnewX.RowsCount : 0, searchSigMaxIter, true,
      hasPcaY ? &pcaOptionsY0 : nullptr, hasPcaX ? &pcaOptionsX0 : nullptr);
  auto W = std::unique_ptr<double[]>(new double[model.WorkSize]);
  auto S = std::unique_ptr<double[]>(new double[model.StorageSize]);
  if (printMsg)
    Rprintf("Starting Calculations ...");

  model.Calculate(mat, m, S.get(), W.get(), hasR ? &restriction_ : nullptr,
                  searchSigMaxProb, hasNewX ? &mnewX : nullptr, nullptr);
  if (printMsg)
    Rprintf("Calculations Finished.\n");

  if (printMsg && searchSigMaxIter > 0) {
    Rprintf("Significant Search Iteration = %i\n", model.Model.mSigSearchIter);
  }

  // save isRestricted before running simulation because pR changes
  auto isRestricted = ldt::Matrix<double>();
  auto isRestrictedD = std::unique_ptr<double[]>();
  if (model.Model.pR) {
    auto num_c = model.Model.pR->RowsCount;
    isRestrictedD = std::unique_ptr<double[]>(new double[num_c]);
    isRestricted =
        ldt::Matrix<double>(isRestrictedD.get(), model.Model.beta.RowsCount,
                            model.Model.beta.ColsCount);
    auto rowinds = std::vector<int>();
    model.Model.pR->RowsSum(isRestricted, rowinds);
    for (int i = 0; i < isRestricted.length(); i++) {
      if (isRestricted.Data[i] != 0)
        isRestricted.Data[i] = 0;
      else
        isRestricted.Data[i] = 1;
    }
  }

  auto metrics = std::vector<ScoringType>(
      {// report all metrics
       ScoringType::kMae, ScoringType::kMape, ScoringType::kRmse,
       ScoringType::kRmspe, ScoringType::kCrps, ScoringType::kSign});
  auto metricNames = std::vector<std::string>();
  for (auto &a : metrics)
    metricNames.push_back(ToString(a));

  SurSimulation simModel;
  std::unique_ptr<double[]> S0;
  if (simFixSize > 0) {

    simModel = SurSimulation(mat.RowsCount, m, k, simTrainRatio,
                             simTrainFixSize, metrics, hasR, searchSigMaxIter,
                             hasPcaY ? &pcaOptionsY0 : nullptr,
                             hasPcaX ? &pcaOptionsX0 : nullptr);

    auto W0 = std::unique_ptr<double[]>(new double[simModel.WorkSize]);
    S0 = std::unique_ptr<double[]>(new double[simModel.StorageSize]);
    if (printMsg)
      Rprintf("Starting Simulation ...");
    bool cancel = false; //??
    simModel.Calculate(mat, m, S0.get(), W0.get(),
                       hasR ? &restriction_ : nullptr, cancel, simFixSize,
                       simSeed, searchSigMaxProb, simMaxConditionNumber);

    if (printMsg)
      Rprintf("Simulation Finished.\n");
  }

  // Metrics
  int metricCount = 7; // logL, aic, sic, ...
  if (simFixSize > 0)
    metricCount += simModel.Results.RowsCount;
  auto metricsResD = std::unique_ptr<double[]>(new double[metricCount * m]);
  auto metricsRes =
      ldt::Matrix<double>(metricsResD.get(), metricCount, model.Y.ColsCount);
  auto metricsResRowNames = std::vector<std::string>(
      {"logL", "aic", "sic", "hqic", "r2", "f", "fProb"});
  metricsRes.SetRow(0, model.Model.logLikelihood);
  metricsRes.SetRow(1, model.Model.Aic);
  metricsRes.SetRow(2, model.Model.Sic);
  metricsRes.SetRow(3, model.Model.Hqic);
  metricsRes.SetRow(4, model.Model.r2);
  metricsRes.SetRow(5, model.Model.f);
  metricsRes.SetRow(6, model.Model.f_prob);
  if (simFixSize > 0) {
    int k = 7;
    for (auto m : metricNames) {
      metricsResRowNames.push_back(m);
      metricsRes.SetRowFromRow(k, simModel.Results, k - 7);
      k++;
    }
  }

  List simFails;
  if (simFixSize > 0) { // Failures
    simFails = List(simModel.Errors.size());
    int fcount = 0;
    int h = -1;
    for (const auto &[k, v] : simModel.Errors) {
      h++;
      fcount += v;
      simFails[h] = List::create(_["message"] = wrap(k), _["count"] = wrap(v));
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
  std::vector<std::string> endoNames;
  std::vector<std::string> exoNames;
  for (auto i = 0; i < mat.ColsCount; i++) {
    if (i < m)
      endoNames.push_back(colNames.at(i));
    else
      exoNames.push_back(colNames.at(i));
  }

  std::vector<std::string> exoNames_pca;
  if (hasPcaX) {
    for (int i = 0; i < model.X.ColsCount; i++) {
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
    for (int i = 0; i < model.Y.ColsCount; i++) {
      if (i < pcaOptionsY0.IgnoreFirstCount)
        endoNames_pca.push_back(endoNames.at(i));
      else
        endoNames_pca.push_back(
            std::string("Y_PC") +
            std::to_string(i - pcaOptionsY0.IgnoreFirstCount + 1));
    }
  } else
    endoNames_pca = endoNames;

  List L = List::create(
      _["counts"] = List::create(
          _["obs"] = wrap(model.Y.RowsCount), _["eq"] = wrap(model.Y.ColsCount),
          _["exoEq"] = wrap(model.X.ColsCount),
          _["exoAll"] = wrap(model.Model.gamma.length())),
      _["estimations"] = List::create(
          _["coefs"] =
              as_matrix(model.Model.beta, &exoNames_pca, &endoNames_pca),
          _["stds"] =
              as_matrix(model.Model.e_beta_std, &exoNames_pca, &endoNames_pca),
          _["tstats"] =
              as_matrix(model.Model.e_beta_t, &exoNames_pca, &endoNames_pca),
          _["pValues"] =
              as_matrix(model.Model.e_beta_prob, &exoNames_pca, &endoNames_pca),
          _["gamma"] = as_matrix(model.Model.gamma),
          _["gammaVar"] = as_matrix(model.Model.gamma_var),
          _["sigma"] =
              as_matrix(model.Model.resid_var, &endoNames_pca, &endoNames_pca),
          _["isRestricted"] =
              model.Model.pR
                  ? (SEXP)as_matrix(isRestricted, &exoNames_pca, &endoNames_pca)
                  : R_NilValue),
      _["metrics"] = as_matrix(metricsRes, &metricsResRowNames, &endoNames_pca),
      _["projection"] =
          hasNewX == false
              ? R_NilValue
              : (SEXP)List::create(
                    _["means"] = as_matrix(model.Projections.Means),
                    _["vars"] = as_matrix(model.Projections.Variances)),
      _["simulation"] =
          simFixSize == 0 ? R_NilValue
                          : (SEXP)List::create(
                                _["validIter"] = wrap(simModel.ValidIters),
                                _["validCounts"] = wrap(simModel.ValidCounts),
                                _["failed"] = simFails),
      _["info"] =
          List::create(_["y"] = y, _["x"] = x, _["pcaOptionsY"] = pcaOptionsY,
                       _["pcaOptionsX"] = pcaOptionsX));

  L.attr("class") =
      std::vector<std::string>({"ldtestimsur", "ldtestim", "list"});
  L.attr("method") = "sur";

  return L;
}
