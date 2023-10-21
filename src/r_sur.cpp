#include "r_ldt.h"
#include "sur.h"

using namespace Rcpp;
using namespace ldt;

// [[Rcpp::export(.SearchSur)]]
SEXP SearchSur(List data, List combinations, List metrics, List modelChecks,
               List items, List options, int searchSigMaxIter,
               double searchSigMaxProb) {

  auto options_ = SearchOptions();
  UpdateSearchOptions(options, options_);

  auto data_ = SearchData();
  UpdateSearchData(data, data_);

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

  auto length1 = data_.NumExo;

  UpdateOptions(items, metrics, modelChecks, metrics_, items_, checks_,
                metricsNames, length1, data_.NumExo, combinations_.NumTargets,
                data_.NumEndo, false, true, "Coefficients", false);

  auto targetNames = std::vector<std::string>(
      colNames.begin(), colNames.begin() + items_.LengthTargets);

  // Modelset
  auto model = SurModelset(data_, combinations_, options_, items_, metrics_,
                           checks_, searchSigMaxIter, searchSigMaxProb);

  bool estimating = true;

  std::unique_ptr<double[]> W;
  try {
    W = std::unique_ptr<double[]>(new double[model.Modelset.WorkSize]);
  } catch (...) {
    throw LdtException(ErrorType::kLogic, "R-sur",
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

  auto extraNames = std::vector<std::string>({"extra"});
  List L = GetModelSetResults(
      model.Modelset, items_, metricsNames, colNames, targetNames, extraNames,
      exoNames, colNames, std::string("coefs"), options_.ReportInterval > 0);

  return L;
}

// [[Rcpp::export(.EstimSur)]]
SEXP EstimSur(List data, int searchSigMaxIter, double searchSigMaxProb,
              SEXP restriction, SEXP pcaOptionsY, SEXP pcaOptionsX,
              int simFixSize, double simTrainRatio, int simTrainFixSize,
              int simSeed, double simMaxConditionNumber) {

  auto data_ = SearchData();
  UpdateSearchData(data, data_);

  // Restrictions
  bool hasR = restriction != R_NilValue;
  ldt::Matrix<double> restriction_;
  if (hasR) {
    auto rest0 = as<NumericMatrix>(restriction);
    restriction_.SetData(&rest0[0], rest0.nrow(), rest0.ncol());
  }

  std::unique_ptr<double[]> R_d;
  if (searchSigMaxIter > 0) {
    // create the R for significant search
    int mk = data_.NumExo * data_.NumEndo;
    hasR = true;
    R_d = std::unique_ptr<double[]>(new double[mk * mk]);
    restriction_.SetData(R_d.get(), mk, mk);
  }

  // PCA
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

  // Estimate

  auto model = SurExtended(
      data_.Data.RowsCount, data_.NumEndo, data_.NumExo, hasR, true, true,
      data_.NewObsCount, searchSigMaxIter, true,
      hasPcaY ? &pcaOptionsY0 : nullptr, hasPcaX ? &pcaOptionsX0 : nullptr);
  auto W = std::unique_ptr<double[]>(new double[model.WorkSize]);
  auto S = std::unique_ptr<double[]>(new double[model.StorageSize]);

  model.Calculate(data_.Data, data_.NumEndo, S.get(), W.get(),
                  hasR ? &restriction_ : nullptr, searchSigMaxProb,
                  data_.NewObsCount > 0 ? &data_.NewX : nullptr, nullptr);

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

    simModel = SurSimulation(
        data_.Data.RowsCount, data_.NumEndo, data_.NumExo, simTrainRatio,
        simTrainFixSize, metrics, hasR, searchSigMaxIter,
        hasPcaY ? &pcaOptionsY0 : nullptr, hasPcaX ? &pcaOptionsX0 : nullptr);

    auto W0 = std::unique_ptr<double[]>(new double[simModel.WorkSize]);
    S0 = std::unique_ptr<double[]>(new double[simModel.StorageSize]);

    bool cancel = false; //??
    simModel.Calculate(data_.Data, data_.NumEndo, S0.get(), W0.get(),
                       hasR ? &restriction_ : nullptr, cancel, simFixSize,
                       simSeed, searchSigMaxProb, simMaxConditionNumber,
                       INT32_MAX,
                       data_.Lambdas.size() > 0 ? &data_.Lambdas : nullptr);
  }

  // Metrics
  int metricCount = 5; // logL, aic, sic, ...
  if (simFixSize > 0)
    metricCount += simModel.Results.RowsCount;
  auto metricsResD =
      std::unique_ptr<double[]>(new double[metricCount * data_.NumEndo]);
  auto metricsRes =
      ldt::Matrix<double>(metricsResD.get(), metricCount, model.Y.ColsCount);
  auto metricsResRowNames = std::vector<std::string>({
      "logL", "aic", "sic", "hqic", "r2" //, "f", "fProb"
  });
  metricsRes.SetRow(0, model.Model.logLikelihood);
  metricsRes.SetRow(1, model.Model.Aic);
  metricsRes.SetRow(2, model.Model.Sic);
  metricsRes.SetRow(3, model.Model.Hqic);
  metricsRes.SetRow(4, model.Model.r2);
  // metricsRes.SetRow(5, model.Model.f); // check its validity
  // metricsRes.SetRow(6, model.Model.f_prob);
  if (simFixSize > 0) {
    int k = 5;
    for (auto m : metricNames) {
      metricsResRowNames.push_back(m);
      metricsRes.SetRowFromRow(k, simModel.Results, k - 5);
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

    /*if (fcount > 0) {
      if (options_.)
        Rprintf("    Failed Count in Simulation=%i\n", fcount);
      int i = -1;
      for (const auto &[k, v] : simModel.Errors) {
        i++;
        if (printMsg)
          Rprintf("        %i. (%i, %.2f) msg=%s\n", i, v,
                  (v / (double)fcount) * 100, k.c_str());
      }
    }*/
  }

  // Names
  auto colNames = as<std::vector<std::string>>(colnames(data["data"]));
  std::vector<std::string> endoNames;
  std::vector<std::string> exoNames;
  for (auto i = 0; i < data_.NumEndo + data_.NumExo; i++) {
    if (i < data_.NumEndo)
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
      _["estimations"] = List::create(
          _["Y"] = as_matrix(model.Y, std::vector<std::string>(),
                             endoNames_pca), // might change due to PCA
          _["X"] = as_matrix(model.X, std::vector<std::string>(),
                             exoNames_pca), // might change due to PCA
          _["coefs"] = as_matrix(model.Model.beta, exoNames_pca, endoNames_pca),
          _["stds"] =
              as_matrix(model.Model.e_beta_std, exoNames_pca, endoNames_pca),
          _["tstats"] =
              as_matrix(model.Model.e_beta_t, exoNames_pca, endoNames_pca),
          _["pValues"] =
              as_matrix(model.Model.e_beta_prob, exoNames_pca, endoNames_pca),
          _["gamma"] = as_matrix(model.Model.gamma),
          _["gammaVar"] = as_matrix(model.Model.gamma_var),
          _["resid"] = as_matrix(model.Model.resid, std::vector<std::string>(),
                                 endoNames_pca),
          _["sigma"] =
              as_matrix(model.Model.resid_var, endoNames_pca, endoNames_pca),
          _["isRestricted"] =
              model.Model.pR
                  ? (SEXP)as_matrix(isRestricted, exoNames_pca, endoNames_pca)
                  : R_NilValue),
      _["metrics"] = as_matrix(metricsRes, metricsResRowNames, endoNames_pca),
      _["projection"] =
          data_.NewObsCount == false
              ? R_NilValue
              : (SEXP)List::create(
                    _["means"] = as_matrix(model.Projections.Means),
                    _["vars"] =
                        model.Projections.mDoVariance
                            ? (SEXP)as_matrix(model.Projections.Variances)
                            : (SEXP)R_NilValue),
      _["simulation"] =
          simFixSize == 0 ? R_NilValue
                          : (SEXP)List::create(
                                _["validIter"] = wrap(simModel.ValidIters),
                                _["validCounts"] = wrap(simModel.ValidCounts),
                                _["failed"] = simFails));

  return L;
}
