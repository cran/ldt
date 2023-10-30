#include "discrete_choice.h"
#include "r_ldt.h"

using namespace Rcpp;
using namespace ldt;

void getCostMatrices(SEXP costMatricesR,
                     std::vector<ldt::Matrix<double>> &costMatrices) {
  if (costMatricesR != R_NilValue) {
    if (is<List>(costMatricesR) == false)
      throw LdtException(ErrorType::kLogic, "R-dc",
                         "'costMatrices' must be list of double matrices");

    List cms = as<List>(costMatricesR);
    for (int i = 0; i < cms.length(); i++) {
      NumericMatrix m = as<NumericMatrix>(cms[i]);
      costMatrices.push_back(ldt::Matrix<double>(&m[0], m.nrow(), m.ncol()));
    }
  }
}

// [[Rcpp::export(.SearchDc)]]
SEXP SearchDc(List data, List combinations, List metrics, List modelChecks,
              List items, List options, SEXP costMatrices, bool searchLogit,
              bool searchProbit, List newtonOptions, List aucOptions,
              int numChoices) {

  auto options_ = SearchOptions();
  UpdateSearchOptions(options, options_);

  auto data_ = SearchData();
  UpdateSearchData(data, data_);

  auto exoStart = data_.NumEndo;
  auto colNames = as<std::vector<std::string>>(colnames(data["data"]));
  auto paramNames =
      std::vector<std::string>(colNames.begin() + exoStart, colNames.end());

  auto combinations_ = SearchCombinations();
  UpdateSearchCombinations(combinations, combinations_);

  auto metrics_ = SearchMetricOptions();
  auto metricsNames = std::vector<std::string>();
  auto items_ = SearchItems();
  auto checks_ = SearchModelChecks();

  auto length1 = data_.NumExo + numChoices - 2;

  UpdateOptions(items, metrics, modelChecks, metrics_, items_, checks_,
                metricsNames, length1, data_.NumExo, combinations_.NumTargets,
                data_.NumEndo, false, true, "Coefficients", false);

  auto targetNames = std::vector<std::string>(
      colNames.begin(), colNames.begin() + items_.LengthTargets);

  // update parameter names
  for (int i = 0; i < numChoices - 2; i++)
    paramNames.push_back(std::string("Threshold") + std::to_string(i + 1));

  if (searchLogit == false && searchProbit == false)
    throw LdtException(
        ErrorType::kLogic, "R-dc",
        "model set is empty. Choose 'Logit' or 'Probit' or both");

  std::vector<ldt::Matrix<double>> costMatrices_;
  getCostMatrices(costMatrices, costMatrices_);

  ldt::RocOptions roc_;
  UpdateRocOptions(aucOptions, roc_);

  ldt::Newton newton_;
  UpdateNewtonOptions(newtonOptions, newton_);

  DiscreteChoiceModelsetBase *model = DiscreteChoiceModelsetBase::GetFromTypes(
      numChoices == 2, data_.HasWeight, data_, combinations_, options_, items_,
      metrics_, checks_, data_.Data, costMatrices_, searchLogit, searchProbit,
      newton_, roc_);
  auto modelr = std::unique_ptr<DiscreteChoiceModelsetBase>(model);

  bool estimating = true;

  std::unique_ptr<double[]> W;
  std::unique_ptr<int[]> Wi;
  try {
    W = std::make_unique<double[]>(model->Modelset.WorkSize);
    Wi = std::make_unique<int[]>(model->Modelset.WorkSizeI);
  } catch (...) {
    throw LdtException(ErrorType::kLogic, "R-dc",
                       "more memory is required for running the project");
  }

  auto alli = model->Modelset.GetExpectedNumberOfModels();

  // handle unhandled exceptions in the async function
  // model->CheckStart();
  auto f = std::async(std::launch::async, [&] {
    model->Start(W.get(), Wi.get());
    estimating = false;
  });

  ReportProgress(model->Modelset, estimating, options_, alli);

  if (options_.RequestCancel)
    return R_NilValue;

  std::vector<std::string> colNames_w = colNames; // no weight for inclusions
  if (data_.HasWeight)
    colNames_w.erase(std::next(colNames_w.begin()));

  auto extraNames = std::vector<std::string>({"dist"});
  List L = GetModelSetResults(model->Modelset, items_, metricsNames, colNames,
                              targetNames, extraNames, paramNames, colNames_w,
                              "coef", options_.ReportInterval > 0);

  return L;
}

// [[Rcpp::export(.EstimDc)]]
SEXP EstimDc(List data, std::string linkFunc, SEXP pcaOptionsX,
             SEXP costMatrices, List newtonOptions, List aucOptions,
             int simFixSize, double simTrainRatio, int simTrainFixSize,
             int simSeed, double simMaxConditionNumber, int numChoices,
             bool weightedEval) {

  auto data_ = SearchData();
  UpdateSearchData(data, data_);

  auto exoStart = data_.NumEndo + (data_.HasWeight ? 1 : 0);
  auto colNames = as<std::vector<std::string>>(colnames(data["data"]));
  auto paramNames =
      std::vector<std::string>(colNames.begin() + exoStart, colNames.end());
  auto endoNames = std::vector<std::string>();
  endoNames.push_back(colNames.at(0));
  for (int i = 0; i < numChoices - 2; i++)
    paramNames.push_back(std::string("Threshold") + std::to_string(i + 1));
  auto labelNames = std::vector<std::string>();
  for (int i = 0; i < numChoices; i++)
    labelNames.push_back(std::string("Y=") + std::to_string(i));

  std::vector<ldt::Matrix<double>> costMatrices_;
  getCostMatrices(costMatrices, costMatrices_);

  ldt::RocOptions roc_;
  UpdateRocOptions(aucOptions, roc_);

  ldt::Newton newton_;
  UpdateNewtonOptions(newtonOptions, newton_);

  auto pcaOptionsX0 = PcaAnalysisOptions();
  bool hasPcaX = pcaOptionsX != R_NilValue;
  if (hasPcaX) {
    List pcaOptionsX_ = as<List>(pcaOptionsX);
    UpdatePcaOptions(pcaOptionsX_, pcaOptionsX0);
    if (data_.HasIntercept)
      pcaOptionsX0.IgnoreFirstCount += 1; // intercept is added here. Ignore it
  }

  const char *modelType = "binary";
  if (numChoices > 2)
    modelType = "ordered";
  DiscreteChoiceModelType modelType0 =
      FromString_DiscreteChoiceModelType(modelType);
  DiscreteChoiceDistType distType0 =
      FromString_DiscreteChoiceDistType(linkFunc.c_str());

  // ESTIMATE

  auto model = DiscreteChoiceExtended(
      modelType0, distType0, data_.Data.RowsCount, data_.Data.ColsCount,
      data_.HasWeight, true, numChoices, true, data_.NewX.RowsCount,
      hasPcaX ? &pcaOptionsX0 : nullptr,
      costMatrices_.size() > 0 ? &costMatrices_ : nullptr, weightedEval);

  auto W = std::make_unique<double[]>(model.WorkSize);
  auto S = std::make_unique<double[]>(model.StorageSize);

  model.Calculate(data_.Data, S.get(), W.get(), true,
                  data_.NewX.RowsCount > 0 ? &data_.NewX : nullptr, roc_);

  // calculate residuals

  auto resid_d = std::make_unique<double[]>(model.X.RowsCount * numChoices);
  auto resid =
      ldt::Matrix<double>(resid_d.get(), model.X.RowsCount, numChoices);
  auto resid_d_w = std::make_unique<double[]>(model.X.RowsCount);
  model.Model->GetProbabilities(model.X, resid, resid_d_w.get());
  resid.GetColumn0(1, resid);
  resid.Restructure0(model.X.RowsCount, 1);
  model.Y.Subtract(resid, resid);
  // calculate Pearson residuals
  if (numChoices > 2)
    throw LdtException(
        ErrorType::kLogic, "R-dc",
        "not implemented for 'numChoices > 2'\n"); // check the mathematics
  auto resid_d_p = std::make_unique<double[]>(resid.RowsCount);
  auto resid_p = ldt::Matrix<double>(resid_d_p.get(), resid.RowsCount, 1);
  for (int i = 0; i < resid.RowsCount; i++) {
    auto yhat_i = model.Y.Data[i] - resid.Data[i];
    resid_p.Data[i] = resid.Data[i] / std::sqrt(yhat_i * (1 - yhat_i));
  }

  // Simulation
  std::unique_ptr<DiscreteChoiceSimBase> simmodel = nullptr;
  std::unique_ptr<double[]> Ss;
  if (simFixSize > 0) {
    simmodel = DiscreteChoiceSimBase::GetFromType(
        data_.HasWeight, modelType0, distType0, data_.Data.RowsCount,
        data_.Data.ColsCount, numChoices, simTrainRatio, simTrainFixSize,
        costMatrices_.size(), true, true, true,
        hasPcaX ? &pcaOptionsX0 : nullptr, weightedEval);
    simmodel->SimulationMax = simFixSize;
    simmodel->Seed = simSeed;

    auto Ws = std::make_unique<double[]>(simmodel->WorkSize);
    auto WIs = std::make_unique<int[]>(simmodel->WorkSizeI);
    Ss = std::make_unique<double[]>(simmodel->StorageSize);
    auto errors = std::set<const char *>();

    bool cancel = false;
    simmodel->Calculate(
        data_.Data, costMatrices_.size() > 0 ? &costMatrices_ : nullptr,
        Ss.get(), Ws.get(), WIs.get(), cancel, roc_, true, &errors);

    if (simmodel->ValidSimulationCount == 0) {
      throw LdtException(ErrorType::kLogic, "R-dc",
                         "no valid simulation exists.\n");
    }
  }

  // update names due to PCA (TODO: param names for ordered models)
  auto exoNames = std::vector<std::string>();
  for (int i = exoStart; i < data_.Data.ColsCount; i++)
    exoNames.push_back(colNames.at(i));
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

  // Metrics
  int metricCount = 6; // logL, aic, sic, brierIn aucIn, costIn,
  if (simFixSize > 0)
    metricCount += 3; // brierOur aucOut, costOut
  int eqCount = 1;
  auto metricsResD =
      std::make_unique<double[]>(metricCount * eqCount); // for 1 equation
  auto metricsRes =
      ldt::Matrix<double>(metricsResD.get(), metricCount, eqCount);
  auto metricsResRowNames = std::vector<std::string>(
      {"logL", "aic", "sic", "brierIn",
       "aucIn", // keep names consistent with the metrics in the estimation
       "frequencyCostIn"});
  metricsRes.Set0(0, 0, model.Model->LogL);
  metricsRes.Set0(1, 0, model.Model->Aic);
  metricsRes.Set0(2, 0, model.Model->Sic);
  metricsRes.Set0(3, 0, model.BrierScore);
  metricsRes.Set0(4, 0, model.Auc);
  metricsRes.Set0(5, 0, model.CostRatioAvg);
  if (simFixSize > 0) {
    metricsResRowNames.push_back("brierOut");
    metricsResRowNames.push_back("aucOut");
    metricsResRowNames.push_back("frequencyCostOut");
    metricsRes.Set0(6, 0, simmodel->BrierScore);
    metricsRes.Set0(7, 0, simmodel->Auc);
    metricsRes.Set0(8, 0, simmodel->CostRatios.Mean());
  }

  List L = List::create(
      _["estimations"] = List::create(
          _["Y"] = as_matrix(model.Y, std::vector<std::string>(), endoNames),
          _["X"] = as_matrix(model.X, std::vector<std::string>(), exoNames_pca),
          _["coefs"] = as_matrix(model.Model->Beta, exoNames_pca, endoNames),
          _["stds"] = as_matrix(model.Model->BetaStd, exoNames_pca, endoNames),
          _["zstats"] = as_matrix(model.Model->BetaZ, exoNames_pca, endoNames),
          _["pValues"] =
              as_matrix(model.Model->BetaProb, exoNames_pca, endoNames),
          _["gamma"] = as_matrix(model.Model->Beta),
          _["gammaVar"] = as_matrix(model.Model->BetaVar),
          _["resid"] = as_matrix(resid, std::vector<std::string>(), endoNames),
          _["residPearson"] =
              as_matrix(resid_p, std::vector<std::string>(), endoNames)),
      _["metrics"] = as_matrix(metricsRes, metricsResRowNames, endoNames),
      _["projection"] =
          data_.NewX.RowsCount == 0
              ? R_NilValue
              : (SEXP)as_matrix(model.PredProbs, std::vector<std::string>(),
                                labelNames),
      _["simulation"] =
          simFixSize == 0
              ? R_NilValue
              : (SEXP)List::create(
                    _["validCounts"] =
                        wrap(simmodel->ValidSimulationCount),
                    _["probFrequencies"] = as_matrix(simmodel->FrequencyTable),
                    _["costRatios"] =
                        costMatrices_.size() > 0
                            ? (SEXP)as_matrix(simmodel->CostRatios)
                            : R_NilValue));

  return L;
}
