#include "discrete_choice.h"
#include "r_ldt.h"

using namespace Rcpp;
using namespace ldt;

void GetCostMatrices(bool printMsg, std::vector<ldt::Matrix<double>> &result,
                     SEXP costMatrices, bool costMatInMetrics) {
  if (costMatrices != R_NilValue) {
    // don't use as.list or a vector such as c(c1,c2) will have invalid values
    if (is<List>(costMatrices) == false)
      throw std::logic_error("'costMatrices' must be list of double matrices.");

    List costMatrices_ = (List)costMatrices;
    for (int i = 0; i < costMatrices_.length(); i++) {
      if (costMatrices_[i] == R_NilValue)
        throw std::logic_error("A frequency cost matrix is null.");
      if (is<NumericMatrix>(costMatrices_[i]) == false)
        throw std::logic_error(
            "A frequency cost matrix must be a 'numeric matrix'.");
      NumericMatrix m = as<NumericMatrix>(costMatrices_[i]);
      result.push_back(ldt::Matrix<double>(&m[0], m.nrow(), m.ncol()));
    }
  }

  if (printMsg) {
    Rprintf("Number of Cost Matrices=%i\n", result.size());
    for (int i = 0; i < (int)result.size(); i++)
      Rprintf("    %i. Dimension=(%i,%i)\n", i + 1, result.at(i).RowsCount,
              result.at(i).ColsCount);
  }
  if (costMatInMetrics && result.size() == 0)
    throw std::logic_error(
        "At least one frequency cost matrix is required for this type "
        "of out-of-sample evaluation.");
}

void checkData(ldt::Matrix<double> &my, ldt::Matrix<double> &mx,
               ldt::Matrix<double> &mw, int &numChoices, bool &isBinary) {
  double minY = my.Minimum();
  double maxY = my.Maximum();
  if (minY != 0)
    throw std::logic_error("Minimum value in 'y' must be zero.");
  numChoices = maxY + 1;
  if (numChoices < 2)
    stop("Invalid data. Number of choices is less than 2.");

  isBinary = numChoices == 2;
  if (isBinary == false)
    warning("Ordered discrete choice model search is not yet tested in R.");

  if (mw.Any(0))
    warning("Zero weight is found.");
}

// [[Rcpp::export(.SearchDc)]]
SEXP SearchDc(SEXP y, SEXP x, SEXP w, SEXP xSizes, SEXP xPartitions,
              SEXP costMatrices, bool searchLogit, bool searchProbit,
              List optimOptions, List aucOptions, List metricOptions,
              List modelCheckItems, List searchItems, List searchOptions) {

  if (y == R_NilValue || x == R_NilValue)
    throw std::logic_error("Invalid data: 'y' or 'x' is null.");
  if (is<NumericMatrix>(y) == false)
    throw std::logic_error("'y' must be a 'numeric matrix'.");
  if (is<NumericMatrix>(x) == false)
    throw std::logic_error("'x' must be a 'numeric matrix'.");

  int numTargets = 1;

  bool printMsg = false;
  auto options = SearchOptions();
  int reportInterval = 0;
  UpdateSearchOptions(searchOptions, options, reportInterval, printMsg);

  y = as<NumericMatrix>(y);
  x = as<NumericMatrix>(x);

  ldt::Matrix<double> my;
  ldt::Matrix<double> mx;
  ldt::Matrix<double> mw;
  ldt::Matrix<double> mnewX;
  ldt::Matrix<double> mat;
  std::vector<std::string> colNames;
  auto d_re = CombineEndoExo(printMsg, mat, colNames, my, mx, mw, mnewX, y, x,
                             w, R_NilValue, false, true, 1, 0, 0);

  bool hasWeight = w != R_NilValue;
  int exoStart = hasWeight ? 2 : 1;
  int exoCount = mat.ColsCount - exoStart;

  int numChoices;
  bool isBinary;
  checkData(my, mx, mw, numChoices, isBinary);

  std::vector<int> xSizes_;
  GetSizes(printMsg, xSizes_, xSizes, exoCount - 1, true); // ?intercept?

  std::vector<std::vector<int>> xPartitions_;
  GetPartitions(printMsg, xPartitions_, xPartitions, exoCount - 1, 0, true);

  Newton newton;
  UpdateNewtonOptions(printMsg, optimOptions, newton);

  RocOptions aucOptions0;
  UpdateRocOptions(printMsg, aucOptions, aucOptions0, "AUC Options:");

  auto metrics = SearchMetricOptions();
  auto metricsNames = std::vector<std::string>();
  auto items = SearchItems();
  auto checks = SearchModelChecks();
  UpdateOptions(printMsg, searchItems, metricOptions, modelCheckItems, metrics,
                items, checks, metricsNames, exoCount, exoCount, numTargets, 1,
                false, true, "Coefficients", true);

  std::vector<ldt::Matrix<double>> costMatrices0;
  GetCostMatrices(printMsg, costMatrices0, costMatrices,
                  Contains(metrics.MetricsOut, ScoringType::kFrequencyCost));

  // get parameter names
  std::vector<std::string> paramNames;
  for (auto i = 0; i < exoCount; i++)
    paramNames.push_back(colNames.at(exoStart + i));
  for (int i = 0; i < numChoices - 2; i++)
    paramNames.push_back(std::string("Threshold") + std::to_string(i + 1));

  if (searchLogit == false && searchProbit == false)
    throw std::logic_error(
        "Model set is empty. Choose 'Logit' or 'Probit' or both.");
  if (printMsg)
    Rprintf("Distribution Type=%s\n", searchLogit && searchProbit
                                          ? "Logit & Probit"
                                          : (searchLogit ? "Logit" : "Probit"));

  DiscreteChoiceModelsetBase *model = DiscreteChoiceModelsetBase::GetFromTypes(
      isBinary, hasWeight, options, items, metrics, checks, xSizes_, mat,
      costMatrices0, xPartitions_, searchLogit, searchProbit, newton,
      aucOptions0);
  auto modelr = std::unique_ptr<DiscreteChoiceModelsetBase>(model);

  if (printMsg) {
    Rprintf("Model Created\n");
    Rprintf("Number of Searchers=%i\n", model->Searchers.size());
  }

  bool estimating = true;

  std::unique_ptr<double[]> W;
  std::unique_ptr<int[]> Wi;
  try {
    W = std::unique_ptr<double[]>(new double[model->Modelset.WorkSize]);
    Wi = std::unique_ptr<int[]>(new int[model->Modelset.WorkSizeI]);
  } catch (...) {
    throw std::logic_error("More memory is required for running the project.");
  }

  auto alli = model->Modelset.GetExpectedNumberOfModels();

  // handle unhandled exceptions in the async function
  // model->CheckStart();
  auto f = std::async(std::launch::async, [&] {
    model->Start(W.get(), Wi.get());
    estimating = false;
  });

  ReportProgress(printMsg, reportInterval, model->Modelset, estimating, options,
                 alli);

  if (options.RequestCancel)
    return R_NilValue;

  std::vector<std::string> colNames_w = colNames; // no weight for inclusions
  if (hasWeight)
    colNames_w.erase(std::next(colNames_w.begin()));

  auto extraLabel = "dist";
  auto extraNames = std::vector<std::string>({"dist"});
  List L = GetModelSetResults(model->Modelset, items, metricsNames, exoCount,
                              extraLabel, &extraNames, 1, paramNames,
                              colNames_w, "coefs", "item");

  L["info"] = List::create(
      _["yNames"] = colnames(y), _["xNames"] = colnames(x),
      _["costMatrices"] = costMatrices, _["searchLogit"] = wrap(searchLogit),
      _["searchProbit"] = wrap(searchProbit), _["optimOptions"] = optimOptions,
      _["metricOptions"] = metricOptions,
      _["modelCheckItems"] = modelCheckItems, _["searchItems"] = searchItems,
      _["searchOptions"] = searchOptions, _["numTargets"] = wrap(numTargets));

  L.attr("class") =
      std::vector<std::string>({"ldtsearchdc", "ldtsearch", "list"});
  L.attr("method") = "bin";

  return L;
}

// [[Rcpp::export(.EstimDc)]]
SEXP EstimDc(SEXP y, SEXP x, SEXP w, std::string linkFunc, SEXP newX,
             SEXP pcaOptionsX, SEXP costMatrices, List aucOptions,
             int simFixSize, double simTrainRatio, int simTrainFixSize,
             int simSeed, bool weightedEval, bool printMsg) {
  if (y == R_NilValue || x == R_NilValue)
    throw std::logic_error("Invalid data: 'y' or 'x' is null.");
  if (is<NumericMatrix>(y) == false)
    throw std::logic_error("'y' must be a 'numeric matrix'.");
  if (is<NumericMatrix>(x) == false)
    throw std::logic_error("'x' must be a 'numeric matrix'.");

  y = as<NumericMatrix>(y);
  x = as<NumericMatrix>(x);

  if (newX != R_NilValue) {
    if (is<NumericMatrix>(newX) == false)
      throw std::logic_error("'newX' must be a 'numeric matrix'.");
    newX = insert_intercept(
        newX); // Combine function does not handle adding intercept to newX
  }

  ldt::Matrix<double> my;
  ldt::Matrix<double> mx;
  ldt::Matrix<double> mw;
  ldt::Matrix<double> mnewX;
  ldt::Matrix<double> mat;
  std::vector<std::string> colNames;
  auto d_re = CombineEndoExo(printMsg, mat, colNames, my, mx, mw, mnewX, y, x,
                             w, newX, true, true, 1, 0, 0);

  bool hasWeight = w != R_NilValue;
  int exoStart = hasWeight ? 2 : 1;

  int numChoices;
  bool isBinary;
  checkData(my, mx, mw, numChoices, isBinary);

  if (simFixSize > 0) {
    simTrainFixSize = std::max(0, simTrainFixSize);
    if (simTrainFixSize == 0) {
      if (simTrainRatio <= 0 || simTrainRatio >= 1)
        throw std::logic_error("Invalid 'simTrainRatio'. It must be in (0,1).");
    }

    if (simSeed < 0)
      throw std::logic_error("Invalid 'simSeed'. It cannot be negative.");
  }

  std::vector<ldt::Matrix<double>> costMatrices0;
  GetCostMatrices(printMsg, costMatrices0, costMatrices,
                  costMatrices != R_NilValue);

  auto pcaOptions0 = PcaAnalysisOptions();
  bool hasPcaX = pcaOptionsX != R_NilValue;
  if (hasPcaX) {
    if (is<List>(pcaOptionsX) == false)
      throw std::logic_error("'pcaOptionsX' must be a 'List'.");
    List pcaOptionsX_ = as<List>(pcaOptionsX);
    UpdatePcaOptions(printMsg, pcaOptionsX_, hasPcaX, pcaOptions0,
                     "Exogenous PCA options");
    pcaOptions0.IgnoreFirstCount += 1; // intercept is added here. Ignore it
  }

  auto endoNames = std::vector<std::string>();
  endoNames.push_back(colNames.at(0));

  const char *modelType = "binary";
  if (numChoices > 2)
    modelType = "ordered";
  DiscreteChoiceModelType modelType0 =
      FromString_DiscreteChoiceModelType(modelType);
  DiscreteChoiceDistType distType0 =
      FromString_DiscreteChoiceDistType(linkFunc.c_str());
  if (printMsg) {
    Rprintf("Model Type=%s\n", ToString(modelType0));
    Rprintf("Distribution Type=%s\n", ToString(distType0));
  }

  bool hasProjection = mnewX.Data != nullptr;
  if (printMsg)
    Rprintf("Number of Projections=%i\n", mnewX.RowsCount);

  RocOptions aucOptions0;
  UpdateRocOptions(printMsg, aucOptions, aucOptions0, "AUC Options:");

  // ESTIMATE

  auto model = DiscreteChoiceExtended(
      modelType0, distType0, mat.RowsCount, mat.ColsCount, hasWeight, true,
      numChoices, true, mnewX.RowsCount, hasPcaX ? &pcaOptions0 : nullptr,
      costMatrices0.size() > 0 ? &costMatrices0 : nullptr, weightedEval);

  auto W = std::unique_ptr<double[]>(new double[model.WorkSize]);
  auto S = std::unique_ptr<double[]>(new double[model.StorageSize]);

  if (printMsg)
    Rprintf("Calculations started...");
  model.Calculate(mat, S.get(), W.get(), numChoices,
                  hasProjection ? &mnewX : nullptr, aucOptions0);
  if (printMsg)
    Rprintf("Calculations finished.\n");

  DiscreteChoiceSimBase *simmodel = nullptr;
  std::unique_ptr<DiscreteChoiceSimBase> simmodelf;
  std::unique_ptr<double[]> Ss;
  if (simFixSize > 0) {
    if (printMsg) {
      Rprintf("Simulating a Discrete Choice Model...\n");
      Rprintf("Train Ratio=%f\n", simTrainRatio);
      Rprintf("Train Fix Size=%f\n", simTrainFixSize);
      Rprintf("Number of Simulations=%i\n", simFixSize);
      Rprintf("Seed=%i\n", simSeed);
    }

    simmodel = DiscreteChoiceSimBase::GetFromType(
        hasWeight, modelType0, distType0, mat.RowsCount, mat.ColsCount,
        numChoices, simTrainRatio, simTrainFixSize, costMatrices0.size(), true,
        true, true, hasPcaX ? &pcaOptions0 : nullptr, weightedEval);
    simmodelf = std::unique_ptr<DiscreteChoiceSimBase>(simmodel);
    simmodel->SimulationMax = simFixSize;
    simmodel->Seed = simSeed;

    auto Ws = std::unique_ptr<double[]>(new double[simmodel->WorkSize]);
    auto WIs = std::unique_ptr<int[]>(new int[simmodel->WorkSizeI]);
    Ss = std::unique_ptr<double[]>(new double[simmodel->StorageSize]);
    auto errors = std::set<const char *>();
    if (printMsg)
      Rprintf("Calculations Started...");

    bool cancel = false;
    simmodel->Calculate(mat, &costMatrices0, Ss.get(), Ws.get(), WIs.get(),
                        cancel, aucOptions0, true, &errors);
    if (printMsg) {
      Rprintf("Calculations Finished.\n");
      if (errors.size() > 0) {
        Rprintf("Errors:");
        for (auto ssd : errors) {
          Rprintf("    - %s\n", ssd);
        }
      }
    }
    if (simmodel->ValidSimulationCount == 0) {
      throw std::logic_error("no valid simulation exists.\n");
    }
  }

  // update names due to PCA (TODO: param names for ordered models)
  auto exoNames = std::vector<std::string>();
  for (int i = exoStart; i < mat.ColsCount; i++)
    exoNames.push_back(colNames.at(i));
  std::vector<std::string> exoNames_pca;
  if (hasPcaX) {
    for (int i = 0; i < model.X.ColsCount; i++) {
      if (i < pcaOptions0.IgnoreFirstCount)
        exoNames_pca.push_back(exoNames.at(i));
      else
        exoNames_pca.push_back(
            std::string("X_PC") +
            std::to_string(i - pcaOptions0.IgnoreFirstCount + 1));
    }
  } else
    exoNames_pca = exoNames;

  // Metrics
  int metricCount = 6; // logL, aic, sic, brierIn aucIn, costIn,
  if (simFixSize > 0)
    metricCount += 3; // brierOur aucOut, costOut
  int eqCount = 1;
  auto metricsResD = std::unique_ptr<double[]>(
      new double[metricCount * eqCount]); // for 1 equation
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
      _["counts"] = List::create(
          _["obs"] = wrap(model.Y.RowsCount), _["eq"] = wrap(model.Y.ColsCount),
          _["exoEq"] = wrap(model.X.ColsCount),
          _["exoAll"] = wrap(model.Model->Beta.length())),
      _["estimations"] = List::create(
          _["coefs"] = as_matrix(model.Model->Beta, &exoNames_pca, &endoNames),
          _["stds"] =
              as_matrix(model.Model->BetaStd, &exoNames_pca, &endoNames),
          _["zstats"] =
              as_matrix(model.Model->BetaZ, &exoNames_pca, &endoNames),
          _["pValues"] =
              as_matrix(model.Model->BetaProb, &exoNames_pca, &endoNames),
          _["gamma"] = as_matrix(model.Model->Beta),
          _["gammaVar"] = as_matrix(model.Model->BetaVar)),
      _["metrics"] = as_matrix(metricsRes, &metricsResRowNames, &endoNames),
      _["projection"] = hasProjection == false
                            ? R_NilValue
                            : (SEXP)as_matrix(model.PredProbs),
      _["simulation"] =
          simFixSize == 0
              ? R_NilValue
              : (SEXP)List::create(
                    _["validCounts"] =
                        wrap(simmodel->ValidSimulationCount),
                    _["probFrequencies"] = as_matrix(simmodel->FrequencyTable),
                    _["costRatios"] =
                        costMatrices0.size() > 0
                            ? (SEXP)as_matrix(simmodel->CostRatios)
                            : R_NilValue),
      _["info"] =
          List::create(_["y"] = y, _["x"] = x, _["pcaOptionsX"] = pcaOptionsX));

  L.attr("class") =
      std::vector<std::string>({"ldtestimdc", "ldtestim", "list"});
  L.attr("method") = "bin";

  return L;
}
