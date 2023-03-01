#include "discrete_choice.h"
#include "r_ldt.h"

using namespace Rcpp;
using namespace ldt;

void GetCostMatrices(bool printMsg, std::vector<ldt::Matrix<double>> &result,
                     SEXP costMatrices, bool costMatInMeasures) {
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
  if (costMatInMeasures && result.size() == 0)
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

// clang-format off

//' Discrete Choice Search
//'
//' @param y (numeric matrix) endogenous data with variable in the column.
//' @param x (numeric matrix) exogenous data with variables in the columns.
//' @param w (numeric vector) weights of the observations in \code{y}. null means equal weights.
//' @param xSizes (nullable int vector) Number of exogenous variables in the regressions. E.g., c(1,2) means the model set contains all the regressions with 1 and 2 exogenous variables. If null, c(1) is used.
//' @param xPartitions (nullable list of int vector) a partition over the indexes of the exogenous variables. No regression is estimated with two variables in the same group. If null, each variable is placed in its own group and the size of the model set is maximized.
//' @param costMatrices (list of numeric matrix) each frequency cost matrix determines how to score the calculated probabilities. Given the number of choices 'n', a frequency cost matrix is a 'm x n+1' matrix. The first column determines the thresholds. Cells in the j-th column determines the costs corresponding to the (j-1)-th choice in \code{y}. It can be null if it is not selected in \code{measureOptions}.
//' @param searchLogit (bool) if \code{TRUE}, logit regressions are added to the model set.
//' @param searchProbit (bool) if \code{TRUE}, probit regressions are added to the model set.
//' @param optimOptions (nullable list) Newton optimization options. see \code{[GetNewtonOptions()]}.
//' @param aucOptions (nullable list) AUC calculation options. see \code{[GetRocOptions()]}.
//' @param measureOptions (nullable list) see \code{[GetMeasureOptions()]}.
//' @param modelCheckItems (nullable list) see \code{[GetModelCheckItems()]}.
//' @param searchItems (nullable list) see \code{[GetSearchItems()]}.
//' @param searchOptions (nullable list) see \code{[GetSearchOptions()]}.
//'
//' @return A list
//'
//' @export
// [[Rcpp::export]]
SEXP DcSearch(SEXP y, SEXP x, SEXP w = R_NilValue, SEXP xSizes = R_NilValue,
              SEXP xPartitions = R_NilValue, SEXP costMatrices = R_NilValue,
              bool searchLogit = true, bool searchProbit = false,
              SEXP optimOptions = R_NilValue, SEXP aucOptions = R_NilValue, SEXP measureOptions = R_NilValue,
              SEXP modelCheckItems = R_NilValue, SEXP searchItems = R_NilValue,
              SEXP searchOptions = R_NilValue)
// clang-format on
{

  if (y == R_NilValue || x == R_NilValue)
    throw std::logic_error("Invalid data: 'y' or 'x' is null.");
  if (is<NumericMatrix>(y) == false)
    throw std::logic_error("'y' must be a 'numeric matrix'.");
  if (is<NumericMatrix>(x) == false)
    throw std::logic_error("'x' must be a 'numeric matrix'.");

  auto startTime = boost::posix_time::to_simple_string(
      boost::posix_time::second_clock::local_time());

  int numTargets = 1;

  List searchOptions_;
  if (searchOptions != R_NilValue) {
    if (is<List>(searchOptions) == false)
      throw std::logic_error("'searchOptions' must be a 'List'.");
    searchOptions_ = as<List>(searchOptions);
    CheckSearchOptions(searchOptions_);
  } else
    searchOptions_ = GetSearchOptions();

  bool printMsg = false;
  auto options = SearchOptions();
  int reportInterval = 0;
  UpdateSearchOptions(searchOptions_, options, reportInterval, printMsg);

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
  if (optimOptions == R_NilValue) {
    List optimOptions_ = GetNewtonOptions();
    UpdateNewtonOptions(printMsg, optimOptions_, newton);
  } else {
    if (is<List>(optimOptions) == false)
      throw std::logic_error("'optimOptions' must be a 'List'.");
    List optimOptions_ = as<List>(optimOptions);
    CheckNewtonOptions(optimOptions_);
    UpdateNewtonOptions(printMsg, optimOptions_, newton);
  }

  RocOptions aucOptions0;
  if (aucOptions == R_NilValue) {
    List aucOptions_ = GetRocOptions();
    UpdateRocOptions(printMsg, aucOptions_, aucOptions0, "AUC Options:");
  } else {
    if (is<List>(aucOptions) == false)
      throw std::logic_error("'aucOption' must be a 'List'.");
    auto aucOptions_ = as<List>(aucOptions);
    CheckRocOptions(aucOptions_);
    UpdateRocOptions(printMsg, aucOptions_, aucOptions0, "AUC Options:");
  }

  List measureOptions_;
  if (measureOptions == R_NilValue)
    measureOptions_ = GetMeasureOptions();
  else {
    if (is<List>(measureOptions) == false)
      throw std::logic_error("'measureOptions' must be a 'List'.");
    measureOptions_ = as<List>(measureOptions);
    CheckMeasureOptions(measureOptions_);
  }

  List modelCheckItems_;
  if (modelCheckItems == R_NilValue)
    modelCheckItems_ = GetModelCheckItems();
  else {
    if (is<List>(modelCheckItems) == false)
      throw std::logic_error("'modelCheckItems' must be a 'List'.");
    modelCheckItems_ = as<List>(modelCheckItems);
    CheckModelCheckItems(modelCheckItems_);
  }

  List searchItems_;
  if (searchItems == R_NilValue)
    searchItems_ = GetSearchItems();
  else {
    if (is<List>(searchItems) == false)
      throw std::logic_error("'searchItems' must be a 'List'.");
    searchItems_ = as<List>(searchItems);
    CheckSearchItems(searchItems_);
  }

  auto measures = SearchMeasureOptions();
  auto measuresNames = std::vector<std::string>();
  auto items = SearchItems();
  auto checks = SearchModelChecks();
  UpdateOptions(printMsg, searchItems_, measureOptions_, modelCheckItems_,
                measures, items, checks, measuresNames, exoCount, exoCount,
                numTargets, 1, false, true, "Coefficients", true);

  std::vector<ldt::Matrix<double>> costMatrices0;
  GetCostMatrices(printMsg, costMatrices0, costMatrices,
                  Contains(measures.MeasuresOut, ScoringType::kFrequencyCost));

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
      isBinary, hasWeight, options, items, measures, checks, xSizes_, mat,
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

  // handle unhandled exceptions in the async function
  // model->CheckStart();
  auto f = std::async(std::launch::async, [&] {
    model->Start(W.get(), Wi.get());
    estimating = false;
  });

  ReportProgress(printMsg, reportInterval, model->Modelset, estimating,
                 options);

  auto endTime = boost::posix_time::to_simple_string(
      boost::posix_time::second_clock::local_time());

  if (options.RequestCancel)
    return R_NilValue;

  std::vector<std::string> colNames_w = colNames; // no weight for inclusions
  if (hasWeight)
    colNames_w.erase(std::next(colNames_w.begin()));

  auto extraLabel = "dist";
  auto extraNames = std::vector<std::string>({"dist"});
  List L = GetModelSetResults(model->Modelset, items, measuresNames, exoCount,
                              extraLabel, &extraNames, 1, paramNames,
                              colNames_w, "coefs", "item");

  L["info"] = List::create(
      _["startTime"] = wrap(startTime), _["endTime"] = wrap(endTime),
      _["yNames"] = colnames(y), _["xNames"] = colnames(x),
      _["costMatrices"] = costMatrices, _["searchLogit"] = wrap(searchLogit),
      _["searchProbit"] = wrap(searchProbit), _["optimOptions"] = optimOptions,
      _["measureOptions"] = measureOptions_,
      _["modelCheckItems"] = modelCheckItems_, _["searchItems"] = searchItems_,
      _["searchOptions"] = searchOptions_, _["numTargets"] = wrap(numTargets));

  L.attr("class") =
      std::vector<std::string>({"ldtsearchdc", "ldtsearch", "list"});
  L.attr("method") = "dc";

  return L;
}

// clang-format off

//' Estimates an Discrete Choice Model
//'
//' @param y (numeric matrix) Data with dependent variable in the column. Given the number of choices 'n', it must contain 0,1,...,n-1 and 'sum(y==i)>0' for i=0,...,n-1.
//' @param x (numeric matrix) Exogenous data with variables in the columns.
//' @param w (numeric vector) Weights of the observations in \code{y}. Null means equal weights.
//' @param distType (string) Distribution assumption. It can be \code{logit} or \code{probit}.
//' @param newX (numeric matrix) If not null, probabilities are projected for each row of this matrix.
//' @param pcaOptionsX (list) A list of options in order to use principal components of the \code{x}, instead of the actual values. set null to disable. Use [GetPcaOptions()] for initialization.
//' @param costMatrices (list of matrices) Each cost table determines how you score the calculated probabilities.
//' @param aucOptions (nullable list) AUC calculation options. see \code{[GetRocOptions()]}.
//' @param simFixSize (int) Number of pseudo out-of-sample simulations. Use zero to disable the simulation. (see [GetMeasureOptions()]).
//' @param simTrainRatio (double) Size of the training sample as a ratio of the number of the observations. It is effective only if \code{simTrainFixSize} is zero.
//' @param simTrainFixSize (int) A fixed size for the training sample. If zero, \code{simTrainRatio} is used.
//' @param simSeed (int) A seed for the pseudo out-of-sample simulation.
//' @param weightedEval (bool) If true, weights will be used in evaluations.
//' @param printMsg (bool) Set false to disable printing the details.
//'
//' @return A list:
//'
//' @export
// [[Rcpp::export]]
SEXP DcEstim(SEXP y, SEXP x, SEXP w = R_NilValue,
             std::string distType = "logit", SEXP newX = R_NilValue,
             SEXP pcaOptionsX = R_NilValue, SEXP costMatrices = R_NilValue,
             SEXP aucOptions = R_NilValue, int simFixSize = 200, double simTrainRatio = 0.5,
             int simTrainFixSize = 0, int simSeed = 0, bool weightedEval = false, bool printMsg = false)
// clang-format on
{
  if (y == R_NilValue || x == R_NilValue)
    throw std::logic_error("Invalid data: 'y' or 'x' is null.");
  if (is<NumericMatrix>(y) == false)
    throw std::logic_error("'y' must be a 'numeric matrix'.");
  if (is<NumericMatrix>(x) == false)
    throw std::logic_error("'x' must be a 'numeric matrix'.");

  y = as<NumericMatrix>(y);
  x = as<NumericMatrix>(x);

  if (newX != R_NilValue){
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
      FromString_DiscreteChoiceDistType(distType.c_str());
  if (printMsg) {
    Rprintf("Model Type=%s\n", ToString(modelType0));
    Rprintf("Distribution Type=%s\n", ToString(distType0));
  }

  bool hasProjection = mnewX.Data != nullptr;
  if (printMsg)
    Rprintf("Number of Projections=%i\n", mnewX.RowsCount);

  RocOptions aucOptions0;
  if (aucOptions == R_NilValue) {
    List aucOptions_ = GetRocOptions();
    UpdateRocOptions(printMsg, aucOptions_, aucOptions0, "AUC Options:");
  } else {
    if (is<List>(aucOptions) == false)
      throw std::logic_error("Invalid 'aucOption'. It is not a list.");
    auto aucOptions_ = as<List>(aucOptions);
    CheckRocOptions(aucOptions_);
    UpdateRocOptions(printMsg, aucOptions_, aucOptions0, "AUC Options:");
  }

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
        true, hasPcaX ? &pcaOptions0 : nullptr, weightedEval);
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

  // Measures
  int measureCount = 5; // logL, aic, sic, aucIn, costIn,
  if (simFixSize > 0)
    measureCount += 2; // aucOut, costOut
  int eqCount = 1;
  auto measuresResD = std::unique_ptr<double[]>(
      new double[measureCount * eqCount]); // for 1 equation
  auto measuresRes =
      ldt::Matrix<double>(measuresResD.get(), measureCount, eqCount);
  auto measuresResRowNames = std::vector<std::string>(
      {"logL", "aic", "sic",
       "aucIn", // keep names consistent with the measures in the search
       "frequencyCostIn"});
  measuresRes.Set(0, 0, model.Model->LogL);
  measuresRes.Set(1, 0, model.Model->Aic);
  measuresRes.Set(2, 0, model.Model->Sic);
  measuresRes.Set(3, 0, model.Auc);
  measuresRes.Set(4, 0, model.CostRatioAvg);
  if (simFixSize > 0) {
    measuresResRowNames.push_back("aucOut");
    measuresResRowNames.push_back("frequencyCostOut");
    measuresRes.Set(5, 0, simmodel->Auc);
    measuresRes.Set(6, 0, simmodel->CostRatios.Mean());
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
      _["measures"] = as_matrix(measuresRes, &measuresResRowNames, &endoNames),
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
  L.attr("method") = "dc";

  return L;
}
