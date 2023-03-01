#include "matrix_utils.h"
#include "r_ldt.h"
#include "varma.h"

using namespace Rcpp;
using namespace ldt;

std::unique_ptr<ldt::DatasetTs<true>> GetDs(bool printMsg,
                                            ldt::Matrix<double> &source,
                                            int exoStart, bool interpolate,
                                            bool adjustLeadLags) {

  auto adjustLeadLags0 = adjustLeadLags ? exoStart : 0;
  auto dataset0 = new DatasetTs<true>(source.RowsCount, source.ColsCount, true,
                                      true, interpolate, adjustLeadLags0);
  auto ds = std::unique_ptr<ldt::DatasetTs<true>>(dataset0);

  dataset0->Data(source);
  if (printMsg)
    Rprintf("Data Adjustments:\n");
  bool adjust = false;
  if (dataset0->WithMissingIndexes.size() > 0) {
    adjust = true;
    if (printMsg)
      Rprintf("    - Variables with Missing Data: %s\n",
              VectorToCsv(dataset0->WithMissingIndexes).c_str());
    int cc = 0;
    for (auto &c : dataset0->InterpolationCounts)
      cc += c;
    if (printMsg)
      Rprintf("    - Interpolation Points Count: %i\n", cc);
  }
  if (dataset0->WithLags.size() > 0) {
    if (printMsg)
      adjust = true;
    Rprintf("    - Variables with Lags: %s\n",
            VectorToCsv(dataset0->WithLags).c_str());
  }
  if (dataset0->WithLeads.size() > 0) {
    adjust = true;
    if (printMsg)
      Rprintf("    - Variables with Leads: %s\n",
              VectorToCsv(dataset0->WithLeads).c_str());
  }
  if (adjust == false) {
    if (printMsg)
      Rprintf("    - none\n");
  }

  return ds;
}

// clang-format off

//' VARMA Search
//'
//' @param y (numeric vector) Endogenous data with variables in the columns.
//' @param x (nullable numeric matrix) Exogenous data with variables in the columns. It can be null.
//' @param numTargets (int) Number of variables in the first columns of \code{y}, regarded as targets. It must be positive and cannot be larger than the number of endogenous variables.
//' @param ySizes (nullable integer vector) Determines the number of endogenous variables (or equations) in the regressions.
//' @param yPartitions (nullable list of int vector) A partition over the indexes of the endogenous variables. No regression is estimated with two variables in the same group. If \code{NULL}, each variable is placed in its own group.
//' @param xGroups (nullable list of int vector) different combinations of the indexes of the exogenous variables to be used as exogenous variables in the SUR regressions.
//' @param maxParams (integer vector, length=6) Maximum values for the parameters of the VARMA model (p,d,q,P,D,Q). If null, c(1,1,1,0,0,0) is used.
//' @param seasonsCount (integer) number of observations per unit of time
//' @param maxHorizon (integer) maximum value for the prediction horizon if \code{type1} is \code{TRUE} in \code{checkItems}. Also, it is used as the maximum prediction horizon in checking the predictions.
//' @param newX (matrix) New exogenous data for out-of-sample prediction. It must have the same number of columns as \code{x}.
//' @param interpolate (logical) if \code{TRUE}, missing observations are interpolated.
//' @param adjustLeadsLags (logical) if \code{TRUE}, leads and lags in the sample are adjusted.
//' @param simUsePreviousEstim (logical) if \code{TRUE}, parameters are initialized in just the first step of the simulation. The initial values of the n-th simulation (with one more observation) is the estimations in the previous step.
//' @param olsStdMultiplier (numeric) a multiplier for the standard deviation of OLS, used for restricting the maximum likelihood estimation.
//' @param lmbfgsOptions (list) Optimization options. see \code{[GetLmbfgsOptions()]}. Use null for default values.
//' @param measureOptions (nullable list) see \code{[GetMeasureOptions()]}.
//' @param modelCheckItems (nullable list) see \code{[GetModelCheckItems()]}.
//' @param searchItems (nullable list) see \code{[GetSearchItems()]}.
//' @param searchOptions (nullable list) see \code{[GetSearchOptions()]}.
//'
//' @return A list
//'
//' @export
// [[Rcpp::export]]
SEXP VarmaSearch(SEXP y, SEXP x = R_NilValue, int numTargets = 1,
                 SEXP ySizes = R_NilValue, SEXP yPartitions = R_NilValue,
                 SEXP xGroups = R_NilValue, SEXP maxParams = R_NilValue,
                 int seasonsCount = 0, int maxHorizon = 0,
                 SEXP newX = R_NilValue, bool interpolate = true,
                 int adjustLeadsLags = true, bool simUsePreviousEstim = true,
                 double olsStdMultiplier = 2.0, SEXP lmbfgsOptions = R_NilValue,
                 SEXP measureOptions = R_NilValue,
                 SEXP modelCheckItems = R_NilValue,
                 SEXP searchItems = R_NilValue,
                 SEXP searchOptions = R_NilValue)
// clang-format on
{

  if (y == R_NilValue)
    throw std::logic_error("Invalid data: 'y' is null.");
  if (is<NumericMatrix>(y) == false)
    throw std::logic_error("'y' must be a 'numeric matrix'.");
  y = as<NumericMatrix>(y);

  if (numTargets < 1)
    throw std::logic_error("Number of targets must be positive.");

  auto startTime = boost::posix_time::to_simple_string(
      boost::posix_time::second_clock::local_time());

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


  if (x != R_NilValue){
    if (is<NumericMatrix>(x) == false)
      throw std::logic_error("'x' must be a 'numeric matrix'.");
    x = as<NumericMatrix>(x);
  }
  if (newX != R_NilValue){
    if (is<NumericMatrix>(newX) == false)
      throw std::logic_error("'newX' must be a 'numeric matrix'.");
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
    throw std::logic_error("'numTargets' cannot be larger than the number of "
                           "endogenous variables (i.e., columns of 'y').");

  mat.Transpose();

  auto dataset =
      GetDs(printMsg, mat, my.ColsCount, interpolate, adjustLeadsLags);

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
    throw std::logic_error("'maxParams_' must have 6 parameters.");
  if (printMsg) {
    Rprintf("Max Parameters:%s(p,d,q)[P,D,Q]s=(%i,%i,%i)[%i,%i,%i]\n",
            my.ColsCount == 1 ? "ARMA" : "VARMA", maxParams_.at(0),
            maxParams_.at(1), maxParams_.at(2), maxParams_.at(3),
            maxParams_.at(4), maxParams_.at(5));
    Rprintf("Number of Seasons=%i\n", seasonsCount);
  }

  LimitedMemoryBfgsbOptions optim;
  if (lmbfgsOptions == R_NilValue) {
    List optimOptions_ = GetLmbfgsOptions();
    UpdateLmbfgsOptions(printMsg, optimOptions_, optim);
  } else {
    if (is<List>(lmbfgsOptions) == false)
      throw std::logic_error("'lmbfgsOptions' must be a 'List'.");
    List optimOptions_ = as<List>(lmbfgsOptions);
    CheckLmbfgsOptions(optimOptions_);
    UpdateLmbfgsOptions(printMsg, optimOptions_, optim);
  }

  // if (maxHorizon > 0 && items.Length1 > 0){ // model must provide predictions
  //   checks.Prediction = true;
  //   checks.Estimation = true;
  // }

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
                measures, items, checks, measuresNames, maxHorizon,
                mx.ColsCount, numTargets, my.ColsCount, true, false, "Horizon",
                false);

  std::vector<std::string> type1Names;
  if (items.Length1 > 0) {
    if (maxHorizon == 0)
      throw std::logic_error(
          "Inconsistent argument and option: If 'type1' is enabled in "
          "'searchItems', 'maxHorizon' cannot be zero.");
    checks.Prediction = true;
    for (int i = 0; i < items.Length1; i++)
      type1Names.push_back(std::string("Horizon") + std::to_string(i + 1));
  }

  // Modelset
  auto model =
      VarmaModelset(options, items, measures, checks, ySizes_, yPartitions_,
                    *dataset.get(), maxParams_, seasonsCount, xGroups_,
                    simUsePreviousEstim, &optim, olsStdMultiplier, maxHorizon);

  bool estimating = true;

  std::unique_ptr<double[]> W;
  try {
    W = std::unique_ptr<double[]>(new double[model.Modelset.WorkSize]);
  } catch (...) {
    throw std::logic_error("More memory is required for running the project.");
  }

  // handle unhandled exceptions in the async function
  // model.CheckStart();
  auto f = std::async(std::launch::async, [&] {
    model.Modelset.Start(W.get(), nullptr);
    estimating = false;
  });

  ReportProgress(printMsg, reportInterval, model.Modelset, estimating, options);

  auto endTime = boost::posix_time::to_simple_string(
      boost::posix_time::second_clock::local_time());

  if (options.RequestCancel)
    return R_NilValue;

  auto extraLabel = "parameters";
  auto extraNames = std::vector<std::string>(
      {"arP", "arD", "arQ", "maP", "maD", "maQ", "numSeasons"});

  List L = GetModelSetResults(model.Modelset, items, measuresNames,
                              (int)items.Length1, extraLabel, &extraNames,
                              -my.ColsCount + 1, type1Names, colNames,
                              "predictions", "horizon");

  L["info"] = List::create(
      _["startTime"] = wrap(startTime), _["endTime"] = wrap(endTime),
      _["yNames"] = colnames(y), _["xNames"] = colnames(x),
      _["seasonsCount"] = wrap(seasonsCount),
      _["olsStdMultiplier"] = wrap(olsStdMultiplier),
      _["simUsePreviousEstim"] = wrap(simUsePreviousEstim),
      _["maxHorizon"] = wrap(checks.Prediction ? maxHorizon : 0),
      _["lmbfgsOptions"] = lmbfgsOptions, _["measureOptions"] = measureOptions_,
      _["modelCheckItems"] = modelCheckItems_, _["searchItems"] = searchItems_,
      _["searchOptions"] = searchOptions_, _["numTargets"] = wrap(numTargets));

  L.attr("class") =
      std::vector<std::string>({"ldtsearchvarma", "ldtsearch", "list"});
  L.attr("method") = "varma";

  return L;
}

// clang-format off

//' Estimates an VARMA Model
//'
//' @param y (matrix) endogenous data with variables in the columns.
//' @param x (matrix) exogenous data with variables in the columns.
//' @param params (integer vector, length=6) parameters of the VARMA model (p,d,q,P,D,Q).
//' @param seasonsCount (integer) number of observations per unit of time
//' @param addIntercept (logical) if \code{TRUE}, intercept is added automatically to x.
//' @param lmbfgsOptions (list) optimization options. See \code{[GetLmbfgsOptions()]}.
//' @param olsStdMultiplier (numeric) a multiplier for the standard deviation of OLS, used for restricting the maximum likelihood estimation.
//' @param pcaOptionsY (list) a list of options in order to use principal components of the \code{y}, instead of the actual values. set \code{NULL} to disable. Use \code{[GetPcaOptions()]} for initialization.
//' @param pcaOptionsX (list) similar to \code{pcaOptionsY} but for \code{x}. see \code{pcaOptionsY}.
//' @param maxHorizon (integer) maximum prediction horizon. Set zero to disable.
//' @param newX (matrix) data of new exogenous variables to be used in the predictions. Its columns must be the same as \code{x}.
//' @param simFixSize (integer) number of pseudo out-of-sample simulations. Use zero to disable the simulation. see also \code{[GetMeasureOptions()]}.
//' @param simHorizons (integer vector) prediction horizons to be used in pseudo out-of-sample simulations. see also \code{[GetMeasureOptions()]}.
//' @param simUsePreviousEstim (logical) if \code{TRUE}, parameters are initialized in just the first step of the simulation. The initial values of the n-th simulation (with one more observation) is the estimations in the previous step.
//' @param simMaxConditionNumber (numeric) maximum value for the condition number in the pseudo out-of-sample simulations.
//' @param printMsg (logical) set \code{FALSE} to disable printing the details.
//'
//' @return A list:
//'
//' @export
// [[Rcpp::export]]
SEXP VarmaEstim(SEXP y, SEXP x = R_NilValue, SEXP params = R_NilValue,
                int seasonsCount = 0, bool addIntercept = true,
                SEXP lmbfgsOptions = R_NilValue, double olsStdMultiplier = 2,
                SEXP pcaOptionsY = R_NilValue, SEXP pcaOptionsX = R_NilValue,
                int maxHorizon = 0, SEXP newX = R_NilValue, int simFixSize = 0,
                SEXP simHorizons = R_NilValue, bool simUsePreviousEstim = true,
                double simMaxConditionNumber = 1e20, bool printMsg = false)
// clang-format on
{

  if (y == R_NilValue)
    throw std::logic_error("Invalid data: 'y' is null.");
  if (is<NumericMatrix>(y) == false)
    throw std::logic_error("'y' must be a 'numeric matrix'.");
  y = as<NumericMatrix>(y);

  auto startTime = boost::posix_time::to_simple_string(
      boost::posix_time::second_clock::local_time());

  if (x != R_NilValue){
    if (is<NumericMatrix>(x) == false)
      throw std::logic_error("'x' must be a 'numeric matrix'.");
    x = as<NumericMatrix>(x);
  }

  if (newX != R_NilValue){
    if (is<NumericMatrix>(newX) == false)
      throw std::logic_error("'newX' must be a 'numeric matrix'.");
  }
  if (addIntercept && maxHorizon > 0) {
    if (newX == R_NilValue){
      auto newX0 = NumericMatrix(maxHorizon, 0);
      newX = insert_intercept(newX0);
    }
    else{
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
    throw std::logic_error("'params' must have 6 parameters.");
  if (printMsg) {
    Rprintf("Model:%s(p,d,q)[P,D,Q]s=(%i,%i,%i)[%i,%i,%i]\n",
            my.ColsCount == 1 ? "ARMA" : "VARMA", params_.at(0), params_.at(1),
            params_.at(2), params_.at(3), params_.at(4), params_.at(5));
    Rprintf("Number of Seasons=%i\n", seasonsCount);
  }
  if (maxHorizon < 0)
    throw std::logic_error(
        "Invalid argument: 'maxHorizon' cannot be negative.");
  if (printMsg)
    Rprintf("Prediction Horizon=%i\n", maxHorizon);

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

  auto restriction = VarmaRestrictionType::kMaFinal; // TODO: as an option
  auto sizes = VarmaSizes(my.RowsCount, my.ColsCount, k, params_.at(0),
                          params_.at(1), params_.at(2), params_.at(3),
                          params_.at(4), params_.at(5), seasonsCount);

  // LMBFGS
  LimitedMemoryBfgsbOptions optim;
  if (sizes.HasMa) {
    if (lmbfgsOptions == R_NilValue) {
      List optimOptions_ = GetLmbfgsOptions();
      UpdateLmbfgsOptions(printMsg, optimOptions_, optim);
    } else {
      if (is<List>(lmbfgsOptions) == false)
        throw std::logic_error("'lmbfgsOptions' must be a 'List'.");
      List optimOptions_ = as<List>(lmbfgsOptions);
      CheckLmbfgsOptions(optimOptions_);
      UpdateLmbfgsOptions(printMsg, optimOptions_, optim);
    }
  } else if (printMsg)
    Rprintf("LMBFGS (skipped).\n");

  // Estimation

  auto model = VarmaExtended(sizes, restriction, true, true, true, maxHorizon,
                             hasPcaY ? &pcaOptionsY0 : nullptr,
                             hasPcaX ? &pcaOptionsX0 : nullptr, &optim);
  auto W = std::unique_ptr<double[]>(new double[model.WorkSize]);
  auto S = std::unique_ptr<double[]>(new double[model.StorageSize]);

  if (printMsg)
    Rprintf("Starting Calculations ...");

  model.Calculate(mat, S.get(), W.get(), false, maxHorizon, 0, -1,
                  olsStdMultiplier);

  if (printMsg)
    Rprintf("Calculations Finished.\n");

  // Simulation

  std::vector<ScoringType> measures;
  std::vector<std::string> measureNames;
  VarmaSimulation simModel;
  std::unique_ptr<double[]> S0;
  if (simFixSize > 0) {

    measures = std::vector<ScoringType>(
        {ScoringType::kSign, ScoringType::kDirection, ScoringType::kMae,
         ScoringType::kScaledMae, ScoringType::kRmse, ScoringType::kScaledRmse,
         ScoringType::kCrps});
    measureNames = std::vector<std::string>();
    for (auto &a : measures)
      measureNames.push_back(ToString(a));

    // Simulation Horizons
    std::vector<int> simHorizons_;
    if (simHorizons == R_NilValue && maxHorizon > 0) {
      for (int i = 0; i < maxHorizon; i++)
        simHorizons_.push_back(i + 1);
    } else if (simHorizons != R_NilValue) {
      if (is<IntegerVector>(simHorizons) == false)
        throw std::logic_error("'simHorizons' must be an 'integer vector'.");
      auto hors = as<IntegerVector>(simHorizons);
      for (int i = 0; i < hors.length(); i++) {
        if (hors[i] <= 0)
          throw std::logic_error("Zero or negative value in 'simHorizons'.");
        simHorizons_.push_back(hors[i]);
      }
    } else
      throw std::logic_error("Simulation horizons are missing.");

    if (printMsg) {
      Rprintf("Simulation Horizons:%s\n",
              VectorToCsv(simHorizons_, ',').c_str());
      Rprintf("Maximum Condition Number=%f\n", simMaxConditionNumber);
    }

    simModel =
        VarmaSimulation(sizes, simFixSize, simHorizons_, measures, &optim, true,
                        restriction, true, hasPcaY ? &pcaOptionsY0 : nullptr,
                        hasPcaX ? &pcaOptionsX0 : nullptr);
    auto W0 = std::unique_ptr<double[]>(new double[simModel.WorkSize]);
    S0 = std::unique_ptr<double[]>(
        new double[simModel.StorageSize]); // don't override S

    if (printMsg)
      Rprintf("Starting Simulation ...");

    simModel.CalculateE(S0.get(), W0.get(), mat, simMaxConditionNumber,
                        olsStdMultiplier, false, simUsePreviousEstim);

    if (printMsg)
      Rprintf("Simulation Finished.\n");
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
  std::vector<std::string> exoNames;
  std::vector<std::string> endoNames;
  for (int i = 0; i < (int)colNames.size(); i++)
    if (i < my.ColsCount)
      endoNames.push_back(colNames.at(i));
    else
      exoNames.push_back(colNames.at(i));

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

  // Measures
  int measureCount = 3; // logL, aic, sic
  if (simFixSize > 0)
    measureCount += simModel.ResultAggs.RowsCount;
  auto measuresResD =
      std::unique_ptr<double[]>(new double[measureCount * my.ColsCount]);
  auto measuresRes = ldt::Matrix<double>(measuresResD.get(), measureCount,
                                         endoNames_pca.size());
  auto measuresResRowNames = std::vector<std::string>({"logL", "aic", "sic"});
  measuresRes.SetRow(0, model.Model.Result.LogLikelihood);
  measuresRes.SetRow(1, model.Model.Result.Aic);
  measuresRes.SetRow(2, model.Model.Result.Sic);

  if (simFixSize > 0) {
    int k = 3;
    for (auto m : measureNames) {
      measuresResRowNames.push_back(m);
      measuresRes.SetRowFromRow(k, simModel.ResultAggs, k - 3);
      k++;
    }
  }

  auto expNames = std::vector<std::string>();
  VarmaStorage::GetExpNames(sizes, endoNames_pca, exoNames_pca, expNames);

  List L = List::create(
      _["counts"] = List::create(
          _["obs"] = wrap(model.Y.RowsCount), _["eq"] = wrap(model.Y.ColsCount),
          _["exoEq"] = wrap(model.X.ColsCount),
          _["exoAll"] = wrap(model.Model.Result.gamma.length())),
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
      _["measures"] =
          as_matrix(measuresRes, &measuresResRowNames, &endoNames_pca),
      _["prediction"] =
          maxHorizon == 0
              ? R_NilValue
              : (SEXP)List::create(
                    _["means"] = as_matrix(model.Forecasts.Forecast,
                                           &endoNames_pca, nullptr),
                    _["vars"] = as_matrix(model.Forecasts.Variance,
                                          &endoNames_pca, nullptr),
                    _["startIndex"] = wrap(model.Forecasts.StartIndex + 1)),
      _["simulation"] = simFixSize == 0
                            ? R_NilValue
                            : (SEXP)List::create(_["validCounts"] =
                                                     wrap(simModel.ValidCounts),
                                                 _["failed"] = simFails),
      _["info"] =
          List::create(_["y"] = y, _["x"] = x, _["pcaOptionsY"] = pcaOptionsY,
                       _["pcaOptionsX"] = pcaOptionsX));

  L.attr("class") =
      std::vector<std::string>({"ldtestimvarma", "ldtestim", "list"});
  L.attr("method") = "varma";

  return L;
}
