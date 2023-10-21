#include "r_ldt.h"

using namespace Rcpp;
using namespace ldt;

RFuncSearcher::RFuncSearcher(
    const SearchData &data, const SearchCombinations &combinations,
    SearchOptions &options, const SearchItems &items,
    const SearchMetricOptions &metrics, const SearchModelChecks &checks,
    const Ti &numPartitions, const std::vector<Ti> &innerIndices,
    const bool &isInnerExogenous, const std::string &rFuncName)
    : SearcherReg::SearcherReg(data, combinations, options, items, metrics,
                               checks, numPartitions, isInnerExogenous,
                               innerIndices, 0) {

  Rcpp::Environment G = Rcpp::Environment::global_env();
  Rcpp::Function func = G[rFuncName];
  mFunc = Rcpp::Nullable<Rcpp::Function>(func);
}

std::string RFuncSearcher::EstimateOneReg(Tv *work, Ti *workI,
                                          VMatrix<Tv> &metrics,
                                          VMatrix<Tv> &type1Mean,
                                          VMatrix<Tv> &type1Var,
                                          VMatrix<Ti> &extra) {
  ReportProgress(); // I couldn't use another thread to call it. If the
                    // time of estimation is larger than reporting, it will
                    // not perform as expected.

  Ti num_endo;
  if (this->IsInnerExogenous)
    num_endo = this->CurrentIndices.Vec.size();
  else
    num_endo = this->InnerIndices.size();

  SEXP columnIndicesR =
      PROTECT(Rf_allocVector(INTSXP, this->ColIndices.size()));
  SEXP numEndoR = PROTECT(Rf_ScalarInteger(num_endo));

  try {
    for (int i = 0; i < (Ti)this->ColIndices.size(); i++)
      INTEGER(columnIndicesR)[i] = this->ColIndices[i];

    if (this->pOptions->RequestCancel)
      return "";

    auto F = as<Function>(mFunc);
    SEXP res =
        F(Named("columnIndices", columnIndicesR), Named("numEndo", numEndoR),
          Named("data", DataR), Named("metrics", MetricsR),
          Named("items", ItemsR), Named("modelChecks", ModelChecksR));

    if (TYPEOF(res) != VECSXP)
      stop("The result of the given R function is not a list.");

    Rcpp::List resList(res);
    std::vector<std::string> requiredElements = {
        "error", "metrics", "type1means", "type1vars", "extra"};
    for (std::string element : requiredElements)
      if (!resList.containsElementNamed(element.c_str()))
        stop("The result of the given R function does not contain the required "
             "element: " +
             element);

    if (resList["error"] != R_NilValue) {
      if (TYPEOF(resList["error"]) == STRSXP) {
        std::string errorMessage = Rcpp::as<std::string>(resList["error"]);
        if (errorMessage.length() > 0)
          throw std::logic_error(errorMessage);
      } else {
        stop("The 'error' element is not a character string.");
      }
    }

    if (resList["metrics"] == R_NilValue)
      stop("The 'metrics' element cannot be NULL.");
    if (TYPEOF(resList["metrics"]) != REALSXP)
      stop("The 'metrics' element is not a numeric matrix.");
    auto metricsR = as<NumericMatrix>(resList["metrics"]);
    if (metrics.Mat.RowsCount != metricsR.nrow() ||
        metrics.Mat.ColsCount != metricsR.ncol())
      stop("The 'metrics' element has invalid dimension. ");
    for (int i = 0; i < metricsR.nrow() * metricsR.ncol(); i++)
      metrics.Mat.Data[i] = metricsR[i];

    if (resList["extra"] != R_NilValue) {
      if (TYPEOF(resList["extra"]) != REALSXP)
        stop("The 'extra' element is not a numeric vector.");
      auto extraR = as<NumericVector>(resList["extra"]);
      for (int i = 0; i < extraR.length(); i++)
        extra.Vec.push_back(extraR[i]);
      extra.Mat.RowsCount = extraR.length();
    }

    if (resList["type1means"] != R_NilValue) {
      if (TYPEOF(resList["type1means"]) != REALSXP)
        stop("The 'type1means' element is not a numeric matrix.");
      auto type1meansR = as<NumericMatrix>(resList["metrics"]);
      if (type1Mean.Mat.RowsCount != type1meansR.nrow() ||
          type1Mean.Mat.ColsCount != type1meansR.ncol())
        stop("The 'type1means' element has invalid dimension. ");
      for (int i = 0; i < type1meansR.nrow() * type1meansR.ncol(); i++)
        type1Mean.Mat.Data[i] = type1meansR[i];
    }

    if (resList["type1vars"] != R_NilValue) {
      if (TYPEOF(resList["type1vars"]) != REALSXP)
        stop("The 'type1vars' element is not a numeric matrix.");
      auto type1varsR = as<NumericMatrix>(resList["metrics"]);
      if (type1Var.Mat.RowsCount != type1varsR.nrow() ||
          type1Var.Mat.ColsCount != type1varsR.ncol())
        stop("The 'type1vars' element has invalid dimension. ");
      for (int i = 0; i < type1varsR.nrow() * type1varsR.ncol(); i++)
        type1Var.Mat.Data[i] = type1varsR[i];
    }

  } catch (...) {
    UNPROTECT(2);
    throw;
  }

  UNPROTECT(2);
  return "TODO";
}

// #pragma endregion

// #pragma region Modelset

RFuncModelset::RFuncModelset(
    const SearchData &data, const SearchCombinations &combinations,
    SearchOptions &options, SearchItems &items, SearchMetricOptions &metrics,
    SearchModelChecks &checks, bool isTimeSeries, bool isOutOfSampleRandom,
    const bool &isInnerExogenous, const std::string &rFuncName) {

  metrics.Update(isOutOfSampleRandom, isTimeSeries);
  checks.Update(metrics);
  items.Update(metrics, items.LengthTargets, items.LengthEndogenous,
               items.LengthExogenous);

  unsigned int co = 0;
  for (auto const &size : combinations.Sizes) {
    if (size <= 0)
      throw LdtException(
          ErrorType::kLogic, "rfunc-modelset",
          "invalid exogenous size (zero or negative). Make sure array is "
          "initialized properly");

    if (combinations.NumFixPartitions > size)
      continue;

    for (auto &inner_group : combinations.InnerGroups) {
      if (inner_group.size() == 0)
        throw LdtException(ErrorType::kLogic, "rfunc-modelset",
                           "empty endogenous indexes");

      if (isInnerExogenous == false) {
        if (inner_group.at(0) > items.LengthTargets)
          continue; // no target here
      }
      /*auto seed = metrics.Seed == 0
                      ? 0
                      : (unsigned int)(metrics.Seed < 0 ? -metrics.Seed
                                                        : (metrics.Seed +
         co));*/
      co++;
      auto se =
          new RFuncSearcher(data, combinations, options, items, metrics, checks,
                            size, inner_group, isInnerExogenous, rFuncName);
      Searchers.push_back(se);
    }
  }
  this->Modelset =
      ModelSet(Searchers, data, combinations, options, items, metrics, checks);
}

// [[Rcpp::export(.SearchRFunc)]]
SEXP SearchRFunc(List data, List combinations, List metrics, List modelChecks,
                 List items, List options, std::string rFuncName, int length1,
                 bool isInnerExogenous) {

  bool isTimeSeries = false;
  bool isOutOfSampleRamdom = false;

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

  UpdateOptions(items, metrics, modelChecks, metrics_, items_, checks_,
                metricsNames, length1, data_.NumExo, combinations_.NumTargets,
                data_.NumEndo, false, true, "Coefficients", false);

  auto targetNames = std::vector<std::string>(
      colNames.begin(), colNames.begin() + items_.LengthTargets);

  // Modelset
  auto model = RFuncModelset(data_, combinations_, options_, items_, metrics_,
                             checks_, isTimeSeries, isOutOfSampleRamdom,
                             isInnerExogenous, rFuncName);
  model.Modelset.ShuffleSearchers = false;

  std::unique_ptr<double[]> W;
  try {
    W = std::unique_ptr<double[]>(new double[model.Modelset.WorkSize]);
  } catch (...) {
    throw LdtException(ErrorType::kLogic, "R-sur",
                       "more memory is required for running the project");
  }

  auto alli = model.Modelset.GetExpectedNumberOfModels();

  // reporting
  auto printMsg = options_.ReportInterval > 0;
  auto start = std::chrono::system_clock::now();
  if (printMsg)
    Rprintf("Calculations Started ...\n");
  if (printMsg)
    Rprintf("Expected Number of Models = %i\n", alli);
  double prePecentage = -1;
  int i = 0;
  std::function<void()> reportProgress = [&]() -> void {
    ReportProgressInner(model.Modelset, options_, alli, prePecentage, i, start,
                        printMsg, false);
  };

  for (auto psch : *model.Modelset.pSearchers) {
    auto sch = static_cast<RFuncSearcher *>(psch);
    sch->ReportProgress = reportProgress;

    sch->DataR = data;
    sch->ItemsR = items;
    sch->MetricsR = metrics;
    sch->ModelChecksR = modelChecks;
  }

  // start execution:
  model.Modelset.Start(W.get(), nullptr);

  if (options_.RequestCancel)
    return R_NilValue;

  auto extraNames = std::vector<std::string>({"extra"});
  List L = GetModelSetResults(
      model.Modelset, items_, metricsNames, colNames, targetNames, extraNames,
      exoNames, colNames, std::string("coefs"), options_.ReportInterval > 0);

  return L;
}
