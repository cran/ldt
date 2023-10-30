#include "r_ldt.h"

using namespace Rcpp;
using namespace ldt;

// [[Rcpp::export(.SupportsParallel)]]
bool SupportsParallel() {
#ifndef _OPENMP
  return false;
#else
  return true;
#endif
}

std::vector<std::vector<int>> listToVectorOfVectors(List list) {
  std::vector<std::vector<int>> result;
  for (int i = 0; i < list.size(); i++) {
    std::vector<int> vec = as<std::vector<int>>(list[i]);
    result.push_back(vec);
  }
  return result;
}

void UpdateSearchData(List &dataR, SearchData &data) {

  auto mat = as<NumericMatrix>(dataR["data"]);
  data.Data.SetData(&mat[0], mat.nrow(), mat.ncol());

  data.NumEndo = as<int>(dataR["numEndo"]);
  data.NumExo = as<int>(dataR["numExo"]);
  data.ObsCount = as<int>(dataR["obsCount"]);
  data.NewObsCount = as<int>(dataR["newObsCount"]);

  if (data.NewObsCount > 0) {
    auto mat1 = as<NumericMatrix>(dataR["newX"]);
    data.NewX.SetData(&mat1[0], mat1.nrow(), mat1.ncol());
  }

  auto lambdasR = dataR["lambdas"];
  if (lambdasR != R_NilValue)
    data.Lambdas = as<std::vector<double>>(lambdasR);

  data.HasIntercept = as<bool>(dataR["hasIntercept"]);
  data.HasWeight = as<bool>(dataR["hasWeight"]);
}

void UpdateSearchCombinations(List combinationsR,
                              SearchCombinations &combinations) {

  auto sizesR = combinationsR["sizes"];
  if (sizesR != R_NilValue)
    combinations.Sizes = as<std::vector<int>>(sizesR);

  auto partsR = combinationsR["partitions"];
  if (partsR != R_NilValue)
    combinations.Partitions = listToVectorOfVectors(partsR);
  combinations.NumFixPartitions = as<int>(combinationsR["numFixPartitions"]);
  combinations.InnerGroups =
      listToVectorOfVectors(combinationsR["innerGroups"]);
  combinations.NumTargets = as<int>(combinationsR["numTargets"]);
}

void UpdateSearchOptions(List &optionsR, SearchOptions &options) {

  options.Parallel = as<bool>(optionsR["parallel"]);
  options.ReportInterval = as<int>(optionsR["reportInterval"]);
}

void UpdateModelCheckItems(List &checksR, SearchModelChecks &checks,
                           const SearchMetricOptions &metrics,
                           const SearchItems &items) {

  checks.Estimation = as<bool>(checksR["estimation"]);
  checks.MinObsCount = as<int>(checksR["minObsCount"]);
  checks.MinDof = as<int>(checksR["minDof"]);
  checks.MinOutSim = as<int>(checksR["minOutSim"]);
  checks.PredictionBoundMultiplier =
      as<double>(checksR["predictionBoundMultiplier"]);

  checks.MinR2 = as<double>(checksR["minR2"]);
  checks.MaxAic = as<double>(checksR["maxAic"]);
  checks.MaxSic = as<double>(checksR["maxSic"]);
  checks.MaxConditionNumber = as<double>(checksR["maxConditionNumber"]);
  checks.Prediction = as<bool>(checksR["prediction"]);

  checks.Update(metrics);
}

void UpdateSearchItems(List &itemsR, SearchItems &items, int length1,
                       int length2, const char *length1Informtion,
                       const char *length2Informtion, bool type1NeedsModelEstim,
                       bool type2NeedsModelEstim) {

  items.KeepModelEvaluations = as<bool>(itemsR["model"]);
  items.KeepAll = as<bool>(itemsR["all"]);
  items.KeepMixture = as<bool>(itemsR["mixture4"]);
  items.KeepInclusionWeights = as<bool>(itemsR["inclusion"]);
  items.KeepBestCount = as<int>(itemsR["bestK"]);
  items.ExtremeBoundsMultiplier = as<double>(itemsR["extremeMultiplier"]);

  items.CdfsAt = as<std::vector<double>>(itemsR["cdfs"]);

  // update length1 and 2
  bool type1 = as<bool>(itemsR["type1"]);
  bool type2 = as<bool>(itemsR["type2"]);
  items.Length1 = type1 ? length1 : 0;
  items.Length2 = type2 ? length2 : 0;

  if (type1NeedsModelEstim && items.Length1 > 0)
    items.KeepModelEvaluations = true;
  if (type2NeedsModelEstim && items.Length2 > 0)
    items.KeepModelEvaluations = true;

  if (items.KeepInclusionWeights)
    items.KeepModelEvaluations = true;
  if (items.KeepModelEvaluations == false && items.Length1 == 0 &&
      items.Length2 == 0)
    throw LdtException(ErrorType::kLogic, "R-ldt",
                       "no evaluation data is saved");

  auto hasGoal = items.KeepBestCount > 0 || items.KeepAll ||
                 items.CdfsAt.size() > 0 || items.KeepMixture ||
                 items.ExtremeBoundsMultiplier > 0 ||
                 items.KeepInclusionWeights;

  if (hasGoal == false)
    throw LdtException(ErrorType::kLogic, "R-ldt", "no goal is set");
}

void UpdatemetricOptions(List &metricsR, SearchMetricOptions &metrics,
                         std::vector<std::string> &metricNames,
                         bool isTimeSeries, bool isDc, int numTargets) {

  bool isOutOfSampleRandom = isTimeSeries == false;
  // bool supportsSimRatio = isTimeSeries;

  auto metricsOut0 = as<StringVector>(metricsR["typesOut"]);
  auto metricsIn0 = as<StringVector>(metricsR["typesIn"]);
  auto lmetricOut = metricsOut0.length();
  auto lmetricIn = metricsIn0.length();

  if (lmetricIn == 0 && lmetricOut == 0)
    throw LdtException(
        ErrorType::kLogic, "R-ldt",
        "No metric is specified. Check the inputs (also, check the number "
        "of simulations)");
  if (lmetricIn > 0) {
    for (auto i = 0; i < lmetricIn; i++) {
      auto a = as<std::string>(metricsIn0[i]);
      boost::algorithm::to_lower(a);
      auto eval = FromString_GoodnessOfFitType(a.c_str());
      metrics.MetricsIn.push_back(eval);
    }
  }
  if (lmetricOut > 0) {
    for (auto i = 0; i < lmetricOut; i++) {
      auto a = as<std::string>(metricsOut0[i]);
      boost::algorithm::to_lower(a);
      auto eval = FromString_ScoringType(a.c_str());
      metrics.MetricsOut.push_back(eval);
    }
  }

  metrics.SimFixSize = as<int>(metricsR["simFixSize"]);
  // metrics.SimRatio = Rf_asInteger(GetListElement(metrics,
  // "simratio"));
  metrics.Seed = as<int>(metricsR["seed"]);

  if (isTimeSeries && lmetricOut > 0) {

    IntegerVector hors = metricsR["horizons"];
    for (auto i = 0; i < hors.length(); i++)
      metrics.Horizons.push_back(hors[i]);

    metrics.TrainFixSize = 0;
    metrics.TrainRatio = 0;
  } else {
    metrics.TrainFixSize = as<int>(metricsR["trainFixSize"]);
    metrics.TrainRatio = as<double>(metricsR["trainRatio"]);
  }

  // set minimum value for metrics with AIC weight formula
  auto minMetrics = as<List>(metricsR["minMetrics"]);

  std::map<GoodnessOfFitType, std::string> mNsIn = {
      {GoodnessOfFitType::kAic, "aic"},
      {GoodnessOfFitType::kSic, "sic"},
      {GoodnessOfFitType::kBrier, "brierIn"}};

  for (const auto &mm : mNsIn) {
    metrics.MinMetricIn.insert(std::make_pair(
        mm.first, as<std::vector<double>>(minMetrics[mm.second])));
  }

  std::map<ScoringType, std::string> mNsOut = {
      {ScoringType::kBrier, "brierOut"}, {ScoringType::kRmse, "rmse"},
      {ScoringType::kRmspe, "rmspe"},    {ScoringType::kMae, "mae"},
      {ScoringType::kMape, "mape"},      {ScoringType::kCrps, "crps"}};

  for (const auto &mm : mNsOut) {
    metrics.MinMetricOut.insert(std::make_pair(
        mm.first, as<std::vector<double>>(minMetrics[mm.second])));
  }

  metrics.Update(isOutOfSampleRandom,
                 isTimeSeries); // update after filling metric vectors

  if (metrics.MetricsIn.size() > 0) {
    for (auto i = 0; i < lmetricIn; i++) {
      auto str = ToString(metrics.MetricsIn.at(i), true);
      metricNames.push_back(str);
    }
  }

  if (metrics.MetricsOut.size() > 0) {

    for (auto i = 0; i < lmetricOut; i++) {
      auto str = ToString(metrics.MetricsOut.at(i), true);
      metricNames.push_back(str);
    }
  }

  if (isDc)
    metrics.WeightedEval = as<bool>(metricsR["weightedEval"]);
}

void UpdateOptions(List &itemsR, List &metricsR, List &checksR,
                   SearchMetricOptions &metrics, SearchItems &items,
                   SearchModelChecks &checks,
                   std::vector<std::string> &metricsNames, int length1,
                   int exoCount, int numTargets, int numEndogenous,
                   bool isTimeSeries, bool type1NeedsModelEstim,
                   const char *length1Informtion, bool isDc) {

  UpdatemetricOptions(metricsR, metrics, metricsNames, isTimeSeries, isDc,
                      numTargets);

  UpdateSearchItems(itemsR, items, length1, 0, length1Informtion, nullptr,
                    type1NeedsModelEstim, false);

  UpdateModelCheckItems(checksR, checks, metrics, items);

  items.LengthTargets = numTargets; // Modelset will use it
  items.LengthEndogenous = numEndogenous;
  items.LengthExogenous = exoCount;
}

void UpdateRocOptions(List &rocOptionsR, RocOptions &options) {

  options.NormalizePoints = true;
  options.LowerThreshold = as<double>(rocOptionsR["lowerThreshold"]);
  options.UpperThreshold = as<double>(rocOptionsR["upperThreshold"]);
  options.Epsilon = as<double>(rocOptionsR["epsilon"]);

  if (rocOptionsR["costs"] != R_NilValue) {
    auto costs0 = as<NumericVector>(rocOptionsR["costs"]);
    auto costMatrix0 = as<NumericMatrix>(rocOptionsR["costMatrix"]);
    options.Costs.SetData(&costs0[0], costs0.length(), 1);
    options.CostMatrix.SetData(&costMatrix0[0], 2, 2);
  }
}

void UpdatePcaOptions(List optionsR, PcaAnalysisOptions &options) {
  options.IgnoreFirstCount = as<int>(optionsR["ignoreFirst"]);
  options.ExactCount = as<int>(optionsR["exactCount"]);
  options.CutoffRate = as<double>(optionsR["cutoffRate"]);
  options.CutoffCountMax = as<int>(optionsR["max"]);
  if (options.IsEnabled())
    options.CheckValidity();
}

void UpdateLbfgsOptions(List &optionsR, LimitedMemoryBfgsbOptions &options) {
  options.Factor = as<double>(optionsR["factor"]);
  options.IterationMax = as<int>(optionsR["maxIterations"]);
  options.ProjectedGradientTol = as<double>(optionsR["projectedGradientTol"]);
  options.mMaxCorrections = as<int>(optionsR["maxCorrections"]);
}

void UpdateNewtonOptions(List &newtonR, Newton &newton) {
  newton.IterationMax = as<int>(newtonR["maxIterations"]);
  newton.TolFunction = as<double>(newtonR["functionTol"]);
  newton.TolGradient = as<double>(newtonR["gradientTol"]);
  newton.UseLineSearch = as<bool>(newtonR["useLineSearch"]);
}

NumericMatrix as_matrix(const ldt::Matrix<double> &mat,
                        const std::vector<std::string> &rowNames,
                        const std::vector<std::string> &colNames) {
  NumericMatrix res = NumericMatrix(mat.RowsCount, mat.ColsCount, mat.Data);
  if (rowNames.size() > -0) {
    if ((int)rowNames.size() != mat.RowsCount) {
      Rcout << "---------------\n";
      Rcout << "Number of Rows: " << mat.RowsCount << "\n";
      Rcout << "Row Names:" << VectorToCsv(rowNames) << "\n";
      throw LdtException(ErrorType::kLogic, "R-ldt",
                         std::string("invalid number of rows/row_names."));
    }
    rownames(res) = wrap(rowNames);
  }
  if (colNames.size() > 0) {
    if ((int)colNames.size() != mat.ColsCount) {
      Rcout << "---------------\n";
      Rcout << "Number of Columns: " << mat.ColsCount << "\n";
      Rcout << "Column Names:" << VectorToCsv(colNames) << "\n";
      throw LdtException(ErrorType::kLogic, "R-ldt",
                         std::string("invalid number of columns/col_names."));
    }
    colnames(res) = wrap(colNames);
  }
  return res;
}

NumericVector as_vector(const ldt::Matrix<double> &vec,
                        const std::vector<std::string> &names) {
  auto res = NumericVector(vec.Data, vec.Data + vec.length());
  if (names.size() > 0) {
    if ((int)names.size() != vec.length()) {
      Rcout << "names:" << VectorToCsv(names);
      throw LdtException(ErrorType::kLogic, "R-ldt",
                         std::string("invalid number of elements/names."));
    }
    res.names() = wrap(names);
  }
  return res;
}

IntegerMatrix as_imatrix(const ldt::Matrix<int> &mat,
                         const std::vector<std::string> &rowNames,
                         const std::vector<std::string> &colNames) {
  auto res = IntegerMatrix(mat.RowsCount, mat.ColsCount, mat.Data);
  if (rowNames.size() > 0) {
    if ((int)rowNames.size() != mat.RowsCount) {
      Rcout << "Row names:" << VectorToCsv(rowNames);
      throw LdtException(ErrorType::kLogic, "R-ldt",
                         std::string("invalid number of rows/row_names."));
    }
    rownames(res) = wrap(rowNames);
  }
  if (colNames.size() > 0) {
    if ((int)colNames.size() != mat.ColsCount) {
      Rcout << "Column names:" << VectorToCsv(colNames);
      throw LdtException(ErrorType::kLogic, "R-ldt",
                         std::string("invalid number of columns/col_names."));
    }
    colnames(res) = wrap(colNames);
  }
  return res;
}

IntegerVector as_ivector(const ldt::Matrix<int> &vec,
                         const std::vector<std::string> &names) {
  auto res = IntegerVector(vec.Data, vec.Data + vec.length());
  if (names.size() > 0) {
    if ((int)names.size() != vec.length()) {
      Rcout << "names:" << VectorToCsv(names);
      throw LdtException(ErrorType::kLogic, "R-ldt",
                         std::string("invalid number of elements/names."));
    }
    res.names() = wrap(names);
  }
  return res;
}
void ReportProgressInner(
    const ModelSet &model, SearchOptions &options, const int &allCount,
    double &prePecentage, int &i,
    const std::chrono::time_point<std::chrono::system_clock> &start,
    const bool &printMsg, const bool &sleep1) {
  if (sleep1)
    std::this_thread::sleep_for(std::chrono::seconds(1));

  try {
    Rcpp::checkUserInterrupt();
  } catch (...) {
    options.RequestCancel = true;
    throw;
  }

  i++;
  if (options.ReportInterval == 0 || i <= options.ReportInterval)
    return;
  i = 0;

  auto now = std::chrono::system_clock::now();

  int c = model.GetNumberOfEstimatedModels();
  double all = (double)allCount;
  double percentage = std::round((c / all) * 10000) / 100;
  if (percentage != prePecentage) {
    std::chrono::duration<double> elapsed_mins = (now - start) / 60.0;
    double remains_mins = (all - c) * elapsed_mins.count() / c;
    if (printMsg)
      Rprintf("    Searched=%i, All=%i  (%.2f%%, %.1f minutes remains)\n", c,
              allCount, percentage > 100 || percentage < 0 ? NAN : percentage,
              remains_mins < 0 ? NAN : remains_mins);
    prePecentage = percentage;
  }
}

void ReportProgress(const ldt::ModelSet &model, bool &estimating,
                    ldt::SearchOptions &options, const int &allCount) {
  auto printMsg = options.ReportInterval > 0;
  auto start = std::chrono::system_clock::now();
  if (printMsg) {
    Rprintf("Calculations Started ...\n");
    Rprintf("Expected Number of Models = %i\n", allCount);
  }
  double prePecentage = -1;
  int i = 0;
  while (estimating) {
    ReportProgressInner(model, options, allCount, prePecentage, i, start,
                        printMsg);
  }
  if (options.RequestCancel)
    throw LdtException(ErrorType::kLogic, "R-ldt", "calculations is canceled");
  else if (printMsg)
    Rprintf("Calculations Ended.\n");
}

static std::vector<std::string>
extractElements(const std::vector<std::string> &vec,
                const std::vector<int> &indices, int skipWeight) {
  std::vector<std::string> extractedElements;
  for (Ti i = 0; i < (Ti)indices.size(); i++)
    extractedElements.push_back(vec[indices.at(i) + skipWeight]);
  return extractedElements;
}

static void
add_CoefInfo(const std::string &eName, const std::string &tName,
             const std::string &typeName,
             const std::vector<std::string> &colNames,
             std::vector<List> &results,
             const std::vector<std::shared_ptr<EstimationKeep>> &list,
             const std::vector<std::string> &extra1Names, const bool &addCoefs,
             const bool &hasWeight) {

  int j = -1;
  for (auto &b : list) {
    j++;

    std::vector<SEXP> value;
    std::vector<std::string> names;

    value.push_back(wrap(b->Metric));
    names.push_back(std::string("metric"));

    value.push_back(wrap(b->Weight));
    names.push_back(std::string("weight"));

    value.push_back(wrap(extractElements(colNames, b->Endogenous, 0)));
    names.push_back(std::string("endogenous"));

    // print(wrap("-----"));
    // print(wrap(b->Exogenouses));
    // print(wrap(colNames));
    // print(wrap(extractElements(colNames, b->Exogenouses, hasWeight)));

    value.push_back(wrap(extractElements(colNames, b->Exogenouses, hasWeight)));
    names.push_back(std::string("exogenous"));

    if (addCoefs) {
      value.push_back(wrap(b->Mean));
      names.push_back(std::string("mean"));

      value.push_back(wrap(b->Variance));
      names.push_back(std::string("variance"));
    }

    if (b->Extra.size() > 0) {
      IntegerVector ex = wrap(b->Extra);
      ex.names() = wrap(extra1Names);
      value.push_back(ex);
      names.push_back("extra");
    }
    Rcpp::List valueR = wrap(value);
    valueR.attr("names") = wrap(names);

    auto L =
        List::create(_["evalName"] = wrap(eName), _["targetName"] = wrap(tName),
                     _["typeName"] = wrap(typeName), _["info"] = wrap(j),
                     _["value"] = valueR);
    L.attr("class") =
        CharacterVector::create("ldt.search.item", "ldt.list", "list");
    results.push_back(L);
  }
}

static void add_Lengthi(const int &eIndex, const std::string &eName,
                        const int &tIndex, std::string tName,
                        const std::vector<std::string> &colNames,
                        std::vector<List> &results, const ModelSet &model,
                        const std::vector<SearcherSummary *> &list,
                        const SearchItems &items,
                        const std::vector<std::string> &length1Names,
                        const std::vector<std::string> &extra1Names) {

  if (items.KeepBestCount > 0) {

    for (auto i = 0; i < items.Length1; i++) {

      auto typeName = std::string("best item for '") + length1Names.at(i) +
                      std::string("'");
      // print(wrap("-------"));
      // print(wrap(i));
      // print(wrap(typeName));

      auto bests = std::vector<std::shared_ptr<EstimationKeep>>();
      model.CombineBests(eIndex, tIndex, i, list, bests);

      if (bests.size() != 0) {
        add_CoefInfo(eName, tName, typeName, colNames, results, bests,
                     extra1Names, true, model.pData->HasWeight);

        // auto last = results.at(results.size() - 1);
        // print(last);
      }
      // print(wrap("-------"));
    }
  }

  // All ? do we really need all

  if (items.CdfsAt.size() > 0) {

    for (int k = 0; k < (int)items.CdfsAt.size(); k++) {

      auto typeName = std::string("cdf");

      auto cdf = RunningMoments<1, true, true, Tv>();
      auto mat_d = std::make_unique<double[]>(items.Length1 * 3);
      auto mat = ldt::Matrix<double>(mat_d.get(), items.Length1, 3);
      auto colnames = std::vector<std::string>({"Mean", "Count", "SumWeights"});
      for (auto i = 0; i < items.Length1; i++) {
        model.CombineCdfAt(eIndex, tIndex, i, k, list, cdf);
        mat.Set0(i, 0, cdf.GetMean());
        mat.Set0(i, 1, (double)cdf.Count);
        mat.Set0(i, 2, cdf.SumWeights);
      }

      auto L = List::create(
          _["evalName"] = wrap(eName), _["targetName"] = wrap(tName),
          _["typeName"] = wrap(typeName), _["info"] = wrap(items.CdfsAt.at(k)),
          _["value"] = as_matrix(mat, length1Names, colnames));
      L.attr("class") =
          CharacterVector::create("ldt.search.item", "ldt.list", "list");
      results.push_back(L);
    }
  }

  if (items.ExtremeBoundsMultiplier > 0) {

    auto typeName = std::string("extreme bound");

    double min = 0, max = 0;
    auto mat_d = std::make_unique<double[]>(items.Length1 * 2);
    auto mat = ldt::Matrix<double>(mat_d.get(), items.Length1, 2);
    auto colnames = std::vector<std::string>({"Lower", "Upper"});
    for (auto i = 0; i < items.Length1; i++) {
      model.CombineExtremeBounds(eIndex, tIndex, i, list, min, max);
      mat.Set0(i, 0, min);
      mat.Set0(i, 1, max);
    }

    auto L =
        List::create(_["evalName"] = wrap(eName), _["targetName"] = wrap(tName),
                     _["typeName"] = wrap(typeName),
                     _["info"] = wrap(items.ExtremeBoundsMultiplier),
                     _["value"] = as_matrix(mat, length1Names, colnames));
    L.attr("class") =
        CharacterVector::create("ldt.search.item", "ldt.list", "list");
    results.push_back(L);
  }

  if (items.KeepMixture) {

    auto typeName = std::string("mixture");

    auto mixture = RunningMoments<4, true, true, Tv>();
    auto mat_d = std::make_unique<double[]>(items.Length1 * 6);
    auto mat = ldt::Matrix<double>(mat_d.get(), items.Length1, 6);
    auto colnames = std::vector<std::string>(
        {"Mean", "Variance", "Skewness", "Kurtosis", "Count", "SumWeights"});
    for (auto i = 0; i < items.Length1; i++) {
      model.CombineMixture(eIndex, tIndex, i, list, mixture);
      mat.Set0(i, 0, mixture.GetMean());
      mat.Set0(i, 1, (double)mixture.GetVariance());
      mat.Set0(i, 2, (double)mixture.GetSkewness());
      mat.Set0(i, 3, (double)mixture.GetKurtosis());
      mat.Set0(i, 4, (double)mixture.Count);
      mat.Set0(i, 5, (double)mixture.SumWeights);
    }

    auto L =
        List::create(_["evalName"] = wrap(eName), _["targetName"] = wrap(tName),
                     _["typeName"] = wrap(typeName), _["info"] = R_NilValue,
                     _["value"] = as_matrix(mat, length1Names, colnames));
    L.attr("class") =
        CharacterVector::create("ldt.search.item", "ldt.list", "list");
    results.push_back(L);
  }
}

List GetModelSetResults(const ModelSet &model, const SearchItems &items,
                        const std::vector<std::string> &metricNames,
                        const std::vector<std::string> &colNames,
                        const std::vector<std::string> &targetNames,
                        const std::vector<std::string> &extra1Names,
                        const std::vector<std::string> &length1Names,
                        const std::vector<std::string> &inclusionNames,
                        const std::string length1Label, const bool &printMsg) {

  List MainList = List(2);
  MainList.names() = std::vector<std::string>({"counts", "results"});

  // counts:
  auto result = SearcherModelingInfo();
  auto list0 = std::vector<SearcherSummary *>();
  auto list1 = std::vector<SearcherSummary *>();
  auto list2 = std::vector<SearcherSummary *>();
  model.CombineInfo(result, list0, list1, list2);
  int fcount = 0;
  List failDetails = List(result.FailsCount.size());
  int i = -1;
  for (const auto &[k, v] : result.FailsCount) {
    i++;
    fcount += v;
    failDetails[i] = List::create(_["message"] = wrap(k), _["count"] = wrap(v));
  }
  MainList[0] = List::create(_["expectedCount"] = wrap(result.ExpectedCount),
                             _["searchedCount"] = wrap(result.SearchedCount),
                             _["failedCount"] = wrap(fcount),
                             _["failedDetails"] = wrap(failDetails));
  if (fcount > 0 && printMsg)
    Rprintf("** Search process ended successfully. However, there are some "
            "failed estimations. See 'result$counts' for more details.");

  // results:
  if ((Ti)targetNames.size() != items.LengthTargets)
    throw LdtException(ErrorType::kLogic, "R-ldt",
                       std::string("invalid number of target names."));
  if ((Ti)metricNames.size() != items.LengthEvals)
    throw LdtException(ErrorType::kLogic, "R-ldt",
                       std::string("invalid number of evaluation names."));

  std::vector<List> results;
  for (Ti eIndex = 0; eIndex < (Ti)metricNames.size(); eIndex++) {
    auto eName = metricNames.at(eIndex);
    for (Ti tIndex = 0; tIndex < (Ti)targetNames.size(); tIndex++) {
      auto tName = targetNames.at(tIndex);

      if (items.KeepModelEvaluations) {
        if (items.KeepBestCount > 0) {
          auto typeName = std::string("best model");

          auto bests = std::vector<std::shared_ptr<EstimationKeep>>();
          model.CombineBests(eIndex, tIndex, 0, list0, bests);

          add_CoefInfo(eName, tName, typeName, colNames, results, bests,
                       extra1Names, false, model.pData->HasWeight);
        }

        if (items.KeepAll) {
          auto typeName = std::string("model");

          auto all = std::vector<std::shared_ptr<EstimationKeep>>();
          model.CombineAll(eIndex, tIndex, 0, list0, all);

          add_CoefInfo(eName, tName, typeName, colNames, results, all,
                       extra1Names, false, model.pData->HasWeight);
        }

        if (items.KeepInclusionWeights) {
          auto typeName = std::string("inclusion");

          auto covars = items.LengthEndogenous + items.LengthExogenous;
          auto incweights = RunningMoments<1, true, false, Tv>();
          auto mat_d = std::make_unique<double[]>(covars * 2);
          auto mat = ldt::Matrix<double>(mat_d.get(), covars, 2);
          auto colnames = std::vector<std::string>({"Mean", "Count"});
          for (auto i = 0; i < covars; i++) {
            model.CombineInclusionWeights(eIndex, tIndex, i, list0, incweights);
            mat.Set0(i, 0, incweights.GetMean());
            mat.Set0(i, 1, (double)incweights.Count);
          }

          auto L = List::create(
              _["evalName"] = wrap(eName), _["targetName"] = wrap(tName),
              _["typeName"] = wrap(typeName), _["info"] = R_NilValue,
              _["value"] = as_matrix(mat, inclusionNames, colnames));
          L.attr("class") =
              CharacterVector::create("ldt.search.item", "ldt.list", "list");
          results.push_back(L);
        }
      }

      if (items.Length1 > 0) {
        add_Lengthi(eIndex, eName, tIndex, tName, colNames, results, model,
                    list1, items, length1Names, extra1Names);
      }

      if (items.Length2 > 0) {
        throw LdtException(ErrorType::kLogic, "R-ldt",
                           "not implemented: length2>0");
      }
    }
  }

  MainList[1] = wrap(results);

  return MainList;
}
