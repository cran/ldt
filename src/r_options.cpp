
#include "r_ldt.h"

using namespace Rcpp;
using namespace ldt;

List GetNelderMeadOptions(int maxIterations, double epsilon, double alpha,
                          double beta, double gamma, double scale) {
  List O = List::create(_["maxIterations"] = wrap(maxIterations),
                        _["epsilon"] = wrap(epsilon), _["alpha"] = wrap(alpha),
                        _["beta"] = wrap(beta), _["gamma"] = wrap(gamma),
                        _["scale"] = wrap(scale));
  CheckNelderMeadOptions(O);
  return O;
}
void CheckNelderMeadOptions(Rcpp::List options) {
  if (as<int>(options["maxIterations"]) <= 0)
    throw std::logic_error(
        "Invalid Nelder-Mead option: 'maxIterations' must be positive.");
  ;
  if (as<double>(options["epsilon"]) < 0)
    throw std::logic_error(
        "Invalid Nelder-Mead option: 'epsilon' cannot be negative.");
  ;
  if (as<double>(options["alpha"]) <= 0)
    throw std::logic_error(
        "Invalid Nelder-Mead option: 'alpha' must be positive.");
  ;
  if (as<double>(options["beta"]) <= 0)
    throw std::logic_error(
        "Invalid Nelder-Mead option: 'beta' must be positive.");
  ;
  if (as<double>(options["gamma"]) <= 0)
    throw std::logic_error(
        "Invalid Nelder-Mead option: 'gamma' must be positive.");
  ;
  if (as<double>(options["scale"]) <= 0)
    throw std::logic_error(
        "Invalid Nelder-Mead option: 'scale' must be positive.");
  ;
}

List GetPcaOptions(int ignoreFirst, int exactCount, double cutoffRate,
                   int max) {
  List O = List::create(
    _["ignoreFirst"] = wrap(ignoreFirst), _["exactCount"] = wrap(exactCount),
    _["cutoffRate"] = wrap(cutoffRate), _["max"] = wrap(max));
  CheckPcaOptions(O);
  return O;
}

void CheckPcaOptions(List options) {
  if (as<int>(options["ignoreFirst"]) < 0)
    throw std::logic_error(
        "Invalid Pca option. 'ignoreFirst' cannot be negative.");
  if (as<int>(options["exactCount"]) < 0)
    throw std::logic_error(
        "Invalid Pca option. 'exactCount' cannot be negative.");
  if (as<double>(options["cutoffRate"]) <= 0 ||
      as<double>(options["cutoffRate"]) > 1)
    throw std::logic_error(
        "Invalid Pca option. 'cutoffRate' must be positive and less than 1.");
  if (as<int>(options["max"]) <= 0)
    throw std::logic_error("Invalid Pca option. 'max' must be positive.");
}

void UpdatePcaOptions(bool printMsg, List &pcaOptionsR, bool hasPca,
                      PcaAnalysisOptions &options, const char *startMsg) {

  if (printMsg)
    Rprintf("%s:\n", startMsg);
  if (hasPca) {
    options.IgnoreFirstCount = as<int>(pcaOptionsR["ignoreFirst"]);
    options.ExactCount = as<int>(pcaOptionsR["exactCount"]);
    options.CutoffRate = as<double>(pcaOptionsR["cutoffRate"]);
    options.CutoffCountMax = as<int>(pcaOptionsR["max"]);
    if (options.IsEnabled()) {
      options.CheckValidity();

      if (printMsg) {
        if (options.IgnoreFirstCount == 1)
          Rprintf("    - Ignores the first variable.\n");
        else if (options.IgnoreFirstCount > 1)
          Rprintf("    - Ignores the first %i variables.\n",
                  options.IgnoreFirstCount);

        if (options.ExactCount == 1)
          Rprintf("    - Uses the first component.\n");
        else if (options.ExactCount > 1)
          Rprintf("    - Uses the first %i components.\n", options.ExactCount);
        else {
          Rprintf("    - Uses a cutoff rate of %f to select the number of the "
                    "components.\n",
                    options.CutoffRate);
          Rprintf("    - Uses at most %i number of the components.\n",
                  options.CutoffCountMax);
        }
      }
    } else if (printMsg) {
      Rprintf("    - PCA options is given, but it is not effective.\n");
      Rprintf("    - Arguments are: %i, %i, %f, %i\n", options.IgnoreFirstCount,
              options.ExactCount, options.CutoffRate, options.CutoffCountMax);
    }
  } else if (printMsg)
    Rprintf("    - disabled.\n");
}

List GetLmbfgsOptions(int maxIterations, double factor,
                      double projectedGradientTol, int maxCorrections) {
  List O = List::create(_["maxIterations"] = wrap(maxIterations),
                        _["factor"] = wrap(factor),
                        _["projectedGradientTol"] = wrap(projectedGradientTol),
                        _["maxCorrections"] = wrap(maxCorrections));
  CheckLmbfgsOptions(O);
  return O;
}

void CheckLmbfgsOptions(List options) {
  if (as<int>(options["maxIterations"]) <= 0)
    throw std::logic_error(
        "Invalid LMBFGS option. 'maxIterations' must be positive.");
  if (as<double>(options["factor"]) <= 0)
    throw std::logic_error("Invalid LMBFGS option. 'factor' must be positive.");
  if (as<double>(options["projectedGradientTol"]) < 0)
    throw std::logic_error(
        "Invalid LMBFGS option. 'projectedGradientTol' cannot be negative.");
  if (as<int>(options["maxCorrections"]) <= 0)
    throw std::logic_error(
        "Invalid LMBFGS option. 'maxCorrections' must be positive.");
}

void UpdateLmbfgsOptions(bool printMsg, List &lmbfgsOptions,
                         LimitedMemoryBfgsbOptions &options) {
  if (printMsg)
    Rprintf("LMBFGS options:\n");
  options.Factor = as<double>(lmbfgsOptions["factor"]);
  options.IterationMax = as<int>(lmbfgsOptions["maxIterations"]);
  options.ProjectedGradientTol =
    as<double>(lmbfgsOptions["projectedGradientTol"]);
  options.mMaxCorrections = as<int>(lmbfgsOptions["maxCorrections"]);
  ;

  if (printMsg) {
    Rprintf("    - Maximum Number of Iterations=%i\n", options.IterationMax);
    Rprintf("    - Factor=%f\n", options.Factor);
    Rprintf("    - Projected Gradient Tolerance=%f\n",
            options.ProjectedGradientTol);
    Rprintf("    - Maximum Corrections=%i\n", options.mMaxCorrections);
  }
}

List GetNewtonOptions(int maxIterations, double functionTol, double gradientTol,
                      bool useLineSearch) {
  List O = List::create(_["maxIterations"] = wrap(maxIterations),
                        _["functionTol"] = wrap(functionTol),
                        _["gradientTol"] = wrap(gradientTol),
                        _["useLineSearch"] = wrap(useLineSearch));
  CheckNewtonOptions(O);
  return O;
}

void CheckNewtonOptions(List options) {
  if (as<int>(options["maxIterations"]) <= 0)
    throw std::logic_error(
        "Invalid Newton option. 'maxIterations' must be positive.");
  if (as<double>(options["functionTol"]) < 0)
    throw std::logic_error(
        "Invalid Newton option. 'functionTol' cannot be negative.");
  if (as<double>(options["gradientTol"]) < 0)
    throw std::logic_error(
        "Invalid Newton option. 'gradientTol' cannot be negative.");
}

void UpdateNewtonOptions(bool printMsg, List &newtonR, Newton &newton) {

  if (printMsg)
    Rprintf("Newton Optimization Parameters:\n");

  newton.IterationMax = as<int>(newtonR["maxIterations"]);
  newton.TolFunction = as<double>(newtonR["functionTol"]);
  newton.TolGradient = as<double>(newtonR["gradientTol"]);
  newton.UseLineSearch = as<bool>(newtonR["useLineSearch"]);

  if (printMsg) {
    Rprintf("    - Iterations (Maximum)=%i\n", newton.IterationMax);
    Rprintf("    - Function Tolerance=%f\n", newton.TolFunction);
    Rprintf("    - Gradient Tolerance=%f\n", newton.TolGradient);
    Rprintf("    - Use Line Search=%s\n",
            newton.UseLineSearch ? "TRUE" : "FALSE");
  }
}

List GetSearchItems(bool model, bool type1, bool type2, int bestK, bool all,
                    bool inclusion, SEXP cdfs, double extremeMultiplier,
                    bool mixture4) {
  NumericVector cdfs_;
  if (cdfs != R_NilValue)
    cdfs_ = internal::convert_using_rfunction(cdfs, "as.numeric");
  else
    cdfs_ = NumericVector();

  List O = List::create(_["model"] = wrap(model), _["type1"] = wrap(type1),
                        _["type2"] = wrap(type2), _["bestK"] = wrap(bestK),
                        _["all"] = wrap(all), _["inclusion"] = wrap(inclusion),
                        _["cdfs"] = cdfs_,
                        _["extremeMultiplier"] = wrap(extremeMultiplier),
                        _["mixture4"] = wrap(mixture4));
  CheckSearchItems(O);
  return O;
}

void CheckSearchItems(List options) {
  if (as<int>(options["bestK"]) < 0)
    throw std::logic_error("Invalid Search item. 'bestK' cannot be negative.");
  if (as<double>(options["extremeMultiplier"]) < 0)
    throw std::logic_error(
        "Invalid Search item. 'extremeMultiplier' cannot be negative.");
}

void UpdateSearchItems(bool printMsg, List &searchItems, SearchItems &items,
                       int length1, int length2, const char *length1Informtion,
                       const char *length2Informtion, bool type1NeedsModelEstim,
                       bool type2NeedsModelEstim) {

  items.KeepModelEvaluations = as<bool>(searchItems["model"]);
  items.KeepAll = as<bool>(searchItems["all"]);
  items.KeepMixture = as<bool>(searchItems["mixture4"]);
  items.KeepInclusionWeights = as<bool>(searchItems["inclusion"]);
  items.KeepBestCount = as<int>(searchItems["bestK"]);
  items.ExtremeBoundsMultiplier = as<double>(searchItems["extremeMultiplier"]);

  items.CdfsAt = as<std::vector<double>>(searchItems["cdfs"]);

  // update length1 and 2
  bool type1 = as<bool>(searchItems["type1"]);
  bool type2 = as<bool>(searchItems["type2"]);
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
    throw std::logic_error("No evaluation data is saved");

  if (printMsg) {

    Rprintf("Saves:\n");
    if (items.KeepModelEvaluations)
      Rprintf("    - models\n");
    if (items.Length1 > 0)
      Rprintf("    - %s\n", length1Informtion);
    if (items.Length2 > 0)
      Rprintf("    - %s\n", length2Informtion);
  }

  bool hasGoal = false;

  if (printMsg)
    Rprintf("Goals:\n");

  if (items.KeepBestCount > 0) {
    hasGoal = true;
    if (printMsg) {
      if (items.KeepBestCount == 1)
        Rprintf("    - Find best model\n");
      else
        Rprintf("    - Find first %i best models\n", items.KeepBestCount);
    }
  }
  if (items.KeepAll) {
    hasGoal = true;
    if (printMsg)
      Rprintf("    - Keep everything\n");
  }
  if (items.CdfsAt.size() > 0) {
    hasGoal = true;
    if (printMsg)
      Rprintf("    - Keep CDFs at %s\n", VectorToCsv(items.CdfsAt).c_str());
  }
  if (items.KeepMixture) {
    hasGoal = true;
    if (printMsg)
      Rprintf("    - Keep mixture distribution\n");
  }
  if (items.ExtremeBoundsMultiplier > 0) {
    hasGoal = true;
    if (printMsg)
      Rprintf("    - Keep extreme bounds (multiplier=%f)\n",
              items.ExtremeBoundsMultiplier);
  }
  if (items.KeepInclusionWeights) {
    hasGoal = true;
    if (printMsg)
      Rprintf("    - Keep inclusion weights\n");
  }

  if (hasGoal == false)
    throw std::logic_error("No goal is set.");
}

List GetSearchOptions(bool parallel, int reportInterval, bool printMsg) {
#ifndef _OPENMP
  if (parallel){
    parallel = false;
    warning("Warning: 'parallel' option is not available.");
  }
#endif
  List O = List::create(_["parallel"] = wrap(parallel),
                        _["reportInterval"] = wrap(reportInterval),
                        _["printMsg"] = wrap(printMsg));
  CheckSearchOptions(O);
  return O;
}

void CheckSearchOptions(List options) {
  if (as<int>(options["reportInterval"]) < 0)
    throw std::logic_error(
        "Invalid Search option. 'reportInterval' cannot be negative.");
}

void UpdateSearchOptions(List &searchOptions, SearchOptions &options,
                         int &reportInterval, bool &printMsg) {

  options.Parallel = as<bool>(searchOptions["parallel"]);
  reportInterval = as<int>(searchOptions["reportInterval"]);
  printMsg = as<bool>(searchOptions["printMsg"]);

  if (printMsg) {
    Rprintf("Search Options:\n");
    Rprintf("    - Is Parallel = %s\n", options.Parallel ? "TRUE" : "FALSE");
    Rprintf("    - Report Interval (seconds) = %i\n", reportInterval);
  }
}

List GetModelCheckItems(bool estimation, double maxConditionNumber,
                        int minObsCount, int minDof, int minOutSim,
                        double minR2, double maxAic, double maxSic,
                        bool prediction, double predictionBoundMultiplier) {
  List O = List::create(
    _["estimation"] = wrap(estimation),
    _["maxConditionNumber"] = wrap(maxConditionNumber),
    _["minObsCount"] = wrap(minObsCount), _["minDof"] = wrap(minDof),
    _["minOutSim"] = wrap(minOutSim), _["maxSic"] = wrap(maxSic),
    _["minR2"] = wrap(minR2), _["maxAic"] = wrap(maxAic),
    _["maxSic"] = wrap(maxSic), _["prediction"] = wrap(prediction),
    _["predictionBoundMultiplier"] = wrap(predictionBoundMultiplier));
  CheckModelCheckItems(O);
  return O;
}

void CheckModelCheckItems(List options) {
  if (as<int>(options["minObsCount"]) < 0)
    throw std::logic_error(
        "Invalid model-Check option. 'minObsCount' cannot be negative.");
  if (as<int>(options["minDof"]) < 0)
    throw std::logic_error(
        "Invalid model-Check option. 'minDof' cannot be negative.");
  if (as<int>(options["minOutSim"]) < 0)
    throw std::logic_error(
        "Invalid model-Check option. 'minOutSim' cannot be negative.");
  if (as<double>(options["maxConditionNumber"]) < 0)
    throw std::logic_error(
        "Invalid model-Check option. 'maxConditionNumber' cannot be negative.");
  if (as<double>(options["predictionBoundMultiplier"]) < 0)
    throw std::logic_error("Invalid model-Check option. "
                             "'predictionBoundMultiplier' cannot be negative.");
}

void UpdateModelCheckItems(bool printMsg, List &checkOptions,
                           SearchModelChecks &checks,
                           const SearchMeasureOptions &measures,
                           const SearchItems &items) {

  checks.Estimation = as<bool>(checkOptions["estimation"]);
  checks.MinObsCount = as<int>(checkOptions["minObsCount"]);
  checks.MinDof = as<int>(checkOptions["minDof"]);
  checks.MinOutSim = as<int>(checkOptions["minOutSim"]);
  checks.PredictionBoundMultiplier =
    as<double>(checkOptions["predictionBoundMultiplier"]);

  checks.MinR2 = as<double>(checkOptions["minR2"]);
  checks.MaxAic = as<double>(checkOptions["maxAic"]);
  checks.MaxSic = as<double>(checkOptions["maxSic"]);
  checks.MaxConditionNumber = as<double>(checkOptions["maxConditionNumber"]);
  checks.Prediction = as<bool>(checkOptions["prediction"]);

  checks.Update(measures);

  if (printMsg) {

    Rprintf("Checks:\n");
    if (checks.Estimation) {
      Rprintf("- Given All Data:\n");
      Rprintf("    - Model Is Estimated\n");
      if (checks.MinObsCount > 0)
        Rprintf("        - Number of Obs. > %i\n", checks.MinObsCount);
      if (checks.MinDof > 0)
        Rprintf("        - DoF > %i\n", checks.MinDof);
      if (std::isinf(checks.MaxAic) == false)
        Rprintf("        - AIC < %.1e\n", checks.MaxAic);
      if (std::isinf(checks.MaxSic) == false)
        Rprintf("        - SIC < %.1e\n", checks.MaxSic);
      if (std::isinf(-checks.MinR2) == false)
        Rprintf("        - R2 > %.1e\n", checks.MinR2);
      if (checks.mCheckCN_all)
        Rprintf("        - CN < %.1e\n", checks.MaxConditionNumber);
    }
    if (measures.SimFixSize > 0) {
      Rprintf("    - Out-of-Sample:\n");
      bool has = false;
      if (checks.mCheckCN) {
        has = true;
        Rprintf("        - CN(s) < %.1e\n", checks.MaxConditionNumber);
      }
      if (checks.MinOutSim > 0) {
        has = true;
        Rprintf("        - Number of Valid Simulations > %i\n",
                checks.MinOutSim);
      }
      if (has == false)
        Rprintf("        - none\n");
    }
    if (checks.Prediction) {
      Rprintf("    - Model is Used for Prediction\n");
      if (checks.mCheckPredBound)
        Rprintf("        - Predictions must lie in a bound (multiplier = %f)\n",
                checks.PredictionBoundMultiplier);
    }
  }
}

List GetMeasureOptions(SEXP typesIn, SEXP typesOut, int simFixSize,
                       double trainRatio, int trainFixSize, int seed,
                       SEXP horizons, bool weightedEval) {

  StringVector typesIn_;
  if (typesIn != R_NilValue)
    typesIn_ = internal::convert_using_rfunction(typesIn, "as.character");
  else
    typesIn_ = StringVector(0);

  StringVector typesOut_;
  if (typesOut != R_NilValue)
    typesOut_ = internal::convert_using_rfunction(typesOut, "as.character");
  else
    typesOut_ = StringVector(0);

  IntegerVector horizons_;
  if (horizons != R_NilValue)
    horizons_ = internal::convert_using_rfunction(horizons, "as.integer");
  else
    horizons_ = {1};

  List O = List::create(_["typesIn"] = typesIn_, _["typesOut"] = typesOut_,
                        _["simFixSize"] = wrap(simFixSize),
                        _["trainRatio"] = wrap(trainRatio),
                        _["trainFixSize"] = wrap(trainFixSize),
                        _["seed"] = wrap(seed), _["horizons"] = horizons_,
                        _["weightedEval"] = weightedEval);

  CheckMeasureOptions(O);
  return O;
}

void CheckMeasureOptions(List options) {
  if (is<StringVector>(options["typesIn"]) == false ||
      is<StringVector>(options["typesOut"]) == false)
    throw std::logic_error(
        "Invalid type ('typesIn', 'typesOut'). Expected a string vector.");

  StringVector typesIn = options["typesIn"];
  StringVector typesOut = options["typesOut"];
  if (typesIn.length() == 0 && typesOut.length() == 0)
    throw std::logic_error(
        "Invalid Measure option. Both 'typesIn' and 'typesOut' are empty.");

  if (typesOut.length() > 0) {
    if (is<IntegerVector>(options["horizons"]) == false)
      throw std::logic_error(
          "Invalid type ('horizons'). Expected an integer vector.");

    IntegerVector hors = options["horizons"];
    for (int i = 0; i < hors.length(); i++) {
      if (hors[i] <= 0) {
        Rcout << "Horizons: " << hors << "\n";
        throw std::logic_error(
            "Invalid Measure option. zero or negative value in 'horizons'.");
      }
    }

    if (as<int>(options["simFixSize"]) < 0)
      throw std::logic_error(
          "Invalid Measure option. 'simFixSize' cannot be negative.");
    if (as<double>(options["trainRatio"]) < 0)
      throw std::logic_error(
          "Invalid Measure option. 'trainRatio' cannot be negative.");
    if (as<int>(options["trainFixSize"]) < 0)
      throw std::logic_error(
          "Invalid Measure option. 'trainFixSize' cannot be negative.");
    // if (options["seed"] < 0)  It can be negative for similar distribution of
    // the seeds in the searchers

    if (as<double>(options["trainRatio"]) == 0 &&
        as<int>(options["trainFixSize"]) == 0)
      throw std::logic_error("Invalid Measure option. Both 'trainRatio' and "
                               "'trainFixSize' are zero.");
  }
}

void UpdateMeasureOptions(bool printMsg, List &measureOptions,
                          SearchMeasureOptions &measures,
                          std::vector<std::string> &measureNames,
                          bool isTimeSeries, bool isDc) {

  bool isOutOfSampleRandom = isTimeSeries == false;
  // bool supportsSimRatio = isTimeSeries;

  auto measuresOut0 = as<StringVector>(measureOptions["typesOut"]);
  auto measuresIn0 = as<StringVector>(measureOptions["typesIn"]);
  auto lmeasureOut = measuresOut0.length();
  auto lmeasureIn = measuresIn0.length();

  if (lmeasureIn == 0 && lmeasureOut == 0)
    throw std::logic_error(
        "No measure is specified. Check the inputs (also, check the number "
        "of simulations).");
  if (lmeasureIn > 0) {
    for (auto i = 0; i < lmeasureIn; i++) {
      auto a = as<std::string>(measuresIn0[i]);
      boost::algorithm::to_lower(a);
      auto eval = FromString_GoodnessOfFitType(a.c_str());
      measures.MeasuresIn.push_back(eval);
    }
  }
  if (lmeasureOut > 0) {
    for (auto i = 0; i < lmeasureOut; i++) {
      auto a = as<std::string>(measuresOut0[i]);
      boost::algorithm::to_lower(a);
      auto eval = FromString_ScoringType(a.c_str());
      measures.MeasuresOut.push_back(eval);
    }
  }

  measures.SimFixSize = as<int>(measureOptions["simFixSize"]);
  // measures.SimRatio = Rf_asInteger(GetListElement(measureOptions,
  // "simratio"));
  measures.Seed = as<int>(measureOptions["seed"]);

  if (isTimeSeries && lmeasureOut > 0) {

    IntegerVector hors = measureOptions["horizons"];
    for (auto i = 0; i < hors.length(); i++)
      measures.Horizons.push_back(hors[i]);

    measures.TrainFixSize = 0;
    measures.TrainRatio = 0;
  } else {
    measures.TrainFixSize = as<int>(measureOptions["trainFixSize"]);
    measures.TrainRatio = as<double>(measureOptions["trainRatio"]);
  }

  measures.Update(isOutOfSampleRandom,
                  isTimeSeries); // update after filling measure vectors

  if (printMsg) {
    Rprintf("Measuring Options:\n");
    Rprintf("    - In-Sample:");
  }
  if (measures.MeasuresIn.size() > 0) {
    for (auto i = 0; i < lmeasureIn; i++) {
      auto str = ToString(measures.MeasuresIn.at(i), true);
      measureNames.push_back(str);
      if (printMsg) {
        Rprintf(str);
        if (i != lmeasureIn - 1)
          Rprintf(", ");
      }
    }
    if (printMsg)
      Rprintf("\n");
  } else if (printMsg)
    Rprintf("none\n");

  if (printMsg)
    Rprintf("    - Out-Of-Sample:");

  if (measures.MeasuresOut.size() > 0) {

    for (auto i = 0; i < lmeasureOut; i++) {
      auto str = ToString(measures.MeasuresOut.at(i), true);
      measureNames.push_back(str);
      if (printMsg) {
        Rprintf(str);
        if (i != lmeasureOut - 1)
          Rprintf(", ");
      }
    }
    if (printMsg)
      Rprintf("\n");

    if (printMsg) {
      // if (supportsSimRatio && measures.SimRatio > 0)
      //	Rprintf("        - Simulation (Ratio) = %i\n",
      // measures.SimRatio); else
      Rprintf("        - Simulation Count = %i\n", measures.SimFixSize);

      if (isTimeSeries == false) {
        if (measures.TrainRatio > 0)
          Rprintf("        - Train Size (Ratio) = %f\n", measures.TrainRatio);
        else
          Rprintf("        - Train Size = %i (fixed)\n", measures.TrainFixSize);
      }
      if (isOutOfSampleRandom)
        Rprintf("        - Seed = %i\n", measures.Seed);

      if (measures.Horizons.size() > 0)
        Rprintf("        - Horizons = %s\n",
                VectorToCsv(measures.Horizons).c_str());
    }
  } else if (printMsg)
    Rprintf("none\n");

  if (isDc){
    measures.WeightedEval = as<bool>(measureOptions["weightedEval"]);
    if (printMsg)
      Rprintf("    - Weighted = %s\n", measures.WeightedEval ? "true" : "false");
  }

}
