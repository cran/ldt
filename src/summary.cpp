/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the GPL 3.0.
 See accompanying file LICENSE
*/

#include "distributions.h"
#include "searchers.h"

using namespace ldt;

// #pragma region Options

void SearchItems::Update(const SearchMetricOptions metrics, Ti targetCount,
                         Ti DepenCount, Ti exoCount) {
  LengthEvals = (Ti)(metrics.MetricsIn.size() + metrics.MetricsOut.size());
  if (targetCount <= 0)
    throw LdtException(ErrorType::kLogic, "searcher-summary",
                       "number of targets must be positive");
  LengthTargets = targetCount;
}

void SearchMetricOptions::Update(bool isOutOfSampleRandom, bool isTimeSeries) {
  mIsTimeSeries = isTimeSeries;
  if (isOutOfSampleRandom == false) {
    Seed = 0;
  }
  if (isTimeSeries == false)
    Horizons.clear();

  bool hasOut = SimFixSize > 0; // || (supportsSimRatio && SimRatio > 0);
  if (hasOut == false && MetricsOut.size() > 0)
    throw LdtException(ErrorType::kLogic, "searcher-summary",
                       "out-of-sample metrics is given, but the number of "
                       "simulations is zero");

  if (TrainFixSize > 0)
    TrainRatio = 0;                   // ignore it
  if (hasOut && isTimeSeries == false // for time prediction, everything is
                                      // determined by simulation size
      && TrainFixSize == 0 && TrainRatio == 0)
    throw LdtException(ErrorType::kLogic, "searcher-summary",
                       "training sample is empty");

  if (isTimeSeries) {
    if ((Horizons.size() == 0 && MetricsOut.size() > 0) ||
        (Horizons.size() > 0 && MetricsOut.size() == 0))
      throw LdtException(
          ErrorType::kLogic, "searcher-summary",
          "invalid number of horizons (or out-of-sample metrics) is found");
  }

  EvalIsPosOrientation.clear();
  for (auto m : MetricsIn) {
    EvalIsPosOrientation.push_back(GoodnessOfFit::IsPositiveOriented(m));
  }
  for (auto m : MetricsOut) {
    EvalIsPosOrientation.push_back(Scoring::IsPositiveOriented(m));
  }

  // indexes
  for (auto m : std::vector<GoodnessOfFitType>(
           {GoodnessOfFitType::kAic, GoodnessOfFitType::kAuc,
            GoodnessOfFitType::kBrier, GoodnessOfFitType::kFrequencyCost,
            GoodnessOfFitType::kSic}))
    MetricInIndices.insert(std::make_pair(m, IndexOf(MetricsIn, m)));

  for (auto m : std::vector<ScoringType>(
           {ScoringType::kAuc, ScoringType::kBrier, ScoringType::kCrps,
            ScoringType::kDirection, ScoringType::kFrequencyCost,
            ScoringType::kMae, ScoringType::kMape, ScoringType::kRmse,
            ScoringType::kRmspe, ScoringType::kSign}))
    MetricOutIndices.insert(std::make_pair(m, IndexOf(MetricsOut, m)));
}

void SearchModelChecks::Update(const SearchMetricOptions &metrics) {
  if (metrics.mIsTimeSeries == false)
    Prediction = false;

  if (Prediction == false) {
    mCheckPredBound = true;
    PredictionBoundMultiplier = 0;
  } else
    Estimation = true;

  if (metrics.SimFixSize > 0 && MinOutSim > metrics.SimFixSize)
    throw LdtException(
        ErrorType::kLogic, "searcher-summary",
        "minimum number of simulations cannot be larger than the number of "
        "simulations");

  auto checkN = MinObsCount > 0;
  auto checkDof = MinDof > 0;
  auto checkAic = std::isinf(MaxAic) == false;
  auto checkSic = std::isinf(MaxSic) == false;
  auto checkR2 = std::isinf(-MinR2) == false;

  mCheckCN =
      metrics.MetricsOut.size() > 0 && std::isinf(MaxConditionNumber) == false;
  mCheckCN_all = Estimation && std::isinf(MaxConditionNumber) ==
                                   false; // note that maximum condition number
                                          // does not affect estimation here
  mCheckPredBound = metrics.mIsTimeSeries && PredictionBoundMultiplier > 0;

  if (Estimation == false && (metrics.MetricsIn.size() > 0 || checkN ||
                              checkDof || checkAic || checkSic || checkR2))
    Estimation = true;
}

// #pragma endregion

// #pragma region EstimationKeep

EstimationKeep::EstimationKeep(Tv metric, Tv weight,
                               const std::vector<Ti> &exogenous,
                               const std::vector<Ti> &extra,
                               const std::vector<Ti> &endogenous, Tv mean,
                               Tv variance)
    : Mean(mean), Variance(variance), Metric(metric), Weight(weight),
      Endogenous(endogenous), Exogenouses(exogenous), Extra(extra) {}

// #pragma endregion

// #pragma region SearcherSummary

SearcherSummary::SearcherSummary(Ti Index1, Ti Index2, Ti Index3,
                                 const SearchItems *option,
                                 bool isPositiveOriented,
                                 const SearchData *data)
    : Bests(EstimationKeepComp(isPositiveOriented)) {
  this->Index1 = Index1;
  this->Index2 = Index2;
  this->Index3 = Index3;
  pItems = option;
  pData = data;

  if (pItems->ExtremeBoundsMultiplier > 0)
    ExtremeBounds = std::vector<Tv>(
        {std::numeric_limits<Tv>::max(), std::numeric_limits<Tv>::min()});
  if (pItems->KeepInclusionWeights) {
    InclusionsInfo = std::vector<RunningMoments<1, true, false, Tv>>(
        data->NumEndo + data->NumExo);
  }
  if (pItems->CdfsAt.size() > 0)
    Cdfs =
        std::vector<RunningMoments<1, true, true, Tv>>(pItems->CdfsAt.size());
}

SearcherSummary::~SearcherSummary() {
  if (All.size() > 0) {
    for (auto &p : All)
      delete p;
  } else { // if 'All', it has deleted the members
    for (auto &p : Bests)
      delete p;
  }
}

void SearcherSummary::Push(EstimationKeep &coef, bool isModel) {
  if (pItems->KeepBestCount != 0) {
    Bests.insert(&coef);
    if ((Ti)Bests.size() > pItems->KeepBestCount) {
      auto it = std::prev(Bests.end());
      if (isModel == false || pItems->KeepAll == false) {
        delete *it;
      } // otherwise, All handles delete
      Bests.erase(it);
    }
  }

  if (isModel) {
    if (pItems->KeepAll) {
      All.push_back(&coef);
    }

    if (pItems->KeepInclusionWeights) {
      Ti adj = pData->HasWeight ? -1 : 0;
      Tv w = 1;

      for (auto &i : coef.Endogenous)
        InclusionsInfo.at(i).PushNew(coef.Weight, w);
      for (auto &i : coef.Exogenouses)
        InclusionsInfo.at(i + adj).PushNew(coef.Weight, w);
    }

    return; // the rest is based on mean and variances which is not
            // available for models
  }

  if (pItems->CdfsAt.size() > 0) {
    Ti i = -1;
    for (auto &v : pItems->CdfsAt) {
      i++;
      Cdfs.at(i).PushNew(Distribution<DistributionType::kNormal>(
                             coef.Mean, std::sqrt(coef.Variance))
                             .GetCdf(v),
                         coef.Weight); // normal distribution ?!
    }
  }

  if (pItems->KeepMixture) {
    Mixture4.Combine(coef.Mean, coef.Variance, 0, 0, coef.Weight);
  }

  if (pItems->ExtremeBoundsMultiplier > 0) {
    double d = pItems->ExtremeBoundsMultiplier * std::sqrt(coef.Variance);
    ExtremeBounds.at(0) = std::min(ExtremeBounds.at(0), coef.Mean - d);
    ExtremeBounds.at(1) = std::max(ExtremeBounds.at(1), coef.Mean + d);
  }
}

// #pragma endregion
