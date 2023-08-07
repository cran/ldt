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
  if (hasOut && MetricsOut.size() == 0)
    throw LdtException(
        ErrorType::kLogic, "searcher-summary",
        "number of simulations is positive but out-of-sample metrics "
        "are missing");

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

  // indexes
  mIndexOfAic = IndexOf(MetricsIn, GoodnessOfFitType::kAic);
  mIndexOfSic = IndexOf(MetricsIn, GoodnessOfFitType::kSic);

  mIndexOfDirection = IndexOf(MetricsOut, ScoringType::kDirection);
  mIndexOfSign = IndexOf(MetricsOut, ScoringType::kSign);
  mIndexOfMae = IndexOf(MetricsOut, ScoringType::kMae);
  mIndexOfMaeSc = IndexOf(MetricsOut, ScoringType::kMape);
  mIndexOfRmse = IndexOf(MetricsOut, ScoringType::kRmse);
  mIndexOfRmseSc = IndexOf(MetricsOut, ScoringType::kRmspe);
  mIndexOfCrps = IndexOf(MetricsOut, ScoringType::kCrps);

  // discrete choice
  mIndexOfCostMatrixIn = IndexOf(MetricsIn, GoodnessOfFitType::kFrequencyCost);
  mIndexOfAucIn = IndexOf(MetricsIn, GoodnessOfFitType::kAuc);
  mIndexOfBrierIn = IndexOf(MetricsIn, GoodnessOfFitType::kBrier);

  mIndexOfCostMatrixOut = IndexOf(MetricsOut, ScoringType::kFrequencyCost);
  mIndexOfAucOut = IndexOf(MetricsOut, ScoringType::kAuc);
  mIndexOfBrierOut = IndexOf(MetricsOut, ScoringType::kBrier);
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

EstimationKeep::EstimationKeep(Tv weight, const Matrix<Ti> *exogenouses,
                               const Matrix<Ti> *extra,
                               const Matrix<Ti> *dependents, Tv mean,
                               Tv variance) {
  Mean = mean;
  Variance = variance;
  Weight = weight;

  if (dependents) {
    Dependents =
        Matrix<Ti>(new Ti[dependents->length()], dependents->length(), (Ti)1);
    dependents->CopyTo00(Dependents);
  }

  if (exogenouses) {
    Exogenouses =
        Matrix<Ti>(new Ti[exogenouses->length()], exogenouses->length(), (Ti)1);
    exogenouses->CopyTo00(Exogenouses);
  }

  if (extra) {
    Extra = Matrix<Ti>(new Ti[extra->length()], extra->length(), (Ti)1);
    extra->CopyTo00(Extra);
  }
}

EstimationKeep::~EstimationKeep() {
  if (Dependents.Data)
    delete[] Dependents.Data;
  if (Exogenouses.Data)
    delete[] Exogenouses.Data;
  if (Extra.Data)
    delete[] Extra.Data;
}

// #pragma endregion

// #pragma region SearcherSummary

SearcherSummary::SearcherSummary(Ti Index1, Ti Index2, Ti Index3,
                                 const SearchItems *option) {
  this->Index1 = Index1;
  this->Index2 = Index2;
  this->Index3 = Index3;
  pItems = option;

  if (pItems->ExtremeBoundsMultiplier > 0)
    ExtremeBounds = std::vector<Tv>(
        {std::numeric_limits<Tv>::max(), std::numeric_limits<Tv>::min()});
  if (pItems->KeepInclusionWeights) {
    Ti max_inc = pItems->LengthDependents + pItems->LengthExogenouses;
    InclusionsInfo = std::vector<RunningMoments<1, true, false, Tv>>(max_inc);
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

void SearcherSummary::Push(EstimationKeep &coef, bool isModel,
                           Matrix<Ti> *overrideInclusioExo) {
  if (pItems->KeepBestCount != 0) {
    for (Ti i = 0; i < (Ti)pItems->KeepBestCount; i++) {
      if (i >= (Ti)Bests.size())
        Bests.push_back(&coef);
      else if (coef.Weight > Bests.at(i)->Weight)
        Bests.insert(Bests.begin() + i, &coef);
      else
        continue;

      if ((Ti)Bests.size() > pItems->KeepBestCount) {
        if (isModel == false ||
            pItems->KeepAll == false) { // All handles delete
          auto last = Bests.back();
          delete last;
        }
        Bests.pop_back();
      }
      break;
    }
  }

  if (isModel) {
    if (pItems->KeepAll) {
      All.push_back(&coef);
    }

    if (pItems->KeepInclusionWeights) { // we keep the inclusion weights in
                                        // the model
      // Ti max_inc = pItems->LengthDependents + pItems->LengthExogenouses;
      Tv w = 1;
      if (coef.Dependents.Data)
        for (Ti i = 0; i < coef.Dependents.length(); i++)
          InclusionsInfo.at(coef.Dependents.Data[i]).PushNew(coef.Weight, w);
      else // a univariate case
        InclusionsInfo.at(0).PushNew(coef.Weight, w);
      if (overrideInclusioExo)
        for (Ti i = 0; i < overrideInclusioExo->length(); i++)
          InclusionsInfo.at(overrideInclusioExo->Data[i])
              .PushNew(coef.Weight, w);
      else if (coef.Exogenouses.Data)
        for (Ti i = 0; i < coef.Exogenouses.length(); i++)
          InclusionsInfo.at(coef.Exogenouses.Data[i]).PushNew(coef.Weight, w);
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
