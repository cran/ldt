/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the GPL 3.0.
 See accompanying file LICENSE
*/

#include "clustering.h"
#include "varma.h"

using namespace ldt;

// #pragma region Searcher

VarmaSearcher::VarmaSearcher(
    const SearchData &data, const SearchCombinations &combinations,
    SearchOptions &options, const SearchItems &items,
    const SearchMetricOptions &metrics, const SearchModelChecks &checks,
    const Ti &numPartitions, const DatasetTs<true> &source,
    const VarmaSizes &sizes, const std::vector<Ti> &exoIndexes,
    const Matrix<Tv> *forLowerBounds, const Matrix<Tv> *forUpperBounds,
    LimitedMemoryBfgsbOptions *optimOptions, const Tv &stdMultiplier,
    const bool &usePreviousEstim, const Ti &maxHorizonCheck)
    : SearcherReg::SearcherReg(data, combinations, options, items, metrics,
                               checks, numPartitions, true, exoIndexes, 6),
      UsePreviousEstim(usePreviousEstim), StdMultiplier(stdMultiplier),
      mMaxHorizonCheck(maxHorizonCheck), pForLowerBounds(forLowerBounds),
      pForUpperBounds(forUpperBounds), Sizes(sizes), Source(source) {
  // copy source for parallel (It uses the indexes)

  // TODO: how should we treat number of fixed partitions if we want to always
  // estimate a model with a target.
  //  Note that targets might all be in the first partition
  // if (this->mFixFirstItems == 0)
  //  throw LdtException(ErrorType::kLogic, "varma-modelset", "At least .");

  Params = VMatrix<Ti>(
      {Sizes.ArP, Sizes.ArD, Sizes.ArQ, Sizes.MaP, Sizes.MaD, Sizes.MaQ}, 6, 1);

  // we will estimate a complete model even in simulation scenario (ignores
  // simulation if there is an error in estimating the whole model)
  if (checks.Estimation) {
    DModel = Varma(Sizes, true, true, true, optimOptions);
    if (checks.Prediction) {
      if (maxHorizonCheck <= 0)
        throw LdtException(ErrorType::kLogic, "varma-modelset",
                           "invalid horizon in checking the predictions");

      // length 1 is the horizon for testing
      FModel = VarmaForecast(Sizes, maxHorizonCheck, true);
    }
  }

  if (metrics.SimFixSize > 0 && metrics.MetricsOut.size() > 0) {
    Model = VarmaSimulation(Sizes, metrics.SimFixSize, metrics.Horizons,
                            metrics.MetricsOut,
                            optimOptions); // no PCA in search
  }

  this->WorkSize = FModel.StorageSize + DModel.Result.StorageSize +
                   Model.StorageSize +
                   std::max(DModel.Result.WorkSize,
                            std::max(Model.WorkSize, FModel.WorkSize));

  if (Sizes.HasMa) {
    Restriction = VarmaRestriction(
        Sizes, VarmaRestrictionType::kMaFinal); // TODO: as an option
    RestrictionData = std::unique_ptr<Tv[]>(new Tv[Restriction.StorageSize]);
    Restriction.Calculate(RestrictionData.get());
  }

  this->WorkSize +=
      source.pData->ColsCount * numPartitions; // for copying the matrices
  if (Sizes.ExoCount > 0)
    this->WorkSize += source.pData->ColsCount * (Ti)exoIndexes.size();
}

std::string VarmaSearcher::EstimateOneReg(Tv *work, Ti *workI,
                                          VMatrix<Tv> &metrics,
                                          VMatrix<Tv> &type1Mean,
                                          VMatrix<Tv> &type1Var,
                                          VMatrix<Ti> &extra) {
  Ti ycount;
  Ti num_exo = InnerIndices.size();

  Source.Update(&ColIndices, nullptr); // update indexes

  if (this->pOptions->RequestCancel)
    return "";

  Ti s = 0;
  ycount = Source.End - Source.Start + 1;
  Y.SetData(&work[s], this->NumPartitions, ycount);
  s += this->NumPartitions * ycount;
  Source.pData->GetSub(Source.Start, ycount, this->CurrentIndices.Vec, false, Y,
                       0, 0, false);

  if (this->pOptions->RequestCancel)
    return "";

  if (num_exo > 0) { // get out of sample data too
    auto xcount = Source.pData->ColsCount - Source.Start;
    X.SetData(&work[s], num_exo, xcount);

    s += num_exo * xcount;
    Source.pData->GetSub(Source.Start, xcount, InnerIndices, false, X, 0, 0,
                         false);
  }

  if (this->pOptions->RequestCancel)
    return "";

  auto R = Restriction.R;
  // auto r = Restriction.r;

  auto S_e = &work[s];
  s += DModel.Result.StorageSize;
  auto S_p = &work[s];
  s += FModel.StorageSize;
  auto S_s = &work[s];
  s += Model.StorageSize;
  auto W = &work[s];
  if (this->pChecks->Estimation) {

    if (this->pChecks) {
      if (this->pChecks->MinObsCount > 0 && this->pChecks->MinObsCount > ycount)
        throw LdtException(ErrorType::kLogic, "varma-modelset",
                           "model check: minimum no. obs");
      if (this->pChecks->MinDof > 0 &&
          this->pChecks->MinDof > ycount - X.RowsCount)
        throw LdtException(ErrorType::kLogic, "varma-modelset",
                           "model check: minimum dof");
    }

    if (this->pOptions->RequestCancel)
      return "";

    DModel.EstimateMl(Y, num_exo > 0 ? &X : nullptr, W, S_e,
                      Restriction.IsRestricted ? &R : nullptr, nullptr, 0,
                      false, StdMultiplier,
                      this->pChecks && this->pChecks->mCheckCN_all
                          ? this->pChecks->MaxConditionNumber
                          : INFINITY);

    /*if (DModel.Result.Optim.Iteration ==
        DModel.Result.Optim.Options.IterationMax)
      throw LdtException(ErrorType::kLogic, "varma-modelset",
                         "maximum number of iteration reached.");*/

    if (this->pChecks) {
      if (this->pChecks->MaxAic < DModel.Result.Aic)
        throw LdtException(ErrorType::kLogic, "varma-modelset",
                           "model check: maximum aic");
      if (this->pChecks->MaxSic < DModel.Result.Sic)
        throw LdtException(ErrorType::kLogic, "varma-modelset",
                           "model check: maximum sic");
      // if (pChecks->MinR2 > DModel.Result.R2)
      //	throw LdtException(ErrorType::kLogic, "varma-modelset", "model
      // check failed: Maximum R2");
    }

    if (this->pChecks->Prediction) {
      try {
        if (this->pOptions->RequestCancel)
          return "";
        FModel.Calculate(DModel, num_exo > 0 ? &X : nullptr, &Y, S_p, W,
                         mMaxHorizonCheck, false);
      } catch (...) {
        throw LdtException(
            ErrorType::kLogic, "varma-modelset",
            std::string("forecast Error: (Exo. count =") +
                std::to_string(X.ColsCount) + std::string("; Obs. count=") +
                std::to_string(this->Sizes.T) + std::string(")"));
        //    Varma::ModelToString(this->Sizes);
        // VectorToCsv(ExoIndexes.Data, ExoIndexes.length());
      }
      if (FModel.Forecast.Any(NAN))
        throw LdtException(ErrorType::kLogic, "varma-modelset",
                           "nan is produced in the prediction matrix");

      // check bounds

      Ti i = -1;
      for (auto &t : this->CurrentIndices.Vec) { // is it a target?
        i++;
        if (t < this->pItems->LengthTargets) {
          Ti j = -1;
          for (auto &h : this->pMetrics->Horizons) {
            j++;

            if (this->pOptions->RequestCancel)
              return "";

            if (pForLowerBounds &&
                FModel.Forecast.Get0(i, h + FModel.StartIndex - 1) <
                    pForLowerBounds->Get0(i, j))
              throw LdtException(ErrorType::kLogic, "varma-modelset",
                                 "model check: prediction lower bound");
            if (pForUpperBounds &&
                FModel.Forecast.Get0(i, h + FModel.StartIndex - 1) >
                    pForUpperBounds->Get0(i, j))
              throw LdtException(ErrorType::kLogic, "varma-modelset",
                                 "model check: prediction upper bound");
          }
        }
      }
    }

    auto ind = this->pMetrics->MetricInIndices.at(GoodnessOfFitType::kAic);
    if (ind >= 0)
      for (Ti t = 0; t < (Ti)this->TargetsPositions.size(); t++)
        metrics.Mat.Set0(ind, t, DModel.Result.Aic);

    ind = this->pMetrics->MetricInIndices.at(GoodnessOfFitType::kSic);
    if (ind >= 0)
      for (Ti t = 0; t < (Ti)this->TargetsPositions.size(); t++)
        metrics.Mat.Set0(ind, t, DModel.Result.Sic);
  }

  if (this->pMetrics->SimFixSize > 0 && this->pMetrics->MetricsOut.size() > 0) {
    auto count0 = Model.mCount;

    if (this->pOptions->RequestCancel)
      return "";

    Model.Calculate(
        S_s, W, Y, this->pOptions->RequestCancel, num_exo > 0 ? &X : nullptr,
        Restriction.IsRestricted ? &R : nullptr, nullptr, UsePreviousEstim,
        this->pChecks && this->pChecks->mCheckCN
            ? this->pChecks->MaxConditionNumber
            : INFINITY,
        StdMultiplier, false, count0 - this->pChecks->MinOutSim,
        this->pData->Lambdas.size() == 0 ? nullptr : &this->pData->Lambdas);

    if (Model.ValidCounts == 0)
      throw LdtException(ErrorType::kLogic, "varma-modelset",
                         "number of valid simulations is 0");

    Ti j = (Ti)this->pMetrics->MetricsIn.size();

    auto ind = this->pMetrics->MetricOutIndices.at(ScoringType::kSign);
    if (ind >= 0)
      for (Ti t = 0; t < (Ti)this->TargetsPositions.size(); t++)
        metrics.Mat.Set0(j + ind, t, Model.ResultAggs.Get0(ind, t));

    ind = this->pMetrics->MetricOutIndices.at(ScoringType::kDirection);
    if (ind >= 0)
      for (Ti t = 0; t < (Ti)this->TargetsPositions.size(); t++)
        metrics.Mat.Set0(j + ind, t, Model.ResultAggs.Get0(ind, t));

    ind = this->pMetrics->MetricOutIndices.at(ScoringType::kMae);
    if (ind >= 0)
      for (Ti t = 0; t < (Ti)this->TargetsPositions.size(); t++)
        metrics.Mat.Set0(j + ind, t, Model.ResultAggs.Get0(ind, t));

    ind = this->pMetrics->MetricOutIndices.at(ScoringType::kMape);
    if (ind >= 0)
      for (Ti t = 0; t < (Ti)this->TargetsPositions.size(); t++)
        metrics.Mat.Set0(j + ind, t, Model.ResultAggs.Get0(ind, t));

    ind = this->pMetrics->MetricOutIndices.at(ScoringType::kRmse);
    if (ind >= 0)
      for (Ti t = 0; t < (Ti)this->TargetsPositions.size(); t++)
        metrics.Mat.Set0(j + ind, t, Model.ResultAggs.Get0(ind, t));

    ind = this->pMetrics->MetricOutIndices.at(ScoringType::kRmspe);
    if (ind >= 0)
      for (Ti t = 0; t < (Ti)this->TargetsPositions.size(); t++)
        metrics.Mat.Set0(j + ind, t, Model.ResultAggs.Get0(ind, t));

    ind = this->pMetrics->MetricOutIndices.at(ScoringType::kCrps);
    if (ind >= 0)
      for (Ti t = 0; t < (Ti)this->TargetsPositions.size(); t++)
        metrics.Mat.Set0(j + ind, t, Model.ResultAggs.Get0(ind, t));
  }

  if (this->pOptions->RequestCancel)
    return "";

  // Update Type1 values:
  Params.Mat.CopyTo00(extra.Mat);

  if (FModel.WorkSize > 0 && this->pItems->Length1 > 0) {
    for (Ti t = 0; t < (Ti)this->TargetsPositions.size(); t++) {

      // length1 is the max horizon
      for (Ti i = 0; i < this->pItems->Length1; i++) {

        type1Mean.Mat.Set(i, t, FModel.Forecast.Get(t, i + FModel.StartIndex));
        type1Var.Mat.Set(i, t, FModel.Variance.Get(t, i + FModel.StartIndex));
      }
    }
  }

  return "";
}

// #pragma endregion

// #pragma region Modelset

VarmaModelset::VarmaModelset(const SearchData &data,
                             const SearchCombinations &combinations,
                             SearchOptions &options, SearchItems &items,
                             SearchMetricOptions &metrics,
                             SearchModelChecks &checks, DatasetTs<true> &source,
                             std::vector<Ti> varmaMaxParameters6,
                             Ti seasonCount, bool usePreviousEstim,
                             LimitedMemoryBfgsbOptions *optimOptions,
                             Tv stdMultiplier, Ti maxHorizonCheck) {
  metrics.Update(false, true);
  checks.Update(metrics);
  items.Update(metrics, items.LengthTargets, items.LengthEndogenous,
               items.LengthExogenous);

  // items.Length1 is the forecast horizon in 'metrics.Type1'
  if (items.Length1 > 0 && checks.Prediction == false)
    throw LdtException(ErrorType::kLogic, "varma-modelset",
                       "'Length1' is the forecast horizon. Set "
                       "'checks.Prediction=true' when "
                       "it is positive");

  // check group indexes and create sizes array
  for (auto const &b : combinations.Partitions) {
    for (auto &a : b) {
      if (a > items.LengthEndogenous)
        throw LdtException(ErrorType::kLogic, "varma-modelset",
                           "invalid endogenous group element (it is larger "
                           "than the number "
                           "of available endogenous variables)");
      if (a < 0)
        throw LdtException(ErrorType::kLogic, "varma-modelset",
                           "invalid exogenous group element (it is negative)");
    }
  }

  if (varmaMaxParameters6.size() != 6)
    throw LdtException(ErrorType::kLogic, "varma-modelset",
                       "length of varma parameters must be 6");
  for (auto i : varmaMaxParameters6)
    if (i < 0)
      throw LdtException(ErrorType::kLogic, "varma-modelset",
                         "invalid varma parameter");

  ExoIndexes = combinations.InnerGroups;
  if (ExoIndexes.size() == 0)
    ExoIndexes.resize(1); // add empty for loop

  auto T = source.Ranges.at(0).EndIndex +
           1; // The first target determines the number of observations
              // (this is required for out-of-sample exogenous data)

  bool hasBounds = checks.Prediction && checks.PredictionBoundMultiplier > 0;
  if (metrics.MetricsOut.size() != 0 && hasBounds) {
    ForecastLowers =
        Matrix<Tv>(new Tv[items.LengthTargets * (Ti)metrics.Horizons.size()],
                   items.LengthTargets, metrics.Horizons.size());
    ForecastUppers =
        Matrix<Tv>(new Tv[items.LengthTargets * (Ti)metrics.Horizons.size()],
                   items.LengthTargets, metrics.Horizons.size());
    for (Ti i = 0; i < items.LengthTargets; i++) {
      auto last = source.pData->Get0(i, T - 1);
      Tv g = 0;
      for (Ti j = 1; j < T; j++)
        g += source.pData->Get0(i, j) - source.pData->Get0(i, j - 1);
      g /= T - 1;
      g = std::abs(checks.PredictionBoundMultiplier * g);
      Ti j = -1;
      for (auto &h : metrics.Horizons) {
        j++;
        ForecastLowers.Set0(i, j, last - h * g);
        ForecastUppers.Set0(i, j, last + h * g);
      }
    }
  }

  for (auto const &s : combinations.Sizes) {
    if (s <= 0)
      throw LdtException(
          ErrorType::kLogic, "varma-modelset",
          "invalid model size (zero or negative). Make sure array is "
          "initialized properly");

    for (auto p = 0; p <= varmaMaxParameters6.at(0); p++) {
      for (auto d = 0; d <= varmaMaxParameters6.at(1); d++) {
        for (auto q = 0; q <= varmaMaxParameters6.at(2); q++) {
          for (auto P = 0; P <= varmaMaxParameters6.at(3); P++) {
            for (auto D = 0; D <= varmaMaxParameters6.at(4); D++) {
              for (auto Q = 0; Q <= varmaMaxParameters6.at(5); Q++) {
                if (p == 0 && q == 0 && P == 0 && Q == 0)
                  continue;

                for (auto &exo : ExoIndexes) {
                  // TODO: generate restrictions for Searchers

                  auto vsizes = VarmaSizes(T, s, (Ti)exo.size(), p, d, q, P, D,
                                           Q, seasonCount, true);

                  auto se = new VarmaSearcher(
                      data, combinations, options, items, metrics, checks, s,
                      source, vsizes, exo,
                      hasBounds ? &ForecastLowers : nullptr,
                      hasBounds ? &ForecastUppers : nullptr, optimOptions,
                      stdMultiplier, usePreviousEstim, maxHorizonCheck);
                  Searchers.push_back(se);
                }
              }
            }
          }
        }
      }
    }
  }

  this->Modelset =
      ModelSet(Searchers, data, combinations, options, items, metrics, checks);
}

// #pragma endregion
