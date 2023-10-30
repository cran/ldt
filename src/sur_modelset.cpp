/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the GPL 3.0.
 See accompanying file LICENSE
*/

#include "clustering.h"
#include "sur.h"

using namespace ldt;

// #pragma region Searcher

SurSearcher::SurSearcher(const SearchData &data,
                         const SearchCombinations &combinations,
                         SearchOptions &options, const SearchItems &items,
                         const SearchMetricOptions &metrics,
                         const SearchModelChecks &checks,
                         const Ti &numPartitions,
                         const std::vector<Ti> &endoIndexes,
                         const Matrix<Tv> &source, const Ti &sigSearchMaxIter,
                         const Tv &sigSearchMaxProb, const unsigned int &seed)
    : SearcherReg::SearcherReg(data, combinations, options, items, metrics,
                               checks, numPartitions, false, endoIndexes, 0),
      Seed(seed), SigSearchMaxIter(sigSearchMaxIter),
      SigSearchMaxProb(sigSearchMaxProb) {

  Ti num_endo = this->InnerIndices.size();
  Ti num_exo = numPartitions;
  pSource = &source;

  this->WorkSize = 0;
  bool isRestricted = SigSearchMaxIter > 0 ? true : false;

  if (this->pChecks->Estimation)
    DModel = SurExtended(
        (Ti)source.RowsCount, num_endo, num_exo, isRestricted, false, true, 0,
        SigSearchMaxIter, false, nullptr,
        nullptr); // do details because we might need to keep variance of a
                  // coefficient (TODO: can be more efficient)

  if (metrics.SimFixSize > 0 &&
      metrics.MetricsOut.size() > 0) // we estimate a simulation model
    Model =
        SurSimulation(source.RowsCount, num_endo, num_exo, metrics.TrainRatio,
                      metrics.TrainFixSize, metrics.MetricsOut, isRestricted,
                      SigSearchMaxIter, nullptr, nullptr); // no PCA in search

  this->WorkSize += DModel.StorageSize + Model.StorageSize +
                    std::max(DModel.WorkSize,
                             Model.WorkSize); // don't share storage size. we
                                              // will use DModel.beta, etc.

  Data = Dataset<Tv>(source.RowsCount, num_exo + num_endo, true, true);
  this->WorkSize += Data.StorageSize;

  Ti km = num_exo * num_endo;
  if (SigSearchMaxProb > 0) {
    R_d = std::make_unique<Tv[]>(km * km);
    R = Matrix<Tv>(R_d.get(), km, km);
  }
}

std::string SurSearcher::EstimateOneReg(Tv *work, Ti *workI,
                                        VMatrix<Tv> &metrics,
                                        VMatrix<Tv> &type1Mean,
                                        VMatrix<Tv> &type1Var,
                                        VMatrix<Ti> &extra) {

  Ti s = 0;
  Data.Calculate(*pSource, &ColIndices, &work[s]);
  s += Data.StorageSize;

  if (this->pChecks->Estimation) { // estimate with all data
    DModel.Calculate(Data.Result, InnerIndices.size(), &work[s],
                     &work[s + DModel.StorageSize],
                     SigSearchMaxIter > 0 ? &R : nullptr, SigSearchMaxProb,
                     nullptr, this->pChecks);
    s += DModel.StorageSize;
    // N, Dof, Cn, Aic, Sic, R2   -> are all checked in calculate

    auto ind = this->pMetrics->MetricInIndices.at(GoodnessOfFitType::kAic);
    if (ind >= 0)
      for (Ti t = 0; t < (Ti)this->TargetsPositions.size(); t++)
        metrics.Mat.Set0(ind, t, DModel.Model.Aic);

    ind = this->pMetrics->MetricInIndices.at(GoodnessOfFitType::kSic);
    if (ind >= 0)
      for (Ti t = 0; t < (Ti)this->TargetsPositions.size(); t++)
        metrics.Mat.Set0(ind, t, DModel.Model.Sic);
  }

  if (this->pOptions->RequestCancel)
    return "";

  if (Model.WorkSize > 0) { // otherwise, not initialized
    Model.Calculate(
        Data.Result, InnerIndices.size(), &work[s],
        &work[s + Model.StorageSize], SigSearchMaxIter > 0 ? &R : nullptr,
        this->pOptions->RequestCancel, this->pMetrics->SimFixSize, Seed,
        SigSearchMaxProb,
        this->pChecks->mCheckCN ? this->pChecks->MaxConditionNumber : INFINITY,
        this->pMetrics->SimFixSize - this->pChecks->MinOutSim,
        this->pData->Lambdas.size() == 0 ? nullptr : &this->pData->Lambdas);

    Ti j = (Ti)this->pMetrics->MetricsIn.size();

    auto ind = this->pMetrics->MetricOutIndices.at(ScoringType::kSign);
    if (ind >= 0)
      for (Ti t = 0; t < (Ti)this->TargetsPositions.size(); t++)
        metrics.Mat.Set0(j + ind, t, Model.Results.Get0(ind, t));

    ind = this->pMetrics->MetricOutIndices.at(ScoringType::kMae);
    if (ind >= 0)
      for (Ti t = 0; t < (Ti)this->TargetsPositions.size(); t++)
        metrics.Mat.Set0(j + ind, t, Model.Results.Get0(ind, t));

    ind = this->pMetrics->MetricOutIndices.at(ScoringType::kMape);
    if (ind >= 0)
      for (Ti t = 0; t < (Ti)this->TargetsPositions.size(); t++)
        metrics.Mat.Set0(j + ind, t, Model.Results.Get0(ind, t));

    ind = this->pMetrics->MetricOutIndices.at(ScoringType::kRmse);
    if (ind >= 0)
      for (Ti t = 0; t < (Ti)this->TargetsPositions.size(); t++)
        metrics.Mat.Set0(j + ind, t, Model.Results.Get0(ind, t));

    ind = this->pMetrics->MetricOutIndices.at(ScoringType::kRmspe);
    if (ind >= 0)
      for (Ti t = 0; t < (Ti)this->TargetsPositions.size(); t++)
        metrics.Mat.Set0(j + ind, t, Model.Results.Get0(ind, t));

    ind = this->pMetrics->MetricOutIndices.at(ScoringType::kCrps);
    if (ind >= 0)
      for (Ti t = 0; t < (Ti)this->TargetsPositions.size(); t++)
        metrics.Mat.Set0(j + ind, t, Model.Results.Get0(ind, t));
  }

  if (this->pOptions->RequestCancel)
    return "";

  // Update Type1 values:
  // no extra
  if (DModel.WorkSize > 0 && this->pItems->Length1 > 0) {
    for (Ti t = 0; t < (Ti)this->TargetsPositions.size(); t++) {
      // we should skip those coefficients that are not present in the current
      // estimation.
      Ti i = -1;
      for (const auto &b : this->CurrentIndices.Vec) {
        i++;
        Ti a = b - this->pData->NumEndo;
        type1Mean.Mat.Set0(a, t, DModel.Model.beta.Get0(i, t));
        type1Var.Mat.Set0(a, t,
                          std::pow(DModel.Model.e_beta_std.Get0(i, t), 2));
      }
    }
  }

  return "";
}

// #pragma endregion

// #pragma region Modelset

SurModelset::SurModelset(const SearchData &data,
                         const SearchCombinations &combinations,
                         SearchOptions &options, SearchItems &items,
                         SearchMetricOptions &metrics,
                         SearchModelChecks &checks, Ti sigSearchMaxIter,
                         Tv sigSearchMaxProb) {
  metrics.Update(true, false);
  checks.Update(metrics);
  items.Update(metrics, items.LengthTargets, items.LengthEndogenous,
               items.LengthExogenous);

  // check items.Length1 with the number of exogenous variables ?!
  if (items.Length1 != 0 && items.Length1 != items.LengthExogenous)
    throw LdtException(ErrorType::kLogic, "sur-modelset",
                       "inconsistent number of exogenous variables");
  if (items.Length1 != 0 && checks.Estimation == false)
    throw LdtException(ErrorType::kLogic, "sur-modelset",
                       "parameters are needed. Set 'checks.Estimation = true'");

  // check group indexes and create sizes array
  for (auto const &b : combinations.Partitions) {
    for (auto &a : b) {
      if (a < items.LengthEndogenous)
        throw LdtException(ErrorType::kLogic, "sur-modelset",
                           "invalid exogenous group element (it is less that "
                           "the index of the first exogenous variable)");
      if (a >= items.LengthEndogenous + items.LengthExogenous)
        throw LdtException(
            ErrorType::kLogic, "sur-modelset",
            "invalid exogenous group element (it is larger than the number "
            "of available exogenous variables)");
      if (a < 0)
        throw LdtException(ErrorType::kLogic, "sur-modelset",
                           "invalid exogenous group element (it is negative)");
    }
  }

  unsigned int co = 0;
  for (auto const &s : combinations.Sizes) {
    if (s <= 0)
      throw LdtException(
          ErrorType::kLogic, "sur-modelset",
          "invalid exogenous size (zero or negative). Make sure array is "
          "initialized properly");
    if (combinations.NumFixPartitions > s)
      continue;
    for (auto &e : combinations.InnerGroups) {
      if (e.size() == 0)
        throw LdtException(ErrorType::kLogic, "sur-modelset",
                           "empty endogenous indexes");

      if (e.at(0) > items.LengthTargets)
        continue; // no target here
      auto seed = metrics.Seed == 0
                      ? 0
                      : (unsigned int)(metrics.Seed < 0 ? -metrics.Seed
                                                        : (metrics.Seed + co));
      co++;
      auto se = new SurSearcher(data, combinations, options, items, metrics,
                                checks, s, e, data.Data, sigSearchMaxIter,
                                sigSearchMaxProb, seed);
      Searchers.push_back(se);
    }
  }

  this->Modelset =
      ModelSet(Searchers, data, combinations, options, items, metrics, checks);
}

// #pragma endregion
