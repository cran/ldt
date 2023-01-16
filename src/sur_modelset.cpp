/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the LGPL 3.0.
 See accompanying file LICENSE
*/

#include "clustering.h"
#include "sur.h"

using namespace ldt;

// #pragma region Searcher

SurSearcher::SurSearcher(SearchOptions &searchOptions,
                         const SearchItems &searchItems,
                         const SearchMeasureOptions &measures,
                         const SearchModelChecks &checks, Ti sizeG,
                         const std::vector<std::vector<Ti>> &groupIndexMap,
                         const std::vector<Ti> &groupSizes, Ti fixFirstG,
                         Matrix<Tv> &source, std::vector<Ti> &endoIndexes,
                         Ti sigSearchMaxIter, Tv sigSearchMaxProb,
                         unsigned int seed)
    : Searcher::Searcher(searchOptions, searchItems, measures, checks, sizeG,
                         groupIndexMap, groupSizes, fixFirstG) {
  Seed = seed;
  pSource = &source; // copy ?!
  EndoIndexes = Matrix<Ti>(&endoIndexes[0], (Ti)endoIndexes.size(), 1);

  SigSearchMaxIter = sigSearchMaxIter;
  SigSearchMaxProb = sigSearchMaxProb;

  this->WorkSize = 0;

  bool isRestricted = SigSearchMaxIter > 0 ? true : false;

  if (this->pChecks->Estimation)
    DModel = SurExtended(
        (Ti)source.RowsCount, (Ti)endoIndexes.size(), sizeG, isRestricted,
        false, true, 0, SigSearchMaxIter, false, nullptr,
        nullptr); // do details because we might need to keep variance of a
                  // coefficient (TODO: can be more efficient)

  if (measures.SimFixSize > 0) // we estimate a simulation model
    Model = SurSimulation(source.RowsCount, (Ti)endoIndexes.size(), sizeG,
                          measures.TrainRatio, measures.TrainFixSize,
                          measures.MeasuresOut, isRestricted, SigSearchMaxIter,
                          nullptr, nullptr); // no PCA in search

  this->WorkSize += DModel.StorageSize + Model.StorageSize +
                    std::max(DModel.WorkSize,
                             Model.WorkSize); // don't share storage size. we
                                              // will use DModel.beta, etc.

  Data =
      Dataset<Tv>(source.RowsCount, sizeG + (Ti)endoIndexes.size(), true, true);
  this->WorkSize += Data.StorageSize;

  Ti km = sizeG * (Ti)endoIndexes.size();
  if (SigSearchMaxProb > 0) {
    R_d = std::unique_ptr<Tv[]>(new Tv[km * km]);
    R = Matrix<Tv>(R_d.get(), km, km);
  }

  Indexes = std::vector<Ti>(sizeG + endoIndexes.size());
  Ti i = -1;
  for (auto a : endoIndexes) {
    i++;
    Indexes.at(i) = a;
    if (a < searchItems.LengthTargets)
      TargetsPositions.push_back(a);
  }
  if (TargetsPositions.size() == 0)
    throw std::logic_error("A searcher with no target is not valid.");

  auto numMeas =
      this->pMeasures->MeasuresOut.size() + this->pMeasures->MeasuresIn.size();
  this->WorkSize += (Ti)(numMeas * TargetsPositions.size()); // weights matrix
}

std::string SurSearcher::EstimateOne(Tv *work, Ti *workI) {
  auto measures = *this->pMeasures;

  Ti i, j = EndoIndexes.RowsCount, t;
  for (i = 0; i < this->SizeG; i++)
    Indexes.at(j++) = this->CurrentIndices.Data[i];

  Ti s = 0;
  auto numMeas = (Ti)(this->pMeasures->MeasuresOut.size() +
                      this->pMeasures->MeasuresIn.size());
  auto weights =
      Matrix<Tv>(NAN, &work[s], numMeas, (Ti)TargetsPositions.size());
  s += (Ti)(numMeas * TargetsPositions.size());
  Data.Calculate(*pSource, &Indexes, &work[s]);
  s += Data.StorageSize;

  Tv weight;
  if (this->pChecks->Estimation) { // estimate with all data
    DModel.Calculate(Data.Result, EndoIndexes.RowsCount, &work[s],
                     &work[s + DModel.StorageSize],
                     SigSearchMaxIter > 0 ? &R : nullptr, SigSearchMaxProb,
                     nullptr, this->pChecks);
    s += DModel.StorageSize;
    // N, Dof, Cn, Aic, Sic, R2   -> are all checked in calculate

    if (measures.mIndexOfAic >= 0) {
      weight =
          GoodnessOfFit::ToWeight(GoodnessOfFitType::kAic, DModel.Model.Aic);
      for (t = 0; t < (Ti)TargetsPositions.size(); t++)
        weights.Set0(measures.mIndexOfAic, t, weight);
    }
    if (measures.mIndexOfSic >= 0) {
      weight =
          GoodnessOfFit::ToWeight(GoodnessOfFitType::kSic, DModel.Model.Sic);
      for (t = 0; t < (Ti)TargetsPositions.size(); t++)
        weights.Set0(measures.mIndexOfSic, t, weight);
    }
  }

  if (Model.WorkSize > 0 /*otherwise, not initialized*/ &&
      this->pMeasures->MeasuresOut.size() > 0) {
    Model.Calculate(
        Data.Result, EndoIndexes.RowsCount, &work[s],
        &work[s + Model.StorageSize], SigSearchMaxIter > 0 ? &R : nullptr,
        this->pOptions->RequestCancel, this->pMeasures->SimFixSize, Seed,
        SigSearchMaxProb,
        this->pChecks->mCheckCN ? this->pChecks->MaxConditionNumber : INFINITY,
        this->pMeasures->SimFixSize - this->pChecks->MinOutSim);

    j = (Ti)this->pMeasures->MeasuresIn.size();
    for (t = 0; t < (Ti)TargetsPositions.size(); t++) {
      if (measures.mIndexOfSign >= 0)
        weights.Set0(
            j + measures.mIndexOfSign, t,
            Scoring::ToWeight(ScoringType::kSign,
                              Model.Results.Get0(measures.mIndexOfSign, t)));
      if (measures.mIndexOfMae >= 0)
        weights.Set0(
            j + measures.mIndexOfMae, t,
            Scoring::ToWeight(ScoringType::kMae,
                              Model.Results.Get0(measures.mIndexOfMae, t)));
      if (measures.mIndexOfMaeSc >= 0)
        weights.Set0(
            j + measures.mIndexOfMaeSc, t,
            Scoring::ToWeight(ScoringType::kScaledMae,
                              Model.Results.Get0(measures.mIndexOfMaeSc, t)));
      if (measures.mIndexOfRmse >= 0)
        weights.Set0(
            j + measures.mIndexOfRmse, t,
            Scoring::ToWeight(ScoringType::kRmse,
                              Model.Results.Get0(measures.mIndexOfRmse, t)));
      if (measures.mIndexOfRmseSc >= 0)
        weights.Set0(
            j + measures.mIndexOfRmseSc, t,
            Scoring::ToWeight(ScoringType::kScaledRmse,
                              Model.Results.Get0(measures.mIndexOfRmseSc, t)));
      if (measures.mIndexOfCrps >= 0)
        weights.Set0(
            j + measures.mIndexOfCrps, t,
            Scoring::ToWeight(ScoringType::kCrps,
                              Model.Results.Get0(measures.mIndexOfCrps, t)));
    }
  }

  bool allNan = true;
  for (i = 0; i < weights.RowsCount; i++) { // evaluation index
    for (j = 0; j < weights.ColsCount;
         j++) { // target index (use TargetsPositions to interpret)
      weight = weights.Get(i, j);
      if (std::isnan(weight))
        continue;

      // TODO: add significant search information
      allNan = false;

      if (this->pItems->KeepModelEvaluations) // Add Model evaluation
      {
        auto ek = new EstimationKeep(weight, &this->CurrentIndices, nullptr,
                                     &EndoIndexes);
        this->Push0(*ek, i, TargetsPositions.at(j));
      }

      if (this->pItems->Length1 > 0) { // Add coefficients
        for (t = 0; t < this->SizeG; t++) {
          auto ek = new EstimationKeep(
              weight, &this->CurrentIndices, nullptr, &EndoIndexes,
              DModel.Model.beta.Get(t, j),
              std::pow(DModel.Model.e_beta_std.Get(t, j), 2));
          this->Push1(*ek, i, TargetsPositions.at(j),
                      this->CurrentIndices.Data[t] -
                          this->pItems->LengthDependents);
        }
      }

      if (this->pItems->Length2 > 0) { // TODO: for saving projections
      }
    }
  }
  if (allNan) {
    throw std::logic_error(
        "All weights are NaN"); // +
                                // Sur::ModelToString(DModel.Sizes).c_str();
                                // // weights are all nan
  }

  return "";
}

// #pragma endregion

// #pragma region Modelset

SurModelset::SurModelset(SearchOptions &searchOptions, SearchItems &searchItems,
                         SearchMeasureOptions &measures,
                         SearchModelChecks &checks,
                         const std::vector<Ti> &exoSizes,
                         std::vector<std::vector<Ti>> &groupIndexMap,
                         Ti numFixXPartitions, Matrix<Tv> &source,
                         std::vector<std::vector<Ti>> &endoIndexes,
                         Ti sigSearchMaxIter, Tv sigSearchMaxProb) {
  measures.Update(true, false);
  checks.Update(measures);
  searchItems.Update(measures, searchItems.LengthTargets,
                     searchItems.LengthDependents,
                     searchItems.LengthExogenouses);

  // check searchItems.Length1 with the number of exogenous variables ?!
  if (searchItems.Length1 != 0 &&
      searchItems.Length1 != searchItems.LengthExogenouses)
    throw std::logic_error("Inconsistent number of exogenous variables");
  if (searchItems.Length1 != 0 && checks.Estimation == false)
    throw std::logic_error(
        "Parameters are needed. Set 'checks.Estimation = true'.");

  // check group indexes and create sizes array
  for (auto const &b : groupIndexMap) {
    for (auto &a : b) {
      if (a < searchItems.LengthDependents ||
          a >= searchItems.LengthDependents + searchItems.LengthExogenouses)
        throw std::logic_error(
            "Invalid exogenous group element (it is larger than the number "
            "of available variables).");
      if (a < 0)
        throw std::logic_error(
            "Invalid exogenous group element (it is negative).");
    }
    GroupSizes.push_back((Ti)b.size());
  }

  unsigned int co = 0;
  for (auto const &s : exoSizes) {
    if (s <= 0)
      throw std::logic_error(
          "Invalid exogenous size (zero or negative). Make sure array is "
          "initialized properly.");
    if (numFixXPartitions > s)
      continue;
    for (auto &e : endoIndexes) {
      if (e.size() == 0)
        throw std::logic_error("Invalid endogenous indexes. It is empty.");

      if (e.at(0) > searchItems.LengthTargets)
        continue; // no target here
      auto seed =
          measures.Seed == 0
              ? 0
              : (unsigned int)(measures.Seed < 0 ? -measures.Seed
                                                 : (measures.Seed + co));
      co++;
      auto se =
          new SurSearcher(searchOptions, searchItems, measures, checks, s,
                          groupIndexMap, GroupSizes, numFixXPartitions, source,
                          e, sigSearchMaxIter, sigSearchMaxProb, seed);
      Searchers.push_back(se);
    }
  }

  this->Modelset = ModelSet(Searchers, groupIndexMap, GroupSizes, searchOptions,
                            searchItems, measures, checks);
}

// #pragma endregion
