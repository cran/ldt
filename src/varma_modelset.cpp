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
    SearchOptions &searchOptions, const SearchItems &searchItems,
    const SearchMeasureOptions &measures, const SearchModelChecks &checks,
    Ti sizeG, const std::vector<std::vector<Ti>> &groupIndexMap,
    const std::vector<Ti> &groupSizes, Ti fixFirstG, DatasetTs<true> &source,
    const VarmaSizes sizes, const std::vector<Ti> &exoIndexes,
    Matrix<Tv> *forLowerBounds, Matrix<Tv> *forUpperBounds,
    LimitedMemoryBfgsbOptions *optimOptions, Tv stdMultiplier,
    bool usePreviousEstim, Ti maxHorizonCheck)
    : Searcher::Searcher(searchOptions, searchItems, measures, checks, sizeG,
                         groupIndexMap, groupSizes, fixFirstG) {
  Source = source; // copy for parallel (It uses the indexes)
  pExoIndexes = &exoIndexes;

  UsePreviousEstim = usePreviousEstim;
  StdMultiplier = stdMultiplier;
  mMaxHorizonCheck = maxHorizonCheck;

  pForLowerBounds = forLowerBounds;
  pForUpperBounds = forUpperBounds;

  Sizes = sizes; //  copy
  Params = Matrix<Ti>(new Ti[7]{Sizes.ArP, Sizes.ArD, Sizes.ArQ, Sizes.MaP,
                                Sizes.MaD, Sizes.MaQ, Sizes.SeasonsCount},
                      (Ti)7, 1);
  if (exoIndexes.size() > 0) {
    ExoIndexes =
        Matrix<Ti>(new Ti[exoIndexes.size()], (Ti)exoIndexes.size(), 1);
    for (Ti i = 0; i < (Ti)exoIndexes.size(); i++)
      ExoIndexes.Data[i] = exoIndexes.at(i);
  }

  // we will estimate a complete model even in simulation scenario (ignores
  // simulation if there is an error in estimating the whole model)
  if (checks.Estimation) {
    DModel = Varma(Sizes, true, true, true, optimOptions);
    if (checks.Prediction) {
      if (maxHorizonCheck <= 0)
        throw std::logic_error("Invalid horizon in checking the predictions");
      FModel =
          VarmaForecast(Sizes,
                        // this->Sizes
                        maxHorizonCheck, // length 1 is the horizon for testing
                        true);
    }
  }

  if (measures.SimFixSize > 0) { // we estimate a simulation model
    if (measures.MeasuresOut.size() == 0)
      throw std::logic_error(
          "Simulation is requested by there is no evaluation measure.");
    Model = VarmaSimulation(Sizes, measures.SimFixSize, measures.Horizons,
                            measures.MeasuresOut,
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

  this->WorkSize += source.pData->ColsCount * sizeG; // for copying the matrices
  if (Sizes.ExoCount > 0)
    this->WorkSize += source.pData->ColsCount * (Ti)exoIndexes.size();

  Indexes = std::vector<Ti>(sizeG + exoIndexes.size());
  Ti i = -1;
  for (auto a : exoIndexes) {
    i++;
    Indexes.at(sizeG + i) = a;
  }

  auto numMeas = (Ti)(this->pMeasures->MeasuresOut.size() +
                      this->pMeasures->MeasuresIn.size());
  this->WorkSize += numMeas * this->pItems->LengthTargets; // weights matrix
}

VarmaSearcher::~VarmaSearcher() {
  delete[] Params.Data;
  delete[] ExoIndexes.Data;
}

std::string VarmaSearcher::EstimateOne(Tv *work, Ti *workI) {

  if (this->CurrentIndices.Data[0] >=
      this->pItems->LengthTargets) // TODO: It should be implemented in the base
                                   // Searcher as a constrain on first Index
    return "";

  Ti ycount;
  bool hasExo;
  auto measures = *this->pMeasures;
  std::vector<Ti> targetPositions;
  Matrix<Tv> weights;
  Ti numMeas;
  Ti s = 0;
  Ti i, j, t;

  for (i = 0; i < this->SizeG; i++) { // update indexes and target positions
    j = this->CurrentIndices.Data[i];
    Indexes.at(i) = j;
    if (j < this->pItems->LengthTargets)
      targetPositions.push_back(j);
  }
  hasExo = ExoIndexes.RowsCount > 0;

  numMeas = (Ti)(this->pMeasures->MeasuresOut.size() +
                 this->pMeasures->MeasuresIn.size());
  weights = Matrix<Tv>(NAN, &work[s], numMeas, (Ti)targetPositions.size());
  s += numMeas * this->pItems->LengthTargets;
  Source.Update(&Indexes, nullptr); // update indexes

  if (this->pOptions->RequestCancel)
    return "";

  ycount = Source.End - Source.Start + 1;
  Y.SetData(&work[s], this->SizeG, ycount);
  s += this->SizeG * ycount;
  Source.pData->GetSub(Source.Start, ycount, this->CurrentIndicesV, false, Y, 0,
                       0, false);

  if (this->pOptions->RequestCancel)
    return "";

  if (hasExo) { // get out of sample data too
    auto xcount = Source.pData->ColsCount - Source.Start;
    X.SetData(&work[s], ExoIndexes.RowsCount, xcount);
    s += ExoIndexes.RowsCount * xcount;
    Source.pData->GetSub(Source.Start, xcount, *pExoIndexes, false, X, 0, 0,
                         false);
  }

  if (this->pOptions->RequestCancel)
    return "";

  auto R = Restriction.R;
  // auto r = Restriction.r;
  Tv weight;

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
        throw std::logic_error(
            "Model check failed: Minimum number of observations");
      if (this->pChecks->MinDof > 0 &&
          this->pChecks->MinDof > ycount - X.RowsCount)
        throw std::logic_error(
            "Model check failed: Minimum number of degrees of "
            "freedom");
    }

    if (this->pOptions->RequestCancel)
      return "";

    DModel.EstimateMl(Y, hasExo ? &X : nullptr, W, S_e,
                      Restriction.IsRestricted ? &R : nullptr, nullptr, 0,
                      false, StdMultiplier,
                      this->pChecks && this->pChecks->mCheckCN_all
                          ? this->pChecks->MaxConditionNumber
                          : INFINITY);

    if (this->pChecks) {
      if (this->pChecks->MaxAic < DModel.Result.Aic)
        throw std::logic_error("Model check failed: Maximum Aic");
      if (this->pChecks->MaxSic < DModel.Result.Sic)
        throw std::logic_error("Model check failed: Maximum Sic");
      // if (pChecks->MinR2 > DModel.Result.R2)
      //	throw std::logic_error("Model check failed: Maximum R2");
    }

    if (this->pChecks->Prediction) {
      try {
        if (this->pOptions->RequestCancel)
          return "";
        FModel.Calculate(DModel, hasExo ? &X : nullptr, &Y, S_p, W,
                         mMaxHorizonCheck, false);
      } catch (...) {
        throw std::logic_error(
            std::string("Forecast Error: (Exo. count =") +
            std::to_string(X.ColsCount) + std::string("; Obs. count=") +
            std::to_string(this->Sizes.T) + std::string(")"));
        //    Varma::ModelToString(this->Sizes);
        // VectorToCsv(ExoIndexes.Data, ExoIndexes.length());
      }
      if (FModel.Forecast.Any(NAN))
        throw std::logic_error("nan is produced in the prediction matrix");

      // check bounds

      i = -1;
      for (auto &t : this->CurrentIndicesV) { // is it a target?
        i++;
        if (t < this->pItems->LengthTargets) {
          j = -1;
          for (auto &h : this->pMeasures->Horizons) {
            j++;

            if (this->pOptions->RequestCancel)
              return "";

            if (pForLowerBounds &&
                FModel.Forecast.Get0(i, h + FModel.StartIndex - 1) <
                    pForLowerBounds->Get0(i, j))
              throw std::logic_error(
                  "Model check failed: Prediction Lower Bound");
            if (pForUpperBounds &&
                FModel.Forecast.Get0(i, h + FModel.StartIndex - 1) >
                    pForUpperBounds->Get0(i, j))
              throw std::logic_error(
                  "Model check failed: Prediction Upper Bound");
          }
        }
      }
    }

    if (measures.mIndexOfAic >= 0) {
      weight =
          GoodnessOfFit::ToWeight(GoodnessOfFitType::kAic, DModel.Result.Aic);
      for (t = 0; t < (Ti)targetPositions.size(); t++)
        weights.Set(measures.mIndexOfAic, t, weight);
    }
    if (measures.mIndexOfSic >= 0) {
      weight =
          GoodnessOfFit::ToWeight(GoodnessOfFitType::kSic, DModel.Result.Sic);
      for (t = 0; t < (Ti)targetPositions.size(); t++)
        weights.Set(measures.mIndexOfSic, t, weight);
    }
  }

  if (this->pMeasures->SimFixSize > 0) {
    auto count0 = Model.mCount;

    if (this->pOptions->RequestCancel)
      return "";

    Model.Calculate(
        S_s, W, Y, this->pOptions->RequestCancel, hasExo ? &X : nullptr,
        Restriction.IsRestricted ? &R : nullptr, nullptr, UsePreviousEstim,
        this->pChecks && this->pChecks->mCheckCN
            ? this->pChecks->MaxConditionNumber
            : INFINITY,
        StdMultiplier, false, count0 - this->pChecks->MinOutSim);

    if (Model.ValidCounts == 0)
      throw std::logic_error("Number of valid simulations is 0.");

    j = (Ti)this->pMeasures->MeasuresIn.size();
    for (t = 0; t < (Ti)targetPositions.size(); t++) {
      if (measures.mIndexOfSign >= 0)
        weights.Set(
            j + measures.mIndexOfSign, t,
            Scoring::ToWeight(ScoringType::kSign,
                              Model.ResultAggs.Get0(measures.mIndexOfSign, t)));
      if (measures.mIndexOfDirection >= 0)
        weights.Set(j + measures.mIndexOfDirection, t,
                    Scoring::ToWeight(
                        ScoringType::kDirection,
                        Model.ResultAggs.Get0(measures.mIndexOfDirection, t)));
      if (measures.mIndexOfMae >= 0)
        weights.Set(
            j + measures.mIndexOfMae, t,
            Scoring::ToWeight(ScoringType::kMae,
                              Model.ResultAggs.Get0(measures.mIndexOfMae, t)));
      if (measures.mIndexOfMaeSc >= 0)
        weights.Set(j + measures.mIndexOfMaeSc, t,
                    Scoring::ToWeight(
                        ScoringType::kScaledMae,
                        Model.ResultAggs.Get0(measures.mIndexOfMaeSc, t)));
      if (measures.mIndexOfRmse >= 0)
        weights.Set(
            j + measures.mIndexOfRmse, t,
            Scoring::ToWeight(ScoringType::kRmse,
                              Model.ResultAggs.Get0(measures.mIndexOfRmse, t)));
      if (measures.mIndexOfRmseSc >= 0)
        weights.Set(j + measures.mIndexOfRmseSc, t,
                    Scoring::ToWeight(
                        ScoringType::kScaledRmse,
                        Model.ResultAggs.Get0(measures.mIndexOfRmseSc, t)));
      if (measures.mIndexOfCrps >= 0)
        weights.Set(
            j + measures.mIndexOfCrps, t,
            Scoring::ToWeight(ScoringType::kCrps,
                              Model.ResultAggs.Get0(measures.mIndexOfCrps, t)));
    }
  }

  if (this->pOptions->RequestCancel)
    return "";

  bool allNan = true;
  for (i = 0; i < weights.RowsCount; i++) { // evaluation index
    for (j = 0; j < weights.ColsCount;
         j++) { // target index (use TargetsPositions to interpret)
      weight = weights.Get0(i, j);
      if (std::isnan(weight))
        continue;

      allNan = false;

      if (this->pItems->KeepModelEvaluations) // Add Model evaluation
      {
        auto ek = new EstimationKeep(weight, &ExoIndexes, &Params,
                                     &this->CurrentIndices);
        this->Push0(*ek, i, targetPositions.at(j));
      }

      if (this->pItems->Length1 > 0) { // Add predictions

        for (Ti t = 0; t < this->pItems->Length1;
             t++) { // length1 is the max horizon

          if (t + Model.Forecast.StartIndex >= FModel.Forecast.ColsCount)
            throw std::logic_error("Not enough number of forecasts.");

          auto ek = new EstimationKeep(
              weight, &ExoIndexes, &Params, &this->CurrentIndices,
              FModel.Forecast.Get(j, t + Model.Forecast.StartIndex),
              FModel.Variance.Get(j, t + Model.Forecast.StartIndex));
          this->Push1(*ek, i, targetPositions.at(j), t);
        }
      }

      if (this->pItems->Length2 > 0) { // TODO: for saving IRF
      }
    }
  }

  if (this->pOptions->RequestCancel)
    return "";

  if (allNan) {
    auto sg = std::unique_ptr<Tv[]>(new Tv[Y.RowsCount]);
    auto sgm = Matrix<Tv>(sg.get(), Y.RowsCount, 1);
    Y.Transpose();
    Y.ColumnsMeans(sgm, false);
    throw std::string("All weights are NaN: ") + Varma::ModelToString(Sizes);
    // VectorToCsv(this->CurrentIndicesV) + std::string(" ; ") +
    // VectorToCsv(sgm.Data, Y.ColsCount);  // weights are all nan
  }

  return "";
}

// #pragma endregion

// #pragma region Modelset

VarmaModelset::VarmaModelset(
    SearchOptions &searchOptions, SearchItems &searchItems,
    SearchMeasureOptions &measures, SearchModelChecks &checks,
    const std::vector<Ti> &sizes, std::vector<std::vector<Ti>> &groupIndexMap,
    DatasetTs<true> &source, std::vector<Ti> varmaMaxParameters6,
    Ti seasonCount, const std::vector<std::vector<Ti>> &exoIndexes,
    bool usePreviousEstim, LimitedMemoryBfgsbOptions *optimOptions,
    Tv stdMultiplier, Ti maxHorizonCheck) {
  measures.Update(false, true);
  checks.Update(measures);
  searchItems.Update(measures, searchItems.LengthTargets,
                     searchItems.LengthDependents,
                     searchItems.LengthExogenouses);

  // searchItems.Length1 is the forecast horizon in 'measures.Type1'
  if (searchItems.Length1 > 0 && checks.Prediction == false)
    throw std::logic_error(
        "Length1 is the forecast horizon. Set 'checks.Prediction=true' when "
        "it is positive.");

  // check group indexes and create sizes array
  for (auto const &b : groupIndexMap) {
    for (auto &a : b) {
      if (a > searchItems.LengthDependents)
        throw std::logic_error(
            "Invalid endogenous group element (it is larger than the number "
            "of available endogenous variables).");
      if (a < 0)
        throw std::logic_error(
            "Invalid exogenous group element (it is negative).");
    }
    GroupSizes.push_back((Ti)b.size());
  }

  if (varmaMaxParameters6.size() != 6)
    throw std::logic_error("length of varma parameters must be 6");
  for (auto i : varmaMaxParameters6)
    if (i < 0)
      throw std::logic_error("invalid varma parameter");
  if (seasonCount < 2) {
    varmaMaxParameters6.at(3) = 0;
    varmaMaxParameters6.at(4) = 0;
    varmaMaxParameters6.at(5) = 0;
  }
  ExoIndexes = exoIndexes;
  if (ExoIndexes.size() == 0)
    ExoIndexes.resize(1); // add empty for loop

  auto T = source.Ranges.at(0).EndIndex +
           1; // The first target determines the number of observations (this
              // is required for out-of-sample exogenous data)

  bool hasBounds = checks.PredictionBoundMultiplier > 0;
  if (measures.MeasuresOut.size() != 0) {
    if (hasBounds && checks.Prediction == false)
      throw std::logic_error(
          "Forecast bounds are given but 'forecast check' is false.");
    if (checks.PredictionBoundMultiplier < 0)
      throw std::logic_error("invalid forecast bound multiplier");
    else { // create matrixes
      ForecastLowers = Matrix<Tv>(
          new Tv[searchItems.LengthTargets * (Ti)measures.Horizons.size()],
          searchItems.LengthTargets, measures.Horizons.size());
      ForecastUppers = Matrix<Tv>(
          new Tv[searchItems.LengthTargets * (Ti)measures.Horizons.size()],
          searchItems.LengthTargets, measures.Horizons.size());
      for (Ti i = 0; i < searchItems.LengthTargets; i++) {
        auto last = source.pData->Get0(i, T - 1);
        Tv g = 0;
        for (Ti j = 1; j < T; j++)
          g += source.pData->Get0(i, j) - source.pData->Get0(i, j - 1);
        g /= T - 1;
        g = std::abs(checks.PredictionBoundMultiplier * g);
        Ti j = -1;
        for (auto &h : measures.Horizons) {
          j++;
          ForecastLowers.Set0(i, j, last - h * g);
          ForecastUppers.Set0(i, j, last + h * g);
        }
      }
    }
  }

  for (auto const &s : sizes) {
    if (s <= 0)
      throw std::logic_error(
          "Invalid model size (zero or negative). Make sure array is "
          "initialized properly.");

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
                      searchOptions, searchItems, measures, checks, s,
                      groupIndexMap, GroupSizes, 0, source, vsizes, exo,
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

  this->Modelset = ModelSet(Searchers, groupIndexMap, GroupSizes, searchOptions,
                            searchItems, measures, checks);
}

// #pragma endregion
