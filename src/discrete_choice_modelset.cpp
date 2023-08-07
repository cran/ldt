/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the GPL 3.0.
 See accompanying file LICENSE
*/

#include "clustering.h"
#include "discrete_choice.h"

using namespace ldt;

// #pragma region Searcher

template <bool hasWeight, DiscreteChoiceModelType modelType,
          DiscreteChoiceDistType distType>
DiscreteChoiceSearcher<hasWeight, modelType, distType>::DiscreteChoiceSearcher(
    SearchOptions &searchOptions, const SearchItems &searchItems,
    const SearchMetricOptions &metrics, const SearchModelChecks &checks,
    Ti SizeG, const std::vector<std::vector<Ti>> &groupIndexMap, Ti fixFirstG,
    const Matrix<Tv> &source, Ti numChoices,
    const std::vector<Matrix<Tv>> &costMatrixes, unsigned int seed,
    Newton &newtonOptions, RocOptions &aucOptions)
    : Searcher::Searcher(searchOptions, searchItems, metrics, checks, SizeG,
                         groupIndexMap, fixFirstG, 0) {

  pCostMatrixes = &costMatrixes;
  pSource =
      &source; // 1st is endogenous, second is intercept, 3rd (can be) weight
  mNumChoices = numChoices;
  Ti numObs = source.RowsCount;
  Ti numExo = SizeG + 1; // intercept
  Ti cols = numExo + (hasWeight ? 2 : 1);
  Data =
      Dataset<Tv>(numObs, cols, true); // size + endogenous, intercept, weight

  pAucOptions = &aucOptions;

  if (this->pChecks->Estimation) {
    DModel =
        DiscreteChoice<modelType, distType>(numObs, numExo, numChoices, false);
    DModel.Optim.IterationMax = newtonOptions.IterationMax;
    DModel.Optim.TolFunction = newtonOptions.TolFunction;
    DModel.Optim.TolGradient = newtonOptions.TolGradient;
    DModel.Optim.UseLineSearch = newtonOptions.UseLineSearch;
  }

  if (metrics.SimFixSize > 0) { // we estimate a simulation model
    Model = DiscreteChoiceSim<hasWeight, modelType, distType>(
        numObs, cols, this->mNumChoices, metrics.TrainRatio,
        (Ti)metrics.TrainFixSize, (Ti)costMatrixes.size(),
        metrics.mIndexOfBrierOut != -1, metrics.mIndexOfAucOut != -1, false,
        nullptr,
        metrics.WeightedEval); // no PCA in search
    Model.Seed = seed;
    Model.SimulationMax = metrics.SimFixSize;

    Model.Optim.IterationMax = newtonOptions.IterationMax;
    Model.Optim.TolFunction = newtonOptions.TolFunction;
    Model.Optim.TolGradient = newtonOptions.TolGradient;
    Model.Optim.UseLineSearch = newtonOptions.UseLineSearch;
  }
  this->WorkSizeI = Model.WorkSizeI;
  this->WorkSize =
      Data.StorageSize + DModel.StorageSize + Model.StorageSize +
      std::max(
          DModel.WorkSize,
          Model.WorkSize); // don't share storage size. we will use beta, etc.

  ExoIndexes = Matrix<Ti>(this->SizeG + 1, 1);
  this->WorkSizeI += this->SizeG + 1;

  Indexes.push_back(0);
  Indexes.push_back(1);
  if constexpr (hasWeight) {
    Indexes.push_back(2);
  }
  for (Ti i = 0; i < this->SizeG; i++)
    Indexes.push_back(0);

  auto numMeas = (Ti)this->pMetrics->MetricsOut.size() +
                 (Ti)this->pMetrics->MetricsIn.size();
  Weights = Matrix<Tv>(numMeas, 1);
  this->WorkSize += numMeas; // weights matrix

  if (metrics.mIndexOfCostMatrixIn != -1 || metrics.mIndexOfAucIn != -1 ||
      metrics.mIndexOfBrierIn != -1) {
    if (hasWeight && metrics.WeightedEval)
      CostIn = std::unique_ptr<FrequencyCostBase>(
          new FrequencyCost<true>((Ti)costMatrixes.size()));
    else
      CostIn = std::unique_ptr<FrequencyCostBase>(
          new FrequencyCost<false>((Ti)costMatrixes.size()));
    Probs = Matrix<Tv>(numObs, numChoices);
    this->WorkSize +=
        std::max(numObs + numChoices - 2, CostIn.get()->StorageSize) +
        numObs * numChoices;
  }
  if (metrics.mIndexOfAucIn != -1) {

    if (modelType == DiscreteChoiceModelType::kBinary) {
      std::logic_error("not implemented discrete choice model type");
    }

    if (hasWeight && metrics.WeightedEval)
      AucIn = std::unique_ptr<RocBase>(new ROC<true, false>(numObs));
    else
      AucIn = std::unique_ptr<RocBase>(new ROC<false, false>(numObs));
  }
}

template <bool hasWeight, DiscreteChoiceModelType modelType,
          DiscreteChoiceDistType distType>
std::string
DiscreteChoiceSearcher<hasWeight, modelType, distType>::EstimateOne(Tv *work,
                                                                    Ti *workI) {
  auto metrics = *this->pMetrics;

  ExoIndexes.SetData(workI);
  ExoIndexes.Data[0] = 1;
  // update Indexes
  Ti j = 2;
  if constexpr (hasWeight) {
    j = 3;
  }
  for (Ti i = 0; i < this->SizeG; i++) {
    Indexes.at(j + i) = j + this->CurrentIndices.Data[i];
    ExoIndexes.Data[i + 1] = this->CurrentIndices.Data[i] + 2;
  }

  Ti s = 0;
  Weights.SetData(NAN, &work[s]);
  s += Weights.RowsCount;

  Data.Calculate(*pSource, &Indexes, &work[s]);
  s += Data.StorageSize;

  if (this->pOptions->RequestCancel)
    return "";

  Ti count = Data.Result.RowsCount;
  Ti numExo = this->SizeG + 1;
  Tv *d = Data.Result.Data;

  Y.SetData(d, count,
            1); // You cannot set y in the constructor due to NAN existence
  if constexpr (hasWeight) {
    W.SetData(&d[count], count, 1);
    X.SetData(&d[2 * count], count, numExo); // +1 for intercept
  } else if constexpr (true) {
    X.SetData(&d[count], count, numExo); // +1 for intercept
  }

  // there is just one endogenous and one target

  if (this->pChecks->Estimation) {

    if (this->pChecks) {
      if (this->pChecks->MinObsCount > 0 && this->pChecks->MinObsCount > count)
        throw LdtException(ErrorType::kLogic, "dc-modelset",
                           "model check: minimum no. obs");
      if (this->pChecks->MinDof > 0 &&
          this->pChecks->MinDof > count - numExo - this->mNumChoices - 2)
        throw LdtException(ErrorType::kLogic, "dc-modelset",
                           "model check: minimum dof");
    }

    if (this->pOptions->RequestCancel)
      return "";

    DModel.Calculate(Y, X, hasWeight ? &W : nullptr, &work[s],
                     &work[s + DModel.StorageSize], mNumChoices, true);
    s += DModel.StorageSize; // keep model storage

    // 'R2' and 'ConditionNumber' is not implemented

    if (this->pChecks) {
      if (pChecks->mCheckCN_all &&
          this->DModel.condition_number > pChecks->MaxConditionNumber)
        throw LdtException(ErrorType::kLogic, "dc-modelset",
                           "model check: maximum cn");

      if (this->pChecks->MaxAic < this->DModel.Aic)
        throw LdtException(ErrorType::kLogic, "dc-modelset",
                           "model check: maximum aic");
      if (this->pChecks->MaxSic < this->DModel.Sic)
        throw LdtException(ErrorType::kLogic, "dc-modelset",
                           "model check: maximum sic");
      // if (pChecks->MinR2 > Model.r2)
      //	throw LdtException(ErrorType::kLogic, "dc-modelset", "model
      // check failed: Maximum R2");
    }
  }

  if (metrics.SimFixSize > 0) {

    if (this->pOptions->RequestCancel)
      return "";

    Model.Calculate(Data.Result, pCostMatrixes, &work[s],
                    &work[s + Model.StorageSize], &workI[this->SizeG + 1],
                    this->pOptions->RequestCancel, *pAucOptions,
                    this->pMetrics->SimFixSize - this->pChecks->MinOutSim,
                    nullptr, INT32_MAX);
    s += Model.StorageSize;
  }

  // collect metrics:

  if (this->pChecks->Estimation) {
    if (metrics.mIndexOfAic != -1)
      Weights.Set0(
          metrics.mIndexOfAic, 0,
          GoodnessOfFit::ToWeight(GoodnessOfFitType::kAic, DModel.Aic));
    if (metrics.mIndexOfSic != -1)
      Weights.Set0(
          metrics.mIndexOfSic, 0,
          GoodnessOfFit::ToWeight(GoodnessOfFitType::kSic, DModel.Sic));

    if (metrics.mIndexOfCostMatrixIn != -1 || metrics.mIndexOfAucIn != -1 ||
        metrics.mIndexOfBrierIn != -1) {
      Probs.SetData(&work[s]);
      s += Probs.length();
      DModel.GetProbabilities(X, Probs, &work[s]);

      if (metrics.mIndexOfBrierIn != -1) {

        Tv brier = 0;
        Tv yi, wi = 1, sum = 0;
        Ti i = -1;
        for (auto ap = Probs.ColBegin(1); ap != Probs.ColEnd(1); ++ap) {
          i++;
          yi = Y.Data[i];
          if constexpr (hasWeight) {
            wi = metrics.WeightedEval ? W.Data[i] : 1;
          }
          brier += wi * std::pow(yi - (*ap), 2);
          sum += wi;
        }
        brier /= sum;
        Weights.Set0(metrics.mIndexOfBrierIn, 0,
                     GoodnessOfFit::ToWeight(GoodnessOfFitType::kBrier, brier));
      }

      if (metrics.mIndexOfCostMatrixIn != -1) {
        if (this->pOptions->RequestCancel)
          return "";

        CostIn.get()->Calculate(
            *pCostMatrixes, Y, Probs,
            hasWeight ? (metrics.WeightedEval ? &W : nullptr) : nullptr,
            &work[s]);
        Weights.Set0(metrics.mIndexOfCostMatrixIn, 0,
                     GoodnessOfFit::ToWeight(GoodnessOfFitType::kFrequencyCost,
                                             CostIn.get()->AverageRatio));
      }
      if (metrics.mIndexOfAucIn != -1) {

        if (this->pOptions->RequestCancel)
          return "";

        AucIn.get()->Calculate(Y, Probs,
                               hasWeight ? (metrics.WeightedEval ? &W : nullptr)
                                         : nullptr,
                               *pAucOptions);
        Weights.Set0(metrics.mIndexOfAucIn, 0,
                     GoodnessOfFit::ToWeight(GoodnessOfFitType::kAuc,
                                             AucIn.get()->Result));
      }
    }
  }

  if (metrics.SimFixSize > 0) {
    Ti cc = (Ti)metrics.MetricsIn.size();
    if (metrics.mIndexOfCostMatrixOut != -1)
      Weights.Set0(cc + metrics.mIndexOfCostMatrixOut, 0,
                   Scoring::ToWeight(ScoringType::kFrequencyCost,
                                     Model.CostRatios.Mean()));
    if (metrics.mIndexOfAucOut != -1)
      Weights.Set0(cc + metrics.mIndexOfAucOut, 0,
                   Scoring::ToWeight(ScoringType::kAuc, Model.Auc));

    if (metrics.mIndexOfBrierOut != -1)
      Weights.Set0(cc + metrics.mIndexOfBrierOut, 0,
                   Scoring::ToWeight(ScoringType::kBrier, Model.BrierScore));
  }

  if (this->pOptions->RequestCancel)
    return "";

  // keep information:

  bool allNan = true;
  Tv weight;
  for (Ti i = 0; i < Weights.RowsCount; i++) { // evaluation index
    weight = Weights.Data[i];
    if (std::isnan(weight))
      continue;
    allNan = false;

    auto extra_data =
        std::unique_ptr<Ti[]>(new Ti[1]); // to save distribution type
    auto extra = Matrix<Ti>(extra_data.get(), 1, 1);
    extra.Data[0] = (Ti)distType;

    if (this->pItems->KeepModelEvaluations) // Add Model evaluation
    {
      auto ek = new EstimationKeep(weight, &this->CurrentIndices, &extra);
      this->Push0(*ek, i, 0, &ExoIndexes);
    }

    if (this->pItems->Length1 > 0) { // Add intercept, coefficients, thresholds
      for (Ti t = 0; t < this->SizeG + 1 + this->mNumChoices - 2; t++) {
        auto ek =
            new EstimationKeep(weight, &this->CurrentIndices, &extra, nullptr,
                               DModel.Beta.Data[t], DModel.BetaVar.Get0(t, t));
        if (t == 0) // intercept
          this->Push1(*ek, i, 0, 0);
        else if (t <= this->SizeG) // beta parameter
          this->Push1(*ek, i, 0, this->CurrentIndices.Data[t - 1] + 1);
        else // threshold
          this->Push1(*ek, i, 0, t);
      }
    }

    if (this->pItems->Length2 > 0) { // TODO: for saving ?!
    }
  }

  if (allNan)
    throw LdtException(ErrorType::kLogic, "dc-modelset", "all weights are NaN");

  return "";
}

// #pragma endregion

// #pragma region Modelset

void DiscreteChoiceModelsetBase::Start(Tv *work, Ti *worki) {
  Modelset.Start(work, worki);
}

DiscreteChoiceModelsetBase *DiscreteChoiceModelsetBase::GetFromTypes(
    bool isBinary, bool hasWeight, SearchOptions &searchOptions,
    SearchItems &searchItems, SearchMetricOptions &metrics,
    SearchModelChecks &checks, const std::vector<Ti> &sizes,
    const Matrix<Tv> &source, std::vector<Matrix<Tv>> &costMatrixes,
    std::vector<std::vector<Ti>> &groupIndexMaps, bool addLogit, bool addProbit,
    Newton &newtonOptions, RocOptions &aucOptions) {
  DiscreteChoiceModelsetBase *modelset;
  if (isBinary) {
    if (hasWeight) {
      modelset =
          new DiscreteChoiceModelset<true, DiscreteChoiceModelType::kBinary>(
              searchOptions, searchItems, metrics, checks, sizes, source,
              costMatrixes, groupIndexMaps, newtonOptions, aucOptions, addLogit,
              addProbit);
    } else {
      modelset =
          new DiscreteChoiceModelset<false, DiscreteChoiceModelType::kBinary>(
              searchOptions, searchItems, metrics, checks, sizes, source,
              costMatrixes, groupIndexMaps, newtonOptions, aucOptions, addLogit,
              addProbit);
    }
  } else {
    if (hasWeight) {
      modelset =
          new DiscreteChoiceModelset<true, DiscreteChoiceModelType::kOrdered>(
              searchOptions, searchItems, metrics, checks, sizes, source,
              costMatrixes, groupIndexMaps, newtonOptions, aucOptions, addLogit,
              addProbit);
    } else {
      modelset =
          new DiscreteChoiceModelset<false, DiscreteChoiceModelType::kOrdered>(
              searchOptions, searchItems, metrics, checks, sizes, source,
              costMatrixes, groupIndexMaps, newtonOptions, aucOptions, addLogit,
              addProbit);
    }
  }
  return modelset;
}

template <bool hasWeight, DiscreteChoiceModelType modelType>
DiscreteChoiceModelset<hasWeight, modelType>::~DiscreteChoiceModelset() {
  for (auto s : Searchers)
    delete s;
}

template <bool hasWeight, DiscreteChoiceModelType modelType>
DiscreteChoiceModelset<hasWeight, modelType>::DiscreteChoiceModelset(
    SearchOptions &searchOptions, SearchItems &searchItems,
    SearchMetricOptions &metrics, SearchModelChecks &checks,
    const std::vector<Ti> &sizes, const Matrix<Tv> &source,
    std::vector<Matrix<Tv>> &costMatrixes,
    std::vector<std::vector<Ti>> &groupIndexMaps, Newton &newtonOptions,
    RocOptions &aucOptions, bool addLogit, bool addProbit) {

  // find numChoices
  Ti r = 0;
  this->mNumChoices = (Ti)(source.MaximumInColumn(0, r) + 1);
  if (this->mNumChoices < 2)
    throw LdtException(ErrorType::kLogic, "dc-modelset",
                       "invalid number of choices");
  searchItems.LengthTargets = 1;
  searchItems.LengthDependents = 1;
  searchItems.LengthExogenouses =
      hasWeight ? (int)(source.ColsCount - 2) : (int)(source.ColsCount - 1);
  if (searchItems.LengthExogenouses <
      1) // =1, means the model has just one intercept. Let estimation process
         // throw error (if any)
    throw LdtException(ErrorType::kLogic, "dc-modelset",
                       "invalid number of exogenous variables");

  metrics.Update(true, false);
  checks.Update(metrics);
  searchItems.Update(metrics, searchItems.LengthTargets,
                     searchItems.LengthDependents,
                     searchItems.LengthExogenouses);

  // check searchItems.Length1 with the number of exogenous variables and
  // thresholds?!
  if (searchItems.Length1 != 0 &&
      searchItems.Length1 !=
          (searchItems.LengthExogenouses + this->mNumChoices - 2))
    throw LdtException(
        ErrorType::kLogic, "dc-modelset",
        "inconsistent number of exogenous variables and thresholds");
  if (searchItems.Length1 != 0 && checks.Estimation == false)
    throw LdtException(ErrorType::kLogic, "dc-modelset",
                       "parameters are needed. Set 'checks.Estimation = true'");

  this->pItems = &searchItems;

  this->pSource = &source;
  this->pCostMatrixes = &costMatrixes;
  this->pGroupIndexMap = &groupIndexMaps;

  // Check intercept
  if constexpr (hasWeight) {
    if (source.EqualsValueColumn(2, 1, 0, false, true) ==
        false) // third column must be intercept. ignore NANs, because I will
               // deal with them later
      throw LdtException(ErrorType::kLogic, "dc-modelset",
                         "third column of data is not intercept");
  } else if constexpr (true) {
    if (source.EqualsValueColumn(1, 1, 0, false, true) ==
        false) // in non-weighted case, second column must be intercept
      throw LdtException(ErrorType::kLogic, "dc-modelset",
                         "second column of data is not intercept");
  }

  // check group indexes and create sizes array
  for (auto const &b : groupIndexMaps) {
    for (auto &a : b) {
      if (a > searchItems.LengthExogenouses)
        throw LdtException(
            ErrorType::kLogic, "dc-modelset",
            "invalid exogenous group element (it is larger than the number "
            "of available variables)");
      if (a < 0)
        throw LdtException(ErrorType::kLogic, "dc-modelset",
                           "invalid exogenous group element (it is negative)");
    }
  }

  // check cost tables
  if (metrics.mIndexOfCostMatrixIn == -1 &&
      metrics.mIndexOfCostMatrixOut == -1) {
    if (costMatrixes.size() > 0)
      throw LdtException(ErrorType::kLogic, "dc-modelset",
                         "There is no frequency cost metric and yet "
                         "frequency cost matrix list is not "
                         "empty!");
  } else if (metrics.mIndexOfCostMatrixIn != -1 ||
             metrics.mIndexOfCostMatrixOut != -1) {
    if (costMatrixes.size() == 0)
      throw LdtException(ErrorType::kLogic, "dc-modelset",
                         "Frequency cost metrics are given, "
                         "however frequency cost matrix list is "
                         "empty!");
    for (auto const &table : costMatrixes) {
      FrequencyCost<hasWeight>::Check(table, this->mNumChoices);
    }
  }

  Ti co = 0;
  for (auto const s : sizes) {
    if (s <= 0)
      throw LdtException(
          ErrorType::kLogic, "dc-modelset",
          "invalid model size (zero or negative). Make sure array is "
          "initialized properly");
    co++;
    auto seed = metrics.Seed == (unsigned int)0
                    ? 0
                    : (metrics.Seed < 0 ? (unsigned int)(-metrics.Seed)
                                        : (unsigned int)(metrics.Seed + co));

    if (addLogit) {
      this->Searchers.push_back(
          new DiscreteChoiceSearcher<hasWeight, modelType,
                                     DiscreteChoiceDistType::kLogit>(
              searchOptions, searchItems, metrics, checks, s, groupIndexMaps, 0,
              source, this->mNumChoices, costMatrixes, seed, newtonOptions,
              aucOptions));
    }
    if (addProbit) {
      this->Searchers.push_back(
          new DiscreteChoiceSearcher<hasWeight, modelType,
                                     DiscreteChoiceDistType::kProbit>(
              searchOptions, searchItems, metrics, checks, s, groupIndexMaps, 0,
              source, this->mNumChoices, costMatrixes, seed, newtonOptions,
              aucOptions));
    }
  }

  this->Modelset = ModelSet(this->Searchers, groupIndexMaps, searchOptions,
                            searchItems, metrics, checks);
}

// #pragma endregion

template class ldt::DiscreteChoiceModelset<true,
                                           DiscreteChoiceModelType::kBinary>;

template class ldt::DiscreteChoiceModelset<true,
                                           DiscreteChoiceModelType::kOrdered>;

template class ldt::DiscreteChoiceModelset<false,
                                           DiscreteChoiceModelType::kBinary>;

template class ldt::DiscreteChoiceModelset<false,
                                           DiscreteChoiceModelType::kOrdered>;

template class ldt::DiscreteChoiceSearcher<
    true, DiscreteChoiceModelType::kBinary, DiscreteChoiceDistType::kLogit>;
template class ldt::DiscreteChoiceSearcher<
    true, DiscreteChoiceModelType::kBinary, DiscreteChoiceDistType::kProbit>;

template class ldt::DiscreteChoiceSearcher<
    true, DiscreteChoiceModelType::kOrdered, DiscreteChoiceDistType::kLogit>;
template class ldt::DiscreteChoiceSearcher<
    true, DiscreteChoiceModelType::kOrdered, DiscreteChoiceDistType::kProbit>;

template class ldt::DiscreteChoiceSearcher<
    false, DiscreteChoiceModelType::kBinary, DiscreteChoiceDistType::kLogit>;
template class ldt::DiscreteChoiceSearcher<
    false, DiscreteChoiceModelType::kBinary, DiscreteChoiceDistType::kProbit>;

template class ldt::DiscreteChoiceSearcher<
    false, DiscreteChoiceModelType::kOrdered, DiscreteChoiceDistType::kLogit>;
template class ldt::DiscreteChoiceSearcher<
    false, DiscreteChoiceModelType::kOrdered, DiscreteChoiceDistType::kProbit>;
