/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the LGPL 3.0.
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
    const SearchMeasureOptions &measures, const SearchModelChecks &checks,
    Ti SizeG, const std::vector<std::vector<Ti>> &groupIndexMap,
    const std::vector<Ti> &groupSizes, Ti fixFirstG, const Matrix<Tv> &source,
    Ti numChoices, const std::vector<Matrix<Tv>> &costMatrixes,
    unsigned int seed, Newton &newtonOptions)
    : Searcher::Searcher(searchOptions, searchItems, measures, checks, SizeG,
                         groupIndexMap, groupSizes, fixFirstG) {

  pCostMatrixes = &costMatrixes;
  pSource =
      &source; // 1st is endogenous, second is intercept, 3rd (can be) weight
  mNumChoices = numChoices;
  Ti numObs = source.RowsCount;
  Ti numExo = SizeG + 1; // intercept
  Ti cols = numExo + (hasWeight ? 2 : 1);
  Data =
      Dataset<Tv>(numObs, cols, true); // size + endogenous, intercept, weight

  if (this->pChecks->Estimation) {
    DModel =
        DiscreteChoice<modelType, distType>(numObs, numExo, numChoices, false);
    DModel.Optim.IterationMax = newtonOptions.IterationMax;
    DModel.Optim.TolFunction = newtonOptions.TolFunction;
    DModel.Optim.TolGradient = newtonOptions.TolGradient;
    DModel.Optim.UseLineSearch = newtonOptions.UseLineSearch;
  }

  if (measures.SimFixSize > 0) { // we estimate a simulation model
    Model = DiscreteChoiceSim<hasWeight, modelType, distType>(
        numObs, cols, this->mNumChoices, measures.TrainRatio,
        (Ti)measures.TrainFixSize, (Ti)costMatrixes.size(),
        measures.mIndexOfAucOut != -1, false, nullptr,
        measures.WeightedEval); // no PCA in search
    Model.Seed = seed;
    Model.SimulationMax = measures.SimFixSize;

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

  auto numMeas = (Ti)this->pMeasures->MeasuresOut.size() +
                 (Ti)this->pMeasures->MeasuresIn.size();
  Weights = Matrix<Tv>(numMeas, 1);
  this->WorkSize += numMeas; // weights matrix

  if (measures.mIndexOfCostMatrixIn != -1 || measures.mIndexOfAucIn != -1) {
    if (hasWeight && measures.WeightedEval)
      CostIn = std::unique_ptr<CostMatrixBase>(
          new CostMatrix<true>((Ti)costMatrixes.size()));
    else
      CostIn = std::unique_ptr<CostMatrixBase>(
          new CostMatrix<false>((Ti)costMatrixes.size()));
    Probs = Matrix<Tv>(numObs, numChoices);
    this->WorkSize +=
        std::max(numObs + numChoices - 2, CostIn.get()->StorageSize) +
        numObs * numChoices;
  }
  if (measures.mIndexOfAucIn != -1) {
    if (hasWeight && measures.WeightedEval)
      AucIn = std::unique_ptr<AucBase>(
          new AUC<true, modelType == DiscreteChoiceModelType::kBinary>(numObs));
    else
      AucIn = std::unique_ptr<AucBase>(
          new AUC<false, modelType == DiscreteChoiceModelType::kBinary>(
              numObs));
  }
}

template <bool hasWeight, DiscreteChoiceModelType modelType,
          DiscreteChoiceDistType distType>
std::string
DiscreteChoiceSearcher<hasWeight, modelType, distType>::EstimateOne(Tv *work,
                                                                    Ti *workI) {
  auto measures = *this->pMeasures;

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
        throw std::logic_error(
            "Model check failed: Minimum number of observations");
      if (this->pChecks->MinDof > 0 &&
          this->pChecks->MinDof > count - numExo - this->mNumChoices - 2)
        throw std::logic_error(
            "Model check failed: Minimum number of degrees of freedom");
    }

    if (this->pOptions->RequestCancel)
      return "";

    DModel.Calculate(Y, X, hasWeight ? &W : nullptr, &work[s],
                     &work[s + DModel.StorageSize], mNumChoices, true);
    s += DModel.StorageSize; // keep model storage

    // 'R2' and 'ConditionNumber' is not implemented

    if (this->pChecks) {
      // if (pChecks->mCheckCN_all && Model.condition_number >
      // pChecks->MaxConditionNumber)
      //	throw std::logic_error("Model check failed: Maximum CN");

      if (this->pChecks->MaxAic < this->DModel.Aic)
        throw std::logic_error("Model check failed: Maximum Aic");
      if (this->pChecks->MaxSic < this->DModel.Sic)
        throw std::logic_error("Model check failed: Maximum Sic");
      // if (pChecks->MinR2 > Model.r2)
      //	throw std::logic_error("Model check failed: Maximum R2");
    }
  }

  if (measures.SimFixSize > 0) {

    if (this->pOptions->RequestCancel)
      return "";

    Model.Calculate(Data.Result, pCostMatrixes, &work[s],
                    &work[s + Model.StorageSize], &workI[this->SizeG + 1],
                    this->pOptions->RequestCancel,
                    this->pMeasures->SimFixSize - this->pChecks->MinOutSim,
                    nullptr, INT32_MAX);
    s += Model.StorageSize;
  }

  // collect measures:

  if (this->pChecks->Estimation) {
    if (measures.mIndexOfAic != -1)
      Weights.Set0(
          measures.mIndexOfAic, 0,
          GoodnessOfFit::ToWeight(GoodnessOfFitType::kAic, DModel.Aic));
    if (measures.mIndexOfSic != -1)
      Weights.Set0(
          measures.mIndexOfSic, 0,
          GoodnessOfFit::ToWeight(GoodnessOfFitType::kSic, DModel.Sic));

    if (measures.mIndexOfCostMatrixIn != -1 || measures.mIndexOfAucIn != -1) {
      Probs.SetData(&work[s]);
      s += Probs.length();
      DModel.GetProbabilities(X, Probs, &work[s]);

      if (measures.mIndexOfCostMatrixIn != -1) {
        if (this->pOptions->RequestCancel)
          return "";

        CostIn.get()->Calculate(
            *pCostMatrixes, Y, Probs,
            hasWeight ? (measures.WeightedEval ? &W : nullptr) : nullptr,
            &work[s]);
        Weights.Set0(measures.mIndexOfCostMatrixIn, 0,
                     1 - CostIn.get()->AverageRatio);
      }
      if (measures.mIndexOfAucIn != -1) {

        if (this->pOptions->RequestCancel)
          return "";

        AucIn.get()->Calculate(
            Y, Probs,
            hasWeight ? (measures.WeightedEval ? &W : nullptr) : nullptr,
            &AucWeightsMc);
        Weights.Set0(measures.mIndexOfAucIn, 0, AucIn.get()->Result);
      }
    }
  }

  if (measures.SimFixSize > 0) {
    Ti cc = (Ti)measures.MeasuresIn.size();
    if (measures.mIndexOfCostMatrixOut != -1)
      Weights.Set0(cc + measures.mIndexOfCostMatrixOut, 0,
                   GoodnessOfFit::ToWeight(GoodnessOfFitType::kCostMatrix,
                                           Model.CostRatios.Mean()));
    if (measures.mIndexOfAucOut != -1)
      Weights.Set0(cc + measures.mIndexOfAucOut, 0,
                   GoodnessOfFit::ToWeight(GoodnessOfFitType::kAuc, Model.Auc));
  }

  if (this->pOptions->RequestCancel)
    return "";

  // keep information:

  bool allNan = true;
  Tv weight;
  for (Ti i = 0; i < Weights.RowsCount; i++) { // evaluation index
    weight = Weights.Get(i, 0);
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
                               DModel.Beta.Data[t], DModel.BetaVar.Get(t, t));
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
    throw std::logic_error("All weights are NaN");

  return "";
}

// #pragma endregion

// #pragma region Modelset

void DiscreteChoiceModelsetBase::Start(Tv *work, Ti *worki) {
  Modelset.Start(work, worki);
}

DiscreteChoiceModelsetBase *DiscreteChoiceModelsetBase::GetFromTypes(
    bool isBinary, bool hasWeight, SearchOptions &searchOptions,
    SearchItems &searchItems, SearchMeasureOptions &measures,
    SearchModelChecks &checks, const std::vector<Ti> &sizes,
    const Matrix<Tv> &source, std::vector<Matrix<Tv>> &costMatrixes,
    std::vector<std::vector<Ti>> &groupIndexMaps, bool addLogit, bool addProbit,
    Newton &newtonOptions) {
  DiscreteChoiceModelsetBase *modelset;
  if (isBinary) {
    if (hasWeight) {
      modelset =
          new DiscreteChoiceModelset<true, DiscreteChoiceModelType::kBinary>(
              searchOptions, searchItems, measures, checks, sizes, source,
              costMatrixes, groupIndexMaps, newtonOptions, addLogit, addProbit);
    } else {
      modelset =
          new DiscreteChoiceModelset<false, DiscreteChoiceModelType::kBinary>(
              searchOptions, searchItems, measures, checks, sizes, source,
              costMatrixes, groupIndexMaps, newtonOptions, addLogit, addProbit);
    }
  } else {
    if (hasWeight) {
      modelset =
          new DiscreteChoiceModelset<true, DiscreteChoiceModelType::kOrdered>(
              searchOptions, searchItems, measures, checks, sizes, source,
              costMatrixes, groupIndexMaps, newtonOptions, addLogit, addProbit);
    } else {
      modelset =
          new DiscreteChoiceModelset<false, DiscreteChoiceModelType::kOrdered>(
              searchOptions, searchItems, measures, checks, sizes, source,
              costMatrixes, groupIndexMaps, newtonOptions, addLogit, addProbit);
    }
  }
  return modelset;
}

template <bool hasWeight, DiscreteChoiceModelType modelType>
DiscreteChoiceModelset<hasWeight, modelType>::DiscreteChoiceModelset(
    SearchOptions &searchOptions, SearchItems &searchItems,
    SearchMeasureOptions &measures, SearchModelChecks &checks,
    const std::vector<Ti> &sizes, const Matrix<Tv> &source,
    std::vector<Matrix<Tv>> &costMatrixes,
    std::vector<std::vector<Ti>> &groupIndexMaps, Newton &newtonOptions,
    bool addLogit, bool addProbit) {

  // find numChoices
  Ti r = 0;
  this->mNumChoices = (Ti)(source.MaximumInColumn(0, r) + 1);
  if (this->mNumChoices < 2)
    throw std::logic_error("Invalid number of choices");
  searchItems.LengthTargets = 1;
  searchItems.LengthDependents = 1;
  searchItems.LengthExogenouses =
      hasWeight ? (int)(source.ColsCount - 2) : (int)(source.ColsCount - 1);
  if (searchItems.LengthExogenouses <
      1) // =1, means the model has just one intercept. Let estimation process
         // throw error (if any)
    throw std::logic_error("Invalid number of exogenous variables.");

  measures.Update(true, false);
  checks.Update(measures);
  searchItems.Update(measures, searchItems.LengthTargets,
                     searchItems.LengthDependents,
                     searchItems.LengthExogenouses);

  // check searchItems.Length1 with the number of exogenous variables and
  // thresholds?!
  if (searchItems.Length1 != 0 &&
      searchItems.Length1 !=
          (searchItems.LengthExogenouses + this->mNumChoices - 2))
    throw std::logic_error(
        "Inconsistent number of exogenous variables and thresholds.");
  if (searchItems.Length1 != 0 && checks.Estimation == false)
    throw std::logic_error(
        "Parameters are needed. Set 'checks.Estimation = true'.");

  this->pItems = &searchItems;

  this->pSource = &source;
  this->pCostMatrixes = &costMatrixes;
  this->pGroupIndexMap = &groupIndexMaps;

  // Check intercept
  if constexpr (hasWeight) {
    if (source.EqualsValueColumn(2, 1, 0, false, true) ==
        false) // third column must be intercept. ignore NANs, because I will
               // deal with them later
      throw std::logic_error("Third column of data is not intercept.");
  } else if constexpr (true) {
    if (source.EqualsValueColumn(1, 1, 0, false, true) ==
        false) // in non-weighted case, second column must be intercept
      throw std::logic_error("Second column of data is not intercept.");
  }

  // check group indexes and create sizes array
  for (auto const &b : groupIndexMaps) {
    for (auto &a : b) {
      if (a > searchItems.LengthExogenouses)
        throw std::logic_error(
            "Invalid exogenous group element (it is larger than the number "
            "of available variables).");
      if (a < 0)
        throw std::logic_error(
            "Invalid exogenous group element (it is negative).");
    }
    this->GroupSizes.push_back((Ti)b.size());
  }

  // check cost tables
  if (measures.mIndexOfCostMatrixIn == -1 &&
      measures.mIndexOfCostMatrixOut == -1) {
    if (costMatrixes.size() > 0)
      throw std::logic_error(
          "There is no cost matrix measure and yet cost matrix list is not "
          "empty!");
  } else if (measures.mIndexOfCostMatrixIn != -1 ||
             measures.mIndexOfCostMatrixOut != -1) {
    if (costMatrixes.size() == 0)
      throw std::logic_error(
          "Cost matrix measures are given, however cost matrix list is "
          "empty!");
    for (auto const &table : costMatrixes) {
      CostMatrix<hasWeight>::Check(table, this->mNumChoices);
    }
  }

  Ti co = 0;
  for (auto const s : sizes) {
    if (s <= 0)
      throw std::logic_error(
          "Invalid model size (zero or negative). Make sure array is "
          "initialized properly.");
    co++;
    auto seed = measures.Seed == (unsigned int)0
                    ? 0
                    : (measures.Seed < 0 ? (unsigned int)(-measures.Seed)
                                         : (unsigned int)(measures.Seed + co));

    if (addLogit) {
      this->Searchers.push_back(
          new DiscreteChoiceSearcher<hasWeight, modelType,
                                     DiscreteChoiceDistType::kLogit>(
              searchOptions, searchItems, measures, checks, s, groupIndexMaps,
              this->GroupSizes, 0, source, this->mNumChoices, costMatrixes,
              seed, newtonOptions));
    }
    if (addProbit) {
      this->Searchers.push_back(
          new DiscreteChoiceSearcher<hasWeight, modelType,
                                     DiscreteChoiceDistType::kProbit>(
              searchOptions, searchItems, measures, checks, s, groupIndexMaps,
              this->GroupSizes, 0, source, this->mNumChoices, costMatrixes,
              seed, newtonOptions));
    }
  }

  this->Modelset = ModelSet(this->Searchers, groupIndexMaps, this->GroupSizes,
                            searchOptions, searchItems, measures, checks);
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
