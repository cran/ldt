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
    const SearchData &data, const SearchCombinations &combinations,
    SearchOptions &options, const SearchItems &items,
    const SearchMetricOptions &metrics, const SearchModelChecks &checks,
    const Ti &numPartitions, const Matrix<Tv> &source, const Ti &numChoices,
    const std::vector<Matrix<Tv>> &costMatrixes, const unsigned int &seed,
    const Newton &newtonOptions, RocOptions &aucOptions)
    : SearcherReg::SearcherReg(data, combinations, options, items, metrics,
                               checks, numPartitions, false,
                               std::vector<Ti>({0}), 1) {

  if (combinations.NumFixPartitions == 0)
    throw LdtException(
        ErrorType::kLogic, "dc-modelset",
        "first partition must be fixed for intercept in binomial regression.");

  pCostMatrixes = &costMatrixes;
  pSource = &source;
  pAucOptions = &aucOptions;

  Ti num_obs = source.RowsCount;
  Ti num_exo = numPartitions;
  Ti num_cols = this->ColIndices.size();

  mNumChoices = numChoices;

  Data = Dataset<Tv>(source.RowsCount, num_cols, true);

  if (this->pChecks->Estimation) { // TODO: why this structure?
    DModel = DiscreteChoice<modelType, distType>(num_obs, num_exo, numChoices,
                                                 false);
    DModel.Optim.IterationMax = newtonOptions.IterationMax;
    DModel.Optim.TolFunction = newtonOptions.TolFunction;
    DModel.Optim.TolGradient = newtonOptions.TolGradient;
    DModel.Optim.UseLineSearch = newtonOptions.UseLineSearch;
  }

  if (metrics.SimFixSize > 0 && metrics.MetricsOut.size() > 0) {
    Model = DiscreteChoiceSim<hasWeight, modelType, distType>(
        num_obs, num_cols, this->mNumChoices, metrics.TrainRatio,
        (Ti)metrics.TrainFixSize, (Ti)costMatrixes.size(),
        metrics.MetricOutIndices.at(ScoringType::kBrier) >= 0,
        metrics.MetricOutIndices.at(ScoringType::kAuc) >= 0, false, nullptr,
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

  if (metrics.MetricInIndices.at(GoodnessOfFitType::kFrequencyCost) >= 0 ||
      metrics.MetricInIndices.at(GoodnessOfFitType::kAuc) >= 0 ||
      metrics.MetricInIndices.at(GoodnessOfFitType::kBrier) >= 0) {
    if (hasWeight && metrics.WeightedEval)
      CostIn = std::unique_ptr<FrequencyCostBase>(
          new FrequencyCost<true>((Ti)costMatrixes.size()));
    else
      CostIn = std::unique_ptr<FrequencyCostBase>(
          new FrequencyCost<false>((Ti)costMatrixes.size()));

    Probs = Matrix<Tv>(num_obs, numChoices);

    this->WorkSize +=
        std::max(num_obs + numChoices - 2, CostIn.get()->StorageSize) +
        num_obs * numChoices;
  }

  if (metrics.MetricInIndices.at(GoodnessOfFitType::kAuc) >= 0) {
    if (modelType != DiscreteChoiceModelType::kBinary)
      std::logic_error("not implemented discrete choice model type");

    if (hasWeight && metrics.WeightedEval)
      AucIn = std::unique_ptr<RocBase>(new ROC<true, false>(num_obs));
    else
      AucIn = std::unique_ptr<RocBase>(new ROC<false, false>(num_obs));
  }
}

template <bool hasWeight, DiscreteChoiceModelType modelType,
          DiscreteChoiceDistType distType>
std::string
DiscreteChoiceSearcher<hasWeight, modelType, distType>::EstimateOneReg(
    Tv *work, Ti *workI, VMatrix<Tv> &metrics, VMatrix<Tv> &type1Mean,
    VMatrix<Tv> &type1Var, VMatrix<Ti> &extra) {

  Ti s = 0;

  Data.Calculate(*pSource, &this->ColIndices, &work[s]);
  s += Data.StorageSize;

  if (this->pOptions->RequestCancel)
    return "";

  Ti count = Data.Result.RowsCount;
  Ti num_exo = this->NumPartitions;
  Tv *d = Data.Result.Data;

  Y.SetData(d, count, 1);
  // You cannot set y in the constructor due to NAN existence

  if constexpr (hasWeight) {
    W.SetData(&d[count], count, 1);
    X.SetData(&d[2 * count], count, num_exo);
  } else if constexpr (true) {
    X.SetData(&d[count], count, num_exo);
  }

  // there is just one endogenous and one target

  if (this->pChecks->Estimation) {

    if (this->pChecks) {
      if (this->pChecks->MinObsCount > 0 && this->pChecks->MinObsCount > count)
        throw LdtException(ErrorType::kLogic, "dc-modelset",
                           "model check: minimum no. obs");
      if (this->pChecks->MinDof > 0 &&
          this->pChecks->MinDof > count - num_exo - this->mNumChoices - 2)
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

  if (this->pMetrics->SimFixSize > 0 && this->pMetrics->MetricsOut.size() > 0) {

    if (this->pOptions->RequestCancel)
      return "";

    Model.Calculate(Data.Result, pCostMatrixes, &work[s],
                    &work[s + Model.StorageSize], &workI[this->NumPartitions],
                    this->pOptions->RequestCancel, *pAucOptions,
                    this->pMetrics->SimFixSize - this->pChecks->MinOutSim,
                    nullptr, INT32_MAX);
    s += Model.StorageSize;
  }

  // collect metrics:

  if (this->pChecks->Estimation) {
    auto ind = this->pMetrics->MetricInIndices.at(GoodnessOfFitType::kAic);
    if (ind >= 0)
      metrics.Mat.Set0(ind, 0, DModel.Aic);

    ind = this->pMetrics->MetricInIndices.at(GoodnessOfFitType::kSic);
    if (ind >= 0)
      metrics.Mat.Set0(ind, 0, DModel.Sic);

    auto costInd =
        this->pMetrics->MetricInIndices.at(GoodnessOfFitType::kFrequencyCost);
    auto aucInd = this->pMetrics->MetricInIndices.at(GoodnessOfFitType::kAuc);
    auto briInd = this->pMetrics->MetricInIndices.at(GoodnessOfFitType::kBrier);
    if (costInd > 0 || aucInd > 0 || briInd > 0) {

      Probs.SetData(&work[s]);
      s += Probs.length();
      DModel.GetProbabilities(X, Probs, &work[s]);

      if (briInd > 0) {

        Tv brier = 0;
        Tv yi, wi = 1, sum = 0;
        Ti i = -1;
        for (auto ap = Probs.ColBegin(1); ap != Probs.ColEnd(1); ++ap) {
          i++;
          yi = Y.Data[i];
          if constexpr (hasWeight) {
            wi = this->pMetrics->WeightedEval ? W.Data[i] : 1;
          }
          brier += wi * std::pow(yi - (*ap), 2);
          sum += wi;
        }
        brier /= sum;

        metrics.Mat.Set0(briInd, 0, brier);
      }

      if (costInd > 0) {
        if (this->pOptions->RequestCancel)
          return "";

        CostIn.get()->Calculate(
            *pCostMatrixes, Y, Probs,
            hasWeight ? (this->pMetrics->WeightedEval ? &W : nullptr) : nullptr,
            &work[s]);
        metrics.Mat.Set0(costInd, 0, CostIn.get()->AverageRatio);
      }

      if (aucInd > 0) {
        if (this->pOptions->RequestCancel)
          return "";

        AucIn.get()->Calculate(
            Y, Probs,
            hasWeight ? (this->pMetrics->WeightedEval ? &W : nullptr) : nullptr,
            *pAucOptions);
        metrics.Mat.Set0(aucInd, 0, AucIn.get()->Result);
      }
    }
  }

  if (this->pMetrics->SimFixSize > 0 && this->pMetrics->MetricsOut.size() > 0) {

    Ti j = (Ti)this->pMetrics->MetricsIn.size();

    auto ind = this->pMetrics->MetricOutIndices.at(ScoringType::kFrequencyCost);
    if (ind >= 0)
      metrics.Mat.Set0(j + ind, 0, Model.CostRatios.Mean());

    ind = this->pMetrics->MetricOutIndices.at(ScoringType::kAuc);
    if (ind >= 0)
      metrics.Mat.Set0(j + ind, 0, Model.Auc);

    ind = this->pMetrics->MetricOutIndices.at(ScoringType::kBrier);
    if (ind >= 0)
      metrics.Mat.Set0(j + ind, 0, Model.BrierScore);
  }

  if (this->pOptions->RequestCancel)
    return "";

  // Update Type1 values:
  extra.Mat.Data[0] = (Ti)distType;

  if (DModel.WorkSize > 0 && this->pItems->Length1 > 0) {

    // we should skip those coefficients that are not present in the current
    // estimation.
    Ti i = -1;
    for (const auto &b : this->CurrentIndices.Vec) {
      i++;
      Ti a = b - (this->pData->HasWeight ? 2 : 1);
      type1Mean.Mat.Data[a] = DModel.Beta.Data[i];
      type1Var.Mat.Data[a] = DModel.BetaVar.Get0(i, i);
    }
  }

  return "";
}

// #pragma endregion

// #pragma region Modelset

void DiscreteChoiceModelsetBase::Start(Tv *work, Ti *worki) {
  Modelset.Start(work, worki);
}

DiscreteChoiceModelsetBase *DiscreteChoiceModelsetBase::GetFromTypes(
    bool isBinary, bool hasWeight, const SearchData &data,
    const SearchCombinations &combinations, SearchOptions &options,
    SearchItems &items, SearchMetricOptions &metrics, SearchModelChecks &checks,
    const Matrix<Tv> &source, std::vector<Matrix<Tv>> &costMatrixes,
    bool addLogit, bool addProbit, Newton &newtonOptions,
    RocOptions &aucOptions) {
  DiscreteChoiceModelsetBase *modelset;
  if (isBinary) {
    if (hasWeight) {
      modelset =
          new DiscreteChoiceModelset<true, DiscreteChoiceModelType::kBinary>(
              data, combinations, options, items, metrics, checks, source,
              costMatrixes, newtonOptions, aucOptions, addLogit, addProbit);
    } else {
      modelset =
          new DiscreteChoiceModelset<false, DiscreteChoiceModelType::kBinary>(
              data, combinations, options, items, metrics, checks, source,
              costMatrixes, newtonOptions, aucOptions, addLogit, addProbit);
    }
  } else {
    if (hasWeight) {
      modelset =
          new DiscreteChoiceModelset<true, DiscreteChoiceModelType::kOrdered>(
              data, combinations, options, items, metrics, checks, source,
              costMatrixes, newtonOptions, aucOptions, addLogit, addProbit);
    } else {
      modelset =
          new DiscreteChoiceModelset<false, DiscreteChoiceModelType::kOrdered>(
              data, combinations, options, items, metrics, checks, source,
              costMatrixes, newtonOptions, aucOptions, addLogit, addProbit);
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
    const SearchData &data, const SearchCombinations &combinations,
    SearchOptions &options, SearchItems &items, SearchMetricOptions &metrics,
    SearchModelChecks &checks, const Matrix<Tv> &source,
    std::vector<Matrix<Tv>> &costMatrixes, Newton &newtonOptions,
    RocOptions &aucOptions, bool addLogit, bool addProbit) {

  // find numChoices
  Ti r = 0;
  this->mNumChoices = (Ti)(source.MaximumInColumn(0, r) + 1);
  if (this->mNumChoices < 2)
    throw LdtException(ErrorType::kLogic, "dc-modelset",
                       "invalid number of choices");
  items.LengthTargets = 1;
  items.LengthEndogenous = 1;
  items.LengthExogenous =
      hasWeight ? (int)(source.ColsCount - 2) : (int)(source.ColsCount - 1);
  if (items.LengthExogenous < 1) // =1, means the model has just one intercept.
                                 // Let estimation process throw error (if any)
    throw LdtException(ErrorType::kLogic, "dc-modelset",
                       "invalid number of exogenous variables");

  metrics.Update(true, false);
  checks.Update(metrics);
  items.Update(metrics, items.LengthTargets, items.LengthEndogenous,
               items.LengthExogenous);

  // check items.Length1 with the number of exogenous variables and
  // thresholds?!
  if (items.Length1 != 0 &&
      items.Length1 != (items.LengthExogenous + this->mNumChoices - 2))
    throw LdtException(
        ErrorType::kLogic, "dc-modelset",
        "inconsistent number of exogenous variables and thresholds");
  if (items.Length1 != 0 && checks.Estimation == false)
    throw LdtException(ErrorType::kLogic, "dc-modelset",
                       "parameters are needed. Set 'checks.Estimation = true'");

  this->pItems = &items;

  this->pSource = &source;
  this->pCostMatrixes = &costMatrixes;

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
  for (auto const &b : combinations.Partitions) {
    for (auto &a : b) {
      if (a > items.LengthExogenous)
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
  if (metrics.MetricInIndices.at(GoodnessOfFitType::kFrequencyCost) == -1 &&
      metrics.MetricOutIndices.at(ScoringType::kFrequencyCost) == -1) {
    if (costMatrixes.size() > 0)
      throw LdtException(ErrorType::kLogic, "dc-modelset",
                         "There is no frequency cost metric and yet "
                         "frequency cost matrix list is not "
                         "empty!");
  } else if (metrics.MetricInIndices.at(GoodnessOfFitType::kFrequencyCost) >
                 0 ||
             metrics.MetricOutIndices.at(ScoringType::kFrequencyCost) > 0) {
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
  for (auto const s : combinations.Sizes) {
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
              data, combinations, options, items, metrics, checks, s, source,
              this->mNumChoices, costMatrixes, seed, newtonOptions,
              aucOptions));
    }
    if (addProbit) {
      this->Searchers.push_back(
          new DiscreteChoiceSearcher<hasWeight, modelType,
                                     DiscreteChoiceDistType::kProbit>(
              data, combinations, options, items, metrics, checks, s, source,
              this->mNumChoices, costMatrixes, seed, newtonOptions,
              aucOptions));
    }
  }

  this->Modelset = ModelSet(this->Searchers, data, combinations, options, items,
                            metrics, checks);
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
