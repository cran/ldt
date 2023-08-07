/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the GPL 3.0.
 See accompanying file LICENSE
*/

#include "data_split.h"
#include "discrete_choice.h"
#include "pca.h"

using namespace ldt;

template <bool hasWeight, DiscreteChoiceModelType modelType,
          DiscreteChoiceDistType distType>
DiscreteChoiceSim<hasWeight, modelType, distType>::DiscreteChoiceSim(
    Ti rows, Ti cols, Ti numChoices, Tv trainPercentage, Ti trainFixSize,
    Ti costMatrixCount, bool doBrier, bool doAuc, bool doFrequencyTable,
    PcaAnalysisOptions *pcaOptions, bool weightedEval) {
  if (numChoices < 1)
    throw LdtException(ErrorType::kLogic, "dc-sim",
                       "number of choices must be larger than 1");
  else if (numChoices == 2 && modelType == DiscreteChoiceModelType::kOrdered)
    throw LdtException(ErrorType::kLogic, "dc-sim",
                       "use binary Model for 2 choices case");
  else if (numChoices > 2 && modelType == DiscreteChoiceModelType::kBinary)
    throw LdtException(
        ErrorType::kLogic, "dc-sim",
        "don't use binary Model when number of choices is larger than 2");

  if (costMatrixCount == 0 && doFrequencyTable == false && doAuc == false)
    throw LdtException(ErrorType::kLogic, "dc-sim",
                       "no goal is set in discrete choice simulation");

  mTrainRatio = trainPercentage;
  mTrainFixSize = trainFixSize;
  if (mTrainFixSize < 0)
    throw LdtException(ErrorType::kLogic, "dc-sim",
                       "invalid size of train sample (it is negative!)");
  else if (mTrainFixSize == 0) { // percentage is effective
    if (trainPercentage >= (Tv)1 || trainPercentage <= (Tv)0)
      throw LdtException(ErrorType::kLogic, "dc-sim",
                         "training percentage is not valid");
  }

  mNumChoices = numChoices;

  mDoFrequecyTable = doFrequencyTable;
  mCostMatrixCount = costMatrixCount;
  mDoBrier = doBrier;
  mDoAuc = doAuc;
  mWeightedEval = hasWeight && weightedEval;
  this->pPcaOptions = pcaOptions;

  Ti N0 = trainFixSize > 0
              ? trainFixSize
              : static_cast<Ti>(std::round(rows * trainPercentage));
  if (N0 == 0 || N0 == rows)
    throw LdtException(ErrorType::kLogic, "dc-sim",
                       "training percentage is not valid");
  this->N1 = rows - N0;
  // Ti numExo = cols - (hasWeight ? 2 : 1);

  // storage
  this->StorageSize = 0;
  if (costMatrixCount > 0)
    this->StorageSize += costMatrixCount;
  if (doFrequencyTable)
    this->StorageSize += 10 * numChoices;

  this->WorkSize = 0;

  // we might use a non-weighted (choose maximum size)
  auto frequencyCost = FrequencyCost<true>(costMatrixCount);
  auto freqCost0 = FrequencyCost<false>(costMatrixCount);

  auto split = DataSplitDiscrete(rows, cols, mNumChoices);
  auto model =
      DiscreteChoiceExtended(modelType, distType, N0, cols, hasWeight, false,
                             numChoices, false, this->N1, pcaOptions, nullptr);

  this->WorkSize += costMatrixCount +
                    std::max(frequencyCost.StorageSize, freqCost0.StorageSize) +
                    split.StorageSize + model.StorageSize + model.WorkSize;
  this->WorkSizeI = split.WorkSizeI;
}

template <bool hasWeight, DiscreteChoiceModelType modelType,
          DiscreteChoiceDistType distType>
void DiscreteChoiceSim<hasWeight, modelType, distType>::Calculate(
    const Matrix<Tv> &data, const std::vector<Matrix<Tv>> *costMatrixes,
    Tv *storage, Tv *work, Ti *workI, bool &cancel, RocOptions &aucOptions,
    bool checkSizes, std::set<const char *> *errors, Ti maxInvalidSim) {

  if (cancel)
    return;

  Ti costCount = costMatrixes ? (Ti)costMatrixes->size() : 0;
  Ti rows = data.RowsCount;
  Ti cols = data.ColsCount;

  // check
  if (checkSizes) {
    auto temp = DiscreteChoiceSim<hasWeight, modelType, distType>(
        rows, data.ColsCount, mNumChoices, mTrainRatio, mTrainFixSize,
        costCount, mDoBrier, mDoAuc, mDoFrequecyTable, this->pPcaOptions,
        mWeightedEval);
    if (temp.WorkSize > this->WorkSize || temp.WorkSizeI > this->WorkSizeI ||
        temp.StorageSize > this->StorageSize)
      throw LdtException(
          ErrorType::kLogic, "dc-sim",
          "inconsistent arguments in discrete choice simulation");
  }

  Ti N0 = rows - this->N1;
  // Ti numExo = cols - (hasWeight ? 2 : 1);
  std::mt19937 eng;
  if (this->Seed != 0)
    eng = std::mt19937(this->Seed);
  else {
    std::random_device rdev{};
    eng = std::mt19937(rdev());
  }

  // storage
  Ti pos = 0;
  if (this->mDoFrequecyTable) {
    this->FrequencyTable.SetData(0, &storage[pos], 10, this->mNumChoices);
    pos += 10 * this->mNumChoices;
  }
  if (costCount > 0) {
    this->CostRatios.SetData(0, &storage[pos], costCount, 1);
    pos += costCount;
  }

  if (modelType != DiscreteChoiceModelType::kBinary) {
    throw LdtException(ErrorType::kLogic, "dc-sim",
                       "not implemented discrete choice model type");
    // TODO: implement it
  }

  // works
  std::unique_ptr<RocBase> auc0;
  std::unique_ptr<FrequencyCostBase> freqCost0;
  if (hasWeight && mWeightedEval) {

    if (modelType != DiscreteChoiceModelType::kBinary) {
      throw LdtException(ErrorType::kLogic, "dc-sim",
                         "not implemented discrete choice model type");
    }
    if (aucOptions.Costs.Data) {
      auc0 = std::unique_ptr<RocBase>(new ROC<true, true>(this->N1));
    } else {
      auc0 = std::unique_ptr<RocBase>(new ROC<true, false>(this->N1));
    }
    freqCost0 =
        std::unique_ptr<FrequencyCostBase>(new FrequencyCost<true>(costCount));
  } else {
    if (aucOptions.Costs.Data) {
      auc0 = std::unique_ptr<RocBase>(new ROC<false, true>(this->N1));
    } else {
      auc0 = std::unique_ptr<RocBase>(new ROC<false, false>(this->N1));
    }
    freqCost0 =
        std::unique_ptr<FrequencyCostBase>(new FrequencyCost<false>(costCount));
  }
  auto auc = auc0.get();
  auto frequencyCost = freqCost0.get();

  this->Auc = 0;
  auto split = DataSplitDiscrete(rows, cols, mNumChoices);

  auto model =
      DiscreteChoiceExtended(modelType, distType, N0, cols, hasWeight, false,
                             mNumChoices, false, this->N1, this->pPcaOptions);
  model.Model->Optim.IterationMax = this->Optim.IterationMax;
  model.Model->Optim.TolFunction = this->Optim.TolFunction;
  model.Model->Optim.TolGradient = this->Optim.TolGradient;
  model.Model->Optim.UseLineSearch = this->Optim.UseLineSearch;

  pos = 0;
  auto cost_storage = &work[pos];
  pos += frequencyCost->StorageSize;
  auto split_storage = &work[pos];
  pos += split.StorageSize;
  auto model_storage = &work[pos];
  pos += model.StorageSize;
  auto overall_count = Matrix<Tv>(0, &work[pos], costCount, 1);
  pos += costCount;
  auto work0 = &work[pos]; // for estimation

  auto y1 = Matrix<Tv>(this->N1, 1);
  auto w1 = Matrix<Tv>(this->N1, 1);
  Ti i, j;

  this->ValidSimulationCount = 0;

  if (cancel)
    return;

  split.Calculate(data, split_storage, mTrainRatio, mTrainFixSize);

  // start main loop
  Ti pexo = 0;
  auto test = Matrix<Tv>();
  // const std::vector<Ti> *Rowi = nullptr;
  Ti invalidCounts = 0;
  for (this->SimulationCounter = 0;
       this->SimulationCounter < this->SimulationMax;
       this->SimulationCounter++) {

    if (cancel)
      return;

    invalidCounts++;
    split.Shuffle(data, workI, eng);
    pexo = 1;
    if constexpr (hasWeight) {
      pexo = 2;
    }
    // get exogenous from poSample1
    test.SetData(&split.Sample1.Data[pexo * split.Sample1.RowsCount],
                 split.Sample1.RowsCount, split.Sample1.ColsCount - pexo);

    if (cancel)
      return;

    if (errors) {
      try {
        model.Calculate(split.Sample0, model_storage, work0, true, &test,
                        aucOptions);
      } catch (std::exception &ex) {
        errors->insert(ex.what());
        continue;
      } catch (...) {
        errors->insert("unknown error!");
        continue;
      }
    } else {
      try {
        model.Calculate(split.Sample0, model_storage, work0, true, &test,
                        aucOptions);
      } catch (...) {
        continue;
      }
    }

    if (cancel)
      return;

    y1.SetData(split.Sample1.Data);
    if constexpr (hasWeight) {
      w1.SetData(&split.Sample1.Data[this->N1]);
    }

    if (mDoBrier) {
      BrierScore = 0;
      Tv yi, wi = 1, sum = 0;
      Ti i = -1;
      for (auto ap = model.PredProbs.ColBegin(1);
           ap != model.PredProbs.ColEnd(1); ++ap) {
        i++;
        yi = y1.Data[i];
        if constexpr (hasWeight) {
          wi = w1.Data[i];
        }
        BrierScore += wi * std::pow(yi - *ap, 2);
        sum += wi;
      }
      BrierScore /= sum;
    }

    if (costCount > 0) {
      frequencyCost->Calculate(
          *costMatrixes, y1, model.PredProbs,
          hasWeight ? (mWeightedEval ? &w1 : nullptr) : nullptr, cost_storage);
      for (i = 0; i < costCount; i++) {
        this->CostRatios.Data[i] += frequencyCost->CostSums.Data[i]; // sums
        overall_count.Data[i] += frequencyCost->CostCounts.Data[i];
      }
    }

    if (cancel)
      return;

    if (mDoAuc) {
      auc->Calculate(y1, model.PredProbs,
                     hasWeight ? (mWeightedEval ? &w1 : nullptr) : nullptr,
                     aucOptions);
      this->Auc += auc->Result; // sum
    }

    if (cancel)
      return;

    if (mDoFrequecyTable) {
      Tv yi, wi = 1, actProb;
      Ti yc;
      for (i = 0; i < this->N1; i++) {
        yi = y1.Data[i];
        if constexpr (hasWeight) {
          wi = w1.Data[i];
        }
        yc = static_cast<Ti>(yi);
        actProb = model.PredProbs.Get0(i, yc);
        if (std::isnan(actProb)) {
          throw LdtException(ErrorType::kLogic, "dc-sim", "probability is nan");
        }
        j = static_cast<Ti>(std::floor(actProb * 10)); // if actual=0.1 => j=1
        if (j == 10) // if actual = 0.1 = > j = 10
          j = 9;
        this->FrequencyTable.Set_Plus0(j, yc, wi);
      }
    }
    invalidCounts--;
    if (invalidCounts > maxInvalidSim)
      throw LdtException(ErrorType::kLogic, "dc-sim",
                         "model check: minimum valid simulations");

    this->ValidSimulationCount++;
  }

  if (cancel)
    return;

  if (invalidCounts > maxInvalidSim)
    throw LdtException(ErrorType::kLogic, "dc-sim",
                       "model check: minimum valid simulations");

  if (this->ValidSimulationCount == 0)
    throw LdtException(ErrorType::kLogic, "dc-sim",
                       "no valid simulation is available");

  for (i = 0; i < costCount; i++)
    this->CostRatios.Data[i] /= overall_count.Data[i];
  this->Auc /= this->ValidSimulationCount;
}

DiscreteChoiceSimBase *DiscreteChoiceSimBase::GetFromType(
    bool hasWeight, DiscreteChoiceModelType modelType,
    DiscreteChoiceDistType distType, Ti numObs, Ti numExo, Ti numChoices,
    Tv trainPercentage, Ti trainFixSize, Ti costMatrixCount, bool doBrier,
    bool doAuc, bool doFrequecyTable, PcaAnalysisOptions *pcaOptions,
    bool weightedEval) {

  DiscreteChoiceSimBase *d = nullptr;

  if (hasWeight) {

    switch (modelType) {
    case ldt::DiscreteChoiceModelType::kBinary:
      switch (distType) {
      case ldt::DiscreteChoiceDistType::kLogit:
        d = new DiscreteChoiceSim<true, DiscreteChoiceModelType::kBinary,
                                  DiscreteChoiceDistType::kLogit>(
            numObs, numExo, numChoices, trainPercentage, trainFixSize,
            costMatrixCount, doBrier, doAuc, doFrequecyTable, pcaOptions,
            weightedEval);
        break;
      case ldt::DiscreteChoiceDistType::kProbit:
        d = new DiscreteChoiceSim<true, DiscreteChoiceModelType::kBinary,
                                  DiscreteChoiceDistType::kProbit>(
            numObs, numExo, numChoices, trainPercentage, trainFixSize,
            costMatrixCount, doBrier, doAuc, doFrequecyTable, pcaOptions,
            weightedEval);
        break;
      default:
        throw LdtException(
            ErrorType::kLogic, "dc-sim",
            "not implemented (distribution type in discrete choice "
            "simulation)");
      }
      break;
    case ldt::DiscreteChoiceModelType::kOrdered:
      switch (distType) {
      case ldt::DiscreteChoiceDistType::kLogit:
        d = new DiscreteChoiceSim<true, DiscreteChoiceModelType::kOrdered,
                                  DiscreteChoiceDistType::kLogit>(
            numObs, numExo, numChoices, trainPercentage, trainFixSize,
            costMatrixCount, doBrier, doAuc, doFrequecyTable, pcaOptions,
            weightedEval);
        break;
      case ldt::DiscreteChoiceDistType::kProbit:
        d = new DiscreteChoiceSim<true, DiscreteChoiceModelType::kOrdered,
                                  DiscreteChoiceDistType::kProbit>(
            numObs, numExo, numChoices, trainPercentage, trainFixSize,
            costMatrixCount, doBrier, doAuc, doFrequecyTable, pcaOptions,
            weightedEval);
        break;
      default:
        throw LdtException(
            ErrorType::kLogic, "dc-sim",
            "not implemented (distribution type in discrete choice "
            "simulation)");
      }
      break;
    default:
      throw LdtException(
          ErrorType::kLogic, "dc-sim",
          "not implemented (Model type in discrete choice simulation)");
    }

  } else {

    switch (modelType) {
    case ldt::DiscreteChoiceModelType::kBinary:
      switch (distType) {
      case ldt::DiscreteChoiceDistType::kLogit:
        d = new DiscreteChoiceSim<false, DiscreteChoiceModelType::kBinary,
                                  DiscreteChoiceDistType::kLogit>(
            numObs, numExo, numChoices, trainPercentage, trainFixSize,
            costMatrixCount, doBrier, doAuc, doFrequecyTable, pcaOptions,
            weightedEval);
        break;
      case ldt::DiscreteChoiceDistType::kProbit:
        d = new DiscreteChoiceSim<false, DiscreteChoiceModelType::kBinary,
                                  DiscreteChoiceDistType::kProbit>(
            numObs, numExo, numChoices, trainPercentage, trainFixSize,
            costMatrixCount, doBrier, doAuc, doFrequecyTable, pcaOptions,
            weightedEval);
        break;
      default:
        throw LdtException(
            ErrorType::kLogic, "dc-sim",
            "not implemented (distribution type in discrete choice "
            "simulation)");
      }
      break;
    case ldt::DiscreteChoiceModelType::kOrdered:
      switch (distType) {
      case ldt::DiscreteChoiceDistType::kLogit:
        d = new DiscreteChoiceSim<false, DiscreteChoiceModelType::kOrdered,
                                  DiscreteChoiceDistType::kLogit>(
            numObs, numExo, numChoices, trainPercentage, trainFixSize,
            costMatrixCount, doBrier, doAuc, doFrequecyTable, pcaOptions,
            weightedEval);
        break;
      case ldt::DiscreteChoiceDistType::kProbit:
        d = new DiscreteChoiceSim<false, DiscreteChoiceModelType::kOrdered,
                                  DiscreteChoiceDistType::kProbit>(
            numObs, numExo, numChoices, trainPercentage, trainFixSize,
            costMatrixCount, doBrier, doAuc, doFrequecyTable, pcaOptions,
            weightedEval);
        break;
      default:
        throw LdtException(
            ErrorType::kLogic, "dc-sim",
            "not implemented (distribution type in discrete choice "
            "simulation)");
      }
      break;
    default:
      throw LdtException(
          ErrorType::kLogic, "dc-sim",
          "not implemented (Model type in discrete choice simulation)");
    }
  }

  return d;
}

template class ldt::DiscreteChoiceSim<true, DiscreteChoiceModelType::kBinary,
                                      DiscreteChoiceDistType::kLogit>;
template class ldt::DiscreteChoiceSim<true, DiscreteChoiceModelType::kBinary,
                                      DiscreteChoiceDistType::kProbit>;

template class ldt::DiscreteChoiceSim<true, DiscreteChoiceModelType::kOrdered,
                                      DiscreteChoiceDistType::kLogit>;
template class ldt::DiscreteChoiceSim<true, DiscreteChoiceModelType::kOrdered,
                                      DiscreteChoiceDistType::kProbit>;

template class ldt::DiscreteChoiceSim<false, DiscreteChoiceModelType::kBinary,
                                      DiscreteChoiceDistType::kLogit>;
template class ldt::DiscreteChoiceSim<false, DiscreteChoiceModelType::kBinary,
                                      DiscreteChoiceDistType::kProbit>;

template class ldt::DiscreteChoiceSim<false, DiscreteChoiceModelType::kOrdered,
                                      DiscreteChoiceDistType::kLogit>;
template class ldt::DiscreteChoiceSim<false, DiscreteChoiceModelType::kOrdered,
                                      DiscreteChoiceDistType::kProbit>;
