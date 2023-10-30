/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the GPL 3.0.
 See accompanying file LICENSE
*/

#include "searchers.h"

using namespace ldt;

Searcher::Searcher(const SearchData &data,
                   const SearchCombinations &combinations,
                   SearchOptions &options, const SearchItems &items,
                   const SearchMetricOptions &metrics,
                   const SearchModelChecks &checks, Ti numPartitions,
                   bool checkForEmpty) {

  if (combinations.NumFixPartitions > numPartitions)
    throw LdtException(
        ErrorType::kLogic, "searcher",
        "fixed number of partitions cannot be larger than length of "
        "the array in the searcher");

  if (items.LengthEvals == 0 || items.LengthTargets == 0)
    throw LdtException(ErrorType::kLogic, "searcher",
                       "no evaluation or target is given");

  if (items.LengthEvals != (Ti)metrics.EvalIsPosOrientation.size())
    throw LdtException(ErrorType::kLogic, "searcher",
                       "metric orientations are not provided.");

  if (items.KeepModelEvaluations == false && items.Length1 == 0 &&
      items.Length2 == 0)
    throw LdtException(ErrorType::kLogic, "searcher",
                       "nothing is saved in the searcher. Check the items");

  pData = &data;
  pCombinations = &combinations;
  pOptions = &options;
  pItems = &items;
  pChecks = &checks;
  pMetrics = &metrics;
  this->NumPartitions = numPartitions;

  CheckForEmpty = checkForEmpty;

  mIsFinished = false;
  States.clear();

  CurrentIndices = VMatrix<Ti>(NumPartitions, 1);
  PartitionIndices = VMatrix<Ti>(NumPartitions, 1);
  ItemIndices = VMatrix<Ti>(NumPartitions, 1);

  CurrentIndices.Mat.SetValue(0);
  PartitionIndices.Mat.SetSequence(0, 1);
  ItemIndices.Mat.SetValue(0);

  for (Ti i = 0; i < (Ti)combinations.Partitions.size(); i++)
    partitionSizes.push_back(combinations.Partitions.at(i).size());

  Summaries0 = std::vector<std::vector<SearcherSummary>>(items.LengthEvals);
  Summaries1 =
      std::vector<std::vector<std::vector<SearcherSummary>>>(items.LengthEvals);
  Summaries2 =
      std::vector<std::vector<std::vector<SearcherSummary>>>(items.LengthEvals);

  for (int i = 0; i < items.LengthEvals; i++) { // evaluations
    Summaries0.at(i) = std::vector<SearcherSummary>(items.LengthTargets);
    Summaries1.at(i) =
        std::vector<std::vector<SearcherSummary>>(items.LengthTargets);
    Summaries2.at(i) =
        std::vector<std::vector<SearcherSummary>>(items.LengthTargets);

    for (int k = 0; k < items.LengthTargets; k++) { // targets
      Summaries0.at(i).at(k) = SearcherSummary(
          i, k, 0, pItems, metrics.EvalIsPosOrientation.at(i), pData);
      Summaries1.at(i).at(k) = std::vector<SearcherSummary>(items.Length1);
      Summaries2.at(i).at(k) = std::vector<SearcherSummary>(items.Length2);

      for (int j = 0; j < items.Length1; j++)
        Summaries1.at(i).at(k).at(j) = SearcherSummary(
            i, k, j, pItems, metrics.EvalIsPosOrientation.at(i), pData);

      for (int j = 0; j < items.Length2; j++)
        Summaries2.at(i).at(k).at(j) = SearcherSummary(
            i, k, j, pItems, metrics.EvalIsPosOrientation.at(i), pData);
    }
  }
}

void Searcher::AddState(std::string state) {
  Counter++;
  if (state.empty())
    return;
  if (States.find(state) != States.end())
    States.at(state)++;
  else {
    States.insert(std::pair<std::string, Ti>(state, 1));
  }
}

void Searcher::Push0(std::shared_ptr<EstimationKeep> &coef, Ti evalIndex,
                     Ti targetIndex) {
  Summaries0.at(evalIndex).at(targetIndex).Push(coef, true);
}

void Searcher::Push1(std::shared_ptr<EstimationKeep> &coef, Ti evalIndex,
                     Ti targetIndex, Ti thirdIndex) {
  Summaries1.at(evalIndex).at(targetIndex).at(thirdIndex).Push(coef, false);
}

void Searcher::Push2(std::shared_ptr<EstimationKeep> &coef, Ti evalIndex,
                     Ti targetIndex, Ti thirdIndex) {
  Summaries2.at(evalIndex).at(targetIndex).at(thirdIndex).Push(coef, false);
}

void Searcher::UpdateCurrent() {
  for (Ti i = 0; i < NumPartitions; i++) {
    CurrentIndices.Mat.Data[i] =
        pCombinations->Partitions.at(PartitionIndices.Mat.Data[i])
            .at(ItemIndices.Mat.Data[i]);
  }
}

void Searcher::CheckStart() {
  if (mIsFinished)
    throw LdtException(ErrorType::kLogic, "searcher",
                       "you cannot reuse this class: search is finished");
  if ((Ti)pCombinations->Partitions.size() < NumPartitions)
    throw LdtException(
        ErrorType::kLogic, "searcher",
        std::string("number of groups is not enough to build the "
                    "model with the given size. Size of model=") +
            std::to_string(NumPartitions) + std::string(", Number of groups=") +
            std::to_string((Ti)pCombinations->Partitions.size()));
}

std::string Searcher::Start(Tv *work, Ti *workI) {
  CheckStart(); // separated because if you want to run it in async function,
                // you cannot leave exceptions unhandled

  if (PartitionIndices.Mat.length() == 0)
    return "";
  if (pOptions->RequestCancel == true)
    return "";

  try {
    Ti c, T, free;

    try {
      UpdateCurrent(); // e.g., update 0,0,0
      AddState(EstimateOne(work, workI));
    } catch (std::exception &ex) {
      AddState(std::string(ex.what()));
    } catch (std::string &ex) {
      AddState(ex);
    } catch (const char *ex) {
      AddState(std::string(ex));
    } catch (...) {
      AddState(std::string("unknown error occurred"));
    }

    while (true) {
      if (MoveNext(c, T, free) == false)
        break;
      if (pOptions->RequestCancel == true)
        break;

      try {
        UpdateCurrent();

        if (CheckForEmpty &&
            CurrentIndices.Vec.at(0) >= this->pItems->LengthTargets)
          continue; // it is empty and we did not count it in the searcher. See
                    // CheckForEmpty member.

        AddState(EstimateOne(work, workI));
      } catch (std::exception &ex) {
        AddState(std::string(ex.what()));
      } catch (std::string &ex) {
        AddState(ex);
      } catch (const char *ex) {
        AddState(std::string(ex));
      } catch (...) {
        AddState(std::string("unknown error occurred"));
      }
    }

    mIsFinished = true;
    // finalize
  } catch (...) {
    // throw;
    //  finalize (of course, this should no happen)
    mIsFinished = true;
  }
  return "";
}

bool next(Ti *g_data, const Ti &numPartitions, const Ti &maxCountG,
          const Ti &mFixFirstPartitions, Ti &c, Ti &T, Ti &free) {
  c = 0;

  for (free = numPartitions; free > mFixFirstPartitions; free--) {
    T = maxCountG - c - 1;
    if (g_data[free - 1] < T)
      break;
    c++;
  }
  if (free == mFixFirstPartitions)
    return false;
  else {
    g_data[free - 1]++;
    for (Ti i = free; i < numPartitions; i++)
      g_data[i] = g_data[i - 1] + 1;
  }
  return true;
}

static bool move_next(Ti &c, Ti &T, Ti &free, Matrix<Ti> &innerIndices,
                      Matrix<Ti> &partitionIndices, const Ti &numPartitions,
                      const std::vector<Ti> &partitionSizes,
                      const std::vector<std::vector<Ti>> &partitions,
                      const Ti &fixFirstG, const Ti &fixFirstI) {
  // move the inner indexes
  if ((Ti)partitions.size() <= partitionIndices.Data[0])
    throw std::logic_error("error 1");

  auto g = &partitions.at(partitionIndices.Data[0]);

  for (Ti i = 0; i < numPartitions; i++) {

    if ((Ti)partitionSizes.size() <= partitionIndices.Data[i])
      throw std::logic_error("error 4");

    if (innerIndices.Data[i] <
        partitionSizes.at(partitionIndices.Data[i]) - 1) {
      innerIndices.Data[i]++;

      if (fixFirstI == 0)
        return true;
      // does it contain any fixed items?
      else if ((int)g->size() > innerIndices.Data[0] &&
               g->at(innerIndices.Data[0]) < fixFirstI)
        return true;
    }

    innerIndices.Data[i] = 0;
  }

  // reset inner indexes
  innerIndices.SetValue(0);

  // move groups
  if (next(partitionIndices.Data, numPartitions, (Ti)partitions.size(),
           fixFirstG, c, T, free)) {
    if (fixFirstI == 0)
      return true;
    else { // does it contain any fixed items?

      if ((Ti)partitions.size() <= partitionIndices.Data[0])
        throw std::logic_error("error 2");

      g = &partitions.at(partitionIndices.Data[0]);

      if ((Ti)g->size() <= innerIndices.Data[0])
        throw std::logic_error("error 3");

      if ((Ti)g->size() > innerIndices.Data[0] &&
          g->at(innerIndices.Data[0]) < fixFirstI)
        return true;
    }
  }

  return false;
}

bool Searcher::MoveNext(Ti &c, Ti &T, Ti &free) {
  return move_next(c, T, free, ItemIndices.Mat, PartitionIndices.Mat,
                   NumPartitions, partitionSizes, pCombinations->Partitions,
                   pCombinations->NumFixPartitions, pCombinations->NumFixItems);
}

Ti Searcher::GetCount(bool effective) const {
  // auto r = NumPartitions - mFixFirstPartitions;
  // auto n = pGroupIndexMap->size() - mFixFirstPartitions;
  // auto countG = choose((Ti)n, (Ti)r); // use boost 'binomial_coefficient'

  // simulate indexation in groups and multiply sizes

  if ((Ti)pCombinations->Partitions.size() < NumPartitions)
    throw LdtException(
        ErrorType::kLogic, "searcher",
        std::string(
            "invalid number of partitions. It is not enough to build the "
            "model with the given size. Size of model=") +
            std::to_string(NumPartitions) +
            std::string(", Number of partitions=") +
            std::to_string((Ti)pCombinations->Partitions.size()));

  Ti count;
  if (pCombinations->NumFixItems == 0 && CheckForEmpty == false) {

    count = 0;
    auto partition_indices = VMatrix<Ti>(NumPartitions, 1);
    partition_indices.Mat.SetSequence(0, 1);

    // first
    Ti mul = 1;
    for (Ti i = 0; i < NumPartitions; i++)
      mul *= partitionSizes.at(i);
    count += mul;

    Ti c, T, free;
    while (next(partition_indices.Mat.Data, NumPartitions,
                (Ti)pCombinations->Partitions.size(),
                pCombinations->NumFixPartitions, c, T, free)) {
      mul = 1;
      for (Ti i = 0; i < NumPartitions; i++)
        mul *= partitionSizes.at(partition_indices.Mat.Data[i]);
      count += mul;
    }
  } else {

    auto partition_indices = VMatrix<Ti>(NumPartitions, 1);
    partition_indices.Mat.SetSequence(0, 1);
    auto item_indices = VMatrix<Ti>(NumPartitions, 1);
    item_indices.Mat.SetValue(0);

    Ti c, T, free;
    count = 1;
    while (move_next(c, T, free, item_indices.Mat, partition_indices.Mat,
                     NumPartitions, partitionSizes, pCombinations->Partitions,
                     pCombinations->NumFixPartitions,
                     pCombinations->NumFixItems)) {
      if (CheckForEmpty) {
        auto c0 = pCombinations->Partitions.at(partition_indices.Mat.Data[0])
                      .at(item_indices.Mat.Data[0]);
        if (c0 >= this->pItems->LengthTargets)
          continue;
      }
      count++;
    }
  }

  // multiply
  if (effective)
    return (Ti)(count * std::pow((Tv)NumPartitions, (Tv)2)); // consider 'size'
  else
    return (Ti)(count);
}

// #pragma region Test

std::string SearcherTest::EstimateOne(Tv *work, Ti *workI) {
  auto ek = std::make_shared<EstimationKeep>(1, NAN, std::vector<Ti>(),
                                             std::vector<Ti>(),
                                             this->CurrentIndices.Vec, 0, 0);
  this->Push0(ek, 0, 0);
  return "";
}

// #pragma endregion
