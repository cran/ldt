/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the GPL 3.0.
 See accompanying file LICENSE
*/

#include "searchers.h"

using namespace ldt;

Searcher::Searcher(SearchOptions &searchOptions, const SearchItems &searchItems,
                   const SearchMetricOptions &metrics,
                   const SearchModelChecks &checks, Ti SizeG,
                   const std::vector<std::vector<Ti>> &groupIndexMap,
                   Ti fixFirstGroups, Ti fixFirstItems) {

  if (fixFirstGroups > SizeG)
    throw LdtException(
        ErrorType::kLogic, "searcher",
        "fixed number of groups cannot be larger than Size in the searcher");

  pOptions = &searchOptions;
  pItems = &searchItems;
  pChecks = &checks;
  pMetrics = &metrics;

  this->SizeG = SizeG;
  this->mFixFirstGroups = fixFirstGroups;
  this->mFixFirstItems = fixFirstItems;
  this->pGroupIndexMap = &groupIndexMap;

  for (auto i = 0; i < (int)this->pGroupIndexMap->size(); i++)
    this->GroupSizes.push_back(this->pGroupIndexMap->at(i).size());

  GroupIndexes = Matrix<Ti>(new Ti[SizeG], SizeG, (Ti)1);
  InnerIndexes = Matrix<Ti>(new Ti[SizeG], SizeG, (Ti)1);

  CurrentIndicesV = std::vector<Ti>(SizeG);
  CurrentIndices = Matrix<Ti>(&CurrentIndicesV[0], SizeG, 1);

  mIsFinished = false;
  States.clear();

  CurrentIndices.SetValue(0);

  GroupIndexes.SetSequence(0, 1);
  InnerIndexes.SetValue(0);

  if (searchItems.LengthEvals == 0 || searchItems.LengthTargets == 0)
    throw LdtException(ErrorType::kLogic, "searcher",
                       "no evaluation or target is given");
  if (searchItems.KeepModelEvaluations == false && searchItems.Length1 == 0 &&
      searchItems.Length2 == 0)
    throw LdtException(
        ErrorType::kLogic, "searcher",
        "nothing is saved in the searcher. Check the searchItems");

  Summaries0 =
      std::vector<std::vector<SearcherSummary>>(searchItems.LengthEvals);
  Summaries1 = std::vector<std::vector<std::vector<SearcherSummary>>>(
      searchItems.LengthEvals);
  Summaries2 = std::vector<std::vector<std::vector<SearcherSummary>>>(
      searchItems.LengthEvals);

  for (int i = 0; i < searchItems.LengthEvals; i++) { // evaluations
    Summaries0.at(i) = std::vector<SearcherSummary>(searchItems.LengthTargets);
    Summaries1.at(i) =
        std::vector<std::vector<SearcherSummary>>(searchItems.LengthTargets);
    Summaries2.at(i) =
        std::vector<std::vector<SearcherSummary>>(searchItems.LengthTargets);

    for (int k = 0; k < searchItems.LengthTargets; k++) { // targets

      Summaries0.at(i).at(k) = SearcherSummary(i, k, 0, pItems);

      Summaries1.at(i).at(k) =
          std::vector<SearcherSummary>(searchItems.Length1);
      Summaries2.at(i).at(k) =
          std::vector<SearcherSummary>(searchItems.Length2);

      for (int j = 0; j < searchItems.Length1; j++)
        Summaries1.at(i).at(k).at(j) = SearcherSummary(i, k, j, pItems);

      for (int j = 0; j < searchItems.Length2; j++)
        Summaries2.at(i).at(k).at(j) = SearcherSummary(i, k, j, pItems);
    }
  }
}

Searcher::~Searcher() {
  delete[] GroupIndexes.Data;
  delete[] InnerIndexes.Data;
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

void Searcher::Push0(EstimationKeep &coef, Ti evalIndex, Ti targetIndex,
                     Matrix<Ti> *overrideInclusioExo) {
  Summaries0.at(evalIndex)
      .at(targetIndex)
      .Push(coef, true, overrideInclusioExo);
}

void Searcher::Push1(EstimationKeep &coef, Ti evalIndex, Ti targetIndex,
                     Ti thirdIndex) {
  Summaries1.at(evalIndex).at(targetIndex).at(thirdIndex).Push(coef, false);
}

void Searcher::Push2(EstimationKeep &coef, Ti evalIndex, Ti targetIndex,
                     Ti thirdIndex) {
  Summaries2.at(evalIndex).at(targetIndex).at(thirdIndex).Push(coef, false);
}

void Searcher::UpdateCurrent() {
  for (Ti i = 0; i < SizeG; i++) {
    CurrentIndices.Data[i] =
        pGroupIndexMap->at(GroupIndexes.Data[i]).at(InnerIndexes.Data[i]);
  }
}

void Searcher::CheckStart() {
  if (mIsFinished)
    throw LdtException(ErrorType::kLogic, "searcher",
                       "you cannot reuse this class: search is finished");
  if ((Ti)pGroupIndexMap->size() < SizeG)
    throw LdtException(
        ErrorType::kLogic, "searcher",
        std::string("number of groups is not enough to build the "
                    "model with the given size. Size of model=") +
            std::to_string(SizeG) + std::string(", Number of groups=") +
            std::to_string((Ti)pGroupIndexMap->size()));
}

std::string Searcher::Start(Tv *work, Ti *workI) {
  CheckStart(); // separated because if you want to run it in async function,
                // you cannot leave exceptions unhandled

  if (GroupIndexes.length() == 0)
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

bool next(Ti *g_data, const Ti &SizeG, const Ti &maxCountG,
          const Ti &mFixFirstGroups, Ti &c, Ti &T, Ti &free) {
  c = 0;

  for (free = SizeG; free > mFixFirstGroups; free--) {
    T = maxCountG - c - 1;
    if (g_data[free - 1] < T)
      break;
    c++;
  }
  if (free == mFixFirstGroups)
    return false;
  else {
    g_data[free - 1]++;
    for (Ti i = free; i < SizeG; i++)
      g_data[i] = g_data[i - 1] + 1;
  }
  return true;
}

static bool move_next(Ti &c, Ti &T, Ti &free, Matrix<Ti> &innerIndexes,
                      Matrix<Ti> &groupIndexes, const Ti &sizeG,
                      const std::vector<Ti> &groupSizes,
                      const std::vector<std::vector<Ti>> &groupIndexMap,
                      const Ti &fixFirstG, const Ti &fixFirstI) {
  // move the inner indexes
  auto g = &groupIndexMap.at(groupIndexes.Data[0]);
  for (Ti i = 0; i < sizeG; i++) {
    if (innerIndexes.Data[i] < groupSizes.at(groupIndexes.Data[i]) - 1) {
      innerIndexes.Data[i]++;

      if (fixFirstI == 0)
        return true;
      // does it contain any fixed items?
      else if ((int)g->size() > innerIndexes.Data[0] &&
               g->at(innerIndexes.Data[0]) < fixFirstI)
        return true;
    }
    innerIndexes.Data[i] = 0;
  }

  // reset inner indexes
  innerIndexes.SetValue(0);

  // move groups
  if (next(groupIndexes.Data, sizeG, (Ti)groupIndexMap.size(), fixFirstG, c, T,
           free)) {

    if (fixFirstI == 0)
      return true;
    else { // does it contain any fixed items?
      auto g = &groupIndexMap.at(groupIndexes.Data[0]);
      if ((Ti)g->size() > innerIndexes.Data[0] &&
          g->at(innerIndexes.Data[0]) < fixFirstI)
        return true;
    }
  }
  return false;
}

bool Searcher::MoveNext(Ti &c, Ti &T, Ti &free) {
  return move_next(c, T, free, InnerIndexes, GroupIndexes, SizeG, GroupSizes,
                   *pGroupIndexMap, mFixFirstGroups, mFixFirstItems);
}

Ti Searcher::GetCount(bool effective) const {
  // auto r = SizeG - mFixFirstGroups;
  // auto n = pGroupIndexMap->size() - mFixFirstGroups;
  // auto countG = choose((Ti)n, (Ti)r); // use boost 'binomial_coefficient'

  // simulate indexation in groups and multiply sizes

  if ((Ti)pGroupIndexMap->size() < SizeG)
    throw LdtException(
        ErrorType::kLogic, "searcher",
        std::string("invalid number of groups. It is not enough to build the "
                    "model with the given size. Size of model=") +
            std::to_string(SizeG) + std::string(", Number of groups=") +
            std::to_string((Ti)pGroupIndexMap->size()));

  Ti count;
  if (mFixFirstItems == 0) {
    count = 0;
    auto td = std::unique_ptr<Ti[]>(new Ti[SizeG]);
    auto t = Matrix<Ti>(td.get(), SizeG, (Ti)1);
    t.SetSequence(0, 1);

    // first
    Ti mul = 1;
    for (Ti i = 0; i < SizeG; i++)
      mul *= GroupSizes.at(i);
    count += mul;

    Ti c, T, free;
    while (next(t.Data, SizeG, (Ti)pGroupIndexMap->size(), mFixFirstGroups, c,
                T, free)) {
      mul = 1;
      for (Ti i = 0; i < SizeG; i++)
        mul *= GroupSizes.at(t.Data[i]);
      count += mul;
    }
  } else {

    auto grpd = std::unique_ptr<Ti[]>(new Ti[SizeG]);
    auto grp = Matrix<Ti>(grpd.get(), SizeG, (Ti)1);
    grp.SetSequence(0, 1);
    auto indd = std::unique_ptr<Ti[]>(new Ti[SizeG]);
    auto ind = Matrix<Ti>(indd.get(), SizeG, (Ti)1);
    ind.SetValue(0);

    Ti c, T, free;
    count = 1;
    while (move_next(c, T, free, ind, grp, SizeG, GroupSizes, *pGroupIndexMap,
                     mFixFirstGroups, mFixFirstItems)) {
      count++;
    }
  }

  // multiply
  if (effective)
    return (Ti)(count * std::pow((Tv)SizeG, (Tv)2)); // consider 'size'
  else
    return (Ti)(count);
}

// #pragma region Test

std::string SearcherTest::EstimateOne(Tv *work, Ti *workI) {
  this->Push0(*new EstimationKeep(1, {}, {}, &this->CurrentIndices, 0, 0), 0,
              0);
  return "";
}

// #pragma endregion
