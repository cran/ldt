/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the GPL 3.0.
 See accompanying file LICENSE
*/

#include <algorithm>
// #include <execution>
#ifdef _OPENMP
#include <omp.h>
#endif
#include <random>

#include "searchers.h"

using namespace ldt;

ModelSet::ModelSet(std::vector<Searcher *> &searchers, const SearchData &data,
                   const SearchCombinations &combinations,
                   const SearchOptions &options, const SearchItems &items,
                   const SearchMetricOptions &metrics,
                   const SearchModelChecks &checks) {
  pSearchers = &searchers;

  pData = &data;
  pCombinations = &combinations;
  pOptions = &options;
  pItems = &items;
  pChecks = &checks;
  pMetrics = &metrics;

  // calculate sizes
  WorkSize = 0;
  WorkSizeI = 0;
  if (pOptions->Parallel) {
    // WorkSize += s->WorkSize; this is not very practical
    // WorkSizeI += s->WorkSizeI;
  } else {
    for (auto s : searchers) { // we can share work

      WorkSize = std::max(WorkSize, s->WorkSize);
      WorkSizeI = std::max(WorkSizeI, s->WorkSizeI);
    }
  }
}

void ModelSet::Start(Tv *work, Ti *workI) {

  if (ShuffleSearchers) {
    // Shuffle (in order to distribute sizes)
    std::random_device rdev{};
    std::mt19937 eng = std::mt19937(rdev());
    std::shuffle(this->pSearchers->begin(), this->pSearchers->end(), eng);
  }

  // loop
  if (pOptions->Parallel) {
    /*
    // distribute work
    auto works = std::map<Ti, std::tuple<Tv*, Ti*>>();
    Ti i = 0;
    Ti a = 0;
    Ti b = 0;
    for (auto& s : *pSearchers) {
        s->mId = i;
        works.insert(std::pair<Ti, std::tuple<Tv*, Ti*>>(i,
    std::tuple(&work[a], &workI[b]))); a += s->WorkSize; b +=
    s->WorkSizeI; i++;
    }
    */

#ifdef _OPENMP
#pragma omp parallel for
    for (Ti i = 0; i < (Ti)pSearchers->size(); i++) {
      auto item = pSearchers->at(i);
      auto w = std::unique_ptr<Tv[]>(new Tv[item->WorkSize]);
      auto wI = std::unique_ptr<Ti[]>(new Ti[item->WorkSizeI]);
      item->Start(w.get(), wI.get());
    }
#else
    throw LdtException(ErrorType::kLogic, "sur-modelset",
                       "parallel execution is not supported");
#endif

    /*std::for_each(std::execution::par, pSearchers->begin(), pSearchers->end(),
                  [&](auto &&item) {
                    auto w =  std::unique_ptr<Tv[]>(new Tv[item->WorkSize]);
                    auto wI = std::unique_ptr<Ti[]>(new Ti[item->WorkSizeI]);
                    item->Start(w.get(), wI.get());
                  });*/
  } else {
    for (auto &s : *pSearchers) {
      if (pOptions->RequestCancel == true)
        break;
      s->Start(work, workI);
    }
  }
}

Ti ModelSet::GetNumberOfEstimatedModels() const {
  Ti c = 0;
  for (auto &a : *pSearchers) {
    c += a->Counter;
  }
  return c;
}

Ti ModelSet::GetExpectedNumberOfModels() const {

  Ti c = 0;
  for (auto &a : *pSearchers) {
    c += a->GetCount();
  }
  return c;
}

// #pragma region combines

void ModelSet::CombineInfo(SearcherModelingInfo &result,
                           std::vector<SearcherSummary *> &list0,
                           std::vector<SearcherSummary *> &list1,
                           std::vector<SearcherSummary *> &list2) const {

  result.ExpectedCount = GetExpectedNumberOfModels();
  result.SearchedCount = GetNumberOfEstimatedModels();

  for (auto &srchr : *pSearchers) {
    for (auto &a : srchr->States) {
      if (result.FailsCount.find(a.first) != result.FailsCount.end())
        result.FailsCount.at(a.first) += a.second;
      else {
        result.FailsCount.insert(std::pair<std::string, Ti>(a.first, a.second));
      }
    }

    if (srchr->pItems->KeepModelEvaluations) {
      for (auto &a : srchr->Summaries0) {
        for (auto &b : a)
          list0.push_back(&b);
      }
    }
    if (srchr->pItems->Length1 > 0) {
      for (auto &a : srchr->Summaries1) {
        for (auto &b : a) {
          for (auto &c : b) { // summary
            list1.push_back(&c);
          }
        }
      }
    }
    if (srchr->pItems->Length2 > 0) {
      for (auto &a : srchr->Summaries2) {
        for (auto &b : a) {
          for (auto &c : b) { // summary
            list2.push_back(&c);
          }
        }
      }
    }
  }
}

void ModelSet::CombineAll(const Ti &index1, const Ti &index2, const Ti &index3,
                          const std::vector<SearcherSummary *> &summaries,
                          std::vector<EstimationKeep *> &result) const {
  if (summaries.size() == 0)
    throw LdtException(ErrorType::kLogic, "sur-modelset",
                       "list of search summaries is empty!");

  for (auto &s : summaries) {
    if (s->Index1 == index1 && s->Index2 == index2 && s->Index3 == index3) {
      for (auto &w : s->All) { // TODO: push vector
        result.push_back(w);
      }
    }
  }
}

void ModelSet::CombineBests(const Ti &index1, const Ti &index2,
                            const Ti &index3,
                            const std::vector<SearcherSummary *> &summaries,
                            std::vector<EstimationKeep *> &result) const {
  if (summaries.size() == 0)
    throw LdtException(ErrorType::kLogic, "sur-modelset",
                       "list of search summaries is empty!");

  auto k = summaries.at(0)->pItems->KeepBestCount;
  if (k <= 0)
    return;
  for (auto &s : summaries) {
    if (s->Index1 == index1 && s->Index2 == index2 && s->Index3 == index3) {
      for (auto &w : s->Bests) {
        for (Ti i = 0; i < k; i++) {
          if (i >= (Ti)result.size())
            result.push_back(w);
          else if (w->Weight > result.at(i)->Weight)
            result.insert(result.begin() + i, w);
          else
            continue;

          if ((Ti)result.size() > k) {
            result.pop_back();
          }
          break;
        }
      }
    }
  }
}

void ModelSet::CombineInclusionWeights(
    const Ti &index1, const Ti &index2, const Ti &index3,
    const std::vector<SearcherSummary *> &summaries,
    RunningMoments<1, true, false, Tv> &result) const {
  if (summaries.size() == 0)
    throw LdtException(ErrorType::kLogic, "sur-modelset",
                       "list of search summaries is empty!");

  result.Reset();

  for (auto &s : summaries) {
    if (s->Index1 == index1 && s->Index2 == index2) {
      result.Combine(s->InclusionsInfo.at(index3));
    }
  }
}

void ModelSet::CombineCdfAt(const Ti &index1, const Ti &index2,
                            const Ti &index3, const Ti &cdfIndex,
                            const std::vector<SearcherSummary *> &summaries,
                            RunningMoments<1, true, true, Tv> &result) const {
  if (summaries.size() == 0)
    throw LdtException(ErrorType::kLogic, "sur-modelset",
                       "list of search summaries is empty!");

  result.Reset();

  for (auto &s : summaries) {
    if (s->Index1 == index1 && s->Index2 == index2 && s->Index3 == index3) {
      result.Combine(s->Cdfs.at(cdfIndex));
    }
  }
}

void ModelSet::CombineExtremeBounds(
    const Ti &index1, const Ti &index2, const Ti &index3,
    const std::vector<SearcherSummary *> &summaries, double &min,
    double &max) const {
  if (summaries.size() == 0)
    throw LdtException(ErrorType::kLogic, "sur-modelset",
                       "list of search summaries is empty!");

  min = std::numeric_limits<Tv>::max();
  max = std::numeric_limits<Tv>::min();

  for (auto &s : summaries) {
    if (s->Index1 == index1 && s->Index2 == index2 && s->Index3 == index3) {
      if (min > s->ExtremeBounds.at(0))
        min = s->ExtremeBounds.at(0);
      if (max < s->ExtremeBounds.at(1))
        max = s->ExtremeBounds.at(1);
    }
  }
}

void ModelSet::CombineMixture(const Ti &index1, const Ti &index2,
                              const Ti &index3,
                              const std::vector<SearcherSummary *> &summaries,
                              RunningMoments<4, true, true, Tv> &result) const {
  if (summaries.size() == 0)
    throw LdtException(ErrorType::kLogic, "sur-modelset",
                       "list of search summaries is empty!");

  result.Reset();
  for (auto &s : summaries) {
    if (s->Index1 == index1 && s->Index2 == index2 && s->Index3 == index3) {
      result.Combine(s->Mixture4);
    }
  }
}

// #pragma endregion
