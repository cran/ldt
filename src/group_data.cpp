/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the LGPL 3.0.
 See accompanying file LICENSE
*/

#include "clustering.h"
#include <set>
#include <stack>
using namespace ldt;

template <HClusterLinkage linkMethod, DistanceMethod distMethod,
          CorrelationMethod corrMethod>
GroupData<linkMethod, distMethod, corrMethod>::GroupData(Ti rows, Ti cols) {

  this->Groups = std::vector<std::vector<Ti> *>();
  this->Removed = std::set<size_t>();
  auto distance = Distance<true, distMethod, corrMethod>(rows, cols);
  this->WorkSize = distance.WorkSize + distance.StorageSize;
}

template <HClusterLinkage linkMethod, DistanceMethod distMethod,
          CorrelationMethod corrMethod>
GroupData<linkMethod, distMethod, corrMethod>::~GroupData() {
  for (auto a : this->Groups)
    delete a;
}

template <HClusterLinkage linkMethod, DistanceMethod distMethod,
          CorrelationMethod corrMethod>
void GroupData<linkMethod, distMethod, corrMethod>::Calculate(
    const Matrix<Tv> &mat, Tv *work, Ti groupCount, Tv removeThreshold) {
  // check
  auto temp = GroupData<linkMethod, distMethod, corrMethod>(mat.RowsCount,
                                                            mat.ColsCount);
  if (temp.WorkSize > this->WorkSize)
    throw std::logic_error("Inconsistent arguments");

  // delete current storage and clear it
  for (auto a : this->Groups)
    delete a;
  this->Groups.clear();

  auto distance =
      Distance<true, distMethod, corrMethod>(mat.RowsCount, mat.ColsCount);
  auto cluster = HCluster<linkMethod>(mat.ColsCount);
  Ti p = 0;
  auto dist_storage = &work[p];
  p += distance.StorageSize;

  distance.Calculate(mat, dist_storage, &work[p]);
  for (auto i = 0; i < distance.Result.length_array(); i++) {
    if (std::isnan(distance.Result.Data[i])) {
      distance.Result.Data[i] = 0;
      this->NaNDistanceFound = true;
    }
  }

  cluster.Calculate(distance.Result);

  // group
  for (Ti i = 0; i < groupCount; i++)
    this->Groups.push_back(new std::vector<Ti>());
  cluster.Group(this->Groups);

  // filter by threshold
  if (removeThreshold > 0) {
    auto remove = std::set<size_t>();
    for (auto &g : this->Groups) {
      for (auto i = 0; i < (Ti)g->size(); i++) {
        for (auto j = i + 1; j < (Ti)g->size(); j++) {
          auto dis = distance.Result.Get(g->at(i), g->at(j));
          if (dis < removeThreshold) {
            remove.insert(j);
            this->Removed.insert(j);
          }
        }
      }
      for (auto itr = remove.rbegin(); itr != remove.rend(); itr++)
        g->erase(g->begin() + *itr); // begin must be unchanged

      remove.clear();
    }
  }
}

// #pragma region Factory

std::unique_ptr<GroupDataBase>
GroupDataBase::GetFromType(HClusterLinkage linkMethod,
                           DistanceMethod distMethod,
                           CorrelationMethod corrMethod, Ti rows, Ti cols) {

  switch (linkMethod) {
  case ldt::HClusterLinkage::kSingle:

    switch (distMethod) {
    case ldt::DistanceMethod::kEuclidean:
      return std::unique_ptr<GroupDataBase>(
          new GroupData<HClusterLinkage::kSingle, DistanceMethod::kEuclidean,
                        CorrelationMethod::kPearson>(rows, cols));

    case ldt::DistanceMethod::kManhattan:
      return std::unique_ptr<GroupDataBase>(
          new GroupData<HClusterLinkage::kSingle, DistanceMethod::kManhattan,
                        CorrelationMethod::kPearson>(rows, cols));

    case ldt::DistanceMethod::kMaximum:
      return std::unique_ptr<GroupDataBase>(
          new GroupData<HClusterLinkage::kSingle, DistanceMethod::kMaximum,
                        CorrelationMethod::kPearson>(rows, cols));

    case ldt::DistanceMethod::kCorrelation:

      switch (corrMethod) {
      case ldt::CorrelationMethod::kPearson:
        return std::unique_ptr<GroupDataBase>(
            new GroupData<HClusterLinkage::kSingle,
                          DistanceMethod::kCorrelation,
                          CorrelationMethod::kPearson>(rows, cols));

      case ldt::CorrelationMethod::kSpearman:
        return std::unique_ptr<GroupDataBase>(
            new GroupData<HClusterLinkage::kSingle,
                          DistanceMethod::kCorrelation,
                          CorrelationMethod::kSpearman>(rows, cols));

      default:
        throw std::logic_error("not implemented");
      }

    case ldt::DistanceMethod::kAbsCorrelation:

      switch (corrMethod) {
      case ldt::CorrelationMethod::kPearson:
        return std::unique_ptr<GroupDataBase>(
            new GroupData<HClusterLinkage::kSingle,
                          DistanceMethod::kAbsCorrelation,
                          CorrelationMethod::kPearson>(rows, cols));

      case ldt::CorrelationMethod::kSpearman:
        return std::unique_ptr<GroupDataBase>(
            new GroupData<HClusterLinkage::kSingle,
                          DistanceMethod::kAbsCorrelation,
                          CorrelationMethod::kSpearman>(rows, cols));

      default:
        throw std::logic_error("not implemented");
      }

    default:
      throw std::logic_error("not implemented");
    }

  case ldt::HClusterLinkage::kComplete:

    switch (distMethod) {
    case ldt::DistanceMethod::kEuclidean:

      return std::unique_ptr<GroupDataBase>(
          new GroupData<HClusterLinkage::kComplete, DistanceMethod::kEuclidean,
                        CorrelationMethod::kPearson>(rows, cols));

    case ldt::DistanceMethod::kManhattan:
      return std::unique_ptr<GroupDataBase>(
          new GroupData<HClusterLinkage::kComplete, DistanceMethod::kManhattan,
                        CorrelationMethod::kPearson>(rows, cols));

    case ldt::DistanceMethod::kMaximum:

      return std::unique_ptr<GroupDataBase>(
          new GroupData<HClusterLinkage::kComplete, DistanceMethod::kMaximum,
                        CorrelationMethod::kPearson>(rows, cols));

    case ldt::DistanceMethod::kCorrelation:

      switch (corrMethod) {
      case ldt::CorrelationMethod::kPearson:
        return std::unique_ptr<GroupDataBase>(
            new GroupData<HClusterLinkage::kComplete,
                          DistanceMethod::kCorrelation,
                          CorrelationMethod::kPearson>(rows, cols));

      case ldt::CorrelationMethod::kSpearman:
        return std::unique_ptr<GroupDataBase>(
            new GroupData<HClusterLinkage::kComplete,
                          DistanceMethod::kCorrelation,
                          CorrelationMethod::kSpearman>(rows, cols));

      default:
        throw std::logic_error("not implemented");
      }

    case ldt::DistanceMethod::kAbsCorrelation:

      switch (corrMethod) {
      case ldt::CorrelationMethod::kPearson:
        return std::unique_ptr<GroupDataBase>(
            new GroupData<HClusterLinkage::kComplete,
                          DistanceMethod::kAbsCorrelation,
                          CorrelationMethod::kPearson>(rows, cols));

      case ldt::CorrelationMethod::kSpearman:
        return std::unique_ptr<GroupDataBase>(
            new GroupData<HClusterLinkage::kComplete,
                          DistanceMethod::kAbsCorrelation,
                          CorrelationMethod::kSpearman>(rows, cols));

      default:
        throw std::logic_error("not implemented");
      }

    default:
      throw std::logic_error("not implemented");
    }

  case ldt::HClusterLinkage::kAverageU:

    switch (distMethod) {
    case ldt::DistanceMethod::kEuclidean:

      return std::unique_ptr<GroupDataBase>(
          new GroupData<HClusterLinkage::kAverageU, DistanceMethod::kEuclidean,
                        CorrelationMethod::kPearson>(rows, cols));

    case ldt::DistanceMethod::kManhattan:

      return std::unique_ptr<GroupDataBase>(
          new GroupData<HClusterLinkage::kAverageU, DistanceMethod::kManhattan,
                        CorrelationMethod::kPearson>(rows, cols));

    case ldt::DistanceMethod::kMaximum:

      return std::unique_ptr<GroupDataBase>(
          new GroupData<HClusterLinkage::kAverageU, DistanceMethod::kMaximum,
                        CorrelationMethod::kPearson>(rows, cols));

    case ldt::DistanceMethod::kCorrelation:

      switch (corrMethod) {
      case ldt::CorrelationMethod::kPearson:
        return std::unique_ptr<GroupDataBase>(
            new GroupData<HClusterLinkage::kAverageU,
                          DistanceMethod::kCorrelation,
                          CorrelationMethod::kPearson>(rows, cols));

      case ldt::CorrelationMethod::kSpearman:
        return std::unique_ptr<GroupDataBase>(
            new GroupData<HClusterLinkage::kAverageU,
                          DistanceMethod::kCorrelation,
                          CorrelationMethod::kSpearman>(rows, cols));

      default:
        throw std::logic_error("not implemented");
      }

    case ldt::DistanceMethod::kAbsCorrelation:

      switch (corrMethod) {
      case ldt::CorrelationMethod::kPearson:
        return std::unique_ptr<GroupDataBase>(
            new GroupData<HClusterLinkage::kAverageU,
                          DistanceMethod::kAbsCorrelation,
                          CorrelationMethod::kPearson>(rows, cols));

      case ldt::CorrelationMethod::kSpearman:
        return std::unique_ptr<GroupDataBase>(
            new GroupData<HClusterLinkage::kAverageU,
                          DistanceMethod::kAbsCorrelation,
                          CorrelationMethod::kSpearman>(rows, cols));

      default:
        throw std::logic_error("not implemented");
      }

    default:
      throw std::logic_error("not implemented");
    }

  case ldt::HClusterLinkage::kAverageW:

    switch (distMethod) {
    case ldt::DistanceMethod::kEuclidean:

      return std::unique_ptr<GroupDataBase>(
          new GroupData<HClusterLinkage::kAverageW, DistanceMethod::kEuclidean,
                        CorrelationMethod::kPearson>(rows, cols));

    case ldt::DistanceMethod::kManhattan:

      return std::unique_ptr<GroupDataBase>(
          new GroupData<HClusterLinkage::kAverageW, DistanceMethod::kManhattan,
                        CorrelationMethod::kPearson>(rows, cols));

    case ldt::DistanceMethod::kMaximum:

      return std::unique_ptr<GroupDataBase>(
          new GroupData<HClusterLinkage::kAverageW, DistanceMethod::kMaximum,
                        CorrelationMethod::kPearson>(rows, cols));

    case ldt::DistanceMethod::kCorrelation:

      switch (corrMethod) {
      case ldt::CorrelationMethod::kPearson:
        return std::unique_ptr<GroupDataBase>(
            new GroupData<HClusterLinkage::kAverageW,
                          DistanceMethod::kCorrelation,
                          CorrelationMethod::kPearson>(rows, cols));

      case ldt::CorrelationMethod::kSpearman:
        return std::unique_ptr<GroupDataBase>(
            new GroupData<HClusterLinkage::kAverageW,
                          DistanceMethod::kCorrelation,
                          CorrelationMethod::kSpearman>(rows, cols));

      default:
        throw std::logic_error("not implemented");
      }

    case ldt::DistanceMethod::kAbsCorrelation:

      switch (corrMethod) {
      case ldt::CorrelationMethod::kPearson:
        return std::unique_ptr<GroupDataBase>(
            new GroupData<HClusterLinkage::kAverageW,
                          DistanceMethod::kAbsCorrelation,
                          CorrelationMethod::kPearson>(rows, cols));

      case ldt::CorrelationMethod::kSpearman:
        return std::unique_ptr<GroupDataBase>(
            new GroupData<HClusterLinkage::kAverageW,
                          DistanceMethod::kAbsCorrelation,
                          CorrelationMethod::kSpearman>(rows, cols));

      default:
        throw std::logic_error("not implemented");
      }

    default:
      throw std::logic_error("not implemented");
    }

  case ldt::HClusterLinkage::kWard:

    switch (distMethod) {
    case ldt::DistanceMethod::kEuclidean:

      return std::unique_ptr<GroupDataBase>(
          new GroupData<HClusterLinkage::kWard, DistanceMethod::kEuclidean,
                        CorrelationMethod::kPearson>(rows, cols));

    case ldt::DistanceMethod::kManhattan:

      return std::unique_ptr<GroupDataBase>(
          new GroupData<HClusterLinkage::kWard, DistanceMethod::kManhattan,
                        CorrelationMethod::kPearson>(rows, cols));

    case ldt::DistanceMethod::kMaximum:

      return std::unique_ptr<GroupDataBase>(
          new GroupData<HClusterLinkage::kWard, DistanceMethod::kMaximum,
                        CorrelationMethod::kPearson>(rows, cols));

    case ldt::DistanceMethod::kCorrelation:

      switch (corrMethod) {
      case ldt::CorrelationMethod::kPearson:
        return std::unique_ptr<GroupDataBase>(
            new GroupData<HClusterLinkage::kWard, DistanceMethod::kCorrelation,
                          CorrelationMethod::kPearson>(rows, cols));

      case ldt::CorrelationMethod::kSpearman:
        return std::unique_ptr<GroupDataBase>(
            new GroupData<HClusterLinkage::kWard, DistanceMethod::kCorrelation,
                          CorrelationMethod::kSpearman>(rows, cols));

      default:
        throw std::logic_error("not implemented");
      }

    case ldt::DistanceMethod::kAbsCorrelation:

      switch (corrMethod) {
      case ldt::CorrelationMethod::kPearson:
        return std::unique_ptr<GroupDataBase>(
            new GroupData<HClusterLinkage::kWard,
                          DistanceMethod::kAbsCorrelation,
                          CorrelationMethod::kPearson>(rows, cols));

      case ldt::CorrelationMethod::kSpearman:
        return std::unique_ptr<GroupDataBase>(
            new GroupData<HClusterLinkage::kWard,
                          DistanceMethod::kAbsCorrelation,
                          CorrelationMethod::kSpearman>(rows, cols));

      default:
        throw std::logic_error("not implemented");
      }

    default:
      throw std::logic_error("not implemented");
    }

  default:
    throw std::logic_error("not implemented");
  }
}

// #pragma endregion

// Correlation
template class ldt::GroupData<HClusterLinkage::kAverageU,
                              DistanceMethod::kEuclidean,
                              CorrelationMethod::kPearson>;

template class ldt::GroupData<HClusterLinkage::kAverageU,
                              DistanceMethod::kCorrelation,
                              CorrelationMethod::kPearson>;
template class ldt::GroupData<HClusterLinkage::kAverageW,
                              DistanceMethod::kCorrelation,
                              CorrelationMethod::kPearson>;
template class ldt::GroupData<HClusterLinkage::kComplete,
                              DistanceMethod::kCorrelation,
                              CorrelationMethod::kPearson>;
template class ldt::GroupData<HClusterLinkage::kSingle,
                              DistanceMethod::kCorrelation,
                              CorrelationMethod::kPearson>;
template class ldt::GroupData<HClusterLinkage::kWard,
                              DistanceMethod::kCorrelation,
                              CorrelationMethod::kPearson>;

template class ldt::GroupData<HClusterLinkage::kAverageU,
                              DistanceMethod::kCorrelation,
                              CorrelationMethod::kSpearman>;
template class ldt::GroupData<HClusterLinkage::kAverageW,
                              DistanceMethod::kCorrelation,
                              CorrelationMethod::kSpearman>;
template class ldt::GroupData<HClusterLinkage::kComplete,
                              DistanceMethod::kCorrelation,
                              CorrelationMethod::kSpearman>;
template class ldt::GroupData<HClusterLinkage::kSingle,
                              DistanceMethod::kCorrelation,
                              CorrelationMethod::kSpearman>;
template class ldt::GroupData<HClusterLinkage::kWard,
                              DistanceMethod::kCorrelation,
                              CorrelationMethod::kSpearman>;

// Abs Correlation

template class ldt::GroupData<HClusterLinkage::kAverageU,
                              DistanceMethod::kAbsCorrelation,
                              CorrelationMethod::kPearson>;
template class ldt::GroupData<HClusterLinkage::kAverageW,
                              DistanceMethod::kAbsCorrelation,
                              CorrelationMethod::kPearson>;
template class ldt::GroupData<HClusterLinkage::kComplete,
                              DistanceMethod::kAbsCorrelation,
                              CorrelationMethod::kPearson>;
template class ldt::GroupData<HClusterLinkage::kSingle,
                              DistanceMethod::kAbsCorrelation,
                              CorrelationMethod::kPearson>;
template class ldt::GroupData<HClusterLinkage::kWard,
                              DistanceMethod::kAbsCorrelation,
                              CorrelationMethod::kPearson>;

template class ldt::GroupData<HClusterLinkage::kAverageU,
                              DistanceMethod::kAbsCorrelation,
                              CorrelationMethod::kSpearman>;
template class ldt::GroupData<HClusterLinkage::kAverageW,
                              DistanceMethod::kAbsCorrelation,
                              CorrelationMethod::kSpearman>;
template class ldt::GroupData<HClusterLinkage::kComplete,
                              DistanceMethod::kAbsCorrelation,
                              CorrelationMethod::kSpearman>;
template class ldt::GroupData<HClusterLinkage::kSingle,
                              DistanceMethod::kAbsCorrelation,
                              CorrelationMethod::kSpearman>;
template class ldt::GroupData<HClusterLinkage::kWard,
                              DistanceMethod::kAbsCorrelation,
                              CorrelationMethod::kSpearman>;
