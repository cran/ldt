/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the GPL 3.0.
 See accompanying file LICENSE
*/

#include "clustering.h"
#include <stack>

using namespace ldt;

std::unique_ptr<DistanceBase>
DistanceBase::GetFromType(bool checkNan, DistanceMethod distMethod,
                          CorrelationMethod corrMethod, Ti rows, Ti cols) {

  if (checkNan) {
    switch (distMethod) {
    case ldt::DistanceMethod::kEuclidean:
      return std::unique_ptr<DistanceBase>(
          new Distance<true, DistanceMethod::kEuclidean,
                       CorrelationMethod::kPearson>(rows, cols));

    case ldt::DistanceMethod::kManhattan:
      return std::unique_ptr<DistanceBase>(
          new Distance<true, DistanceMethod::kManhattan,
                       CorrelationMethod::kPearson>(rows, cols));

    case ldt::DistanceMethod::kMaximum:
      return std::unique_ptr<DistanceBase>(
          new Distance<true, DistanceMethod::kMaximum,
                       CorrelationMethod::kPearson>(rows, cols));

    case ldt::DistanceMethod::kCorrelation:
      switch (corrMethod) {
      case ldt::CorrelationMethod::kPearson:
        return std::unique_ptr<DistanceBase>(
            new Distance<true, DistanceMethod::kCorrelation,
                         CorrelationMethod::kPearson>(rows, cols));

      case ldt::CorrelationMethod::kSpearman:
        return std::unique_ptr<DistanceBase>(
            new Distance<true, DistanceMethod::kCorrelation,
                         CorrelationMethod::kSpearman>(rows, cols));

      default:
        throw std::logic_error("not implemented (correlation method)");
      }

    case ldt::DistanceMethod::kAbsCorrelation:
      switch (corrMethod) {
      case ldt::CorrelationMethod::kPearson:
        return std::unique_ptr<DistanceBase>(
            new Distance<true, DistanceMethod::kAbsCorrelation,
                         CorrelationMethod::kPearson>(rows, cols));

      case ldt::CorrelationMethod::kSpearman:
        return std::unique_ptr<DistanceBase>(
            new Distance<true, DistanceMethod::kAbsCorrelation,
                         CorrelationMethod::kSpearman>(rows, cols));

      default:
        throw std::logic_error("not implemented (correlation method)");
      }

    default:
      throw std::logic_error("not implemented (distance method)");
    }
  } else {
    switch (distMethod) {
    case ldt::DistanceMethod::kEuclidean:
      return std::unique_ptr<DistanceBase>(
          new Distance<false, DistanceMethod::kEuclidean,
                       CorrelationMethod::kPearson>(rows, cols));

    case ldt::DistanceMethod::kManhattan:
      return std::unique_ptr<DistanceBase>(
          new Distance<false, DistanceMethod::kManhattan,
                       CorrelationMethod::kPearson>(rows, cols));

    case ldt::DistanceMethod::kMaximum:
      return std::unique_ptr<DistanceBase>(
          new Distance<false, DistanceMethod::kMaximum,
                       CorrelationMethod::kPearson>(rows, cols));

    case ldt::DistanceMethod::kCorrelation:
      switch (corrMethod) {
      case ldt::CorrelationMethod::kPearson:
        return std::unique_ptr<DistanceBase>(
            new Distance<false, DistanceMethod::kCorrelation,
                         CorrelationMethod::kPearson>(rows, cols));

      case ldt::CorrelationMethod::kSpearman:
        return std::unique_ptr<DistanceBase>(
            new Distance<false, DistanceMethod::kCorrelation,
                         CorrelationMethod::kSpearman>(rows, cols));

      default:
        throw std::logic_error("not implemented (correlation method)");
      }

    case ldt::DistanceMethod::kAbsCorrelation:
      switch (corrMethod) {
      case ldt::CorrelationMethod::kPearson:
        return std::unique_ptr<DistanceBase>(
            new Distance<false, DistanceMethod::kAbsCorrelation,
                         CorrelationMethod::kPearson>(rows, cols));

      case ldt::CorrelationMethod::kSpearman:
        return std::unique_ptr<DistanceBase>(
            new Distance<false, DistanceMethod::kAbsCorrelation,
                         CorrelationMethod::kSpearman>(rows, cols));

      default:
        throw std::logic_error("not implemented (correlation method)");
      }

    default:
      throw std::logic_error("not implemented (distance method)");
    }
  }
}

template <bool checkNan, DistanceMethod method, CorrelationMethod corrMethod>
Distance<checkNan, method, corrMethod>::~Distance() {}

template <bool checkNan, DistanceMethod method, CorrelationMethod corrMethod>
Distance<checkNan, method, corrMethod>::Distance(Ti rows, Ti cols) {

  this->Result = MatrixSym<false>(cols);
  this->StorageSize = cols * (cols - 1) / (Ti)2;
  this->WorkSize = 0;
  if constexpr (method == DistanceMethod::kCorrelation ||
                method == DistanceMethod::kAbsCorrelation) {
    auto cor = Correlation<checkNan, CorrelationType::kCorrelation, corrMethod>(
        rows, cols, true);
    this->WorkSize += cor.StorageSize + cor.WorkSize;
  }
}

template <bool checkNan, DistanceMethod method, CorrelationMethod corrMethod>
void Distance<checkNan, method, corrMethod>::Calculate(const Matrix<Tv> &data,
                                                       Tv *storage, Tv *work) {

  // check
  auto temp =
      Distance<checkNan, method, corrMethod>(data.RowsCount, data.ColsCount);
  if (temp.StorageSize > this->StorageSize || temp.WorkSize > this->WorkSize)
    throw std::logic_error("Inconsistent arguments");

  this->Result.SetData(storage);

  if constexpr (method == DistanceMethod::kCorrelation ||
                method == DistanceMethod::kAbsCorrelation) {
    auto cor = Correlation<checkNan, CorrelationType::kCorrelation, corrMethod>(
        data.RowsCount, data.ColsCount, true);
    cor.Calculate(data, work, &work[cor.WorkSize]);
    for (Ti i = 0; i < data.ColsCount; i++) {
      for (Ti j = 0; j < data.ColsCount; j++) {
        if (i < j) {
          if constexpr (method == DistanceMethod::kCorrelation) {
            this->Result.Set(i, j,
                             std::sqrt((1 - cor.Result.Get(i, j)) / (Tv)2));
          } else if constexpr (true) {
            this->Result.Set(
                i, j, std::sqrt(1 - std::pow(cor.Result.Get(i, j), (Tv)2)));
          }
        }
      }
    }
  } else if constexpr (true) {

    for (Ti i = 0; i < data.ColsCount; i++) {
      for (Ti j = 0; j < data.ColsCount; j++) {
        if (i < j) {
          auto col1 = &data.Data[data.RowsCount * i];
          auto col2 = &data.Data[data.RowsCount * j];

          if constexpr (method == DistanceMethod::kEuclidean) {
            Tv sum = 0, s;
            for (Ti k = 0; k < data.RowsCount; k++) {
              s = col1[k] - col2[k];
              if constexpr (checkNan) {
                if (std::isnan(s))
                  continue;
              }
              sum += std::pow(s, (Tv)2);
            }
            this->Result.Set(i, j, std::sqrt(sum));
          } else if constexpr (method == DistanceMethod::kManhattan) {
            Tv sum = 0, s;
            for (Ti k = 0; k < data.RowsCount; k++) {
              s = col1[k] - col2[k];
              if constexpr (checkNan) {
                if (std::isnan(s))
                  continue;
              }
              sum += std::abs(s);
            }
            this->Result.Set(i, j, sum);
          } else if constexpr (method == DistanceMethod::kMaximum) {
            Tv max = -INFINITY, d;
            for (Ti k = 0; k < data.RowsCount; k++) {
              d = std::abs(col1[k] - col2[k]);
              if constexpr (checkNan) {
                if (std::isnan(d))
                  continue;
              }
              if (max < d)
                max = d;
            }
            this->Result.Set(i, j, max);
          }
        }
      }
    }
  }
}

template class ldt::Distance<false, DistanceMethod::kEuclidean,
                             CorrelationMethod::kPearson>;
template class ldt::Distance<false, DistanceMethod::kManhattan,
                             CorrelationMethod::kPearson>;
template class ldt::Distance<false, DistanceMethod::kMaximum,
                             CorrelationMethod::kPearson>;

template class ldt::Distance<true, DistanceMethod::kEuclidean,
                             CorrelationMethod::kPearson>;
template class ldt::Distance<true, DistanceMethod::kManhattan,
                             CorrelationMethod::kPearson>;
template class ldt::Distance<true, DistanceMethod::kMaximum,
                             CorrelationMethod::kPearson>;

// Pearson correlation
template class ldt::Distance<false, DistanceMethod::kCorrelation,
                             CorrelationMethod::kPearson>;
template class ldt::Distance<false, DistanceMethod::kAbsCorrelation,
                             CorrelationMethod::kPearson>;
template class ldt::Distance<true, DistanceMethod::kCorrelation,
                             CorrelationMethod::kPearson>;
template class ldt::Distance<true, DistanceMethod::kAbsCorrelation,
                             CorrelationMethod::kPearson>;

// Spearman
template class ldt::Distance<false, DistanceMethod::kCorrelation,
                             CorrelationMethod::kSpearman>;
template class ldt::Distance<false, DistanceMethod::kAbsCorrelation,
                             CorrelationMethod::kSpearman>;
template class ldt::Distance<true, DistanceMethod::kCorrelation,
                             CorrelationMethod::kSpearman>;
template class ldt::Distance<true, DistanceMethod::kAbsCorrelation,
                             CorrelationMethod::kSpearman>;
