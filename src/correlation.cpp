/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the LGPL 3.0.
 See accompanying file LICENSE
*/

#include "correlation.h"
#include "matrix_utils.h"
#include "statistics.h"

using namespace ldt;

template <bool checkNaN, CorrelationType type, CorrelationMethod method>
Correlation<checkNaN, type, method>::Correlation(Ti rows, Ti cols,
                                                 bool byColumn) {
  this->mRows = rows;
  this->mCols = cols;
  this->mByColumn = byColumn;

  if (byColumn == false)
    throw std::logic_error("By column is not implemented."); // not implemented

  auto len = this->mByColumn ? this->mCols : this->mRows;

  this->Result = Matrix<Tv>();
  this->StorageSize = len * len;
  this->WorkSize = 0;
  if constexpr (checkNaN) {
    this->ResultCounts = MatrixSym<true>();
    this->StorageSize += len * (len + 1) / 2;
  }

  if constexpr (checkNaN == false) {
    this->WorkSize = this->mCols * this->mRows;
    this->StorageSize += len;
    this->Means = Matrix<Tv>();
    this->StorageSize += len;
    if constexpr (type == CorrelationType::kCorrelation) {
      this->Variances = Matrix<Tv>();
      this->StorageSize += len;
    }

    if constexpr (method == CorrelationMethod::kSpearman) {
      auto rankm = Rank(this->mRows, this->mCols);
      this->WorkSize =
          std::max(rankm.WorkSize, this->WorkSize) + rankm.StorageSize;
    }

  } else {
    this->StorageSize += len * (len + 1) / 2; // for keeping the counts

    if constexpr (method == CorrelationMethod::kSpearman) {
      auto rankm = Rank(this->mRows, 2);
      auto corr = Correlation<false, type, CorrelationMethod::kPearson>(
          this->mRows, 2, true);
      this->WorkSize += std::max(rankm.WorkSize, corr.WorkSize);
      auto twoCols = Dataset<Tv>(this->mRows, 2, true); // for pairs

      this->WorkSize +=
          twoCols.StorageSize + rankm.StorageSize + corr.StorageSize;
      ;
    }
  }
}

template <bool checkNaN, CorrelationType type, CorrelationMethod method>
void Correlation<checkNaN, type, method>::pearson(const Matrix<Tv> &mat,
                                                  Tv *work, bool adjustDoF,
                                                  bool setLower) {

  // auto len = this->mByColumn ? mat.ColsCount : mat.RowsCount;

  if constexpr (checkNaN) { // we need to create pairs of columns and remove
                            // NANs
    for (Ti i = 0; i < mat.ColsCount; i++) {

      if constexpr (type == CorrelationType::kCovariance) { // Calculate
                                                            // variance
        Tv mea = 0;
        Ti count = 0;
        this->Result.Set(i, i,
                         mat.VarianceColumn(i, mea, count, adjustDoF, true));
        this->ResultCounts.Set(i, i, (Tv)count);
      } else if constexpr (type == CorrelationType::kCorrelation) {
        this->Result.Set(i, i, 1);
      }

      // set off diagonals
      for (Ti j = 0; j < mat.ColsCount; j++) {
        if (i < j) {
          Tv s = 0;
          Ti count = 0;
          if constexpr (type == CorrelationType::kCovariance) {
            Tv mea1 = 0, mea2 = 0;
            s = mat.CovarianceColumn(i, j, mea1, mea2, count, adjustDoF, true);
          } else if constexpr (type == CorrelationType::kCorrelation) {
            Tv mea1 = 0, mea2 = 0, var1 = 0, var2 = 0;
            s = mat.CorrelationColumn(i, j, mea1, mea2, var1, var2, count,
                                      adjustDoF, true);
          }

          this->Result.Set(i, j, s);
          this->ResultCounts.Set(i, j, (Tv)count);
          if (setLower)
            this->Result.Set(j, i, s);
        }
      }
    }

  } else {

    auto mat_c = Matrix<Tv>(work, mat.RowsCount, mat.ColsCount);
    mat.CopyTo00(mat_c);

    if constexpr (type == CorrelationType::kCovariance) {
      mat_c.ColumnsMeans(this->Means, false);
      mat_c.ColumnsStandard(&this->Means, nullptr, true);
    } else if constexpr (type == CorrelationType::kCorrelation) {
      mat_c.ColumnsMeansVariances(this->Means, this->Variances, false,
                                  false); // not sample
      mat_c.ColumnsStandard(&this->Means, &this->Variances, true);
    }

    mat_c.Dot_AtA0(this->Result, setLower);
    if constexpr (type == CorrelationType::kCovariance) {
      if (adjustDoF)
        this->Result.Divide_in(static_cast<double>(mat.RowsCount - 1));
      else
        this->Result.Divide_in(static_cast<double>(mat.RowsCount));
    } else if constexpr (type == CorrelationType::kCorrelation) {
      this->Result.Divide_in(
          static_cast<double>(mat.RowsCount)); // no correction
    }
  }
}

template <bool checkNaN, CorrelationType type, CorrelationMethod method>
void Correlation<checkNaN, type, method>::spearman(const Matrix<Tv> &mat,
                                                   Tv *work, Tv *storage,
                                                   bool adjustDoF,
                                                   bool setLower) {

  // auto len = this->mByColumn ? mat.ColsCount : mat.RowsCount;

  if constexpr (checkNaN) { // we need to create pairs of columns and remove
                            // NANs, then rank them
    Ti q = 0;
    auto twoCols = Dataset<Tv>(mat.RowsCount, 2, true);
    auto twostorage = work;
    q += twoCols.StorageSize;
    auto corr = Correlation<false, type, CorrelationMethod::kPearson>(
        mat.RowsCount, 2, true);
    auto corrstorage = &work[q];
    q += corr.StorageSize;
    auto rankm = Rank(mat.RowsCount, 2);
    auto rankstorage = &work[q];
    q += rankm.StorageSize;
    auto rankcorrwork = &work[q];
    auto cols = std::vector<Ti>(2);
    Tv s;
    for (Ti i = 0; i < mat.ColsCount; i++) {
      cols.at(0) = i;
      for (Ti j = 0; j < mat.ColsCount; j++) {
        if (i <= j) { // = to set diagonal
          cols.at(1) = j;
          twoCols.Calculate(mat, &cols, twostorage);
          rankm.Calculate(twoCols.Result, rankcorrwork, rankstorage, false);
          corr.Calculate(rankm.Result, rankcorrwork, corrstorage, adjustDoF,
                         false);
          s = corr.Result.Data[2];
          this->Result.Set(i, j, s);
          this->ResultCounts.Set(i, j, (Tv)twoCols.Result.RowsCount);
          if (setLower)
            this->Result.Set(j, i, s);
        }
      }
    }

  } else {
    // calculate rank matrix and then pearson

    auto rankm = Rank(mat.RowsCount, mat.ColsCount);
    // auto rankstorage = work;
    auto workS = &work[rankm.StorageSize];
    rankm.Calculate(mat, workS, work, false);

    auto corr = Correlation<false, type, CorrelationMethod::kPearson>(
        mat.RowsCount, mat.ColsCount, true);
    corr.Calculate(rankm.Result, workS, storage, adjustDoF, setLower);
  }
}

template <bool checkNaN, CorrelationType type, CorrelationMethod method>
void Correlation<checkNaN, type, method>::Calculate(const Matrix<Tv> &mat,
                                                    Tv *work, Tv *storage,
                                                    bool adjustDoF,
                                                    bool setLower) {

  // check sizes
  auto temp = Correlation<checkNaN, type, method>(mat.RowsCount, mat.ColsCount,
                                                  this->mByColumn);
  if (temp.WorkSize > this->WorkSize || temp.StorageSize > this->StorageSize)
    throw std::logic_error("inconsistent arguments");

  if (mByColumn == false)
    throw std::logic_error("By Column is not implemented.");

  auto len = this->mByColumn ? mat.ColsCount : mat.RowsCount;

  // storage
  Ti q = 0;
  this->Result.SetData(storage, len, len);
  q += len * len;
  if constexpr (checkNaN) {
    this->ResultCounts.SetData(&storage[q], len);
    q += this->ResultCounts.length_array();
  } else if constexpr (true) {
    this->Means.SetData(&storage[q], len, 1);
    q += len;
    if constexpr (type == CorrelationType::kCorrelation) {
      this->Variances.SetData(&storage[q], len, 1);
      q += len;
    }
  }

  if constexpr (method == CorrelationMethod::kPearson) {
    pearson(mat, work, adjustDoF, setLower);
  } else if constexpr (method == CorrelationMethod::kSpearman) {
    spearman(mat, work, storage, adjustDoF,
             setLower); // I send all the storage to override
  } else if constexpr (true) {
    throw std::logic_error("not implemented");
  }
}

// Pearson
template class ldt::Correlation<false, CorrelationType::kCorrelation,
                                           CorrelationMethod::kPearson>;
template class ldt::Correlation<false, CorrelationType::kCovariance,
                                           CorrelationMethod::kPearson>;
template class ldt::Correlation<true, CorrelationType::kCorrelation,
                                           CorrelationMethod::kPearson>;
template class ldt::Correlation<true, CorrelationType::kCovariance,
                                           CorrelationMethod::kPearson>;

// Spearman:
template class ldt::Correlation<false, CorrelationType::kCorrelation,
                                           CorrelationMethod::kSpearman>;
template class ldt::Correlation<false, CorrelationType::kCovariance,
                                           CorrelationMethod::kSpearman>;
template class ldt::Correlation<true, CorrelationType::kCorrelation,
                                           CorrelationMethod::kSpearman>;
template class ldt::Correlation<true, CorrelationType::kCovariance,
                                           CorrelationMethod::kSpearman>;
