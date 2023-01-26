/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the GPL 3.0.
 See accompanying file LICENSE
*/

#include "matrix_utils.h"
#include <algorithm>
#include <cctype>
#include <iostream>
#include <locale>
#include <numeric>
#include <random>
#include <sstream>
#include <string>

using namespace ldt;

//#pragma region Dataset

template <typename Tw>
Dataset<Tw>::Dataset(Ti rows, Ti cols, bool hasNan, bool selectColumn) {
  if (cols <= 0 || rows <= 0)
    throw std::logic_error("invalid size in 'dataset'");
  mHasNaN = hasNan;
  mSelectColumn = selectColumn;

  Result = Matrix<Tw>();
  StorageSize = rows * cols;
}

template <typename Tw>
void Dataset<Tw>::Calculate(const Matrix<Tw> &data, std::vector<Ti> *colIndexes,
                            Tw *storage) {

  if (mSelectColumn) {
    if (!colIndexes)
      throw std::invalid_argument("colIndexes");
    if (mHasNaN) {
      auto v = std::vector<Ti>();
      data.GetAnyNanRow(v, false, colIndexes);
      Result.SetData(storage, (Ti)v.size(), (Ti)colIndexes->size());
      data.GetSub(v, *colIndexes, Result, 0, 0);
    } else {
      Result.SetData(storage, data.RowsCount, (Ti)colIndexes->size());
      data.GetSub(0, data.RowsCount, *colIndexes, true, Result, 0, 0, false);
    }
  } else {
    if (mHasNaN) {
      auto v = std::vector<Ti>();
      data.GetAnyNanRow(v, false, nullptr);
      Result.SetData(storage, (Ti)v.size(), data.ColsCount);
      data.GetSub(0, data.ColsCount, v, false, Result, 0, 0);
    } else { // simply copy
      Result.SetData(storage, data.RowsCount, data.ColsCount);
      data.CopyTo00(Result);
    }
  }
}

//#pragma endregion

//#pragma region Dataset Time - Series

template <bool byRow, typename Tw>
DatasetTs<byRow, Tw>::DatasetTs(Ti rows, Ti cols, bool hasNan, bool select,
                                bool interpolate, Ti adjustLeadLagsCount) {
  if (cols <= 0 || rows <= 0)
    throw std::logic_error("invalid size in 'datasetT'.");
  mHasNaN = hasNan;
  mSelect = select;
  mInterpolate = interpolate;
  mAdjustLeadLagsCount = adjustLeadLagsCount;

  StorageSize = rows * cols; // Results

  if (interpolate) {
    if constexpr (std::numeric_limits<Tw>::has_quiet_NaN == false) {
      throw std::logic_error("invalid type. disable interpolation, etc.");
    }
  }
}

template <bool byRow, typename Tw>
void DatasetTs<byRow, Tw>::Data(Matrix<Tw> &data) {
  pData = &data;

  // find indexes
  Ranges.clear();
  InterpolationCounts.clear();
  if (mHasNaN) {

    bool hasmissing;
    Ti count;
    if constexpr (byRow) {
      count = data.RowsCount;
    } else if constexpr (true) {
      count = data.ColsCount;
    }
    for (Ti i = 0; i < count; i++) {
      if constexpr (byRow) {
        Ranges.push_back(pData->GetRangeRow(hasmissing, i));
      } else if constexpr (true) {
        Ranges.push_back(pData->GetRangeColumn(hasmissing, i));
      }

      if (hasmissing) {
        HasMissingData = true;
        WithMissingIndexes.push_back(i);
        InterpolationCounts.push_back(0);
      }
    }

    for (auto &r : Ranges)
      if (r.IsNotValid())
        throw std::logic_error("Data is not valid. Check missing data points.");
  }

  if constexpr (std::numeric_limits<Tw>::has_quiet_NaN) {

    if (HasMissingData && mInterpolate) {
      Ti q = -1;
      for (auto &p : WithMissingIndexes) {
        q++;
        bool inMissing = false;
        Tw first = NAN, last = NAN, d;
        Ti length = 1;
        auto range = Ranges.at(p);

        if constexpr (byRow) {

          for (Ti i = range.StartIndex; i <= range.EndIndex; i++) {
            d = data.Get0(p, i);
            auto isNaN = std::isnan(d);

            if (isNaN)
              length++;

            if (isNaN == false && inMissing) {
              last = d;

              // calculate and set
              Tw step = (last - first) / length;
              for (int j = 1; j < length; j++) {
                data.Set0(p, i - j, d - j * step);
                InterpolationCounts.at(q)++;
              }

              length = 1;
              inMissing = false;
            }

            if (isNaN && inMissing == false) {
              first = data.Get0(p, i - 1);
              inMissing = true;
            }
          }

        } else if constexpr (true) {

          Tw *col = &data.Data[data.RowsCount * p];
          for (Ti i = range.StartIndex; i <= range.EndIndex; i++) {
            auto isNaN = std::isnan(col[i]);

            if (isNaN)
              length++;

            if (isNaN == false && inMissing) {
              last = col[i];

              // calculate and set
              Tw step = (last - first) / length;
              for (int j = 1; j < length; j++) {
                col[i - j] = col[i] - j * step;
                InterpolationCounts.at(q)++;
              }

              length = 1;
              inMissing = false;
            }

            if (isNaN && inMissing == false) {
              first = col[i - 1];
              inMissing = true;
            }
          }
        }
      }

      HasMissingData = false;
      // do not clear 'WithMissingIndexes'  to both signal interpolation and
      // keep information
    }

    if (mAdjustLeadLagsCount > 0) { // with respect to the first variable
      Ti count;
      if constexpr (byRow) {
        count = data.RowsCount;
      } else if constexpr (true) {
        count = data.ColsCount;
      }
      Ti lastIndex = Ranges.at(0).EndIndex;
      for (Ti i = 1; i < std::min(mAdjustLeadLagsCount, count); i++) {
        auto index = Ranges.at(i).EndIndex;
        auto len = lastIndex - index;
        if (len > 0) { // lag ... move data forward (create a lagged variable)
          WithLags.push_back(i);
          for (Ti j = lastIndex; j >= 0; j--) {
            if constexpr (byRow) {
              if (j >= len)
                data.Set0(i, j, data.Get0(i, j - len));
              else
                data.Set(i, j, NAN);
            } else if constexpr (true) {
              if (j >= len)
                data.Set0(j, i, data.Get0(j - len, i));
              else
                data.Set(j, i, NAN);
            }
          }
          Ranges.at(i).StartIndex = Ranges.at(i).StartIndex + len;
          Ranges.at(i).EndIndex = lastIndex;
        } else if (len < 0) { // lead ... just set NAN
          WithLeads.push_back(i);
          for (Ti j = 0; j < -len; j++) {
            if constexpr (byRow) {
              data.Set(i, lastIndex + j + 1, NAN);
            } else if constexpr (true) {
              data.Set(lastIndex + j + 1, i, NAN);
            }
          }
          Ranges.at(i).EndIndex = lastIndex;
        }
      }
    }
  }
}

void biggestWithoutNaN(std::vector<IndexRange> &ranges,
                       std::vector<Ti> *indexes, Ti &start, Ti &end) {
  start = 0;                            // maximum of starts
  end = std::numeric_limits<Ti>::max(); // minimum of ends
  if (indexes) {
    for (auto &i : *indexes) {
      auto r = ranges.at(i);
      if (r.StartIndex > start)
        start = r.StartIndex;
      if (r.EndIndex < end)
        end = r.EndIndex;
    }
  } else {
    for (auto &r : ranges) {
      if (r.StartIndex > start)
        start = r.StartIndex;
      if (r.EndIndex < end)
        end = r.EndIndex;
    }
  }
}

template <bool byRow, typename Tw>
void DatasetTs<byRow, Tw>::Update(std::vector<Ti> *indexes, Tw *storage) {

  if (storage) // if null, we just update Start and End
    Result.SetData(storage);
  if (mSelect) {
    Start = 0;
    End = 0;
    if constexpr (byRow) {
      End = pData->ColsCount - 1;
    } else if constexpr (true) {
      End = pData->RowsCount - 1;
    }
    if (mHasNaN)
      biggestWithoutNaN(Ranges, indexes, Start, End);

    if constexpr (byRow) {
      Result.Restructure0((Ti)indexes->size(), (Ti)(End - Start + 1));
    } else if constexpr (true) {
      Result.Restructure0((Ti)(End - Start + 1), (Ti)indexes->size());
    }

    if (storage)
      pData->GetSub(Start, End - Start + 1, *indexes, byRow == false, Result, 0,
                    0);
  } else {
    if (mHasNaN) {
      Start = 0;
      End = 0;
      biggestWithoutNaN(Ranges, nullptr, Start, End);
      if constexpr (byRow) {
        Result.Restructure0(pData->RowsCount, End - Start + 1);
        if (storage)
          pData->GetSub(0, Start, pData->RowsCount, End - Start + 1, Result, 0,
                        0);
      } else if constexpr (true) {
        Result.Restructure0(End - Start + 1, pData->ColsCount);
        if (storage)
          pData->GetSub(Start, 0, End - Start + 1, pData->ColsCount, Result, 0,
                        0);
      }
    } else { // simply copy
      Result.Restructure0(pData->RowsCount, pData->ColsCount);
      if (storage)
        pData->CopyTo00(Result);
    }
  }
}

//#pragma endregion

//#pragma region Standardize

template <typename Tw>
MatrixStandardized<Tw>::MatrixStandardized(Ti rows, Ti cols, bool removeZeroVar,
                                           bool center, bool scale) {
  if (cols <= 0 || rows <= 0)
    throw std::logic_error("invalid size in 'MatrixStandardized'.");
  if (scale == false)
    removeZeroVar = false;

  mRemoveZeroVar = removeZeroVar;
  mCenter = center;
  mScale = scale;

  Result = Matrix<Tw>();
  StorageSize = rows * cols;

  if (mCenter) {
    ColumnMeans = Matrix<Tw>();
    StorageSize += cols;
  }
  if (mScale) {
    ColumnVars = Matrix<Tw>();
    StorageSize += cols;
  }
  if (removeZeroVar)
    RemovedZeroVar = std::vector<Ti>();
}

template <typename Tw>
void MatrixStandardized<Tw>::Calculate(const Matrix<Tw> &mat, Tw *storage,
                                       const Matrix<Tw> *overrideMean,
                                       const Matrix<Tw> *overrideVariance) {
  Ti cols = mat.ColsCount;
  Ti rows = mat.RowsCount;
  auto temp = MatrixStandardized(rows, cols, mRemoveZeroVar, mCenter, mScale);
  if (temp.StorageSize > StorageSize)
    throw std::logic_error("inconsistent size in 'MatrixStandardized'");

  Ti q = 0;
  Result.SetData(&storage[q], rows, cols);
  q += rows * cols;
  mat.CopyTo00(Result);

  const Matrix<Tw> *useMean = nullptr;
  const Matrix<Tw> *useVar = nullptr;

  if (mCenter && mScale) {

    if (!overrideMean && !overrideVariance) {
      ColumnMeans.SetData(&storage[q], cols, 1);
      q += cols;
      ColumnVars.SetData(&storage[q], cols, 1);
      q += cols;
      Result.ColumnsMeansVariances(ColumnMeans, ColumnVars, Sample, CheckNan);
      useMean = &ColumnMeans;
      useVar = &ColumnVars;
    } else if (!overrideMean) {
      ColumnMeans.SetData(&storage[q], cols, 1);
      q += cols;
      Result.ColumnsMeans(ColumnMeans, CheckNan);
      useMean = &ColumnMeans;
      useVar = overrideVariance;
    } else if (!overrideVariance) {
      ColumnVars.SetData(&storage[q], cols, 1);
      q += cols;
      Result.ColumnsVariances(ColumnVars, Sample, CheckNan);
      useMean = overrideMean;
      useVar = &ColumnVars;
    } else {
      useMean = overrideMean;
      useVar = overrideVariance;
    }

    Result.ColumnsStandard(useMean, useVar, true);
  } else if (mScale) {
    if (!overrideVariance) {
      ColumnVars.SetData(&storage[q], cols, 1);
      q += cols;
      Result.ColumnsVariances(ColumnVars, Sample, CheckNan);
      useVar = &ColumnVars;
    } else
      useVar = overrideVariance;
    Result.ColumnsStandard(nullptr, useVar, true);
  } else if (mCenter) {
    if (!overrideMean) {
      ColumnMeans.SetData(&storage[q], cols, 1);
      q += cols;
      Result.ColumnsMeans(ColumnMeans, CheckNan);
      useMean = &ColumnMeans;
    } else
      useMean = overrideMean;
    Result.ColumnsStandard(useMean);
  }

  if (mScale && mRemoveZeroVar) {
    RemovedZeroVar.clear();
    useVar->IndicesOfVector(0, RemovedZeroVar);
    Result.RemoveColumnsIn(RemovedZeroVar);
  }
}

//#pragma endregion

template class ldt::Dataset<Tv>;
template class ldt::Dataset<Ti>;

template class ldt::DatasetTs<true, Tv>;
template class ldt::DatasetTs<true, Ti>;
template class ldt::DatasetTs<false, Tv>;
template class ldt::DatasetTs<false, Ti>;

template class ldt::MatrixStandardized<Tv>;
template class ldt::MatrixStandardized<Ti>;
