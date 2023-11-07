
/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the GPL 3.0.
 See accompanying file LICENSE
*/

#include "matrix.h"

#include <functional>
#include <iomanip>
#include <iostream>
#include <numeric>
#include <random>
#include <sstream>
#include <vector>

#include "matrix_la.cpp"

using namespace ldt;

// #pragma region Constructors

template <typename Tw> Matrix<Tw>::~Matrix<Tw>() { Data = nullptr; }

template <typename Tw> Matrix<Tw>::Matrix() {}

template <typename Tw>
Matrix<Tw>::Matrix(Ti m, Ti n) : RowsCount{m}, ColsCount{n} {}

template <typename Tw>
Matrix<Tw>::Matrix(Tw *values, Ti m, Ti n) : RowsCount{m}, ColsCount{n} {
  Data = values;
}

template <typename Tw> Matrix<Tw>::Matrix(std::vector<Tw> *values) {
  RowsCount = static_cast<Ti>(values->size());
  if (RowsCount == 0)
    ColsCount = 0;
  else {
    ColsCount = 1;
    Data = &values->at(0);
  }
}

template <typename Tw>
Matrix<Tw>::Matrix(std::vector<Tw> *values, Ti m, Ti n)
    : RowsCount{m}, ColsCount{n} {
  Data = &values->at(0);
}

template <typename Tw>
Matrix<Tw>::Matrix(Tw defvalue, Tw *values, Ti m, Ti n)
    : RowsCount{m}, ColsCount{n} {
  Data = values;
  for (Ti i = 0; i < m * n; i++)
    Data[i] = defvalue;
}

template <typename Tw>
Matrix<Tw>::Matrix(Tw defvalue, std::vector<Tw> &values, Ti m, Ti n)
    : RowsCount{m}, ColsCount{n} {
  Data = &values.at(0);
  for (Ti i = 0; i < m * n; i++)
    Data[i] = defvalue;
}

// #pragma endregion

// #pragma region Iterators

template <typename Tw> Tw *Matrix<Tw>::begin() { return Data; }

template <typename Tw> Tw *Matrix<Tw>::end() {
  return Data + RowsCount * ColsCount;
}

template <typename Tw> Tw *Matrix<Tw>::ColBegin(Ti col) {
  return Data + col * RowsCount;
}

template <typename Tw> Tw *Matrix<Tw>::ColEnd(Ti col) {
  return Data + (col + 1) * RowsCount;
}

template <typename Tw>
MatIterator<Tw>::MatIterator(Tw *p, int s) : ptr(p), stride(s) {}

template <typename Tw> MatIterator<Tw> &MatIterator<Tw>::operator++() {
  ptr += stride;
  return *this;
}

template <typename Tw> MatIterator<Tw> MatIterator<Tw>::operator++(Ti) {
  MatIterator tmp(*this);
  operator++();
  return tmp;
}

template <typename Tw>
bool MatIterator<Tw>::operator==(const MatIterator &rhs) const {
  return ptr == rhs.ptr;
}

template <typename Tw>
bool MatIterator<Tw>::operator!=(const MatIterator &rhs) const {
  return ptr != rhs.ptr;
}

template <typename Tw> Tw &MatIterator<Tw>::operator*() { return *ptr; }

template <typename Tw> MatIterator<Tw> Matrix<Tw>::RowBegin(Ti row) {
  return MatIterator(Data + row, RowsCount);
}

template <typename Tw> MatIterator<Tw> Matrix<Tw>::RowEnd(Ti row) {
  return MatIterator(Data + row + ColsCount * RowsCount, RowsCount);
}

// #pragma endregion

// #pragma region Data

template <typename Tw> void Matrix<Tw>::Restructure(Ti newrows, Ti newcols) {
  if (newrows * newcols != RowsCount * ColsCount)
    throw LdtException(ErrorType::kLogic, "matrix",
                       "number of elements does not match");
  Restructure0(newrows, newcols);
}

template <typename Tw> void Matrix<Tw>::Restructure0(Ti newrows, Ti newcols) {
  RowsCount = newrows;
  ColsCount = newcols;
}

template <typename Tw> Ti Matrix<Tw>::length() const {
  return RowsCount * ColsCount;
}

template <typename Tw>
void Matrix<Tw>::SetData(Tw *data, Ti newrows, Ti newcols) {
  if (newrows != -1)
    RowsCount = newrows;
  if (newcols != -1)
    ColsCount = newcols;
  Data = data;
}

template <typename Tw>
void Matrix<Tw>::SetData(Tw defaultvalue, Tw *data, Ti newrows, Ti newcols) {
  if (newrows != -1)
    RowsCount = newrows;
  if (newcols != -1)
    ColsCount = newcols;
  Data = data;
  for (Ti i = 0; i < RowsCount * ColsCount; i++)
    Data[i] = defaultvalue;
}

template <typename Tw> bool Matrix<Tw>::IsVector() const {
  return ColsCount == 1;
}

template <typename Tw> bool Matrix<Tw>::IsEmpty() const {
  return length() == 0;
}

template <typename Tw> bool Matrix<Tw>::IsSquare() const {
  return RowsCount == ColsCount;
}

template <typename Tw> bool Matrix<Tw>::IsSymmetric(Tw epsilon) const {
  auto N = RowsCount;
  if (ColsCount != N)
    throw LdtException(ErrorType::kLogic, "matrix",
                       "invalid operation: Matrix is not square");
  Tw d;
  for (Ti i = 0; i < N; i++)
    for (Ti j = 0; j < N; j++) {
      if (i >= j)
        continue;
      d = std::abs(Get0(i, j) - Get0(j, i));
      if (d > epsilon)
        return false;
    }
  return true;
}

template <typename Tw> bool Matrix<Tw>::HasNaN() const {
  if constexpr (std::numeric_limits<Tw>::has_quiet_NaN) {
    Ti L = length();
    if (L == 0)
      return false;
    Ti i;
    Tw d;
    for (i = 0; i < L; i++) {
      d = Data[i];
      if (std::isnan(d))
        return true;
    }
    return false;
  } else
    throw LdtException(ErrorType::kLogic, "matrix", "invalid operation");
}

// #pragma endregion

// #pragma region Index

template <typename Tw>
void Matrix<Tw>::TranslateIndex(Ti index, Ti &rowIndex, Ti &colIndex) const {
  colIndex = index / RowsCount;
  rowIndex = index % RowsCount;
}

template <typename Tw> Tw Matrix<Tw>::Get(Ti i, Ti j) const {
  if (!Data)
    throw std::out_of_range("Get: Data is not initialized.");
  if (i >= RowsCount || j >= ColsCount || i < 0 || j < 0)
    throw std::out_of_range(format(
        "index out-of-range in get function: (i, j)=({}, {}), Dim=({}, {})", i,
        j, RowsCount, ColsCount));
  return Get0(i, j);
}

template <typename Tw> Tw Matrix<Tw>::Get0(Ti i, Ti j) const {
  return Data[i + RowsCount * j];
}

template <typename Tw> Tw &Matrix<Tw>::Get0r(Ti i, Ti j) {
  return Data[i + RowsCount * j];
}

template <typename Tw> Tw Matrix<Tw>::Get(Ti i) const {
  if (!Data)
    throw std::out_of_range("Get: Data is not initialized.");
  if (i < 0 || i >= length())
    throw std::out_of_range(format(
        "index out-of-range in get function: i={}, Length={}", i, length()));
  return Data[i];
}

template <typename Tw> Tw Matrix<Tw>::GetVector(Ti i) const {
  if (IsVector() == false)
    throw LdtException(ErrorType::kLogic, "matrix", "a vector is expected");
  if (i < 0 || i >= RowsCount)
    throw std::out_of_range("index out-of-range exception");
  return Data[i];
}

template <typename Tw> void Matrix<Tw>::Set(Ti i, Ti j, Tw value) {
  if (!Data)
    throw std::out_of_range("Set: Data is not initialized.");
  if (i >= RowsCount || j >= ColsCount || i < 0 || j < 0)
    throw std::out_of_range(format(
        "index out-of-range in set function: (i, j)=({}, {}), Dim=({}, {})", i,
        j, RowsCount, ColsCount));
  Set0(i, j, value);
}

template <typename Tw> void Matrix<Tw>::Set0(Ti i, Ti j, Tw value) {
  Data[i + RowsCount * j] = value;
}

template <typename Tw> void Matrix<Tw>::Set_Plus(Ti i, Ti j, Tw value) {
  if (i >= RowsCount || j >= ColsCount || i < 0 || j < 0)
    throw std::out_of_range(format(
        "index out-of-range in set function: i={}, Length={}", i, length()));
  Set_Plus0(i, j, value);
}

template <typename Tw> void Matrix<Tw>::Set_Plus0(Ti i, Ti j, Tw value) {
  Data[i + RowsCount * j] += value;
}

template <typename Tw> void Matrix<Tw>::Set_Minus(Ti i, Ti j, Tw value) {
  if (i >= RowsCount || j >= ColsCount || i < 0 || j < 0)
    throw std::out_of_range("index out-of-range exception");
  Set_Minus0(i, j, value);
}

template <typename Tw> void Matrix<Tw>::Set_Minus0(Ti i, Ti j, Tw value) {
  Data[i + RowsCount * j] -= value;
}

template <typename Tw> void Matrix<Tw>::Set(Ti i, Tw value) {
  if (!Data)
    throw std::out_of_range("Set: Data is not initialized.");
  if (i >= length() || i < 0)
    throw std::out_of_range(format(
        "index out-of-range in set function: i={}, Length={}", i, length()));
  Data[i] = value;
}

template <typename Tw> void Matrix<Tw>::SetVector(Ti i, Tw value) {
  if (IsVector() == false)
    throw std::out_of_range("a vector is expected");
  if (i < 0 || i >= RowsCount)
    throw std::out_of_range("index out-of-range exception");
  Data[i] = value;
}

// #pragma endregion

// #pragma region Equality

template <typename Tw>
bool EqualsValueArray(Tw *array, Ti length, Tw b, Tw epsilon, bool ignoreNAN,
                      bool nansAreEqual) {
  if constexpr (std::numeric_limits<Tw>::has_quiet_NaN) {
    bool bIsNan = std::isnan(b);
    if (bIsNan && ignoreNAN)
      return true;

    for (Ti i = 0; i < length; i++) {
      Tw a = array[i];
      bool aIsNan = std::isnan(a);
      if (aIsNan && ignoreNAN)
        continue;

      if (bIsNan || aIsNan) {
        if (bIsNan && aIsNan && nansAreEqual)
          continue;
        else
          return false;
      }

      if (std::abs(b - a) > epsilon)
        return false;
    }
    return true;
  } else if constexpr (true) // ignore NAN
  {
    for (Ti i = 0; i < length; i++) {
      Tw a = array[i];

      if constexpr (std::is_unsigned<Tw>() == false) {
        if (std::abs(b - a) > epsilon)
          return false;
      } else if constexpr (true) {
        if (a != b)
          return false;
      }
    }
    return true;
  }
}

template <typename Tw>
bool Matrix<Tw>::EqualsValue(Tw b, Tw epsilon, bool nansAreEqual,
                             bool ignoreNan) const {
  return EqualsValueArray(Data, length(), b, epsilon, ignoreNan, nansAreEqual);
}

template <typename Tw>
bool Matrix<Tw>::EqualsValueColumn(Ti colIndex, Tw b, Tw epsilon,
                                   bool nansAreEqual, bool ignoreNan) const {
  return EqualsValueArray(&Data[colIndex * RowsCount], RowsCount, b, epsilon,
                          ignoreNan, nansAreEqual);
}

template <typename Tw>
bool Matrix<Tw>::Equals(const Matrix<Tw> &m, Tw epsilon, bool throwForSize,
                        bool nansAreEqual) const {
  Tw d;
  return Equals(m, d, epsilon, throwForSize, nansAreEqual);
}

template <typename Tw>
bool Matrix<Tw>::Equals(const Matrix<Tw> &m, Tw &abs_diff, Tw epsilon,
                        bool throwForSize, bool nansAreEqual) const {
  if (this == &m)
    return true; // same object

  if (m.RowsCount != RowsCount || m.ColsCount != ColsCount) { // different sizes
    if (throwForSize) {
      throw ::std::logic_error("unequal size");
    } else {
      return false;
    }
  }

  if constexpr (std::numeric_limits<Tw>::has_quiet_NaN) {
    for (Ti i = 0; i < length(); i++) {
      Tw a = Data[i];
      Tw b = m.Data[i];
      bool aIsNan = std::isnan(a);
      bool bIsNan = std::isnan(b);
      if (bIsNan || aIsNan) {
        if (bIsNan && aIsNan && nansAreEqual)
          continue;
        else {
          abs_diff = NAN;
          return false;
        }
      }
      abs_diff = abs(b - a);
      if (abs_diff > epsilon)
        return false;
    }
    return true;
  } else { // ignore NAN (there is no nan)
    for (Ti i = 0; i < length(); i++) {
      Tw a = Data[i];
      Tw b = m.Data[i];
      if constexpr (std::is_unsigned<Tw>() == false) {
        abs_diff = std::abs(b - a);
        if (abs_diff > epsilon)
          return false;
      } else if constexpr (true) {
        if (a != b)
          return false;
      }
    }
    return true;
  }
}

// #pragma endregion

// #pragma region NAN

template <typename Tw>
IndexRange Matrix<Tw>::GetRangeColumn(bool &hasMissing, Ti j) const {
  if constexpr (std::numeric_limits<Tw>::has_quiet_NaN) {

    Tw *col = &Data[RowsCount * j];
    auto range = Array<Tw>::GetRange(col, RowsCount, hasMissing);
    return range;

  } else if constexpr (true) {
    throw LdtException(ErrorType::kLogic, "matrix",
                       "invalid operation"); // there is no NAN
  }
}

template <typename Tw>
IndexRange Matrix<Tw>::GetRangeRow(bool &hasMissing, Ti i) const {
  if constexpr (std::numeric_limits<Tw>::has_quiet_NaN) {
    hasMissing = false;
    Ti start;
    Ti end;
    for (start = 0; start < ColsCount; start++)
      if (std::isnan(Get0(i, start)) == false)
        break;

    for (end = ColsCount; end > 0; end--)
      if (std::isnan(Get0(i, end - 1)) == false) {
        end--;
        break;
      }

    auto range = IndexRange(start, end);

    if (range.IsNotValid())
      return range;

    for (Ti j = range.StartIndex; j <= range.EndIndex; j++)
      if (std::isnan(Get0(i, j))) {
        hasMissing = true;
        break;
      }
    return range;
  } else if constexpr (true) {
    throw LdtException(ErrorType::kLogic, "matrix",
                       "invalid operation"); // there is no NAN
  }
}

template <typename Tw>
IndexRange Matrix<Tw>::InterpolateColumn(Ti &count, Ti colIndex) {

  if constexpr (std::numeric_limits<Tw>::has_quiet_NaN) {

    Tw *col = &Data[RowsCount * colIndex];
    auto range = Array<Tw>::Interpolate(col, RowsCount, count);
    return range;

  } else if constexpr (true) {
    throw LdtException(ErrorType::kLogic, "matrix",
                       "invalid operation"); // there is no NAN
  }
}

template <typename Tw>
IndexRange Matrix<Tw>::InterpolateRow(Ti &count, Ti rowIndex) {
  if constexpr (std::numeric_limits<Tw>::has_quiet_NaN) {
    bool hasMissing = false;
    auto range = GetRangeRow(hasMissing, rowIndex);
    count = 0;
    if (hasMissing) {
      bool inMissing = false;
      Tw first = NAN, d, last = NAN;
      Ti length = 1;

      for (Ti i = range.StartIndex; i <= range.EndIndex; i++) {
        d = Get0(rowIndex, i);
        auto isNaN = std::isnan(d);

        if (isNaN)
          length++;

        if (isNaN == false && inMissing) {
          last = d;

          // calculate and set
          Tw step = (last - first) / length;
          for (int j = 1; j < length; j++) {
            Set0(rowIndex, i - j, d - j * step);
            count++;
          }

          length = 1;
          inMissing = false;
        }

        if (isNaN && inMissing == false) {
          first = Get0(rowIndex, i - 1);
          inMissing = true;
        }
      }
    }
    return range;
  } else if constexpr (true)
    throw LdtException(ErrorType::kLogic, "matrix",
                       "invalid operation"); // there is no NAN
}

template <typename Tw>
IndexRange Matrix<Tw>::GetRange(bool &anyColHasMissing) const {
  anyColHasMissing = false;
  Ti start = 0;
  Ti end = std::numeric_limits<Ti>::max();
  for (Ti j = 0; j < ColsCount; j++) {
    bool colHasM;
    auto rng = GetRangeColumn(colHasM, j);
    if (anyColHasMissing == false && colHasM)
      anyColHasMissing = colHasM;
    start = std::max(start, rng.StartIndex);
    end = std::min(end, rng.EndIndex);
    if (start > end) {
      return IndexRange(-1, -1);
    }
  }

  return IndexRange(start, end);
}

template <typename Tw>
void Matrix<Tw>::GetAnyNanRow(std::vector<Ti> &rowIndexes, bool checkInfinity,
                              std::vector<Ti> *colIndexes) const {
  if constexpr (std::numeric_limits<Tw>::has_quiet_NaN) {
    Ti i, j = 0;
    Tw d;
    bool isBroken;
    Tw *row;
    if (colIndexes) {
      if (checkInfinity == false) {
        for (i = 0; i < RowsCount; i++) {
          row = &Data[i];
          isBroken = false;
          for (auto &k : *colIndexes) {
            d = row[k * RowsCount];
            if (std::isnan(d)) {
              isBroken = true;
              break;
            }
          }
          if (isBroken == false)
            rowIndexes.push_back(i);
        }
      } else {
        for (i = 0; i < RowsCount; i++) {
          row = &Data[i];
          isBroken = false;
          for (auto &k : *colIndexes) {
            d = row[k * RowsCount];
            if (std::isnan(d) || std::isinf(d)) {
              isBroken = true;
              break;
            }
          }
          if (isBroken == false)
            rowIndexes.push_back(i);
        }
      }
    } else {
      if (checkInfinity == false) {
        for (i = 0; i < RowsCount; i++) {
          row = &Data[i];
          isBroken = false;
          for (j = 0; j < ColsCount; j++) {
            d = row[j * RowsCount];
            if (std::isnan(d)) {
              isBroken = true;
              break;
            }
          }
          if (isBroken == false)
            rowIndexes.push_back(i);
        }
      } else {
        for (i = 0; i < RowsCount; i++) {
          row = &Data[i];
          isBroken = false;
          for (j = 0; j < ColsCount; j++) {
            d = row[j * RowsCount];
            if (std::isnan(d) || std::isinf(d)) {
              isBroken = true;
              break;
            }
          }
          if (isBroken == false)
            rowIndexes.push_back(i);
        }
      }
    }
  } else
    throw LdtException(ErrorType::kLogic, "matrix",
                       "invalid operation"); // there is no NAN
}

// #pragma endregion

// #pragma region Convert

template <typename Tw> void Matrix<Tw>::RemoveNanVector_in(bool removeInf) {
  if constexpr (std::numeric_limits<Tw>::has_quiet_NaN) {
    Ti j = 0;
    Tw d;
    if (removeInf == false) {
      for (Ti i = 0; i < length(); i++) {
        d = Data[i];
        if (std::isnan(d))
          continue;
        else {
          Data[j] = d;
          j++;
        }
      }
    } else {
      for (Ti i = 0; i < length(); i++) {
        d = Data[i];
        if (std::isnan(d) || std::isinf(d))
          continue;
        else {
          Data[j] = d;
          j++;
        }
      }
    }

    if (ColsCount > 1)
      Restructure0(1, j);
    else
      Restructure0(j, 1);
  } else
    throw LdtException(ErrorType::kLogic, "matrix",
                       "invalid operation"); // there is no NAN
}

template <typename Tw> void Matrix<Tw>::RemoveColumnsIn(std::vector<Ti> &cols) {
  if (cols.size() == 0)
    return;
  // I cannot use the same algorithm for removing rows
  // because of the data structure (column-wise)

  bool remove;
  Ti i, j, ww = 0;
  Tw *r_col;
  Tw *w_col;
  for (j = 0; j < ColsCount; j++) {
    r_col = &Data[j * RowsCount];
    w_col = &Data[ww * RowsCount];
    // should I copy r_col to w_col?
    remove = std::find(cols.begin(), cols.end(), j) != cols.end();
    if (remove)
      continue;
    // copy r_col to w_col and move to the next column
    for (i = 0; i < RowsCount; i++)
      w_col[i] = r_col[i];
    ww++;
  }

  this->Restructure0(RowsCount, ww);
}

template <typename Tw> void Matrix<Tw>::RemoveColumnsAnyNan_in(bool removeInf) {
  if constexpr (std::numeric_limits<Tw>::has_quiet_NaN) {
    // I cannot use the same algorithm for removing rows
    // because of the data structure (column-wise)
    // it writes data and if NAN/INF is located, it restarts writing in the
    // same column, from the next column

    bool broken;
    Ti i, j, rr = 0, ww = 0;
    Tw *r_col;
    Tw *w_col;
    Tw d;
    if (removeInf == false) {
      for (i = 0; i < ColsCount; i++) {
        r_col = &Data[rr * RowsCount];
        w_col = &Data[ww * RowsCount];
        broken = false;
        for (j = 0; j < RowsCount; j++) {
          d = r_col[j];
          if (std::isnan(d)) {
            broken = true;
            break;
          }
          w_col[j] = d;
        }
        rr++;
        if (broken == false)
          ww++; // go to the next row for writing
                // else, we stay in this
      }
    } else {
      for (i = 0; i < ColsCount; i++) {
        r_col = &Data[rr * RowsCount];
        w_col = &Data[ww * RowsCount];
        broken = false;
        for (j = 0; j < RowsCount; j++) {
          d = r_col[j];
          if (std::isnan(d) || std::isinf(d)) {
            broken = true;
            break;
          }
          w_col[j] = d;
        }
        rr++;
        if (broken == false)
          ww++; // go to the next row for writing
                // else, we stay in this
      }
    }

    Restructure0(RowsCount, ww);
  } else
    throw LdtException(ErrorType::kLogic, "matrix",
                       "invalid operation"); // there is no NAN
}

template <typename Tw>
Ti Matrix<Tw>::RemoveNanVector(Matrix<Tw> &data, Matrix<Tw> &storage) {
  if constexpr (std::numeric_limits<Tw>::has_quiet_NaN) {
    if (storage.Data == nullptr) {
      Ti count = 0;
      for (Ti i = 0; i < data.length(); i++) {
        if (std::isnan(data.Data[i]) == false)
          count++;
      }
      return count;
    }

    Ti j = 0;
    for (Ti i = 0; i < data.length(); i++) {
      auto d = data.Data[i];
      if (std::isnan(d) == false) {
        storage.Data[j] = d;
        j++;
      }
    }
    return storage.length();
  } else
    throw LdtException(ErrorType::kLogic, "matrix",
                       "invalid operation"); // there is no NAN
}

// #pragma endregion

// #pragma region linq - like functions

template <typename Tw> Tw Matrix<Tw>::Last() { return Data[length()]; }
template <typename Tw> Tw Matrix<Tw>::First() { return Data[0]; }

template <typename Tw>
void Matrix<Tw>::IndicesOfVector(Tw value, std::vector<Ti> &vec) const {
  Ti i;
  if constexpr (std::numeric_limits<Tw>::has_quiet_NaN) {
    if (std::isnan(value)) {
      for (i = 0; i < length(); i++)
        if (std::isnan(Data[i]))
          vec.push_back(i);
    } else {
      for (i = 0; i < length(); i++)
        if (Data[i] == value)
          vec.push_back(i);
    }
  } else {
    for (i = 0; i < length(); i++)
      if (Data[i] == value)
        vec.push_back(i);
  }
}

template <typename Tw> bool Matrix<Tw>::Any(Tw value) const {
  Ti i;
  if constexpr (std::numeric_limits<Tw>::has_quiet_NaN) {
    if (std::isnan(value)) {
      for (i = 0; i < length(); i++)
        if (std::isnan(Data[i]))
          return true;
    } else {
      for (i = 0; i < length(); i++)
        if (Data[i] == value)
          return true;
    }
  } else if constexpr (true) {
    for (i = 0; i < length(); i++)
      if (Data[i] == value)
        return true;
  }
  return false;
}

template <typename Tw> bool Matrix<Tw>::All(Tw value) const {
  Ti i;
  if constexpr (std::numeric_limits<Tw>::has_quiet_NaN) {
    if (std::isnan(value)) {
      for (i = 0; i < length(); i++)
        if (std::isnan(Data[i]) == false)
          return false;
    } else {
      for (i = 0; i < length(); i++)
        if (Data[i] != value)
          return false;
    }
  } else if constexpr (true) {
    for (i = 0; i < length(); i++)
      if (Data[i] != value)
        return false;
  }
  return true;
}

template <typename Tw>
void Matrix<Tw>::Sort(Matrix<Tw> &storage, bool ascending) const {
  if (storage.ColsCount != this->ColsCount ||
      storage.RowsCount != this->RowsCount)
    throw LdtException(ErrorType::kLogic, "matrix",
                       "invalid dimension: storage");

  CopyTo00(storage);

  if (ascending) {
    for (Ti j = 0; j < ColsCount; j++) {
      auto col = &storage.Data[RowsCount * j];
      std::sort(col, &col[RowsCount]);
    }
  } else {
    for (Ti j = 0; j < ColsCount; j++) {
      auto col = &storage.Data[RowsCount * j];
      std::sort(col, &col[RowsCount], std::greater<Tw>());
    }
  }
}

template <typename Tw>
void Matrix<Tw>::SortByVector(Matrix<Tw> &storage, std::vector<Ti> &indexes) {
  if (storage.length() != this->length())
    throw LdtException(ErrorType::kLogic, "matrix", "invalid length: storage");
  if ((Ti)indexes.size() != this->length())
    throw LdtException(ErrorType::kLogic, "matrix", "invalid size: indexes");
  auto max = *std::max_element(indexes.begin(), indexes.end());
  if (max > storage.length() - 1)
    throw LdtException(ErrorType::kLogic, "matrix",
                       "invalid maximum element: indexes");

  SortByVector0(storage, indexes);
}

template <typename Tw>
void Matrix<Tw>::SortByVector0(Matrix<Tw> &storage, std::vector<Ti> &indexes) {
  Ti j = 0;
  for (auto &i : indexes) {
    storage.Data[j] = this->Data[i];
    j++;
  }
}

template <typename Tw>
void Matrix<Tw>::SortRowsBy(Matrix<Tw> &storage, std::vector<Ti> &row_indexes) {
  if (storage.RowsCount != this->RowsCount ||
      storage.ColsCount != this->ColsCount)
    throw LdtException(ErrorType::kLogic, "matrix",
                       "invalid dimension: storage");
  if ((Ti)row_indexes.size() != this->RowsCount)
    throw LdtException(ErrorType::kLogic, "matrix",
                       "invalid size: row_indexes");
  auto max = *std::max_element(row_indexes.begin(), row_indexes.end());
  if (max > storage.RowsCount - 1)
    throw LdtException(ErrorType::kLogic, "matrix",
                       "invalid maximum element: row_indexes");

  SortRowsBy0(storage, row_indexes);
}
template <typename Tw>
void Matrix<Tw>::SortRowsBy0(Matrix<Tw> &storage,
                             std::vector<Ti> &row_indexes) {
  Ti j = 0;
  for (auto &i : row_indexes) {
    storage.SetRowFromRow(j, *this, i);
    j++;
  }
}

template <typename Tw>
void Matrix<Tw>::SortColumnsBy(Matrix<Tw> &storage,
                               std::vector<Ti> &col_indexes) {
  if (storage.RowsCount != this->RowsCount ||
      storage.ColsCount != this->ColsCount)
    throw LdtException(ErrorType::kLogic, "matrix",
                       "invalid dimension: storage");
  if ((Ti)col_indexes.size() != this->ColsCount)
    throw LdtException(ErrorType::kLogic, "matrix",
                       "invalid size: row_indexes");
  auto max = *std::max_element(col_indexes.begin(), col_indexes.end());
  if (max > storage.ColsCount - 1)
    throw LdtException(ErrorType::kLogic, "matrix",
                       "invalid maximum element: col_indexes");

  SortColumnsBy0(storage, col_indexes);
}
template <typename Tw>
void Matrix<Tw>::SortColumnsBy0(Matrix<Tw> &storage,
                                std::vector<Ti> &col_indexes) {
  Ti j = 0;
  for (auto &i : col_indexes) {
    storage.SetColumnFromColumn(j, *this, i);
    j++;
  }
}

template <typename Tw>
void Matrix<Tw>::SortIndicesVector(std::vector<Ti> &indexes,
                                   bool ascending) const {
  indexes.resize(this->length());
  std::iota(indexes.begin(), indexes.end(), 0);
  if (ascending)
    std::stable_sort(indexes.begin(), indexes.end(),
                     [&](Ti i1, Ti i2) { return Data[i1] < Data[i2]; });
  else
    std::stable_sort(indexes.begin(), indexes.end(),
                     [&](Ti i1, Ti i2) { return Data[i1] > Data[i2]; });
}

// #pragma endregion

// #pragma region Apply

template <typename Tw> void Matrix<Tw>::SetValue(Tw value) {
  for (Ti i = 0; i < length(); i++)
    Data[i] = value;
}

template <typename Tw> void Matrix<Tw>::SetSequence(Tw start, Tw step) {
  Tw s = start;
  for (Ti i = 0; i < length(); i++) {
    Data[i] = s;
    s += step;
  }
}

template <typename Tw> void Matrix<Tw>::SetValueDiag(Tw diag) {
  if (RowsCount != ColsCount)
    throw LdtException(ErrorType::kLogic, "matrix",
                       "invalid dimensions: matrix is not square");
  for (Ti i = 0; i < RowsCount; i++)
    Set0(i, i, diag);
}

template <typename Tw> void Matrix<Tw>::SetValueDiag(Tw diag, Tw off_diag) {
  if (RowsCount != ColsCount)
    throw LdtException(ErrorType::kLogic, "matrix",
                       "invalid dimensions: matrix is not square");
  SetValue(off_diag);
  SetValueDiag(diag);
}

template <typename Tw> void Matrix<Tw>::SetValueOffDiag(Tw offdiag) {
  if (RowsCount != ColsCount)
    throw LdtException(ErrorType::kLogic, "matrix",
                       "invalid dimensions: Matrix<Tw> is not square");
  for (Ti i = 0; i < RowsCount; i++)
    for (Ti j = 0; j < RowsCount; j++)
      if (i != j)
        Set0(i, j, offdiag);
}

template <typename Tw> void Matrix<Tw>::Apply_in(std::function<Tw(Tw)> &func) {
  for (Ti i = 0; i < length(); i++)
    Data[i] = func(Data[i]);
}

template <typename Tw>
void Matrix<Tw>::Apply_in(const Matrix<Tw> &B,
                          std::function<Tw(Tw, Tw)> &func) {
  if (B.length() != length())
    throw std::invalid_argument("B");
  for (Ti i = 0; i < length(); i++)
    Data[i] = func(Data[i], B.Data[i]);
}

template <typename Tw>
void Matrix<Tw>::ApplyRow_in(Ti i, std::function<Tw(Tw)> &func) {
  auto d = &Data[i];
  Ti p;
  for (Ti j = 0; j < ColsCount; j++) {
    p = RowsCount * j;
    d[p] = func(d[p]);
  }
}

template <typename Tw>
void Matrix<Tw>::ApplyColumn_in(Ti j, std::function<Tw(Tw)> &func) {
  auto d = &Data[RowsCount * j];
  for (Ti i = 0; i < RowsCount; i++)
    d[i] = func(d[i]);
}

template <typename Tw>
void Matrix<Tw>::Apply(std::function<Tw(Tw)> &func, Matrix<Tw> &storage) const {
  if (storage.length() != length())
    throw std::invalid_argument("storage");
  Apply0(func, storage);
}

template <typename Tw>
void Matrix<Tw>::Apply0(std::function<Tw(Tw)> &func,
                        Matrix<Tw> &storage) const {
  for (Ti i = 0; i < length(); i++)
    storage.Data[i] = func(Data[i]);
}

template <typename Tw>
void Matrix<Tw>::Apply(const Matrix<Tw> &B, std::function<Tw(Tw, Tw)> &func,
                       Matrix<Tw> &storage) const {
  if (storage.length() != length())
    throw std::invalid_argument("storage");
  if (B.length() != length())
    throw std::invalid_argument("B");
  Apply0(B, func, storage);
}

template <typename Tw>
void Matrix<Tw>::Apply0(const Matrix<Tw> &B, std::function<Tw(Tw, Tw)> &func,
                        Matrix<Tw> &storage) const {
  for (Ti i = 0; i < length(); i++)
    storage.Data[i] = func(Data[i], B.Data[i]);
}

// #pragma endregion

// #pragma region Copy

template <typename Tw> void Matrix<Tw>::CopyTo(Matrix<Tw> &storage) const {
  if (storage.RowsCount != RowsCount || storage.ColsCount != ColsCount)
    throw LdtException(ErrorType::kLogic, "matrix",
                       "dimensions does not match");
  CopyTo0(storage);
}

template <typename Tw> void Matrix<Tw>::CopyTo0(Matrix<Tw> &storage) const {
  if (this->length() != storage.length())
    throw LdtException(ErrorType::kLogic, "matrix", "lengths are not equal");
  CopyTo00(storage);
}

template <typename Tw> void Matrix<Tw>::CopyFrom(Matrix<Tw> &source) {
  if (source.RowsCount != RowsCount || source.ColsCount != ColsCount)
    throw LdtException(ErrorType::kLogic, "matrix",
                       "dimensions does not match");
  CopyFrom0(source);
}

template <typename Tw> void Matrix<Tw>::CopyFrom0(Matrix<Tw> &source) {
  if (this->length() != source.length())
    throw LdtException(ErrorType::kLogic, "matrix", "lengths are not equal");
  CopyFrom00(source);
}

template <typename Tw> void Matrix<Tw>::CopyFrom00(const Matrix<Tw> &source) {
  source.CopyTo00(*this);
}

// #pragma endregion

// #pragma region Sub

template <typename Tw>
void Matrix<Tw>::SetSub(Ti rowstart, Ti colstart, const Matrix<Tw> &source,
                        Ti sourcerowstart, Ti sourcecolstart, Ti rowcount,
                        Ti colcount) {
  Ti lastrow = rowstart + rowcount;
  Ti lastcol = colstart + colcount;
  if (lastrow > RowsCount)
    throw std::invalid_argument(
        "inconsistent size: this  'rowstart' or 'rowcount'");
  if (lastcol > ColsCount)
    throw std::invalid_argument(
        "inconsistent size: this 'colstart' or 'colcount'");

  Ti sourcelastrow = sourcerowstart + rowcount;
  Ti sourcelastcol = sourcecolstart + colcount;
  if (sourcelastrow > source.RowsCount)
    throw std::invalid_argument(
        "inconsistent size: source  'rowstart' or 'rowcount'");
  if (sourcelastcol > source.ColsCount)
    throw std::invalid_argument(
        "inconsistent size: source 'colstart' or 'colcount'");

  SetSub0(rowstart, colstart, source, sourcerowstart, sourcecolstart, rowcount,
          colcount);
}

template <typename Tw>
void Matrix<Tw>::SetSub0(Ti rowstart, Ti colstart, const Matrix<Tw> &source,
                         Ti sourcerowstart, Ti sourcecolstart, Ti rowcount,
                         Ti colcount) {
  Ti lastrow = rowstart + rowcount;
  Ti lastcol = colstart + colcount;
  Ti p = sourcerowstart;
  for (Ti i = rowstart; i < lastrow; i++) {
    Ti q = sourcecolstart;
    for (Ti j = colstart; j < lastcol; j++) {
      Set0(i, j, source.Get0(p, q));
      q++;
    }
    p++;
  }
}

template <typename Tw>
void Matrix<Tw>::SetSub_t(Ti rowstart, Ti colstart, const Matrix<Tw> &source,
                          Ti sourcerowstart, Ti sourcecolstart, Ti rowcount,
                          Ti colcount) {
  Ti lastrow = rowstart + rowcount;
  Ti lastcol = colstart + colcount;
  if (lastrow > RowsCount)
    throw std::invalid_argument(
        "inconsistent size: this  'rowstart' or 'rowcount'");
  if (lastcol > ColsCount)
    throw std::invalid_argument(
        "inconsistent size: this 'colstart' or 'colcount'");

  Ti sourcelastrow = sourcecolstart + rowcount;
  Ti sourcelastcol = sourcerowstart + colcount;
  if (sourcelastrow > source.ColsCount)
    throw std::invalid_argument(
        "inconsistent size: source  'colstart' or 'colcount'");
  if (sourcelastcol > source.RowsCount)
    throw std::invalid_argument(
        "inconsistent size: source 'rowstart' or 'rowcount'");

  SetSub_t0(rowstart, colstart, source, sourcerowstart, sourcecolstart,
            rowcount, colcount);
}

template <typename Tw>
void Matrix<Tw>::SetSub_t0(Ti rowstart, Ti colstart, const Matrix<Tw> &source,
                           Ti sourcerowstart, Ti sourcecolstart, Ti rowcount,
                           Ti colcount) {
  Ti lastrow = rowstart + rowcount;
  Ti lastcol = colstart + colcount;
  Ti p = sourcecolstart;
  for (Ti i = rowstart; i < lastrow; i++) {
    Ti q = sourcerowstart;
    for (Ti j = colstart; j < lastcol; j++) {
      Set0(i, j, source.Get0(q, p));
      q++;
    }
    p++;
  }
}

template <typename Tw>
void Matrix<Tw>::SetSubVector(Ti start, const Matrix<Tw> &source,
                              Ti sourcestart, Ti count) {
  Ti last = start + count;
  if (last > RowsCount)
    throw std::invalid_argument("inconsistent size: 'start' or 'count'");
  SetSubVector0(start, source, sourcestart, count);
}

template <typename Tw>
void Matrix<Tw>::SetSubVector0(Ti start, const Matrix<Tw> &source,
                               Ti sourcestart, Ti count) {
  Ti last = start + count;
  Ti p = sourcestart;
  for (Ti i = start; i < last; i++) {
    Data[i] = source.Data[p];
    p++;
  }
}

template <typename Tw>
void Matrix<Tw>::GetSub(Ti rowstart, Ti colstart, Ti rowcount, Ti colcount,
                        Matrix<Tw> &storage, Ti storagerowstart,
                        Ti storaecolstart) const {
  Ti lastrow = rowstart + rowcount;
  Ti lastcol = colstart + colcount;
  if (lastrow > RowsCount)
    throw std::invalid_argument(
        "inconsistent size: this  'rowstart' or 'rowcount'");
  if (lastcol > ColsCount)
    throw std::invalid_argument(
        "inconsistent size: this 'colstart' or 'colcount'");

  auto storreqrow = rowcount + storagerowstart;
  auto storreqcol = colcount + storaecolstart;

  if (storage.RowsCount > storreqrow || storage.ColsCount > storreqcol)
    throw std::invalid_argument("inconsistent size in get sub (1). ");

  GetSub0(rowstart, colstart, rowcount, colcount, storage, storagerowstart,
          storaecolstart);
}

template <typename Tw>
void Matrix<Tw>::GetSub0(Ti rowstart, Ti colstart, Ti rowcount, Ti colcount,
                         Matrix<Tw> &storage, Ti storagerowstart,
                         Ti storaecolstart) const {
  storage.SetSub0(storagerowstart, storaecolstart, *this, rowstart, colstart,
                  rowcount, colcount);
}

template <typename Tw>
void Matrix<Tw>::GetSub(Ti firststart, Ti firstcount,
                        const std::vector<Ti> &secondindexes, bool firstIsRow,
                        Matrix<Tw> &storage, Ti storagerowstart,
                        Ti storaecolstart, bool exclude) const {
  if (exclude) {
    if (firstIsRow) {
      if (storage.RowsCount != firstcount + storagerowstart)
        throw std::out_of_range(
            format("index out-of-range in get sub function: storage rows={}, "
                   "row count={}, storage row start={}",
                   storage.RowsCount, firstcount, storagerowstart));
      if (storage.ColsCount !=
          ColsCount - (Ti)secondindexes.size() + storaecolstart)
        throw std::out_of_range(format(
            "index out-of-range in get sub function: storage columns={}, "
            "columns count={}"
            "column indices size={}, storage column start={}",
            storage.ColsCount, ColsCount, secondindexes.size(),
            storaecolstart));
    }
    if (firstIsRow == false) {
      if (storage.ColsCount != firstcount + storaecolstart)
        throw std::out_of_range(format(
            "index out-of-range in get sub function: storage columns={}, "
            "columns count={}, storage column start={}",
            storage.ColsCount, firstcount, storaecolstart));
      if (storage.RowsCount !=
          RowsCount - (Ti)secondindexes.size() + storagerowstart)
        throw std::out_of_range(
            format("index out-of-range in get sub function: storage rows={}, "
                   "rows count={}"
                   "row indices size={}, storage row start={}",
                   storage.RowsCount, RowsCount, secondindexes.size(),
                   storagerowstart));
    }
  } else {
    if (firstIsRow) {
      if (storage.RowsCount != firstcount + storagerowstart)
        throw std::out_of_range(
            format("index out-of-range in get sub function: storage rows={}, "
                   "rows count={}, storage row start={}",
                   storage.RowsCount, firstcount, storagerowstart));
      if (storage.ColsCount != (Ti)secondindexes.size() + storaecolstart)
        throw std::out_of_range(format(
            "index out-of-range in get sub function: storage columns={}, "
            "column indices size={}, storage column start={}",
            storage.ColsCount, secondindexes.size(), storaecolstart));
    }
    if (firstIsRow == false) {
      if (storage.ColsCount != firstcount + storaecolstart)
        throw std::out_of_range(format(
            "index out-of-range in get sub function: storage columns={}, "
            "columns count={}, storage row start={}",
            storage.ColsCount, firstcount, storaecolstart));
      if (storage.RowsCount != (Ti)secondindexes.size() + storagerowstart)
        throw std::out_of_range(
            format("index out-of-range in get sub function: storage rows={}, "
                   "row indices size={}, storage row start={}",
                   storage.RowsCount, secondindexes.size(), storagerowstart));
    }
  }
  GetSub0(firststart, firstcount, secondindexes, firstIsRow, storage,
          storagerowstart, storaecolstart, exclude);
}

template <typename Tw>
void Matrix<Tw>::GetSub0(Ti firststart, Ti firstcount,
                         const std::vector<Ti> &secondindexes, bool firstIsRow,
                         Matrix<Tw> &storage, Ti storagerowstart,
                         Ti storaecolstart, bool exclude_indexes) const {
  std::vector<Ti> indexes = secondindexes;
  if (exclude_indexes) {
    indexes = std::vector<Ti>();
    auto count = firstIsRow ? ColsCount : RowsCount;
    for (Ti j = 0; j < count; j++) {
      if (std::find(secondindexes.begin(), secondindexes.end(), j) !=
          secondindexes.end())
        continue;
      indexes.push_back(j);
    }
  }

  if (firstIsRow) {
    Ti p = storagerowstart;
    for (Ti i = firststart; i < firststart + firstcount; i++) {
      Ti q = storaecolstart;
      for (auto &j : indexes) {
        storage.Set0(p, q, this->Get0(i, j));
        q++;
      }
      p++;
    }
  } else {
    Ti q = storaecolstart;
    for (Ti j = firststart; j < firststart + firstcount; j++) {
      Ti p = storagerowstart;
      for (auto &i : indexes) {
        storage.Set0(p, q, this->Get0(i, j));
        p++;
      }
      q++;
    }
  }
}

template <typename Tw>
void Matrix<Tw>::GetSub(std::vector<Ti> &rowindexes,
                        std::vector<Ti> &colindexes, Matrix<Tw> &storage,
                        Ti storagerowstart, Ti storaecolstart) const {
  if (storage.RowsCount != (Ti)rowindexes.size() + storagerowstart)
    throw std::invalid_argument("inconsistent size: 'storage'");
  if (storage.ColsCount != (Ti)colindexes.size() + storaecolstart)
    throw std::invalid_argument("inconsistent size: 'storage'");

  GetSub0(rowindexes, colindexes, storage, storagerowstart, storaecolstart);
}

template <typename Tw>
void Matrix<Tw>::GetSub0(std::vector<Ti> &rowindexes,
                         std::vector<Ti> &colindexes, Matrix<Tw> &storage,
                         Ti storagerowstart, Ti storaecolstart) const {
  Ti p = storagerowstart;
  for (auto &i : rowindexes) {
    Ti q = storaecolstart;
    for (auto &j : colindexes) {
      storage.Set0(p, q, this->Get0(i, j));
      q++;
    }
    p++;
  }
}

template <typename Tw>
void Matrix<Tw>::GetSubVector(Ti start, Ti count, Matrix<Tw> &storage,
                              Ti storagestart) const {
  if (IsVector() == false)
    throw LdtException(ErrorType::kLogic, "matrix",
                       "use this method for vectors");

  auto storreqrow = count + storagestart;
  if (storage.RowsCount != storreqrow)
    throw std::invalid_argument("inconsistent size: 'storage'");

  GetSubVector0(start, count, storage, storagestart);
}

template <typename Tw>
void Matrix<Tw>::GetSubVector0(Ti start, Ti count, Matrix<Tw> &storage,
                               Ti storagestart) const {
  storage.SetSubVector(storagestart, *this, start, count);
}

template <typename Tw> void Matrix<Tw>::SetRow(Ti i, Tw value) {
  if (i >= RowsCount || i < 0)
    throw std::invalid_argument("invalid index");
  SetRow0(i, value);
}
template <typename Tw> void Matrix<Tw>::SetRow0(Ti i, Tw value) {
  auto d = &Data[i];
  for (Ti j = 0; j < ColsCount; j++)
    d[RowsCount * j] = value;
}

template <typename Tw> void Matrix<Tw>::SetRow_plus(Ti i, Tw value) {
  if (i >= RowsCount || i < 0)
    throw std::invalid_argument("invalid index");
  SetRow_plus0(i, value);
}

template <typename Tw> void Matrix<Tw>::SetRow_minus(Ti i, Tw value) {
  if (i >= RowsCount || i < 0)
    throw std::invalid_argument("invalid index");
  SetRow_minus0(i, value);
}

template <typename Tw> void Matrix<Tw>::SetRow_plus0(Ti i, Tw value) {
  auto d = &Data[i];
  for (Ti j = 0; j < ColsCount; j++)
    d[RowsCount * j] += value;
}
template <typename Tw> void Matrix<Tw>::SetRow_minus0(Ti i, Tw value) {
  auto d = &Data[i];
  for (Ti j = 0; j < ColsCount; j++)
    d[RowsCount * j] -= value;
}

template <typename Tw> void Matrix<Tw>::SetRow(Ti i, const Matrix<Tw> &source) {
  if (i >= RowsCount || i < 0)
    throw std::invalid_argument("invalid index");
  SetRow0(i, source);
}
template <typename Tw>
void Matrix<Tw>::SetRow0(Ti i, const Matrix<Tw> &source) {
  auto d = &Data[i];
  for (Ti j = 0; j < ColsCount; j++)
    d[RowsCount * j] = source.Data[j];
}

template <typename Tw>
void Matrix<Tw>::SetRowFromRow(Ti i, const Matrix<Tw> &source, Ti k) {
  if (i >= RowsCount || i < 0)
    throw std::invalid_argument("invalid index: i");
  if (k >= source.RowsCount || k < 0)
    throw std::invalid_argument("invalid index: k");
  SetRowFromRow0(i, source, k);
}
template <typename Tw>
void Matrix<Tw>::SetRowFromRow0(Ti i, const Matrix<Tw> &source, Ti k) {
  auto d = &Data[i];
  auto b = &source.Data[k];
  for (Ti j = 0; j < ColsCount; j++)
    d[RowsCount * j] = b[source.RowsCount * j];
}

template <typename Tw>
void Matrix<Tw>::SetRowFromDiag(Ti i, const Matrix<Tw> &source) {
  if (i >= RowsCount || i < 0)
    throw std::invalid_argument("invalid index: i");
  if (ColsCount != source.ColsCount || ColsCount != source.RowsCount)
    throw std::invalid_argument("invalid dimension: source");
  SetRowFromDiag0(i, source);
}
template <typename Tw>
void Matrix<Tw>::SetRowFromDiag0(Ti i, const Matrix<Tw> &source) {
  auto d = &Data[i];
  Ti b = 0;
  for (Ti j = 0; j < ColsCount; j++) {
    d[RowsCount * j] = source.Data[b];
    b += source.RowsCount + 1;
  }
}

template <typename Tw>
void Matrix<Tw>::SetSubRow(Ti row, Ti colstart, Matrix<Tw> &source, Ti length) {
  if (row >= RowsCount || row < 0)
    throw std::invalid_argument("invalid index: i");
  if (colstart + length > ColsCount)
    throw std::invalid_argument("invalid dimension: this");
  if (source.length() < length)
    throw std::invalid_argument("invalid dimension: source");
  SetSubRow0(row, colstart, source.Data, length);
}
template <typename Tw>
void Matrix<Tw>::SetSubRow0(Ti row, Ti colstart, Tw *source, Ti length) {
  auto d = &Data[row];
  for (Ti j = colstart; j < colstart + length; j++)
    d[RowsCount * j] = source[j - colstart];
}

template <typename Tw>
void Matrix<Tw>::SetColumn(Ti j, const Matrix<Tw> &source) {
  if (j >= ColsCount || j < 0)
    throw std::invalid_argument("invalid index");
  SetColumn0(j, source);
}
template <typename Tw>
void Matrix<Tw>::SetColumn0(Ti j, const Matrix<Tw> &source) {
  auto d = &Data[RowsCount * j];
  for (Ti i = 0; i < RowsCount; i++)
    d[i] = source.Data[i];
}

template <typename Tw>
void Matrix<Tw>::SetColumnFromRow(Ti j, const Matrix<Tw> &source, Ti k) {
  if (j >= ColsCount || j < 0)
    throw std::invalid_argument("invalid index: j");
  if (k >= source.RowsCount || k < 0)
    throw std::invalid_argument("invalid index: k");
  SetColumnFromRow0(j, source, k);
}
template <typename Tw>
void Matrix<Tw>::SetColumnFromRow0(Ti j, const Matrix<Tw> &source, Ti k) {
  auto d = &Data[RowsCount * j];
  for (Ti i = 0; i < ColsCount; i++)
    d[i] = source.Get0(k, i);
}

template <typename Tw>
void Matrix<Tw>::SetColumnFromColumn(Ti j, const Matrix<Tw> &source, Ti k) {
  if (j >= ColsCount || j < 0)
    throw std::invalid_argument("invalid index: j");
  if (k >= source.ColsCount || k < 0)
    throw std::invalid_argument("invalid index: k");
  SetColumnFromColumn0(j, source, k);
}
template <typename Tw>
void Matrix<Tw>::SetColumnFromColumn0(Ti j, const Matrix<Tw> &source, Ti k) {
  auto d = &Data[RowsCount * j];
  auto b = &source.Data[RowsCount * k];
  for (Ti i = 0; i < RowsCount; i++)
    d[i] = b[i];
}

template <typename Tw>
void Matrix<Tw>::SetColumnFromDiag(Ti j, const Matrix<Tw> &source) {
  if (j >= ColsCount || j < 0)
    throw std::invalid_argument("invalid index: j");
  if (source.RowsCount != RowsCount || source.ColsCount != RowsCount)
    throw std::invalid_argument("invalid dimention: source");
  SetColumnFromDiag0(j, source);
}
template <typename Tw>
void Matrix<Tw>::SetColumnFromDiag0(Ti j, const Matrix<Tw> &source) {
  auto d = &Data[RowsCount * j];
  Ti b = 0;
  for (Ti i = 0; i < RowsCount; i++) {
    d[i] = source.Data[b];
    b += source.RowsCount + 1;
  }
}

template <typename Tw> void Matrix<Tw>::SetColumn(Ti j, Tw value) {
  if (j >= ColsCount || j < 0)
    throw std::invalid_argument("invalid index");
  SetColumn0(j, value);
}

template <typename Tw> void Matrix<Tw>::SetColumn0(Ti j, Tw value) {
  auto d = &Data[RowsCount * j];
  for (Ti i = 0; i < RowsCount; i++)
    d[i] = value;
}

template <typename Tw> void Matrix<Tw>::SetColumn_plus(Ti j, Tw value) {
  if (j >= ColsCount || j < 0)
    throw std::invalid_argument("invalid index");
  SetColumn_plus0(j, value);
}

template <typename Tw> void Matrix<Tw>::SetColumn_minus(Ti j, Tw value) {
  if (j >= ColsCount || j < 0)
    throw std::invalid_argument("invalid index");
  SetColumn_minus0(j, value);
}

template <typename Tw> void Matrix<Tw>::SetColumn_plus0(Ti j, Tw value) {
  auto d = &Data[RowsCount * j];
  for (Ti i = 0; i < RowsCount; i++)
    d[i] += value;
}

template <typename Tw> void Matrix<Tw>::SetColumn_minus0(Ti j, Tw value) {
  auto d = &Data[RowsCount * j];
  for (Ti i = 0; i < RowsCount; i++)
    d[i] -= value;
}

template <typename Tw>
void Matrix<Tw>::GetRow(Ti i, Matrix<Tw> &storage) const {
  if (i >= RowsCount || i < 0)
    throw std::invalid_argument("invalid index");
  if (storage.length() != ColsCount)
    throw std::invalid_argument("invalid length: storage");
  GetRow0(i, storage);
}
template <typename Tw>
void Matrix<Tw>::GetRow0(Ti i, Matrix<Tw> &storage) const {
  auto d = &Data[i];
  for (Ti j = 0; j < ColsCount; j++)
    storage.Data[j] = d[RowsCount * j];
}

template <typename Tw>
void Matrix<Tw>::GetColumn(Ti j, Matrix<Tw> &storage) const {
  if (j >= ColsCount || j < 0)
    throw std::invalid_argument("invalid index");
  if (storage.length() != RowsCount)
    throw std::invalid_argument("invalid length: storage");
  GetColumn0(j, storage);
}
template <typename Tw>
void Matrix<Tw>::GetColumn0(Ti j, Matrix<Tw> &storage) const {
  auto d = &Data[RowsCount * j];
  for (Ti i = 0; i < RowsCount; i++)
    storage.Data[i] = d[i];
}

template <typename Tw> void Matrix<Tw>::GetDiag(Matrix<Tw> &storage) const {
  if (RowsCount != ColsCount)
    throw LdtException(ErrorType::kLogic, "matrix", "matrix is not square");
  if (storage.length() < RowsCount)
    throw std::invalid_argument("invalid dimension: storage");
  GetDiag0(storage);
}
template <typename Tw> void Matrix<Tw>::GetDiag0(Matrix<Tw> &storage) const {
  Ti b = 0;
  for (Ti i = 0; i < RowsCount; i++) {
    storage.Data[i] = Data[b];
    b += RowsCount + 1;
  }
}

// #pragma endregion

// #pragma region Helpers

template <typename Tw>
void Matrix<Tw>::Diagonal(Matrix<Tw> &storage, Tw diagv, Tw offdiagv) {
  Ti m = storage.RowsCount;
  if (m != storage.ColsCount)
    throw LdtException(ErrorType::kLogic, "matrix", "storage is not square");

  storage.SetValue(offdiagv);
  for (Ti i = 0; i < m; i++)
    storage.Set0(i, i, diagv);
}

template <typename Tw>
bool Matrix<Tw>::IsDiagonal(Matrix<Tw> &mat, Tw diagv, Tw offdiagv,
                            Tw epsilon) {
  Ti m = mat.RowsCount;
  if (m != mat.ColsCount)
    throw LdtException(ErrorType::kLogic, "matrix", "matrix is not square");

  for (Ti i = 0; i < m; i++) {
    if constexpr (std::is_unsigned<Tw>() == false) {
      if (std::abs(diagv - mat.Get0(i, i)) > epsilon)
        return false;
    } else if constexpr (true) {
      if (diagv != mat.Get0(i, i))
        return false;
    }
  }
  for (Ti i = 0; i < m; i++)
    for (Ti j = 0; j < m; j++) {
      if constexpr (std::is_unsigned<Tw>() == false) {
        if (i != j && std::abs(offdiagv - mat.Get0(i, j)) > epsilon)
          return false;
      } else if constexpr (true) {
        if (i != j && offdiagv != mat.Get0(i, j))
          return false;
      }
    }
  return true;
}

template <typename Tw>
void Matrix<Tw>::MakeTriangular(Matrix<Tw> &storage, Matrix<Tw> &elements,
                                int up, bool diag, bool byrow) {
  Ti m = storage.RowsCount;
  if (m != storage.ColsCount)
    throw LdtException(ErrorType::kLogic, "matrix", "storage is not square");

  if (diag && elements.length() != m * (m + 1) / 2)
    throw LdtException(ErrorType::kLogic, "matrix",
                       "wrong number of elements!");
  if (diag == false && elements.length() != m * (m - 1) / 2)
    throw LdtException(ErrorType::kLogic, "matrix",
                       "wrong number of elements!");

  MakeTriangular0(storage, elements, up, diag, byrow);
}

template <typename Tw>
void Matrix<Tw>::MakeTriangular0(Matrix<Tw> &storage, Matrix<Tw> &elements,
                                 int up, bool diag, bool byrow) {
  Ti m = static_cast<Ti>(storage.RowsCount);
  if (diag) {
    Ti i = 0;
    if (up == 0 || up == 1) // || up == true)??
    {
      if (byrow)
        for (Ti k = 0; k < static_cast<Ti>(elements.length()); k++) {
          auto d = elements.Data[k];
          auto res = div(i, m);
          storage.Set0(static_cast<Ti>(res.quot), static_cast<Ti>(res.rem), d);
          if (res.rem == m - 1)
            i += res.quot + 1;
          if (up == 0)
            storage.Set0(static_cast<Ti>(res.rem), static_cast<Ti>(res.quot),
                         d);
          i++;
        }
      else
        throw LdtException(ErrorType::kLogic, "matrix", "not implemented");
    } else // low
    {
      if (byrow == false)
        for (Ti k = 0; k < static_cast<Ti>(elements.length()); k++) {
          auto d = elements.Data[k];
          auto res = div(i, m);
          storage.Set0(static_cast<Ti>(res.rem), static_cast<Ti>(res.quot), d);
          if (res.rem == m - 1)
            i += res.quot + 1;
          i++;
        }
      else
        throw LdtException(ErrorType::kLogic, "matrix", "not implemented");
    }
  } else // no diagonal
  {
    Ti i = 0;
    if (up == 0 || up == 1) // || up == true)
    {
      if (byrow)
        for (Ti k = 0; k < static_cast<Ti>(elements.length()); k++) {
          auto d = elements.Data[k];
          i++;
          auto res = div(i, m);
          storage.Set0(static_cast<Ti>(res.quot), static_cast<Ti>(res.rem), d);
          if (res.rem == m - 1)
            i += res.quot + 2;
          if (up == 0)
            storage.Set0(static_cast<Ti>(res.rem), static_cast<Ti>(res.quot),
                         d);
        }
      else
        throw LdtException(ErrorType::kLogic, "matrix", "not implemented");
    } else {
      if (byrow == false)
        for (Ti k = 0; k < static_cast<Ti>(elements.length()); k++) {
          auto d = elements.Data[k];
          i++;
          auto res = div(i, m);
          storage.Set0(static_cast<Ti>(res.rem), static_cast<Ti>(res.quot), d);
          if (res.rem == m - 1)
            i += res.quot + 2;
        }
      else
        throw LdtException(ErrorType::kLogic, "matrix", "not implemented");
    }
  }
}

template <typename Tw>
void Matrix<Tw>::FillRandom_normal(Matrix<Tw> &storage, unsigned int seed,
                                   Tw mean, Tw variance) {
  std::default_random_engine eng;

  if (seed != 0)
    eng = std::default_random_engine(seed);
  else {
    std::random_device rdev{};
    eng = std::default_random_engine(rdev());
  }

  if constexpr (std::is_floating_point<const Tw>()) {
    std::normal_distribution<Tw> dist(mean, variance);
    for (Ti i = 0; i < storage.length(); i++)
      storage.Data[i] = dist(eng);
  } else { // generate double and cast to Tw ?!

    // WARNING: this is not a floating point type. mean, variance and values
    // are cast to 'double'

    std::normal_distribution<double> dist(static_cast<double>(mean),
                                          static_cast<double>(variance));
    for (Ti i = 0; i < storage.length(); i++)
      storage.Data[i] = static_cast<Tw>(dist(eng));
  }
}

template <typename Tw>
void Matrix<Tw>::FillRandom_uniform(Matrix<Tw> &storage, unsigned int seed,
                                    Tw min, Tw max) {
  std::default_random_engine eng;

  if (seed != 0)
    eng = std::default_random_engine(seed);
  else {
    std::random_device rdev{};
    eng = std::default_random_engine(rdev());
  }

  if constexpr (std::is_floating_point<const Tw>()) {
    std::uniform_real_distribution<Tw> dist(min, max);
    for (Ti i = 0; i < storage.length(); i++)
      storage.Data[i] = dist(eng);
  } else {
    std::uniform_int_distribution<Tw> dist(min, max);
    for (Ti i = 0; i < storage.length(); i++)
      storage.Data[i] = dist(eng);
  }
}

// #pragma endregion

// #pragma region String

template <typename Tw>
std::string Matrix<Tw>::ToString(char colsep, char rowsep,
                                 std::streamsize precesion) const {
  if (!Data)
    return "";

  std::ostringstream str;
  str << "Tw Matrix<Tw> (" << RowsCount << " x " << ColsCount << ")";
  if (RowsCount == 0 || ColsCount == 0) {
    return str.str();
  }
  str << rowsep;
  str << std::fixed;
  str << std::setprecision(precesion);
  for (Ti i = 0; i < RowsCount; i++) {
    for (Ti j = 0; j < ColsCount; j++) {
      Tw value = Get0(i, j);
      str << value;
      if (j < ColsCount - 1)
        str << colsep;
    }
    if (i < RowsCount - 1)
      str << rowsep;
  }
  return str.str();
}

template <typename Tw>
std::string Matrix<Tw>::ToString0(char colsep, char rowsep,
                                  std::streamsize precesion) const {
  if (!Data || RowsCount == 0 || ColsCount == 0) {
    return "";
  }
  std::ostringstream str;
  str << std::fixed;
  str << std::setprecision(precesion);
  for (Ti i = 0; i < RowsCount; i++) {
    for (Ti j = 0; j < ColsCount; j++) {
      Tw value = (*this).Get0(i, j);
      str << value;
      if (j < ColsCount - 1)
        str << colsep;
    }
    if (i < RowsCount - 1)
      str << rowsep;
  }
  return str.str();
}

template <typename Tw>
std::string Matrix<Tw>::ToString_R_Matrix(std::streamsize precesion,
                                          Ti line_count, std::string start,
                                          bool breakDim) const {
  auto len = length();
  if (!Data || len == 0) {
    return start + std::string("matrix(nrow = 0, ncol = 0)");
  }

  std::ostringstream str;
  str << std::fixed;
  str << std::setprecision(precesion);

  str << start + std::string("matrix(c(");
  for (Ti i = 0; i < len; i++) {
    str << Data[i];
    if (i < len - 1)
      str << ',';
    if (i != 0 &&
        i % line_count == 0) // seems that RStudio does not support long lines
      str << '\n';
  }
  str << "),";
  if (breakDim)
    str << "\n";
  else
    str << " ";
  str << "nrow=";
  str << RowsCount;
  str << ", ncol=";
  str << ColsCount;
  str << ")";
  return str.str();
}

// #pragma endregion

// #pragma region Linear Algebra

// #pragma region Element - wise

template <typename Tw> void Matrix<Tw>::Add0(Tw b, Matrix<Tw> &storage) const {
  for (Ti i = 0; i < length(); i++)
    storage.Data[i] = Data[i] + b;
}

template <typename Tw>
void Matrix<Tw>::Multiply0(Tw b, Matrix<Tw> &storage, Tw beta) const {
  if (beta == 0)
    for (Ti i = 0; i < length(); i++)
      storage.Data[i] = Data[i] * b;
  else
    for (Ti i = 0; i < length(); i++)
      storage.Data[i] = Data[i] * b + beta * storage.Data[i];
}

template <typename Tw>
void Matrix<Tw>::Subtract0(Tw b, Matrix<Tw> &storage) const {
  for (Ti i = 0; i < length(); i++)
    storage.Data[i] = Data[i] - b;
}

template <typename Tw>
void Matrix<Tw>::Divide0(Tw b, Matrix<Tw> &storage) const {
  for (Ti i = 0; i < length(); i++)
    storage.Data[i] = Data[i] / b;
}

template <typename Tw>
void Matrix<Tw>::Add0(const Matrix<Tw> &b, Matrix<Tw> &storage) const {
  for (Ti i = 0; i < length(); i++)
    storage.Data[i] = Data[i] + b.Data[i];
}

template <typename Tw>
void Matrix<Tw>::Multiply0(const Matrix<Tw> &b, Matrix<Tw> &storage,
                           Tw beta) const {
  if (beta == 0)
    for (Ti i = 0; i < length(); i++)
      storage.Data[i] = Data[i] * b.Data[i];
  else
    for (Ti i = 0; i < length(); i++)
      storage.Data[i] = Data[i] * b.Data[i] + beta * storage.Data[i];
}

template <typename Tw>
void Matrix<Tw>::Subtract0(const Matrix<Tw> &b, Matrix<Tw> &storage) const {
  for (Ti i = 0; i < length(); i++)
    storage.Data[i] = Data[i] - b.Data[i];
}

template <typename Tw>
void Matrix<Tw>::Divide0(const Matrix<Tw> &b, Matrix<Tw> &storage) const {
  for (Ti i = 0; i < length(); i++)
    storage.Data[i] = Data[i] / b.Data[i];
}

// bounds check

template <typename Tw> void Matrix<Tw>::Add(Tw b, Matrix<Tw> &storage) const {
  if (storage.RowsCount != this->RowsCount ||
      storage.ColsCount != this->ColsCount)
    throw std::invalid_argument("inconsistent size: storage");
  Add0(b, storage);
}

template <typename Tw>
void Matrix<Tw>::Multiply(Tw b, Matrix<Tw> &storage, Tw beta) const {
  if (storage.RowsCount != this->RowsCount ||
      storage.ColsCount != this->ColsCount)
    throw std::invalid_argument("inconsistent size: storage");
  Multiply0(b, storage, beta);
}

template <typename Tw>
void Matrix<Tw>::Subtract(Tw b, Matrix<Tw> &storage) const {
  if (storage.RowsCount != this->RowsCount ||
      storage.ColsCount != this->ColsCount)
    throw std::invalid_argument("inconsistent size: storage");
  Subtract0(b, storage);
}

template <typename Tw>
void Matrix<Tw>::Divide(Tw b, Matrix<Tw> &storage) const {
  if (storage.RowsCount != this->RowsCount ||
      storage.ColsCount != this->ColsCount)
    throw std::invalid_argument("inconsistent size: storage");
  Divide0(b, storage);
}

template <typename Tw>
void Matrix<Tw>::Add(const Matrix<Tw> &b, Matrix<Tw> &storage) const {
  if (storage.RowsCount != this->RowsCount ||
      storage.ColsCount != this->ColsCount)
    throw std::invalid_argument("inconsistent size: storage");
  if (b.RowsCount != this->RowsCount || b.ColsCount != this->ColsCount)
    throw std::invalid_argument("inconsistent size: b");
  Add0(b, storage);
}

template <typename Tw>
void Matrix<Tw>::Multiply(const Matrix<Tw> &b, Matrix<Tw> &storage,
                          Tw beta) const {
  if (storage.RowsCount != this->RowsCount ||
      storage.ColsCount != this->ColsCount)
    throw std::invalid_argument("inconsistent size: storage");
  if (b.RowsCount != this->RowsCount || b.ColsCount != this->ColsCount)
    throw std::invalid_argument("inconsistent size: b");
  Multiply0(b, storage, beta);
}

template <typename Tw>
void Matrix<Tw>::Subtract(const Matrix<Tw> &b, Matrix<Tw> &storage) const {
  if (storage.RowsCount != this->RowsCount ||
      storage.ColsCount != this->ColsCount)
    throw std::invalid_argument("inconsistent size: storage");
  if (b.RowsCount != this->RowsCount || b.ColsCount != this->ColsCount)
    throw std::invalid_argument("inconsistent size: b");
  Subtract0(b, storage);
}

template <typename Tw>
void Matrix<Tw>::Divide(const Matrix<Tw> &b, Matrix<Tw> &storage) const {
  if (storage.RowsCount != this->RowsCount ||
      storage.ColsCount != this->ColsCount)
    throw std::invalid_argument("inconsistent size: storage");
  if (b.RowsCount != this->RowsCount || b.ColsCount != this->ColsCount)
    throw std::invalid_argument("inconsistent size: b");
  Divide0(b, storage);
}

// in-place

template <typename Tw> void Matrix<Tw>::Add_in(Tw b) {
  for (Ti i = 0; i < length(); i++)
    Data[i] = Data[i] + b;
}

template <typename Tw> void Matrix<Tw>::Multiply_in(Tw b) {
  for (Ti i = 0; i < length(); i++)
    Data[i] = Data[i] * b;
}

template <typename Tw> void Matrix<Tw>::Power_in(Tw b) {
  if constexpr (std::is_same<Tw, const float>() ||
                std::is_same<Tw, const double>() ||
                std::is_same<Tw, const long double>()) {
    for (Ti i = 0; i < length(); i++)
      Data[i] = std::pow<Tw, Tw>(Data[i], b);
  } else if constexpr (true) {
    for (Ti i = 0; i < length(); i++)
      Data[i] = (Tw)std::pow(Data[i], b);
  }
}

template <typename Tw> void Matrix<Tw>::Subtract_in(Tw b) {
  for (Ti i = 0; i < length(); i++)
    Data[i] = Data[i] - b;
}

template <typename Tw> void Matrix<Tw>::Divide_in(Tw b) {
  for (Ti i = 0; i < length(); i++)
    Data[i] = Data[i] / b;
}

template <typename Tw> void Matrix<Tw>::Add_in0(const Matrix<Tw> &b) {
  for (Ti i = 0; i < length(); i++)
    Data[i] = Data[i] + b.Data[i];
}

template <typename Tw> void Matrix<Tw>::Multiply_in0(const Matrix<Tw> &b) {
  for (Ti i = 0; i < length(); i++)
    Data[i] = Data[i] * b.Data[i];
}

template <typename Tw> void Matrix<Tw>::Subtract_in0(const Matrix<Tw> &b) {
  for (Ti i = 0; i < length(); i++)
    Data[i] = Data[i] - b.Data[i];
}

template <typename Tw> void Matrix<Tw>::Divide_in0(const Matrix<Tw> &b) {
  for (Ti i = 0; i < length(); i++)
    Data[i] = Data[i] / b.Data[i];
}

// bounds check

template <typename Tw> void Matrix<Tw>::Add_in(const Matrix<Tw> &b) {
  if (b.RowsCount != this->RowsCount || b.ColsCount != this->ColsCount)
    throw std::invalid_argument("inconsistent size: b");
  Add_in0(b);
}

template <typename Tw> void Matrix<Tw>::Multiply_in(const Matrix<Tw> &b) {
  if (b.RowsCount != this->RowsCount || b.ColsCount != this->ColsCount)
    throw std::invalid_argument("inconsistent size: b");
  Multiply_in0(b);
}

template <typename Tw> void Matrix<Tw>::Subtract_in(const Matrix<Tw> &b) {
  if (b.RowsCount != this->RowsCount || b.ColsCount != this->ColsCount)
    throw std::invalid_argument("inconsistent size: b");
  Subtract_in0(b);
}

template <typename Tw> void Matrix<Tw>::Divide_in(const Matrix<Tw> &b) {
  if (b.RowsCount != this->RowsCount || b.ColsCount != this->ColsCount)
    throw std::invalid_argument("inconsistent size: b");
  Divide_in0(b);
}

// #pragma endregion

// #pragma region dot

template <typename Tw>
Tw Matrix<Tw>::VectorDotVector(const Matrix<Tw> &b) const {
  if (IsVector() == false)
    throw std::invalid_argument("a vector is expected");
  if (b.length() != this->length())
    throw std::invalid_argument("inconsistent size: b");

  return VectorDotVector0(b);
}

template <typename Tw>
void Matrix<Tw>::DotVector(const Matrix<Tw> &b, Matrix<Tw> &storage, Tw alpha,
                           Tw beta) const {
  if (b.IsVector() == false)
    throw LdtException(ErrorType::kLogic, "matrix", "a vector is expected: b");
  if (storage.IsVector() == false)
    throw LdtException(ErrorType::kLogic, "matrix",
                       "a vector is expected: storage");
  if (this->ColsCount != b.RowsCount)
    throw std::invalid_argument("inconsistent size: b");
  if (this->RowsCount != storage.RowsCount)
    throw std::invalid_argument("inconsistent size: storage");

  DotVector0(b, storage, alpha, beta);
}

template <typename Tw>
void Matrix<Tw>::tDotVector(const Matrix<Tw> &b, Matrix<Tw> &storage, Tw alpha,
                            Tw beta) const {
  if (b.IsVector() == false)
    throw LdtException(ErrorType::kLogic, "matrix", "a vector is expected: b");
  if (storage.IsVector() == false)
    throw LdtException(ErrorType::kLogic, "matrix",
                       "a vector is expected: storage");
  if (this->RowsCount != b.RowsCount)
    throw std::invalid_argument("inconsistent size: b");
  if (this->ColsCount != storage.RowsCount)
    throw std::invalid_argument("inconsistent size: storage");

  tDotVector0(b, storage, alpha, beta);
}

template <typename Tw>
void Matrix<Tw>::Dot(const Matrix<Tw> &b, Matrix<Tw> &storage, Tw alpha,
                     Tw beta) const {
  if (this->ColsCount != b.RowsCount)
    throw std::invalid_argument("inconsistent size: b");
  if (this->RowsCount != storage.RowsCount || b.ColsCount != storage.ColsCount)
    throw std::invalid_argument("inconsistent size: storage");

  Dot0(b, storage, alpha, beta);
}

template <typename Tw>
void Matrix<Tw>::DotTr(const Matrix<Tw> &b, Matrix<Tw> &storage, Tw alpha,
                       Tw beta) const {
  if (this->ColsCount != b.ColsCount)
    throw std::invalid_argument("inconsistent size: b");
  if (this->RowsCount != storage.RowsCount || b.RowsCount != storage.ColsCount)
    throw std::invalid_argument("inconsistent size: storage");

  DotTr0(b, storage, alpha, beta);
}

template <typename Tw>
void Matrix<Tw>::TrDot(const Matrix<Tw> &b, Matrix<Tw> &storage, Tw alpha,
                       Tw beta) const {
  if (this->RowsCount != b.RowsCount)
    throw std::invalid_argument("inconsistent size: b");
  if (this->ColsCount != storage.RowsCount || b.ColsCount != storage.ColsCount)
    throw std::invalid_argument("inconsistent size: storage");

  TrDot0(b, storage, alpha, beta);
}

template <typename Tw>
void Matrix<Tw>::TrDotTr(const Matrix<Tw> &b, Matrix<Tw> &storage, Tw alpha,
                         Tw beta) const {
  if (this->RowsCount != b.ColsCount)
    throw std::invalid_argument("inconsistent size: b");
  if (this->ColsCount != storage.RowsCount || b.RowsCount != storage.ColsCount)
    throw std::invalid_argument("inconsistent size: storage");

  TrDotTr0(b, storage, alpha, beta);
}

template <typename Tw>
void Matrix<Tw>::DotDiag(const Matrix<Tw> &b, Matrix<Tw> &storage) const {
  if (ColsCount != b.length())
    throw std::invalid_argument("inconsistent size: b");
  if (RowsCount != storage.RowsCount || b.length() != storage.ColsCount)
    throw std::invalid_argument("inconsistent size: storage");
  DotDiag0(b, storage);
}

template <typename Tw>
void Matrix<Tw>::DiagDot(const Matrix<Tw> &b, Matrix<Tw> &storage) const {
  if (b.RowsCount != this->length())
    throw std::invalid_argument("inconsistent size: b");
  if (b.ColsCount != storage.ColsCount || this->length() != storage.RowsCount)
    throw std::invalid_argument("inconsistent size: storage");
  DiagDot0(b, storage);
}

template <typename Tw>
void Matrix<Tw>::Dot_AAt(Matrix<Tw> &storage, bool setLower, Tw alpha,
                         Tw beta) const {
  if (storage.RowsCount != RowsCount || storage.ColsCount != RowsCount)
    throw std::invalid_argument("inconsistent size: storage");
  Dot_AAt0(storage, setLower, alpha, beta);
}

template <typename Tw>
void Matrix<Tw>::Dot_AtA(Matrix<Tw> &storage, bool setLower, Tw alpha,
                         Tw beta) const {
  if (storage.RowsCount != ColsCount || storage.ColsCount != ColsCount)
    throw std::invalid_argument("inconsistent size: storage");
  Dot_AtA0(storage, setLower, alpha, beta);
}

template <typename Tw>
void Matrix<Tw>::Dot_AtA_nan(Matrix<Tw> &storage, Matrix<Tw> &counts_storage,
                             bool setLower) const {
  if (storage.RowsCount != ColsCount || storage.ColsCount != ColsCount)
    throw std::invalid_argument("inconsistent size: storage");
  if (counts_storage.RowsCount != ColsCount ||
      counts_storage.ColsCount != ColsCount)
    throw std::invalid_argument("inconsistent size: counts");
  Dot_AtA_nan0(storage, counts_storage, setLower);
}

template <typename Tw>
void Matrix<Tw>::SymDot(const Matrix<Tw> &b, Matrix<Tw> &storage,
                        bool uppertrig, Tw alpha, Tw beta) const {
  if (IsSquare() == false)
    throw std::invalid_argument(
        "inconsistent size: this matrix must be a square Matrix<Tw>");
  if (this->ColsCount != b.RowsCount)
    throw std::invalid_argument("inconsistent size: b");
  if (this->RowsCount != storage.RowsCount || b.ColsCount != storage.ColsCount)
    throw std::invalid_argument("inconsistent size: storage");

  SymDot0(b, storage, uppertrig, alpha, beta);
}

template <typename Tw>
void Matrix<Tw>::DotSym(const Matrix<Tw> &b, Matrix<Tw> &storage,
                        bool uppertrig, Tw alpha, Tw beta) const {
  if (b.IsSquare() == false)
    throw std::invalid_argument(
        "inconsistent size: this matrix must be a square Matrix<Tw>");
  if (this->ColsCount != b.RowsCount)
    throw std::invalid_argument("inconsistent size: b");
  if (this->RowsCount != storage.RowsCount || b.ColsCount != storage.ColsCount)
    throw std::invalid_argument("inconsistent size: storage");

  DotSym0(b, storage, uppertrig, alpha, beta);
}

// #pragma endregion

// #pragma region Others

template <typename Tw> void Matrix<Tw>::Transpose(Matrix<Tw> &storage) const {
  if (storage.RowsCount != ColsCount || storage.ColsCount != RowsCount)
    throw std::invalid_argument("invalid dimension: storage");
  Transpose0(storage);
}

template <typename Tw>
void Matrix<Tw>::Kron(const Matrix<Tw> &B, Matrix<Tw> &storage) const {
  if (storage.ColsCount != ColsCount * B.ColsCount ||
      storage.RowsCount != RowsCount * B.RowsCount)
    throw std::invalid_argument("invalid dimension: storage");
  Kron0(B, storage);
}

template <typename Tw>
void Matrix<Tw>::KronIden(Ti m, Matrix<Tw> &storage) const {
  if (storage.ColsCount != ColsCount * m || storage.RowsCount != RowsCount * m)
    throw std::invalid_argument("invalid dimension: storage");
  KronIden0(m, storage);
}

template <typename Tw>
void Matrix<Tw>::IdenKron(Ti m, Matrix<Tw> &storage) const {
  if (storage.ColsCount != ColsCount * m || storage.RowsCount != RowsCount * m)
    throw std::invalid_argument("invalid dimension: storage");
  IdenKron0(m, storage);
}

template <typename Tw>
void Matrix<Tw>::TrKronIden(Ti m, Matrix<Tw> &storage) const {
  if (storage.ColsCount != RowsCount * m || storage.RowsCount != ColsCount * m)
    throw std::invalid_argument("invalid dimension: storage");
  TrKronIden0(m, storage);
}

// #pragma endregion

// #pragma region decompositions

template <typename Tw> Tw Matrix<Tw>::Det_pd0() {
  if (ColsCount == 1)
    return Get0(0, 0);

  auto res = Chol0(false);

  if (res != 0) {
    if constexpr (std::numeric_limits<Tw>::has_quiet_NaN) {
      return NAN; // throw std::invalid_argument("Cholesky decomposition
                  // failed with code: " + res);
    } else if constexpr (true) {
      throw LdtException(ErrorType::kLogic, "matrix", "not implemented");
    }
  }

  Tw det = 1;
  for (Ti i = 0; i < ColsCount; i++)
    det *= Get0(i, i);
  return det * det;
}

template <typename Tw> Ti Matrix<Tw>::Inv(Matrix<Tw> &storage) const {
  if (IsSquare() == false)
    throw std::invalid_argument("matrix is not square");
  if (this->RowsCount != storage.RowsCount ||
      this->ColsCount != storage.ColsCount)
    throw std::invalid_argument("inconsistent size: storage");

  this->CopyTo(storage);
  auto info = storage.Inv0();
  return info;
}

template <typename Tw> Ti Matrix<Tw>::Inv0() {
  const Ti M = static_cast<Ti>(this->RowsCount);

  auto ipiv = std::make_unique<Ti[]>(M + 1);
  auto work = std::make_unique<Tw[]>(M * M);

  auto info = Inv00(ipiv.get(), work.get());

  return info;
}

template <typename Tw>
Ti Matrix<Tw>::Chol(Matrix<Tw> &storage, bool upper) const {
  if (IsSquare() == false)
    throw LdtException(ErrorType::kLogic, "matrix",
                       "invalid operation: Matrix<Tw> is not square");
  if (IsSymmetric() == false)
    throw LdtException(ErrorType::kLogic, "matrix",
                       "invalid operation: Matrix<Tw> is not symmetric");
  if (storage.RowsCount != RowsCount)
    throw std::invalid_argument("invalid dimension: storage");

  this->CopyTo(storage);
  return storage.Chol0(upper);
}

template <typename Tw> Ti Matrix<Tw>::QR(Matrix<Tw> &Q, Matrix<Tw> &R) {
  Ti M = RowsCount;
  Ti N = ColsCount;
  if (static_cast<Ti>(Q.RowsCount) != M || Q.ColsCount != Q.RowsCount)
    throw std::invalid_argument("invalid dimension: Q");
  if (static_cast<Ti>(R.RowsCount) != N || R.ColsCount != R.RowsCount)
    throw std::invalid_argument("invalid dimension: R");

  Ti minMN = std::min(M, N);
  auto tau0 = std::make_unique<Tw[]>(minMN);
  Tw *tau = tau0.get();

  Ti info = QR0(tau);

  delete[] tau;

  if (info != 0)
    return info;

  throw LdtException(ErrorType::kLogic, "matrix", "not implemented");
}

template <typename Tw>
Ti Matrix<Tw>::SolveTrian(Matrix<Tw> &b, bool upper, bool transpose,
                          bool unitdiag) {
  if (IsSquare() == false)
    throw std::invalid_argument("matrix must be square");
  if ((transpose == false && b.RowsCount != RowsCount) ||
      (transpose && b.RowsCount != ColsCount))
    throw std::invalid_argument("invalid dimension: b");

  return SolveTrian0(b, upper, transpose, unitdiag);
}

template <typename Tw> Ti Matrix<Tw>::SolvePos(Matrix<Tw> &b, bool upper) {
  if (IsSquare() == false)
    throw std::invalid_argument("matrix must be square");
  if (b.RowsCount != RowsCount)
    throw std::invalid_argument("invalid dimension: b");
  return SolvePos0(b, upper);
}

// #pragma endregion

// #pragma endregion

// #pragma region Statistics

template <typename Tw> Tw Matrix<Tw>::Maximum() const {
  Tw m;
  if (std::numeric_limits<Tw>::has_infinity) {
    if constexpr (std::is_unsigned<Tw>() == false) {
      m = -std::numeric_limits<Tw>::infinity();
    } else if constexpr (true) {
      m = 0;
    }
  } else
    m = std::numeric_limits<Tw>::min();

  for (Ti i = 0; i < length(); i++) {
    if (Data[i] > m)
      m = Data[i];
  }
  return m;
}

template <typename Tw> Tw Matrix<Tw>::Minimum() const {
  Tw m;
  if (std::numeric_limits<Tw>::has_infinity)
    m = std::numeric_limits<Tw>::infinity();
  else
    m = std::numeric_limits<Tw>::max();

  for (Ti i = 0; i < length(); i++) {
    if (Data[i] < m)
      m = Data[i];
  }
  return m;
}

template <typename Tw> Tw Matrix<Tw>::max(Ti &rowIndex, Ti &colIndex) const {
  Tw m;
  if (std::numeric_limits<Tw>::has_infinity) {
    if constexpr (std::is_unsigned<Tw>() == false) {
      m = -std::numeric_limits<Tw>::infinity();
    } else if constexpr (true) {
      m = 0;
    }
  } else
    m = std::numeric_limits<Tw>::min();
  Ti index = 0;
  for (Ti i = 0; i < length(); i++) {
    if (Data[i] > m) {
      m = Data[i];
      index = i;
    }
  }
  TranslateIndex(index, rowIndex, colIndex);
  return m;
}

template <typename Tw> Tw Matrix<Tw>::min(Ti &rowIndex, Ti &colIndex) const {
  Tw m;
  if (std::numeric_limits<Tw>::has_infinity)
    m = std::numeric_limits<Tw>::infinity();
  else
    m = std::numeric_limits<Tw>::max();

  Ti index = 0;
  for (Ti i = 0; i < length(); i++) {
    if (Data[i] < m) {
      m = Data[i];
      index = i;
    }
  }
  TranslateIndex(index, rowIndex, colIndex);
  return m;
}

template <typename Tw> Tw Matrix<Tw>::MaximumInRow(Ti i, Ti &colIndex) const {
  Tw m, t;

  if (std::numeric_limits<Tw>::has_infinity) {
    if constexpr (std::is_unsigned<Tw>() == false) {
      m = -std::numeric_limits<Tw>::infinity();
    } else if constexpr (true) {
      m = 0;
    }
  } else
    m = std::numeric_limits<Tw>::min();

  auto d = &Data[i];
  for (Ti j = 0; j < ColsCount; j++) {
    t = d[RowsCount * j];
    if (t > m) {
      m = t;
      colIndex = j;
    }
  }
  return m;
}

template <typename Tw> Tw Matrix<Tw>::MinimumInRow(Ti i, Ti &colIndex) const {
  Tw m, t;

  if (std::numeric_limits<Tw>::has_infinity)
    m = std::numeric_limits<Tw>::infinity();
  else
    m = std::numeric_limits<Tw>::max();

  auto d = &Data[i];
  for (Ti j = 0; j < ColsCount; j++) {
    t = d[RowsCount * j];
    if (t < m) {
      m = t;
      colIndex = j;
    }
  }
  return m;
}

template <typename Tw>
Tw Matrix<Tw>::MaximumInColumn(Ti j, Ti &rowIndex) const {
  Tw m, t;

  if (std::numeric_limits<Tw>::has_infinity) {
    if constexpr (std::is_unsigned<Tw>() == false) {
      m = -std::numeric_limits<Tw>::infinity();
    } else if constexpr (true) {
      m = 0;
    }
  } else
    m = std::numeric_limits<Tw>::min();

  auto d = &Data[RowsCount * j];
  for (Ti i = 0; i < RowsCount; i++) {
    t = d[i];
    if (t > m) {
      m = t;
      rowIndex = i;
    }
  }
  return m;
}

template <typename Tw>
Tw Matrix<Tw>::MinimumInColumn(Ti j, Ti &rowIndex) const {
  Tw m, t;

  if (std::numeric_limits<Tw>::has_infinity)
    m = std::numeric_limits<Tw>::infinity();
  else
    m = std::numeric_limits<Tw>::max();

  auto d = &Data[RowsCount * j];
  for (Ti i = 0; i < RowsCount; i++) {
    t = d[i];
    if (t < m) {
      m = t;
      rowIndex = i;
    }
  }
  return m;
}

template <typename Tw> Tw Matrix<Tw>::Trace() const {
  if (RowsCount != ColsCount)
    throw LdtException(ErrorType::kLogic, "matrix",
                       "invalid dimension. needs a square Matrix<Tw>");
  Tw s = 0;
  for (Ti i = 0; i < RowsCount; i++)
    s += Get0(i, i);
  return s;
}

template <typename Tw> Tw Matrix<Tw>::Sum() const {
  Tw s = 0;
  for (Ti i = 0; i < length(); i++)
    s += Data[i];
  return s;
}

template <typename Tw> Tw Matrix<Tw>::Mean(bool check_nan) const {
  if constexpr (std::numeric_limits<Tw>::has_quiet_NaN) {
    auto len = length();
    Tw me;
    if (len == 0) {
      me = NAN;
    } else if (len == 1) {
      me = Data[0];
    } else {
      if (check_nan) {
        Tw d;
        Tw diff;
        me = 0;
        Tw s;
        for (Ti i = 0; i < len; i++) {
          d = Data[i];
          if (std::isnan(d))
            continue;
          diff = d - me;
          s = diff / (i + 1);
          me += s;
        }
      } else {
        Tw d;
        Tw diff;
        me = 0;
        Tw s;
        for (Ti i = 0; i < len; i++) {
          d = Data[i];
          diff = d - me;
          s = diff / (i + 1);
          me += s;
        }
      }
    }
    return me;
  } else // ignore nan since there is none
  {
    throw LdtException(ErrorType::kLogic, "matrix",
                       "not implemented"); // you should deal with e.g.
                                           // integer division. HOW?!!

    /*auto len = length();
    Tw me;
    if (len == 0) {
            throw LdtException(ErrorType::kLogic, "matrix", "invalid length");
    }
    else if (len == 1) {
            me = Data[0];
    }
    else {
            Tw d;
            Tw diff;
            me = 0;
            Tw s;
            for (Ti i = 0; i < len; i++) {
                    d = Data[i];
                    diff = d - me;
                    s = diff / (i + 1);
                    me += s;
            }
    }
    return me;*/
  }
}

template <typename Tw>
Tw Matrix<Tw>::Variance(Tw &me, bool sample, bool check_nan) const {
  if constexpr (std::numeric_limits<Tw>::has_quiet_NaN) {
    auto len = length();
    Tw var;
    if (len == 0) {
      me = NAN;
      var = NAN;
    } else if (len == 1) {
      var = NAN;
      me = Data[0];
    } else {
      if (check_nan) {
        Tw d;
        Tw diff;
        me = 0;
        Tw s;
        Tw m2 = 0;
        Ti j = 0;
        for (Ti i = 0; i < len; i++) {
          d = Data[i];
          if (std::isnan(d))
            continue;
          diff = d - me;
          s = diff / (Tw)(j + 1);
          me += s;
          m2 += diff * s * j;
          j++;
        }
        var = sample ? m2 / (Tw)(j - 1) : m2 / j;
      } else {
        Tw d;
        Tw diff;
        me = 0;
        Tw s;
        Tw m2 = 0;
        for (Ti i = 0; i < len; i++) {
          d = Data[i];
          diff = d - me;
          s = diff / (Tw)(i + 1);
          me += s;
          m2 += diff * s * i;
        }
        var = sample ? m2 / (Tw)(len - 1) : m2 / len;
      }
    }
    return var;
  } else // ignore nan since there is none
  {
    throw LdtException(ErrorType::kLogic, "matrix",
                       "not implemented"); // you should deal with e.g.
                                           // integer division. HOW?!!

    /*auto len = length();
    Tw var;
    if (len == 0) {
            throw LdtException(ErrorType::kLogic, "matrix", "invalid length");
    }
    else if (len == 1) {
            throw LdtException(ErrorType::kLogic, "matrix", "invalid length");
    }
    else {
            Tw d;
            Tw diff;
            me = 0;
            Tw s;
            Tw m2 = 0;
            for (Ti i = 0; i < len; i++) {
                    d = Data[i];
                    diff = d - me;
                    s = diff / (i + 1);
                    me += s;
                    m2 += diff * s * i;
            }
            var = sample ? m2 / (len - 1) : m2 / len;
    }
    return var;*/
  }
}

template <typename Tw>
Tw Matrix<Tw>::VarianceColumn(Ti j, Tw &me, Ti &count, bool sample,
                              bool check_nan) const {
  count = RowsCount;
  if constexpr (std::numeric_limits<Tw>::has_quiet_NaN) {
    Tw var;
    if (RowsCount == 0) {
      me = NAN;
      var = NAN;
    } else if (RowsCount == 1) {
      var = NAN;
      me = Get0(0, j);
    } else {
      Tw *col = &Data[RowsCount * j];
      if (check_nan) {
        Tw d;
        Tw diff;
        me = 0;
        Tw s;
        Tw m2 = 0;
        count = 0;
        for (Ti i = 0; i < RowsCount; i++) {
          d = col[i];
          if (std::isnan(d))
            continue;
          diff = d - me;
          s = diff / (Tw)(count + 1);
          me += s;
          m2 += diff * s * count;
          count++;
        }
        var = sample ? m2 / (Tw)(count - 1) : m2 / count;
      } else {
        Tw d;
        Tw diff;
        me = 0;
        Tw s;
        Tw m2 = 0;
        for (Ti i = 0; i < RowsCount; i++) {
          d = col[i];
          diff = d - me;
          s = diff / (Tw)(i + 1);
          me += s;
          m2 += diff * s * i;
        }
        var = sample ? m2 / (Tw)(RowsCount - 1) : m2 / RowsCount;
      }
    }
    return var;
  } else // ignore nan since there is none
  {
    throw LdtException(ErrorType::kLogic, "matrix",
                       "not implemented"); // you should deal with e.g.
                                           // integer division. HOW?!!
  }
}

template <typename Tw>
Tw Matrix<Tw>::CovarianceColumn(Ti j1, Ti j2, Tw &mean1, Tw &mean2, Ti &count,
                                bool sample, bool check_nan) const {
  if (j1 >= ColsCount || j2 >= ColsCount || j1 < 0 || j2 < 0)
    throw LdtException(ErrorType::kLogic, "matrix",
                       "out-of-range column index");
  count = RowsCount;
  if constexpr (std::numeric_limits<Tw>::has_quiet_NaN) {
    Tw covar;
    if (RowsCount == 0) {
      mean1 = NAN;
      mean2 = NAN;
      covar = NAN;
    } else if (RowsCount == 1) {
      mean1 = Get0(0, j1);
      mean2 = Get0(0, j2);
      covar = NAN;
    } else {
      Tw *col1 = &Data[RowsCount * j1];
      Tw *col2 = &Data[RowsCount * j2];

      if (check_nan) {
        Tw d1, d2, m2 = 0, dx;
        count = 0;
        mean1 = 0;
        mean2 = 0;
        for (Ti i = 0; i < RowsCount; i++) {
          d1 = col1[i];
          if (std::isnan(d1))
            continue;
          d2 = col2[i];
          if (std::isnan(d2))
            continue;
          count++;
          dx = d1 - mean1;
          mean1 += dx / count;
          mean2 += (d2 - mean2) / count;
          m2 += dx * (d2 - mean2);
        }
        covar = sample ? m2 / (count - 1) : m2 / count;
      } else {
        Tw d1, d2, m2 = 0, dx;
        count = 0;
        mean1 = 0;
        mean2 = 0;
        for (Ti i = 0; i < RowsCount; i++) {
          d1 = col1[i];
          d2 = col2[i];
          count++;
          dx = d1 - mean1;
          mean1 += dx / count;
          mean2 += (d2 - mean2) / count;
          m2 += dx * (d2 - mean2);
        }
        covar = sample ? m2 / (Tw)(RowsCount - 1) : m2 / RowsCount;
      }
    }
    return covar;
  } else // ignore nan since there is none
  {
    throw LdtException(ErrorType::kLogic, "matrix", "not implemented");
  }
}

template <typename Tw>
Tw Matrix<Tw>::CorrelationColumn(Ti j1, Ti j2, Tw &mean1, Tw &mean2, Tw &var1,
                                 Tw &var2, Ti &count, bool sample,
                                 bool check_nan) const {
  if (j1 >= ColsCount || j2 >= ColsCount || j1 < 0 || j2 < 0)
    throw LdtException(ErrorType::kLogic, "matrix",
                       "out-of-range column index");

  if constexpr (std::numeric_limits<Tw>::has_quiet_NaN) {
    Tw cor;
    if (RowsCount == 0) {
      mean1 = NAN;
      mean2 = NAN;
      var1 = NAN;
      var2 = NAN;
      cor = NAN;
    } else if (RowsCount == 1) {
      mean1 = Get0(0, j1);
      mean2 = Get0(0, j2);
      var1 = NAN;
      var2 = NAN;
      cor = NAN;
    } else {
      Tw *col1 = &Data[RowsCount * j1];
      Tw *col2 = &Data[RowsCount * j2];

      if (check_nan) {
        Tw d1, d2, m2 = 0, dx1, dx2, s1, s2, w1 = 0, w2 = 0;
        count = 0;
        mean1 = 0;
        mean2 = 0;
        for (Ti i = 0; i < RowsCount; i++) {
          d1 = col1[i];
          if (std::isnan(d1))
            continue;
          d2 = col2[i];
          if (std::isnan(d2))
            continue;
          count++;

          dx1 = d1 - mean1;
          s1 = dx1 / count;
          dx2 = d2 - mean2;
          s2 = dx2 / count;
          mean1 += s1;
          mean2 += s2;

          m2 += dx1 * (d2 - mean2);
          w1 += dx1 * s1 * (count - 1);
          w2 += dx2 * s2 * (count - 1);
        }
        var1 = sample ? w1 / (Tw)(RowsCount - 1) : w1 / RowsCount;
        var2 = sample ? w2 / (Tw)(RowsCount - 1) : w2 / RowsCount;
        if (m2 == (Tw)0 && w1 == (Tw)0 &&
            w2 == (Tw)0) // 0/0 case . both are constant
          cor = (Tw)1;
        else
          cor = m2 / (std::sqrt(w1) * std::sqrt(w2));
      } else {
        Tw d1, d2, m2 = 0, dx1, dx2, s1, s2, w1 = 0, w2 = 0;
        count = 0;
        mean1 = 0;
        mean2 = 0;
        for (Ti i = 0; i < RowsCount; i++) {
          d1 = col1[i];
          d2 = col2[i];
          count++;
          dx1 = d1 - mean1;
          s1 = dx1 / count;
          dx2 = d2 - mean2;
          s2 = dx2 / count;
          mean1 += s1;
          mean2 += s2;

          m2 += dx1 * (d2 - mean2);
          w1 += dx1 * s1 * i;
          w2 += dx2 * s2 * i;
        }
        var1 = sample ? w1 / (Tw)(RowsCount - 1) : w1 / RowsCount;
        var2 = sample ? w2 / (Tw)(RowsCount - 1) : w2 / RowsCount;
        if (m2 == (Tw)0 && w1 == (Tw)0 &&
            w2 == (Tw)0) // 0/0 case . both are constant
          cor = (Tw)1;
        else
          cor = m2 / (std::sqrt(w1) * std::sqrt(w2));
      }
    }
    return cor;
  } else // ignore nan since there is none
  {
    throw LdtException(ErrorType::kLogic, "matrix", "not implemented");
  }
}

template <typename Tw>
void Matrix<Tw>::RowsSum(Matrix<Tw> &storage, std::vector<Ti> &rowinds) const {
  if (rowinds.size() == 0) {
    rowinds.resize(RowsCount);
    std::iota(rowinds.begin(), rowinds.end(), 0);
  }

  if (storage.length() != (Ti)rowinds.size())
    throw std::invalid_argument("invalid dimension: storage");
  auto n = ColsCount;
  Ti q = 0;
  for (auto &i : rowinds) {
    Tw sum = 0;
    for (Ti j = 0; j < n; j++)
      sum += Get0(i, j);
    storage.Data[q] = sum;
    q++;
  }
}

template <typename Tw>
void Matrix<Tw>::ColumnsSum(Matrix<Tw> &storage,
                            std::vector<Ti> &colinds) const {
  if (colinds.size() == 0) {
    colinds.resize(ColsCount);
    std::iota(colinds.begin(), colinds.end(), 0);
  }

  if (storage.length() != (Ti)colinds.size())
    throw std::invalid_argument("invalid dimension: storage");
  auto n = RowsCount;
  Ti q = 0;
  for (auto &j : colinds) {
    Tw sum = 0;
    for (Ti i = 0; i < n; i++)
      sum += Get0(i, j);
    storage.Data[q] = sum;
    q++;
  }
}

template <typename Tw>
void Matrix<Tw>::ColumnsMean(Matrix<Tw> &storage,
                             std::vector<Ti> &colinds) const {
  if constexpr (std::is_floating_point<Tw>()) {
    if (colinds.size() == 0) {
      colinds.resize(ColsCount);
      std::iota(colinds.begin(), colinds.end(), 0);
    }

    if (storage.length() != (Ti)colinds.size())
      throw std::invalid_argument("invalid dimension: storage");

    auto n = RowsCount;
    Ti q = 0;
    for (auto &j : colinds) {
      Tw sum = 0;
      for (Ti i = 0; i < n; i++)
        sum += Get0(i, j);
      storage.Data[q] = sum / n;
      q++;
    }
  } else {
    throw LdtException(
        ErrorType::kLogic, "matrix",
        "not implemented"); // You should deal with integer division ?!
  }
}

template <typename Tw>
void Matrix<Tw>::ColumnsVariance(Matrix<Tw> &storage, std::vector<Ti> &colinds,
                                 bool sample) const {
  if constexpr (std::is_floating_point<Tw>()) {
    if (colinds.size() == 0) {
      colinds.resize(ColsCount);
      std::iota(colinds.begin(), colinds.end(), 0);
    }
    auto m = static_cast<Ti>(colinds.size());
    if (storage.RowsCount != m || storage.ColsCount != m)
      throw std::invalid_argument("invalid dimension: storage");

    auto means_d = std::make_unique<Tw[]>(m);
    Matrix<Tw> means = Matrix<Tw>(means_d.get(), m);
    ColumnsMean(means, colinds);

    auto n = RowsCount;
    auto nn = sample ? n - 1 : n;
    Ti q0 = 0;
    Ti q1 = 0;
    for (auto &j : colinds) {
      q1 = 0;
      for (auto &k : colinds) {
        if (q0 > q1) {
          q1++;
          continue;
        }

        Tw sum = 0;
        for (Ti i = 0; i < n; i++)
          sum += (Get0(i, j) - means.Data[q0]) * (Get0(i, k) - means.Data[q1]);
        auto res = sum / nn;
        storage.Set0(q0, q1, res);
        if (q0 != q1)
          storage.Set0(q1, q0, res);
        q1++;
      }
      q0++;
    }
  } else {
    throw LdtException(ErrorType::kLogic, "matrix",
                       "not implemented"); // integer division
  }
}

template <typename Tw>
void Matrix<Tw>::ColumnsStandard(const Matrix<Tw> *means,
                                 const Matrix<Tw> *stds, bool is_var) {
  if constexpr (std::is_floating_point<Tw>()) {
    if ((means && means->length() != ColsCount) ||
        (stds && stds->length() != ColsCount))
      throw std::invalid_argument("invalid length: means || vars");

    if constexpr (std::numeric_limits<Tw>::has_quiet_NaN) {
      auto n = RowsCount;
      Tw *col;
      Tw m;
      if (stds && means) {
        Tw s;
        for (Ti j = 0; j < ColsCount; j++) {
          col = &Data[j * n];
          m = means->Data[j];
          s = stds->Data[j];
          if (s == 0) {
            for (Ti i = 0; i < n; i++)
              col[i] = NAN;
          } else {
            if (is_var)
              s = std::sqrt(s);
            for (Ti i = 0; i < n; i++)
              col[i] = (col[i] - m) / s;
          }
        }
      } else if (stds) {
        Tw s;
        for (Ti j = 0; j < ColsCount; j++) {
          col = &Data[j * n];
          s = stds->Data[j];
          if (s == 0) {
            for (Ti i = 0; i < n; i++)
              col[i] = NAN;
          } else {
            if (is_var)
              s = std::sqrt(s);
            for (Ti i = 0; i < n; i++)
              col[i] = col[i] / s;
          }
        }
      } else if (means) {
        for (Ti j = 0; j < ColsCount; j++) {
          m = means->Data[j];
          col = &Data[j * n];
          for (Ti i = 0; i < n; i++)
            col[i] -= m;
        }
      } else
        throw LdtException(ErrorType::kLogic, "matrix",
                           "invalid operation: no means or stds are given");
    } else
      throw LdtException(ErrorType::kLogic, "matrix", "not implemented");
  } else {
    throw LdtException(ErrorType::kLogic, "matrix",
                       "not implemented"); // integer division?!
  }
}

template <typename Tw>
void Matrix<Tw>::ColumnsMeans(Matrix<Tw> &storage_mean, bool check_nan) const {
  if constexpr (std::is_floating_point<Tw>()) {
    if (storage_mean.length() != ColsCount)
      throw std::invalid_argument("invalid length: storage");

    auto n = RowsCount;
    Tw *col;
    Tw m;
    for (Ti j = 0; j < ColsCount; j++) {
      col = &Data[j * n];
      m = Matrix<Tw>(col, n).Mean(check_nan);

      storage_mean.Data[j] = m;
    }
  } else {
    throw LdtException(ErrorType::kLogic, "matrix",
                       "not implemented"); // inetger division
  }
}

template <typename Tw>
void Matrix<Tw>::ColumnsVariances(Matrix<Tw> &storage_var, bool sample,
                                  bool check_nan) const {
  if constexpr (std::is_floating_point<Tw>()) {
    if (storage_var.length() != ColsCount)
      throw std::invalid_argument("invalid length: storage");

    auto n = RowsCount;
    Tw *col;
    Tw mean, variance;
    for (Ti j = 0; j < ColsCount; j++) {
      col = &Data[j * n];
      variance = Matrix<Tw>(col, n).Variance(mean, sample, check_nan);
      storage_var.Data[j] = variance;
    }
  } else {
    throw LdtException(ErrorType::kLogic, "matrix",
                       "not implemented"); // integer division
  }
}

template <typename Tw>
void Matrix<Tw>::ColumnsMeansVariances(Matrix<Tw> &storage_mean,
                                       Matrix<Tw> &storage_var, bool sample,
                                       bool check_nan) const {
  if constexpr (std::is_floating_point<Tw>()) {
    if (storage_mean.length() != ColsCount || storage_var.length() != ColsCount)
      throw std::invalid_argument("invalid length: storage");

    auto n = RowsCount;
    Tw *col;
    Tw mean, variance;
    for (Ti j = 0; j < ColsCount; j++) {
      col = &Data[j * n];
      variance = Matrix<Tw>(col, n).Variance(mean, sample, check_nan);

      storage_mean.Data[j] = mean;
      storage_var.Data[j] = variance;
    }
  } else {
    throw LdtException(ErrorType::kLogic, "matrix",
                       "not implemented"); // integer division?!
  }
}

template <typename Tj> std::string join(const std::vector<Tj> *numbers) {
  std::ostringstream result;
  for (const auto number : *numbers) {
    if (result.tellp() > 0) {
      result << ", ";
    }
    result << number;
  }
  return result.str();
}

// #pragma endregion

template class ldt::MatIterator<Ti>;
template class ldt::MatIterator<Tv>;

template class ldt::Matrix<Ti>;
template class ldt::Matrix<Tv>;
