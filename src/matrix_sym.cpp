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

using namespace ldt;

template <bool has_diag, typename Tw>
MatrixSym<has_diag, Tw>::MatrixSym(Ti rows) {
  this->RowsCount = rows;
}

template <bool has_diag, typename Tw>
MatrixSym<has_diag, Tw>::MatrixSym(Tw *values, Ti rows) {
  this->RowsCount = rows;
  this->Data = values;
}

template <bool has_diag, typename Tw>
Ti MatrixSym<has_diag, Tw>::length_array() const {
  if constexpr (has_diag)
    return RowsCount * (RowsCount + 1) / 2;
  else if constexpr (has_diag == false)
    return RowsCount * (RowsCount - 1) / 2;
}

template <bool has_diag, typename Tw>
void MatrixSym<has_diag, Tw>::SetData(Tw *data, Ti newRows) {
  if (newRows != -1)
    RowsCount = newRows;
  this->Data = data;
}

template <bool has_diag, typename Tw>
void MatrixSym<has_diag, Tw>::SetData(Tw defaultvalue, Tw *data, Ti newRows) {
  if (newRows != -1)
    RowsCount = newRows;
  this->Data = data;
  for (Ti i = 0; i < length_array(); i++)
    Data[i] = defaultvalue;
}

template <bool has_diag, typename Tw>
Tw MatrixSym<has_diag, Tw>::Get(Ti i, Ti j) const {
  if (i >= RowsCount || j >= RowsCount || i < 0 || j < 0)
    throw std::out_of_range("index out-of-range exception");
  return Get0(i, j);
}

template <bool has_diag, typename Tw>
Tw MatrixSym<has_diag, Tw>::Get0(Ti i, Ti j) const {

  if constexpr (has_diag)
    if (i > j)
      return Data[j * RowsCount + i -
                  j * (j + 1) / 2]; // index in a general Matrix minus sum of
                                    // jumps due to symmetry
    else
      return Data[i * RowsCount + j - i * (i + 1) / 2];
  else if constexpr (has_diag == false) {
    if (i == j)
      throw LdtException(ErrorType::kLogic, "matrix-sym",
                         "invalid operation: diagonal is not stored");
    if (i > j)
      return Data[j * RowsCount + i - (j + 1) * (j + 2) / 2];
    else
      return Data[i * RowsCount + j - (i + 1) * (i + 2) / 2];
  }
}

template <bool has_diag, typename Tw>
void MatrixSym<has_diag, Tw>::Set(Ti i, Ti j, Tw value) {
  if (i >= RowsCount || j >= RowsCount || i < 0 || j < 0)
    throw std::out_of_range("index out-of-range exception");
  Set0(i, j, value);
}

template <bool has_diag, typename Tw>
void MatrixSym<has_diag, Tw>::Set0(Ti i, Ti j, Tw value) {
  if constexpr (has_diag)
    if (i > j)
      Data[j * RowsCount + i - j * (j + 1) / 2] = value;
    else
      Data[i * RowsCount + j - i * (i + 1) / 2] = value;
  else if constexpr (has_diag == false) {
    if (i == j)
      throw LdtException(ErrorType::kLogic, "matrix-sym",
                         "invalid operation: diagonal is not stored");
    if (i > j)
      Data[j * RowsCount + i - (j + 1) * (j + 2) / 2] = value;
    else
      Data[i * RowsCount + j - (i + 1) * (i + 2) / 2] = value;
  }
}

template <bool has_diag, typename Tw>
std::string
MatrixSym<has_diag, Tw>::ToString(char colsep, char rowsep,
                                  std::streamsize precession) const {

  std::ostringstream str;
  str << "sym Tw Matrix (" << RowsCount << " x " << RowsCount << ")";
  if (!Data || RowsCount == 0) {
    return str.str();
  }
  Tw value;
  str << rowsep;
  str << std::fixed;
  str << std::setprecision(precession);
  for (Ti i = 0; i < RowsCount; i++) {
    for (Ti j = 0; j < RowsCount; j++) {

      if constexpr (std::numeric_limits<Tw>::has_quiet_NaN) {
        if (i == j && has_diag == false)
          value = NAN;
        else
          value = Get0(i, j);
        str << value;

      } else if constexpr (true) {
        str << "NAN";
      }
      if (j < RowsCount - 1)
        str << colsep;
    }
    if (i < RowsCount - 1)
      str << rowsep;
  }
  return str.str();
}

template <bool has_diag, typename Tw>
bool MatrixSym<has_diag, Tw>::Any(Tw value) const {
  Ti i;
  if constexpr (std::numeric_limits<Tw>::has_quiet_NaN) {

    if (std::isnan(value)) {
      for (i = 0; i < length_array(); i++)
        if (std::isnan(Data[i]))
          return true;
    } else {
      for (i = 0; i < length_array(); i++)
        if (Data[i] == value)
          return true;
    }
  } else if constexpr (true) {
    for (i = 0; i < length_array(); i++)
      if (Data[i] == value)
        return true;
  }
  return false;
}

template <bool has_diag, typename Tw>
bool MatrixSym<has_diag, Tw>::All(Tw value) const {
  Ti i;
  if constexpr (std::numeric_limits<Tw>::has_quiet_NaN) {

    if (std::isnan(value)) {
      for (i = 0; i < length_array(); i++)
        if (std::isnan(Data[i]) == false)
          return false;
    } else {
      for (i = 0; i < length_array(); i++)
        if (Data[i] != value)
          return false;
    }
  } else if constexpr (true) {
    for (i = 0; i < length_array(); i++)
      if (Data[i] != value)
        return false;
  }
  return true;
}

template class ldt::MatrixSym<true, Tv>;
template class ldt::MatrixSym<true, Ti>;

template class ldt::MatrixSym<false, Tv>;
template class ldt::MatrixSym<false, Ti>;
