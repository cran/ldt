/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the GPL 3.0.
 See accompanying file LICENSE
*/

#include "distributions.h"
#include "statistics.h"
#include <random>

using namespace ldt;

void fill(Matrix<Tv> *sorteddata, Matrix<Tv> *axis, Matrix<Ti> *storageCount) {

  auto n = axis->length();
  auto m = sorteddata->length();
  double d, a;
  Ti p, i, j = 0;
  bool added;

  for (i = 0; i < n + 1; i++)
    storageCount->Data[i] = 0;

  for (i = 0; i < m; i++) {
    d = sorteddata->Data[i];
    if (i == m - 1) { // the last one
      added = false;
      for (p = j; p < n; p++) {
        a = axis->Data[p];
        if (d <= a) { //  <= for the last one
          storageCount->Data[p] = storageCount->Data[p] + 1; // use ++
          j = p; // don't check others
          added = true;
          break;
        }
      }
      if (added == false)
        storageCount->Data[n] = storageCount->Data[n] + 1; // use ++
    } else {
      added = false;
      for (p = j; p < n; p++) {
        a = axis->Data[p];
        if (d < a) {
          storageCount->Data[p] = storageCount->Data[p] + 1; // use ++
          j = p; // don't check others
          added = true;
          break;
        }
      }
      if (added == false)
        storageCount->Data[n] = storageCount->Data[n] + 1; // use ++
    }
  }
}

Ti Histogram::Compute(bool forsize, Matrix<Tv> *data, Matrix<Tv> *storageAxis,
                      Matrix<Ti> *storageCount, Ti &bincount, double &min,
                      double &max, double iqrMultiply, bool checkNAN,
                      double step) {

  // remove NAN
  if (checkNAN)
    data->RemoveNanVector_in(true); // data must not have infinity or NAN

  auto values = *data;
  Ti L = values.length();
  if (L == 0)
    throw std::logic_error("invalid length"); // no data

  if (forsize) {
    if (max <= min)
      throw std::logic_error("Invalid min/max for histogram");

    std::sort(values.Data, values.Data + L); // sort

    if (std::isnan(min) || std::isnan(max) || bincount <= 0) {

      auto st = Descriptive(&values); // sorted
      auto min0 = st.QuantileSorted(0.0);
      auto q1 = st.QuantileSorted(0.25);
      auto q3 = st.QuantileSorted(0.75);
      auto max0 = st.QuantileSorted(1.0);

      if (std::isinf(min0) || std::isinf(max0))
        throw std::logic_error("Data contains 'infinity'"); // inf found

      auto iqr = q3 - q1;

      if (std::isnan(min))
        min = std::max(
            min0,
            q1 -
                iqr *
                    iqrMultiply); // if there is no obs, don't expand the range.
      if (std::isnan(max))
        max = std::min(max0, q3 + iqr * iqrMultiply);

      if (std::isnan(min) || std::isnan(max))
        throw std::logic_error("Data is 'NAN' or contains 'NaN'"); // nan found

      if (std::isnan(step)) {
        if (bincount == 0)
          bincount = static_cast<Ti>(
              ((max - min) < DBL_EPSILON)
                  ? 1
                  : (std::pow(L, 1.0 / 3.0) * (max - min) /
                     (2.0 * iqr))); // I use the relative min,max not the
                                    // absolute ones
      } else                        // get bincount from given 'step'
        bincount = static_cast<Ti>(std::ceil((max - min) / step));
    }
    if (min == max) { // generally, 1 element case. If min and max are given and
                      // equal, we throw exception
      min -= 0.01;
      max += 0.01;
    }
    return bincount + 1; // size
  }

  if (bincount == 1) {
    storageAxis->Data[0] = min;
    storageAxis->Data[1] = max;
    storageCount->Data[0] = 0;
    storageCount->Data[1] = L;
    storageCount->Data[2] = 0;
  } else {

    if (std::isnan(step)) {
      if (bincount == 0)
        throw std::logic_error("bincount is zero for histogram");
      step = (max - min) / bincount;
    } else if (step == 0.0)
      throw std::logic_error("Step is zero for histogram");
    else
      bincount = storageAxis->length() - 1;

    if (bincount == 0)
      throw std::logic_error("bincount is zero for histogram");

    for (Ti i = 0; i < bincount; i++) {      // axis size is bincount+1
      storageAxis->Data[i] = min + i * step; // don't increment
    }
    storageAxis->Data[bincount] =
        min + bincount * step +
        DBL_EPSILON; // we treat the last obs differently, or obs might consider
                     // overflow due to double error

    fill(data, storageAxis, storageCount);
  }
  return L;
}

Ti Histogram::ComputeV(Matrix<Tv> *data, Matrix<Tv> *storageAxis,
                       Matrix<Ti> *storageCount, bool checkNAN) {

  // remove NAN
  if (checkNAN)
    data->RemoveNanVector_in(true); // data must not have infinity or NAN

  auto values = *data;
  Ti L = values.length();
  if (L == 0)
    throw std::logic_error("invalid length");

  std::sort(values.Data, values.Data + L); // sort
  fill(data, storageAxis, storageCount);
  return L;
}
