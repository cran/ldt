/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the GPL 3.0.
 See accompanying file LICENSE
*/

#include "scoring.h"

using namespace ldt;

template <bool hasWeight> FrequencyCost<hasWeight>::FrequencyCost(Ti count) {
  StorageSize = 2 * count;
}

template <bool hasWeight>
void FrequencyCost<hasWeight>::Calculate(const std::vector<Matrix<Tv>> &costs,
                                         const Matrix<Tv> &y,
                                         const Matrix<Tv> &scores,
                                         const Matrix<Tv> *weights,
                                         Tv *storage) {

  Ti count = (Ti)costs.size();
  CostSums.SetData(0, storage, count, 1);
  CostCounts.SetData(0, &storage[count], count, 1);

  Ti n = y.length();
  Ti rowIndex, actualCol, j;
  Tv weight, maxprob, value, yi;
  for (Ti i = 0; i < n; i++) {
    yi = y.Data[i];
    if (std::isnan(yi))
      continue;
    actualCol = static_cast<Ti>(yi);
    if constexpr (hasWeight) {
      weight = weights->Data[i];
    } else if constexpr (true) {
      weight = 1;
    }
    maxprob = scores.Get0(i, actualCol);
    j = -1;
    for (const auto &c : costs) {
      j++;
      // first column of 'c' is the thresholds. e.g. [0.3, 0.7, 1] means four
      // probability bounds
      for (rowIndex = 0; rowIndex < c.RowsCount; rowIndex++) {
        if (c.Get0(rowIndex, 0) >= maxprob)
          break;
      }
      // the value at 'actualCol+1' column defines how we should increment the
      // wrong count
      value = c.Get0(rowIndex, actualCol + 1);
      if (value == 0) {
        CostCounts.Data[j] += weight;
        continue;
      }
      CostSums.Data[j] += value * weight;
      CostCounts.Data[j] += weight;
    }
  }

  AverageRatio = 0;
  for (j = 0; j < (Ti)costs.size(); j++)
    AverageRatio += CostSums.Data[j] / CostCounts.Data[j];
  AverageRatio /= costs.size();
}

template <bool hasWeight>
void FrequencyCost<hasWeight>::Check(const Matrix<Tv> frequencyCost,
                                     const Ti &numChoices) {
  if (frequencyCost.RowsCount <= 1)
    throw std::logic_error(
        "Invalid frequency cost matrix. I expect 2 or more rows.");
  if (frequencyCost.ColsCount != numChoices + 1)
    throw std::logic_error(
        "Invalid frequency cost matrix. 'number of columns' must be = 'number "
        "of "
        "choices' + 1."); // number of columns must be 1+number of choices
  Tv pre = 0;
  for (Ti i = 0; i < frequencyCost.RowsCount; i++) {
    auto d = frequencyCost.Get0(i, 0);
    if (d < 0 || d > 1)
      throw std::logic_error("Invalid frequency cost matrix. Values in the "
                             "first column must be in [0,1] "
                             "range.");
    if (i > 0) {
      if (d <= pre)
        throw std::logic_error("Invalid frequency cost matrix. Values in the "
                               "first column must be in "
                               "ascending order.");
    }
    pre = d;
  }
}

template class ldt::FrequencyCost<true>;
template class ldt::FrequencyCost<false>;
