/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the GPL 3.0.
 See accompanying file LICENSE
*/

#include "running.h"

using namespace ldt;

RunningWeightedVariance::RunningWeightedVariance() {}

RunningWeightedVariance::RunningWeightedVariance(const Matrix<Tv> &data,
                                                 Tv weight) {
  PushNewRange(data, weight);
}

void RunningWeightedVariance::PushNew(Tv value, Tv weight) {
  mCount++;
  Tv d2 = value - mM1;
  d2 = d2 * d2;
  Tv sW = mSumWeights + weight;
  mM1 = (mSumWeights * mM1 + weight * value) / sW;
  mM2 += d2 * mSumWeights * weight / sW;
  mSumWeights = sW;
}

void RunningWeightedVariance::PushNewRange(const Matrix<Tv> &data, Tv weight) {
  for (Ti i = 0; i < data.length(); i++)
    PushNew(data.Data[i], weight);
}

void RunningWeightedVariance::Combine(RunningWeightedVariance b) {
  if (mSumWeights == 0) {
    mSumWeights = b.mSumWeights;
    mCount = b.mCount;
    mM1 = b.mM1;
  } else if (b.mSumWeights == 0)
    return;

  mCount += b.mCount;
  Tv d2 = b.mM1 - mM1;
  d2 = d2 * d2;
  Tv sW = mSumWeights + b.mSumWeights;
  mM1 = (mSumWeights * mM1 + b.mSumWeights * b.mM1) / sW;
  mM2 += b.mM2 + d2 * mSumWeights * b.mSumWeights / sW;
  mSumWeights = sW;
}
