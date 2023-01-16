/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the LGPL 3.0.
 See accompanying file LICENSE
*/

#include "running.h"

using namespace ldt;

RunningWeightedMean::RunningWeightedMean() {}

RunningWeightedMean::RunningWeightedMean(const Matrix<Tv> &data, Tv weight) {
  PushNewRange(data, weight);
}

void RunningWeightedMean::PushNew(Tv value, Tv weight) {
  mCount++;
  Tv sW = mSumWeights + weight;
  mM1 = (mSumWeights * mM1 + weight * value) / sW;
  mSumWeights = sW;
}

void RunningWeightedMean::PushNewRange(const Matrix<Tv> &data, Tv weight) {
  for (Ti i = 0; i < data.length(); i++)
    PushNew(data.Data[i], weight);
}

void RunningWeightedMean::Combine(RunningWeightedMean &b) {
  if (mSumWeights == 0) {
    mSumWeights = b.mSumWeights;
    mCount = b.mCount;
    mM1 = b.mM1;
  } else if (b.mSumWeights == 0) {
    // no change
  } else {
    mCount = mCount + b.mCount;
    Tv sW = mSumWeights + b.mSumWeights;
    mM1 = (mSumWeights * mM1 + b.mSumWeights * b.mM1) / sW;
    mSumWeights = sW;
  }
}
