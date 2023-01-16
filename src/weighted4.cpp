/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the LGPL 3.0.
 See accompanying file LICENSE
*/

#include "running.h"

using namespace ldt;

RunningWeighted4::RunningWeighted4() {}

RunningWeighted4::RunningWeighted4(const Matrix<Tv> &data, Tv weight) {
  PushNewRange(data, weight);
}

void RunningWeighted4::PushNew(Tv value, Tv weight) {
  mCount++;
  Tv d = value - mM1;
  Tv d2 = d * d;
  Tv d3 = d2 * d;
  Tv d4 = d2 * d2;
  Tv sW = mSumWeights + weight;

  mM1 = (mSumWeights * mM1 + weight * value) / sW;
  Tv m2 = mM2 + d2 * mSumWeights * weight / sW;
  Tv m3 = mM3 + d3 * mSumWeights * weight * (mSumWeights - weight) / (sW * sW) +
          3 * d * (-weight * mM2) / sW;
  Tv m4 =
      mM4 +
      d4 * mSumWeights * weight *
          (mSumWeights * mSumWeights - mSumWeights * weight + weight * weight) /
          (sW * sW * sW) +
      6 * d2 * (weight * weight * mM2) / (sW * sW) +
      4 * d * (-weight * mM3) / sW;

  mM2 = m2;
  mM3 = m3;
  mM4 = m4;
  mSumWeights = sW;
}

void RunningWeighted4::PushNewDistribution(Tv mean, Tv variance, Tv skewness,
                                           Tv kurtosis, Tv weight, Ti count) {

  auto rwm = RunningWeighted4();
  rwm.mCount = count;
  rwm.mSumWeights = weight;
  rwm.mM1 = mean;
  rwm.mM2 = variance * weight;
  if (skewness != 0)
    rwm.mM3 = std::pow(rwm.mM2, (Tv)1.5) * skewness / std::sqrt(weight);
  if (kurtosis != 0)
    rwm.mM4 = (kurtosis + (Tv)3) * (rwm.mM2 * rwm.mM2) / weight;

  Combine(rwm);
}

void RunningWeighted4::PushNewRange(const Matrix<Tv> &data, Tv weight) {
  for (Ti i = 0; i < data.length(); i++)
    PushNew(data.Data[i], weight);
}

void RunningWeighted4::Combine(RunningWeighted4 &b) {
  if (b.mCount == 0)
    return;
  if (mCount == 0) {
    mCount = b.mCount;
    mSumWeights = b.mSumWeights;
    mM1 = b.mM1;
    mM2 = b.mM2;
    mM3 = b.mM3;
    mM4 = b.mM4;
    return;
  }

  mCount = mCount + b.mCount;
  auto sW = mSumWeights + b.mSumWeights;
  Tv d = b.mM1 - mM1;
  Tv d2 = d * d;
  Tv d3 = d2 * d;
  Tv d4 = d2 * d2;

  Tv m1 = (mSumWeights * mM1 + b.mSumWeights * b.mM1) / sW;
  Tv m2 = mM2 + b.mM2 + d2 * mSumWeights * b.mSumWeights / sW;
  Tv m3 = mM3 + b.mM3 +
          d3 * mSumWeights * b.mSumWeights * (mSumWeights - b.mSumWeights) /
              (sW * sW) +
          3 * d * (mSumWeights * b.mM2 - b.mSumWeights * mM2) / sW;
  Tv m4 = mM4 + b.mM4 +
          d4 * mSumWeights * b.mSumWeights *
              (mSumWeights * mSumWeights - mSumWeights * b.mSumWeights +
               b.mSumWeights * b.mSumWeights) /
              (sW * sW * sW) +
          6 * d2 *
              (mSumWeights * mSumWeights * b.mM2 +
               b.mSumWeights * b.mSumWeights * mM2) /
              (sW * sW) +
          4 * d * (mSumWeights * b.mM3 - b.mSumWeights * mM3) / sW;

  mM1 = m1;
  mM2 = m2;
  mM3 = m3;
  mM4 = m4;
  mSumWeights = sW;
}
