/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the GPL 3.0.
 See accompanying file LICENSE
*/

#include "distributions.h"
#include "helpers.h"

#include <iostream>
#include <locale>
#include <math.h>
#include <numeric>
#include <random>
#include <sstream>

using namespace ldt;

NormalM::NormalM(Ti m, Matrix<Tv> *mean, Matrix<Tv> *variance,
                 Ti sampling_length, bool samples_in_rows, bool mean_is_const,
                 Tv mean_const, bool variance_is_const, Tv variance_const,
                 bool covariance_is_const, Tv covariance_const) {
  pM = m;
  pSampleInRows = samples_in_rows;

  // make sure mean is not null
  if (!mean) {
    Mean = Matrix<Tv>(new double[m], m, 1);
    pDeleteMean = true;
    mean_is_const = true;
  } else {
    Mean = *mean;
    if (Mean.length() != m)
      throw std::invalid_argument("invalid dimension: mean");
  }
  if (mean_is_const)
    Mean.SetValue(mean_const);

  pIsZeroVariance = variance_is_const && variance_const == 0;
  if (pIsZeroVariance == false) {
    pIsConstantDiagVariance =
        variance_is_const && covariance_is_const && covariance_const == 0;
    if (pIsConstantDiagVariance)
      pConstantVariance = variance_const;
  }

  if (pIsZeroVariance == false && pIsConstantDiagVariance == false) {
    // make sure variance is not null if it is not zero or is not diagonal
    if (!variance) {
      Variance = Matrix<Tv>(new double[m * m], m, m);
      pDeleteVariance = true;
    } else {
      Variance = *variance;
      if (Variance.RowsCount != m || Variance.ColsCount != m)
        throw std::invalid_argument("invalid dimension: variance");
    }

    if (variance_is_const && covariance_is_const)
      Variance.SetValueDiag(variance_const, covariance_const);
    else if (covariance_is_const)
      Variance.SetValueOffDiag(covariance_const);
    else if (variance_is_const)
      Variance.SetValueDiag(variance_const);
  }

  WorkSize = 0;
  StorageSize = 0;

  if (sampling_length > 0) {
    if (pSampleInRows)
      Sample = Matrix<Tv>(sampling_length, m);
    else
      Sample = Matrix<Tv>(m, sampling_length);

    WorkSize += (pIsZeroVariance || pIsConstantDiagVariance) ? 0 : 2 * m;
    StorageSize = m * sampling_length;
  }
}

NormalM::~NormalM() {
  if (pDeleteMean) {
    delete Mean.Data;
  }
  if (pDeleteVariance) {
    delete Variance.Data;
  }
}

void NormalM::GetSample(Tv *storage, Tv *WORK, unsigned int seed) {

  Sample.SetData(storage);
  Ti i, j;
  Ti n = pSampleInRows ? Sample.RowsCount : Sample.ColsCount;

  if (pIsZeroVariance || pIsConstantDiagVariance) {
    if (pSampleInRows)
      for (i = 0; i < n; i++)
        Sample.SetRow0(i, Mean);
    else
      for (j = 0; j < n; j++)
        Sample.SetColumn0(j, Mean);

    if (pIsZeroVariance)
      return;
  }

  // distribution
  std::default_random_engine eng;
  if (seed != 0)
    eng = std::default_random_engine(seed);
  else {
    std::random_device rdev{};
    eng = std::default_random_engine(rdev());
  }
  std::normal_distribution<Tv> dist(0, 1);

  if (pIsConstantDiagVariance) { // already inserted mean. add random number
                                 // with each row or column

    if (pSampleInRows)
      for (i = 0; i < n; i++)
        for (j = 0; j < pM; j++)
          Sample.Set_Plus0(i, j, dist(eng));
    else
      for (j = 0; j < n; j++)
        for (i = 0; i < pM; i++)
          Sample.Set_Plus0(i, j, dist(eng));

    return;
  }

  // we are sure that both mean and variance matrixes exist here

  Ti q = 0;
  auto stdnorm = Matrix<Tv>(&WORK[q], pM);
  q += pM;
  auto temp = Matrix<Tv>(&WORK[q], pM);
  q += pM;

  int info = Variance.Chol0(false);
  if (info != 0)
    throw LdtException(
        ErrorType::kLogic, "mnormal",
        "Cholesky decomposition failed in calculating variance matrix");

  for (i = 0; i < n; i++) {
    for (j = 0; j < pM; j++)
      stdnorm.Data[j] = dist(eng);
    Variance.Dot0(stdnorm, temp);
    Mean.Add0(temp, temp);
    if (pSampleInRows)
      Sample.SetRow0(i, temp);
    else
      Sample.SetColumn0(i, temp);
  }
}

// TODO: add density option to the constructor and calculate Cholesky just once

Ti NormalM::GetDensity(Matrix<Tv> *x, Matrix<Tv> *storage, Tv *WORK,
                       bool log) const {

  if (x->RowsCount != pM)
    throw std::invalid_argument("invalid dimension: x (rows)");
  if (storage->length() != x->ColsCount)
    throw std::invalid_argument("invalid length: storage");
  auto n = storage->length();

  if (pIsZeroVariance) { // zero variance
    // if any row is equal to mean, set +infinity, otherwise, 0 (or -infinity
    // for log)
    constexpr auto posinf = std::numeric_limits<Tv>::infinity();
    constexpr auto neginf = -posinf;
    for (Ti j = 0; j < n; j++) {
      bool eq = true;
      for (Ti i = 0; i < pM; i++) {
        if (Mean.Data[i] != x->Get0(i, j)) {
          eq = false;
          break;
        }
      }
      if (eq)
        storage->Data[j] = posinf;
      else
        storage->Data[j] = (log ? neginf : 0.0);
    }
  }

  const Tv hafmlog2pi = static_cast<Tv>(pM * 0.5 * std::log(2 * c_pi));
  // subtract mean. it destroys x
  Tv mi;
  for (Ti i = 0; i < pM; i++) {
    mi = Mean.Data[i];
    for (Ti j = 0; j < n; j++)
      x->Set0(i, j, x->Get0(i, j) - mi);
  }

  auto temp = Matrix<Tv>(WORK, n); // work
  if (Variance.Data) {             // identity variance
    x->Power_in(2);
    auto vv = std::vector<Ti>();
    x->ColumnsSum(temp, vv);
    if (log)
      for (Ti i = 0; i < n; i++)
        storage->Data[i] = -hafmlog2pi - 0.5 * temp.Data[i];
    else
      for (Ti i = 0; i < n; i++)
        storage->Data[i] = std::exp(-hafmlog2pi - 0.5 * temp.Data[i]);
    return 0;
  }

  auto L = Matrix<Tv>(&WORK[n], pM, pM);
  Variance.CopyTo00(L);
  auto info = L.Chol0(true);
  if (info != 0)
    return info;

  Tv sumdiaglogL = 0.0;
  for (Ti i = 0; i < pM; i++)
    sumdiaglogL += std::log(L.Get0(i, i));

  L.SolveTrian(*x, true, true, false);
  x->Power_in(2);
  auto vv = std::vector<Ti>();
  x->ColumnsSum(temp, vv);

  if (log)
    for (Ti i = 0; i < n; i++)
      storage->Data[i] = -sumdiaglogL - hafmlog2pi - 0.5 * temp.Data[i];
  else
    for (Ti i = 0; i < n; i++)
      storage->Data[i] =
          std::exp(-sumdiaglogL - hafmlog2pi - 0.5 * temp.Data[i]);

  return 0;
}
