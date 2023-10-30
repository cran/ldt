/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the GPL 3.0.
 See accompanying file LICENSE
*/

#include "statistics.h"

#include <functional>
#include <iomanip>
#include <iostream>
#include <numeric>
#include <random>
#include <sstream>
#include <vector>

using namespace ldt;

// #pragma region Rank

Rank::Rank(Ti rows, Ti cols) {
  StorageSize = rows * cols;
  WorkSize = rows * cols; // to create sorted matrix

  Result = Matrix<Tv>();
}

void Rank::Calculate(const Matrix<Tv> &mat, Tv *work, Tv *storage,
                     bool ascending) {
  // check size
  auto temp = Rank(mat.RowsCount, mat.ColsCount);
  if (temp.WorkSize > WorkSize || temp.StorageSize > StorageSize)
    throw LdtException(ErrorType::kLogic, "statistics",
                       "inconsistent arguments");

  this->Result.SetData(storage, mat.RowsCount, mat.ColsCount);

  auto sorted = Matrix<Tv>(work, mat.RowsCount, mat.ColsCount);
  mat.Sort(sorted, ascending);

  // if ascending, at ties, we get the minimum
  //  otherwise, we get the maximum

  // linear search to find the ranks ?!
  Tv *col1;
  Tv *col2;
  Tv *rcol;
  Tv v, w;
  Ti r, i, j, k;
  for (j = 0; j < mat.ColsCount; j++) {
    r = j * mat.RowsCount;
    col1 = &mat.Data[r];
    col2 = &sorted.Data[r];
    rcol = &this->Result.Data[r];
    for (i = 0; i < mat.RowsCount; i++) {
      v = col1[i];
      for (k = 0; k < mat.RowsCount; k++) {
        w = col2[k];
        if (w == v) {
          rcol[i] = static_cast<Tv>(k);
          break;
        }
      }
    }
  }
}

// #pragma endregion

// #pragma region OLS

Ols::Ols(Ti N, Ti m, Ti k, bool resid, bool sigma) {
  if (sigma)
    resid = true; // we need it
  mDoResid = resid;
  mDoSigma = sigma;

  StorageSize = k * m;

  if (resid) {
    StorageSize += N * m;
  }
  if (sigma) {
    StorageSize += m * m;
  }

  WorkSize = 2 * k * k + k * N;
}

void Ols::Calculate(const Matrix<Tv> &y, const Matrix<Tv> &x, Tv *storage,
                    Tv *work) {
  Ti N = y.RowsCount;
  Ti k = x.ColsCount;
  Ti m = y.ColsCount;

  if (x.RowsCount != N)
    throw LdtException(ErrorType::kLogic, "statistics", "invalid length");
  if (N < k)
    throw LdtException(ErrorType::kLogic, "statistics",
                       "low degrees of freedom");

  // check size
  auto temp = Ols(N, m, k, mDoResid, mDoSigma);
  if (temp.WorkSize < WorkSize || temp.StorageSize < StorageSize)
    throw LdtException(ErrorType::kLogic, "statistics",
                       "inconsistent arguments");

  Ti p = 0;
  Beta.SetData(storage, k, m);
  p += k * m;

  // auto m = y.ColsCount;
  Ti pos = 0;
  Matrix<Tv> xx = Matrix<Tv>(work, k, k);
  pos += k * k;
  auto ip = std::make_unique<int[]>(k + 1);
  double *invW = &work[pos];
  pos += k * k;
  Matrix<Tv> xxx = Matrix<Tv>(&work[pos], k, N);
  pos += k * N; // can we  use Resid if available?!

  x.TrDot0(x, xx); // kxk
  auto info = xx.Inv00(ip.get(), invW);
  if (info != 0)
    throw LdtException(ErrorType::kLogic, "statistics", "matrix singularity");
  xx.DotTr0(x, xxx); // kxk . kxN  .  kxN
  xxx.Dot0(y, Beta); // kxN . Nxm  .  kxm

  if (mDoResid) {
    Resid.SetData(&storage[p], N, m);
    p += N * m;

    x.Dot0(Beta, Resid); // yhat: Nxk . k.m  .  Nxm
    y.Subtract0(Resid, Resid);
    if (mDoSigma) {
      Sigma.SetData(&storage[p], m, m);
      p += m * m;

      Resid.TrDot(Resid, Sigma); // mxN . Nxm  .  mxm
    }
  }
}

// #pragma endregion

// #pragma region GLS

Gls::Gls(Ti N, Ti m, Ti k, bool resid, bool sigma, bool isOmegaInv) {
  if (sigma)
    resid = true; // we need it
  mDoResid = resid;
  mDoSigma = sigma;
  mIsOmegaInv = isOmegaInv;

  StorageSize = k * m;

  if (mDoResid) {
    StorageSize += N * m;
  }
  if (mDoSigma) {
    StorageSize += m * m;
  }

  Ti invs = isOmegaInv ? k : N;
  WorkSize = invs * invs + k * k + 2 * k * N;
}

void Gls::Calculate(const Matrix<Tv> &y, const Matrix<Tv> &x, Matrix<Tv> &omega,
                    Tv *storage, Tv *work) {
  Ti N = y.RowsCount;
  Ti k = x.ColsCount;
  Ti m = y.ColsCount;

  if (x.RowsCount != N)
    throw LdtException(ErrorType::kLogic, "statistics", "invalid length");
  if (N < k)
    throw LdtException(ErrorType::kLogic, "statistics",
                       "low degrees of freedom");

  // check size
  auto temp = Gls(N, m, k, mDoResid, mDoSigma);
  if (temp.WorkSize < WorkSize || temp.StorageSize < StorageSize)
    throw LdtException(ErrorType::kLogic, "statistics",
                       "inconsistent arguments");

  Ti p = 0;
  Beta.SetData(storage, k, m);
  p += k * m;

  Ti pos = 0;
  Ti invs = mIsOmegaInv ? k : N;
  auto ip = std::make_unique<int[]>(invs + 1);
  auto invW = &work[pos];
  pos += invs * invs;
  auto xo = Matrix<Tv>(&work[pos], k, N);
  pos += k * N;
  auto xox = Matrix<Tv>(&work[pos], k, k);
  pos += k * k;
  auto xoxxo = Matrix<Tv>(&work[pos], k, N);
  pos += k * N;

  Ti info = 0;
  if (mIsOmegaInv == false) {
    info = omega.Inv00(ip.get(), invW);
    if (info != 0)
      throw LdtException(ErrorType::kLogic, "statistics", "matrix singularity");
  }

  x.TrDot0(omega, xo); // kxN . NxN  .  kxN
  xo.Dot(x, xox);      // kxN . Nxk  .  kxk
  info = xox.Inv00(ip.get(), invW);
  if (info != 0)
    throw LdtException(ErrorType::kLogic, "statistics", "matrix singularity");
  xox.Dot(xo, xoxxo);  // kxk . kxN  .  kxN
  xoxxo.Dot0(y, Beta); // kxN . Nxm  .  kxm

  if (mDoResid) {
    Resid.SetData(&storage[p], N, m);
    p += N * m;

    x.Dot0(Beta, Resid); // yhat: Nxk . k.m  .  Nxm
    y.Subtract0(Resid, Resid);
    if (mDoSigma) {
      Sigma.SetData(&storage[p], m, m);
      p += m * m;

      Resid.TrDot(Resid, Sigma); // mxN . Nxm  .  mxm
    }
  }
}

// #pragma endregion
