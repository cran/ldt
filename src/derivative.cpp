/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the GPL 3.0.
 See accompanying file LICENSE
*/

#include "functionx.h"
#include <algorithm>
#include <iostream>
#include <math.h>
#include <sstream>
#include <string>
#include <vector>

using namespace ldt;

Derivative::Derivative(Ti n, bool doFirstDerivative, bool doSecondDerivative,
                       Ti count) {
  mCount = count;
  mN = n;
  mDoFirstDerivative = doFirstDerivative;
  mDoSecondDerivative = doSecondDerivative;

  // calculate sizes

  Ti s1 = 0;
  if (doFirstDerivative) {
    s1 = 3 * n + mCount * n;
    for (Ti c = 1; c < mCount; c++)
      s1 += (mCount - c) * n;

    StorageSize1 = mN;
    Result1 = Matrix<Tv>(mN, 1);
  }
  Ti s2 = 0;
  if (doSecondDerivative) {
    Ti nn = n * (n + 1) / 2;
    s2 = 3 * n + mCount * nn;
    for (Ti c = 1; c < mCount; c++)
      s2 += nn * (mCount - c);

    StorageSize2 = mN * mN;
    Result2 = Matrix<Tv>(mN, mN);
  }

  WorkSize = std::max(s1, s2);
}

void Derivative::CalculateFirst(const std::function<Tv(const Matrix<Tv> &)> &f,
                                const Matrix<Tv> &x, Tv *storage, Tv *work) {

  Ti n = x.length();
  if (n > mN)
    throw LdtException(ErrorType::kLogic, "derivative",
                       "invalid size in 'first derivative'");
  if (mDoFirstDerivative == false)
    throw LdtException(ErrorType::kLogic, "derivative", "invalid request");

  Result1.SetData(0, storage);
  Result1.Restructure(n, 1);

  // auto dir = _dir(x);
  // if (dir.empty() == false && dir.size() != n)
  //	throw std::invalid_argument("wrong number of directions. It must be
  // equal to the size of x");

  Ti pos = 0;
  auto x0 = Matrix<Tv>(&work[pos], x.RowsCount, x.ColsCount);
  pos += n; // we should keep the dimensions
  auto h_l = Matrix<Tv>(0.0, &work[pos], n);
  pos += n;
  auto h_r = Matrix<Tv>(0.0, &work[pos], n);
  pos += n;
  auto A0 = Matrix<Tv>(0.0, &work[pos], mCount, n);
  pos += mCount * n;
  auto As = std::vector<Matrix<Tv>>(mCount);
  As[0] = A0;
  for (Ti c = 1; c < mCount; c++) {
    As[c] = Matrix<Tv>(0.0, &work[pos], mCount - c, n);
    pos += (mCount - c) * n;
  }

  auto s0_2 = Step0 / 2;
  auto s1_2 = Step1 / 2;
  Tv ax;
  Ti p;
  for (Ti i = 0; i < n; i++) {
    ax = std::abs(x.Data[i]);
    p = 0; // (dir.empty() || dir[i] == 0) ? 0 : dir[i] > 0 ? 1 : 0;
    switch (p) {
    case 0:
      h_l.Data[i] = ax < Epsilon ? s0_2 : s1_2 * ax;
      h_r.Data[i] = -h_l.Data[i];
      break;
    case 1:
      h_l.Data[i] = ax < Epsilon ? Step0 : Step1 * ax;
      h_r.Data[i] = 0.0;
      break;
    case -1:
      h_l.Data[i] = 0.0;
      h_r.Data[i] = ax < Epsilon ? -Step0 : -Step1 * ax;
      break;
    }
  }

  // The goal is to calculate
  //     A_{i+1}(h) = (T^k_i A_{i}(h/T) - A_{i}(h))/(T^k_i - 1)    for   i = 0
  //     ... Count-1

  // These are functions of A0(h), A0(h/T), A0(h/2t),A0(h/4t)...
  // where A0(h) :: A0(_h,h_)=(f(x+h_)-f(x+_h))/Step
  // where h_ - _h for each i is equal to Step (based on direction, they are 0.0
  // or +-Step/2

  // we save A0(h) in rows of A0

  x.CopyTo00(x0);
  Tv f_l, f_r, dif;
  for (Ti c = 0; c < mCount; c++) {
    for (Ti i = 0; i < n; i++) {
      if (c > 0 && A0.Get0(c - 1, i) == 0.0) {
        A0.Set0(c, i, 0.0);
      } else {
        x0.Data[i] = x.Data[i] + h_l.Data[i];
        f_l = f(x0);
        x0.Data[i] = x.Data[i] + h_r.Data[i];
        f_r = f(x0);
        dif = (f_l - f_r) / (h_l.Data[i] - h_r.Data[i]);
        A0.Set0(c, i, std::abs(dif) <= 1e-16 ? 0.0 : dif); //?
        x0.Data[i] = x.Data[i];
      }
    }
    h_l.Divide_in(T);
    h_r.Divide_in(T);
  }

  // I assume that k_i = i

  As[0] = A0;
  Tv tki;
  for (Ti c = 1; c < mCount; c++) {
    tki = pow(T, c);

    As[c - 1].GetSub0(1, 0, mCount - c, n, As[c]);
    As[c].Multiply_in(tki);
    // subtract
    for (Ti i = 0; i < mCount - c; i++)
      for (Ti j = 0; j < n; j++)
        As[c].Set0(i, j, As[c].Get0(i, j) - As[c - 1].Get0(i, j));
    As[c].Divide_in(tki - 1);
  }
  As[mCount - 1].CopyTo00(Result1);
}

void Derivative::CalculateSecond(const std::function<Tv(const Matrix<Tv> &)> &f,
                                 const Matrix<Tv> &x, Tv *storage, Tv *work) {

  Ti n = x.length();

  if (n > mN)
    throw LdtException(ErrorType::kLogic, "derivative",
                       "invalid size in 'second derivative'");
  if (mDoSecondDerivative == false)
    throw LdtException(ErrorType::kLogic, "derivative", "invalid request");

  Result2.SetData(0, storage);
  Result2.Restructure(n, n); // in case n is smaller

  Ti nn = n * (n + 1) / 2;
  Ti pos = 0;
  Ti c, i, j, p;

  // auto dir = _dir(x);
  // if (dir.empty() == false && dir.size() != n)
  //	throw std::invalid_argument("wrong number of directions. It must be
  // equal to the size of x");

  auto x0 = Matrix<Tv>(work, x.RowsCount, x.ColsCount);
  pos += n; // we should keep the dimensions
  auto h_l = Matrix<Tv>(0.0, &work[pos], n);
  pos += n;
  auto h_r = Matrix<Tv>(0.0, &work[pos], n);
  pos += n;
  auto A0 = Matrix<Tv>(0.0, &work[pos], mCount, nn);
  pos += nn * mCount;
  auto As = std::vector<Matrix<Tv>>(mCount);
  As[0] = A0;
  for (c = 1; c < mCount; c++) {
    As[c] = Matrix<Tv>(0.0, &work[pos], mCount - c, nn);
    pos += nn * (mCount - c);
  }

  auto s0_2 = 2 * Step0;
  auto s1_2 = 2 * Step1;
  Tv ax;
  for (i = 0; i < n; i++) {
    ax = std::abs(x.Data[i]);
    p = 0; // (dir.empty() || dir[i] == 0) ? 0 : dir[i] > 0 ? 1 : 0;
    switch (p) {
    case 0:
      h_l.Data[i] = ax < 1 ? s0_2 : s1_2 * ax;
      h_r.Data[i] = -h_l.Data[i];
      break;
    case 1:
      h_l.Data[i] = ax < 1 ? Step0 : Step1 * ax;
      h_r.Data[i] = 0.0;
      break;
    case -1:
      h_l.Data[i] = 0.0;
      h_r.Data[i] = ax < 1 ? -Step0 : -Step1 * ax;
      break;
    }
  }

  x.CopyTo00(x0);
  Tv f_ll, f_lr, f_rl, f_rr, df, dif;
  for (c = 0; c < mCount; c++) {
    p = 0;
    for (i = 0; i < n; i++) {
      for (j = i; j < n; j++) {
        if (c > 0 && A0.Get0(c - 1, p) == 0.0)
          A0.Set0(c, p, 0.0);
        else {
          if (i == j) {
            x0.Data[i] = x.Data[i] + 2.0 * h_l.Data[i];
            f_ll = f(x0);

            x0.Data[i] = x.Data[i] + h_l.Data[i] + h_r.Data[i];
            f_lr = f(x0);
            f_rl = f_lr;

            x0.Data[i] = x.Data[i] + 2.0 * h_r.Data[i];
            f_rr = f(x0);
          } else {
            x0.Data[i] = x.Data[i] + h_l.Data[i];
            x0.Data[j] = x.Data[j] + h_l.Data[j];
            f_ll = f(x0);

            x0.Data[i] = x.Data[i] + h_l.Data[i];
            x0.Data[j] = x.Data[j] + h_r.Data[j];
            f_lr = f(x0);

            x0.Data[i] = x.Data[i] + h_r.Data[i];
            x0.Data[j] = x.Data[j] + h_l.Data[j];
            f_rl = f(x0);

            x0.Data[i] = x.Data[i] + h_r.Data[i];
            x0.Data[j] = x.Data[j] + h_r.Data[j];
            f_rr = f(x0);
          }

          // reset values
          x0.Data[i] = x.Data[i];
          x0.Data[j] = x.Data[j];

          df = f_ll - f_lr - f_rl + f_rr;
          dif =
              df / ((h_r.Data[i] - h_l.Data[i]) * (h_r.Data[j] - h_l.Data[j]));
          A0.Set0(c, p, std::abs(dif) <= 1e-16 ? 0.0 : dif); //?
        }

        p++;
      }
    }
    h_l.Divide_in(T);
    h_r.Divide_in(T);
  }
  Tv tki;
  for (c = 1; c < mCount; c++) {
    tki = pow(T, c);

    As[c - 1].GetSub0(1, 0, mCount - c, nn, As[c]);
    As[c].Multiply_in(tki);
    // subtract
    for (i = 0; i < mCount - c; i++)
      for (j = 0; j < nn; j++)
        As[c].Set0(i, j, As[c].Get0(i, j) - As[c - 1].Get0(i, j));
    As[c].Divide_in(tki - 1);
  }

  Matrix<Tv>::MakeTriangular0(Result2, As[mCount - 1], 0, true, true);
}
