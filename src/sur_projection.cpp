/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the GPL 3.0.
 See accompanying file LICENSE
*/

#include "sur.h"

using namespace ldt;

SurProjection::SurProjection(Ti n, Ti m, Ti k, bool isRestricted,
                             bool doVariance) {
  mIsRestricted = isRestricted;
  mDoVariance = doVariance;
  auto km = k * m;
  StorageSize = m * n + (doVariance ? (m * m + m * n) : 0);
  WorkSize = m + k + (doVariance ? ((isRestricted ? 3 : 2) * m * km) : 0);
}

void SurProjection::Calculate(const Sur &model, const Matrix<Tv> &x,
                              Tv *storage, Tv *work) {

  Ti n = x.RowsCount;
  Ti m = model.pY->ColsCount;
  Ti k = x.ColsCount;
  auto temp = SurProjection(n, m, k, mIsRestricted, mDoVariance);
  if (temp.WorkSize > WorkSize || temp.StorageSize > StorageSize)
    throw LdtException(ErrorType::kLogic, "sur-projection",
                       "inconsistent arguments 'in SurProjection'");

  Ti km = k * m;
  Ti nm = n * m;

  Means.SetData(storage, n, m);
  if (mDoVariance) {
    Variances.SetData(&storage[nm], n, m);
    Covariance.SetData(&storage[2 * nm], m, m);
  }

  Ti q = 0;
  Matrix<Tv> t = Matrix<Tv>(&work[q], m, 1);
  q += m;
  Matrix<Tv> row = Matrix<Tv>(&work[q], k, 1);
  q += k;

  auto Iox = Matrix<Tv>();
  auto RIox = Matrix<Tv>();
  auto RIox_gv = Matrix<Tv>();
  Ti qStar = mIsRestricted ? model.pR->ColsCount : km;

  if (mDoVariance) {
    Iox.SetData(&work[q], km, m);
    q += m * km;
    RIox_gv.SetData(&work[q], m, qStar);
    q += m * qStar;
    if (mIsRestricted) {
      RIox.SetData(&work[q], qStar, m);
      q += m * qStar;
    }
  }

  for (Ti i = 0; i < n; i++) {
    x.GetRow0(i, row);
    model.beta.tDotVector(row, t);
    Means.SetRow0(i, t); // B'x (mxk  *  kx1 -> mx1)

    if (mDoVariance) {

      if (mIsRestricted) {
        row.IdenKron(m, Iox);       // [x o I_m]: km x m
        model.pR->TrDot(Iox, RIox); // R'[x o I_m]: qStar x m
        RIox.TrDot(model.gamma_var,
                   RIox_gv); // [x' o I_m]R [R'[S^-1 o X'X]R]^{-1}:  m x qStar
        RIox_gv.Dot(RIox, Covariance); // [x' o I_m]R [R'[S^-1 o X'X]R]^{-1}R'[x
                                       // o I_m] : m x m
        Covariance.Add_in(model.resid_var); // S^ + [x' o I_m]R [R'[S^-1 o
                                            // X'X]R]^{-1}R'[x o I_m]
        // you don't need the last line if this is for fitted

      } else {
        row.IdenKron(m, Iox);                // km x m
        Iox.TrDot(model.gamma_var, RIox_gv); // m x km
        RIox_gv.Dot(Iox, Covariance);        // m x m
        Covariance.Add_in(
            model.resid_var); // you don't need it if this is for fitted
      }

      Variances.SetRowFromDiag0(i, Covariance);
    }
  }
}
