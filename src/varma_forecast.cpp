/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the LGPL 3.0.
 See accompanying file LICENSE
*/

#include "varma.h"

using namespace ldt;

VarmaForecast::VarmaForecast(const VarmaSizes &sizes, Ti horizon,
                             bool doVariance, bool coefUncertainty) {
  mHorizon = horizon;
  mCoefUncertainty = coefUncertainty;
  mDoVariance = doVariance;

  Ti g = sizes.EqsCount;
  Ti k = sizes.NumParamsEq;
  Ti h_armax_d = horizon + sizes.ArMax_d;

  StorageSize = g * h_armax_d;
  if (doVariance && coefUncertainty)
    StorageSize *= 3;
  else if (doVariance && !coefUncertainty)
    StorageSize *= 2;

  // work

  WorkSize = k + g;

  if (doVariance) {
    auto arma = VarmaArma(sizes, mDoVariance ? mHorizon : 0);
    WorkSize += arma.WorkSize + arma.StorageSize;
    WorkSize += 3 * g * g;
    if (coefUncertainty)
      WorkSize += horizon * k;
  }

  StartIndex = sizes.ArMax_d;
}

void xtprime(Matrix<Tv> &xt, const Matrix<Tv> &forecast,
             const Matrix<Tv> &resid, Ti fRow, Ti x_ind, Ti resid_ind,
             const Matrix<Tv> *exoData, VarmaSizes &sizes) {
  xt.SetValue(0); // necessary for residuals

  Ti sR = 0;

  if (sizes.HasAr)
    for (auto &l : sizes.ArLags) {
      xt.SetSub0(sR, 0, forecast, 0, fRow - l, forecast.RowsCount, 1);
      sR += forecast.RowsCount;
    }

  if (sizes.ExoCount > 0) {
    xt.SetSub0(sR, 0, *exoData, 0, x_ind + fRow, exoData->RowsCount,
               1); // +ArMax_d because observations are not omitted from
                   // the start of exo
    sR += exoData->RowsCount;
  }

  if (sizes.HasMa) { // you can ignore it if you want structural, in the
                     // sense of Eviews
    for (auto &l : sizes.MaLags) {
      auto ind = resid_ind + fRow - l;
      if (ind < resid.ColsCount)
        xt.SetSub0(sR, 0, resid, 0, ind, forecast.RowsCount, 1);
      sR += forecast.RowsCount;
    }
  }
}

void VarmaForecast::Calculate(const Varma &estimate, const Matrix<Tv> *exo,
                              const Matrix<Tv> *undiff_y, Tv *storage, Tv *work,
                              Ti horizon, bool exoIsNew) {
  auto sizes = estimate.Sizes;
  if (horizon == -1)
    horizon = mHorizon;
  else {
    if (horizon > mHorizon)
      throw std::logic_error("inconsistent horizon");
  }

  Ti g = sizes.EqsCount;
  Ti k = sizes.NumParamsEq;
  Ti diffl = sizes.DiffPoly.size() == 0 ? 0 : (Ti)(sizes.DiffPoly.size() - 1);
  Ti h_armax_d = horizon + sizes.ArMax_d;
  Ti resid_ind = sizes.T - estimate.SampleEnd - sizes.ArMax_d;
  Ti x_ind = sizes.T - estimate.SampleEnd;
  if (exoIsNew)
    x_ind = -sizes.ArMax_d;
  else {
    // check
    if (resid_ind < 0)
      throw std::logic_error(
          "in forecast, end-sample must be larger than 'maxp+diff'");
    if (sizes.ExoCount != 0 && (exo->ColsCount < x_ind + h_armax_d))
      throw std::logic_error(
          "in forecasting by VARMA, length of exogenous data is "
          "less than the requested horizon");
  }

  if (undiff_y == nullptr)
    undiff_y = &estimate.Result.y;

  // set storage

  Ti p = 0;
  Forecast.SetData(&storage[p], g, h_armax_d);
  p += h_armax_d * g;
  if (mDoVariance) {
    Variance.SetData(&storage[p], g, h_armax_d);
    p += h_armax_d * g;
    if (mCoefUncertainty) {
      Variance2.SetData(&storage[p], g, h_armax_d);
      p += h_armax_d * g;
    }
  }

  // work
  p = 0;
  auto xt = Matrix<Tv>(&work[p], k, 1);
  p += k;
  auto yhat = Matrix<Tv>(&work[p], g);
  p += g;
  auto arma = VarmaArma(sizes, mDoVariance ? horizon : 0);
  auto arma_work = &work[p];
  p += arma.WorkSize;
  auto arma_storage = &work[p];
  p += arma.StorageSize;

  Matrix<Tv> varp;
  Matrix<Tv> temp1;
  Matrix<Tv> temp2;
  Matrix<Tv> Xfs;
  if (mDoVariance) {
    varp = Matrix<Tv>(0, &work[p], g, g);
    p += g * g; // init. 0
    temp1 = Matrix<Tv>(&work[p], g, g);
    p += g * g;
    temp2 = Matrix<Tv>(&work[p], g, g);
    p += g * g;

    if (mCoefUncertainty) {
      Xfs = Matrix<Tv>(&work[p], horizon, k);
      p += horizon * k;
    }
  }

  if (!estimate.Result.coef.Data)
    throw std::logic_error("Coefficient matrix is not calculated");

  // Mean
  for (int i = 0; i < sizes.ArMax_d; i++) {
    if (i < diffl)
      Forecast.SetSub0(
          0, i, *undiff_y, 0,
          undiff_y->ColsCount - estimate.SampleEnd - sizes.ArMax_d + i, g,
          1); // how many cols to skip?
    else
      Forecast.SetSub0(0, i, estimate.Result.y, 0,
                       sizes.T - estimate.SampleEnd - sizes.ArMax_d + i, g, 1);
  }

  for (Ti i = sizes.ArMax_d; i < Forecast.ColsCount; i++) {
    xtprime(xt, Forecast, estimate.Result.resid, i, x_ind, resid_ind, exo,
            sizes);

    estimate.Result.coef.Dot0(xt, yhat);
    Forecast.SetColumn(i, yhat);
    if (mCoefUncertainty)
      Xfs.SetRow0(i - sizes.ArMax_d, xt);
  }

  if (sizes.HasDiff)
    Varma::UnDiferences(sizes.DiffPoly, Forecast);

  // variance
  if (mDoVariance) {
    for (int i = 0; i < sizes.ArMax_d; i++) {
      Variance.SetColumn0(i, (Tv)0);
      if (mCoefUncertainty)
        Variance2.SetColumn0(i, (Tv)0);
    }

    arma.Calculate(estimate.Result.coef, arma_storage, arma_work);

    for (int i = 0; i < horizon; i++) {
      arma.MaInf.Coefficients.at(i)->Dot0(estimate.Result.sigma2,
                                          temp2); // optimize: symmetric
      temp2.DotTr0(*arma.MaInf.Coefficients.at(i),
                   temp1); // optimize: just diagonal
      varp.Add_in0(temp1);
      Variance.SetColumnFromDiag0(i + sizes.ArMax_d, varp);
    }

    if (mCoefUncertainty) {
      throw std::logic_error("not implemented");
      // TODO:
      /*
      auto Gs = std::vector<Matrix<Tv>*>();
      auto xtprime = Matrix<Tv>(1, k);
      Xfs.GetRow(0, &xtprime);
      auto xtprimeones = Matrix<Tv>(g, g * k);
      xtprime.KronIden(g, &xtprimeones);
      Gs.push_back(&xtprimeones);
      auto temp = Matrix<Tv>(g, g * k);
      for (Ti s = 1; s < horizon; s++) {
          auto sum = new Matrix<Tv>(g, g * k, 0);
          Xfs.GetRow(s, &xtprime);
          xtprime.KronIden(g, sum);
          for (Ti i = 0; i < std::min(s, armax_d); i++) {
              arcumm._a.at(i + 1).Dot(Gs.at(s - i - 1), &temp);
              sum.Subtract_in(&temp);
          }
          Gs.push_back(sum);
      }
      auto temp0 = Matrix<Tv>(g, g);
      for (Ti s = 0; s < horizon; s++) {
          Gs.at(s).Dot(estimate.coefvar, &temp);
          temp.DotTr(Gs.at(s), &temp0);
          auto j = s + armax_d;
          for (Ti i = 0; i < g; i++)
              v_storage.Set(j, i, v_storage.Get(j, i) + temp0.Get(i, i));
      }
      */
    }
  }
}
