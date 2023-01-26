/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the GPL 3.0.
 See accompanying file LICENSE
*/

#include "varma.h"

using namespace ldt;

Varma::Varma(const VarmaSizes &sizes, bool isRestricted, bool doDetails,
             bool calculateVarCoefs, LimitedMemoryBfgsbOptions *optimOptions) {
  Sizes = VarmaSizes(sizes);
  mIsRestricted = isRestricted;
  mDoDetails = doDetails;
  mCalculateVarCoefs = calculateVarCoefs;

  Result = VarmaStorage(doDetails, isRestricted, sizes, calculateVarCoefs,
                        optimOptions);
}

// #pragma region Helpers

void Varma::Difference(std::vector<Ti> &polyDiff, const Matrix<Tv> &y,
                       Matrix<Tv> &storage) {
  Ti g = (Ti)(polyDiff.size() - 1);
  Ti h;
  for (Ti i = 0; i < y.ColsCount; i++) {
    if (i < g)
      continue;
    storage.SetColumn0(i - g, (Tv)0);
    for (Ti j = 0; j < y.RowsCount; j++) {
      h = i;
      for (auto f = 0; f < (Ti)polyDiff.size(); f++)
        storage.Set0(j, i - g,
                     storage.Get0(j, i - g) + polyDiff.at(f) * y.Get0(j, h--));
    }
  }
}

void Varma::UnDiferences(std::vector<Ti> &polyDiff, Matrix<Tv> &storage) {
  // Ti arCount = _lagsAR.size();
  // Ti armax = arCount == 0 ? 0 : _lagsAR.at(arCount - 1);
  Ti h = (Ti)(polyDiff.size() - 1);

  for (Ti j = 0; j < storage.ColsCount; j++) {
    if (j < h)
      continue;

    for (Ti k = 1; k <= h; k++) {
      auto d = polyDiff.at(k);
      for (Ti i = 0; i < storage.RowsCount; i++) {
        storage.Set0(i, j, storage.Get0(i, j) - d * storage.Get0(i, j - k));
      }
    }
  }
}

std::tuple<Matrix<Tv>, Matrix<Tv>>
Varma::Simulate(std::vector<Matrix<Tv> *> *ar, std::vector<Matrix<Tv> *> *ma,
                Matrix<Tv> *intercept, Matrix<Tv> *exocoef, Matrix<Tv> *sigma,
                Ti n, Ti skip, unsigned int seed, Matrix<Tv> *y0, Ti horizon) {
  Ti m = sigma ? sigma->ColsCount
               : (ar->size() > 0 ? ar->at(0)->ColsCount
                                 : (ma->size() > 0 ? ma->at(0)->ColsCount
                                                   : intercept->length()));
  bool hasExo = exocoef;
  Ti k = hasExo ? exocoef->ColsCount : 0;
  Ti p = (Ti)ar->size();
  Ti q = (Ti)ma->size();

  if (exocoef && exocoef->RowsCount != m)
    throw std::invalid_argument("invalid dimension: exocoef");
  if (intercept && intercept->length() != m)
    throw std::invalid_argument("invalid dimension: intercept");
  if (sigma && (sigma->ColsCount != m || sigma->RowsCount != m))
    throw std::invalid_argument("invalid dimension: sigma");
  if (y0 && y0->RowsCount != m)
    throw std::invalid_argument("invalid dimension: y0");

  for (auto a : *ar)
    if (a->ColsCount != m || a->RowsCount != m)
      throw std::invalid_argument("invalid dimension: ar");
  for (auto a : *ma)
    if (a->ColsCount != m || a->RowsCount != m)
      throw std::invalid_argument("invalid dimension: ma");

  std::default_random_engine eng;
  if (seed != 0)
    eng = std::default_random_engine(seed);
  else {
    std::random_device rdev{};
    eng = std::default_random_engine(rdev());
  }
  std::uniform_int_distribution<unsigned int> unifseed(1000, 600000);

  auto count = n + skip;

  // create exogenous data from normal distribution
  Matrix<Tv> exodata;
  if (hasExo) {
    exodata = Matrix<Tv>(new std::vector<Tv>((count + horizon) * k), k,
                         count + horizon); // (count + horizon) * k

    throw std::logic_error("not implemented (exogenous is varma simulation).");
    // Matrix<Tv>::FillRandom_normal(&exodata, unifseed(eng), 0, 1);
  }

  auto e = NormalM(m, {}, sigma, (count + horizon), false, true, 0);

  auto shocks = new Tv[e.StorageSize];
  Tv *We = new Tv[e.WorkSize];
  e.GetSample(shocks, We, unifseed(eng));

  auto y = Matrix<Tv>(0, new Tv[count * m], m, count); // count * m
  if (y0)
    y.SetSub0(0, 0, *y0, 0, 0, y0->RowsCount, y0->ColsCount);

  auto tmp = Matrix<Tv>(new Tv[m], m);  // m
  auto tmp0 = Matrix<Tv>(new Tv[m], m); // m
  auto tmp1 = Matrix<Tv>(new Tv[m], m); // m
  Matrix<Tv> *tmp2 = nullptr;
  if (hasExo)
    tmp2 = new Matrix<Tv>(new Tv[k], k); // k

  for (Ti i = std::max(p, q); i < count; i++) {
    e.pSample->GetColumn0(i, tmp);
    for (Ti j = 1; j <= q; j++) {
      e.pSample->GetColumn0(i - j, tmp1);
      ma->at(j - 1)->Dot0(tmp1, tmp0);
      tmp.Add_in0(tmp0);
    }
    for (Ti j = 1; j <= p; j++) {
      y.GetColumn0(i - j, tmp1);
      ar->at(j - 1)->Dot0(tmp1, tmp0);
      tmp.Add_in0(tmp0);
    }
    if (intercept)
      tmp.Add0(*intercept, tmp);
    if (hasExo) {
      exodata.GetColumn0(i, *tmp2);
      exocoef->Dot0(*tmp2, tmp1);
      tmp.Add0(tmp1, tmp);
    }
    y.SetColumn0(i, tmp);
  }
  auto y1 = Matrix<Tv>(new std::vector<Tv>(n * m), m, n); // m x n
  y.GetSub0(0, skip, m, n, y1);
  Matrix<Tv> x1;
  if (k != 0) {
    x1 = Matrix<Tv>(new std::vector<Tv>((n + horizon) * m), m,
                    n + horizon); // m x (n+horizon)
    exodata.GetSub0(0, skip, m, n + horizon, x1);
  }

  delete[] shocks;
  delete[] We;

  // TODO: add work arrays
  /*delete[] We;
  delete[] tmp.SetData();
  delete[] tmp1.SetData();
  if (tmp2) {
      delete[] tmp2.SetData();
      delete[] exodata.SetData();
  }
  delete[] shocks.SetData();
  delete[] y.SetData();*/

  return std::tuple<Matrix<Tv>, Matrix<Tv>>(y1, x1);
}

// #pragma endregion

// #pragma region OLS

void Varma::EstimateOls(const Matrix<Tv> &data, const Matrix<Tv> *exoData,
                        const Matrix<Tv> *R, const Matrix<Tv> *r, Tv *work,
                        Tv *storage, Ti sampleEnd, bool noestimation,
                        Matrix<Tv> *ma_resid, Tv maxCondNum) {
  Sizes.ExoCount = exoData ? exoData->RowsCount : 0;
  Sizes.EqsCount = data.RowsCount;
  Sizes.ObsCount = data.ColsCount;
  Sizes.UpdateChanged();

  SampleEnd = sampleEnd;

  auto q = Sizes.NumParams; // it is less than this value in a restricted model
  auto g = Sizes.EqsCount;
  auto f = Sizes.NumParamsEq;
  auto T = Sizes.T - sampleEnd;

  if (R)
    q = R->ColsCount;

  // set storage
  Result.SetStorage(storage, Sizes, sampleEnd);

  Ti pos = 0;

  // Prepare Y
  Matrix<Tv> data_d;
  if (Sizes.HasDiff) {
    Ti ds = data.ColsCount - (Ti)Sizes.DiffPoly.size() + 1;
    data_d.SetData(&work[pos], g, ds);
    pos += g * ds;
    Difference(Sizes.DiffPoly, data, data_d);
  } else
    data_d.SetData(data.Data, data.RowsCount, data.ColsCount);
  data_d.GetSub0(0, Sizes.ArMax, g, T, Result.y, 0, 0);

  // estimate AR(p) and populate _resid_
  // vec( YX'(XX')^-1 )   // g x k
  if (Sizes.HasArExo || ma_resid) {
    Result.Xt.Restructure0(T, Sizes.MaStart);
    if (R) {
      Result.gamma.Restructure0(q, 1);
      Result.gammavar.Restructure0(q, q);
    }

    // set lags
    Ti row = 0;
    for (Ti c = 0; c < Sizes.ArLength; c++) {
      auto l = Sizes.ArLags.at(c);
      Result.Xt.SetSub_t0(0, row, data_d, 0, Sizes.ArMax - l, T, g);
      row += g;
    }
    // set exogenous
    if (Sizes.ExoCount != 0)
      Result.Xt.SetSub_t0(0, row, *exoData, 0, Sizes.ArMax_d, T,
                          Sizes.ExoCount);

    // Work arrays
    if (noestimation)
      Result.Xt.Restructure0(T, f);
    else {
      auto sxx = Matrix<Tv>(&work[pos], f, f);
      pos += f * f;
      auto sxxoI = Matrix<Tv>(&work[pos], f * g, f * g);
      pos += f * g * f * g; // XX' Kron I_g
      auto vecY = Matrix<Tv>(&work[pos], g * T);
      pos += g * T;
      auto xtkronI = Matrix<Tv>(&work[pos], T * g, f * g);
      pos += f * g * T * g;
      auto rxkronIy = Matrix<Tv>(&work[pos], q);
      pos += q; // R'(X o I)(y - (X' o I)r)

      auto sxxx = Matrix<Tv>(&work[pos], T, f);
      pos += T * f;

      Matrix<Tv> RsxxoI;
      Matrix<Tv> rxkronI;
      Matrix<Tv> xtkronr;
      if (R) {
        RsxxoI = Matrix<Tv>(&work[pos], q, g * f);
        pos += q * g * f;
        rxkronI = Matrix<Tv>(&work[pos], q, T * g);
        pos += q * T * g;

        if (r) {
          xtkronr = Matrix<Tv>(&work[pos], f * g);
          pos += f * g; //(X' o I)r
        }
      }
      int info;
      if (Sizes.HasMa && ma_resid) { // in this case, there is no need for
                                     // estimation. We just use the given
                                     // residuals for estimation with MA
        if (ma_resid != &Result.resid)
          ma_resid->CopyTo(Result.resid);
      } else {
        // estimate
        sxx.Restructure0(Sizes.MaStart, Sizes.MaStart);
        sxxx.Restructure0(T, Sizes.MaStart);
        Result.gamma.Restructure0(g, Sizes.MaStart);

        Result.Xt.TrDot0(Result.Xt, sxx);
        Result.cn = sxx.Norm('1');
        info = sxx.Inv0();
        if (info != 0) {
          throw exp_mat_sin; // Matrix<Tv> singularity
          return;
        }
        Result.cn *= sxx.Norm('1');
        if (Result.cn > maxCondNum)
          throw std::logic_error("Model check failed: Maximum CN");

        Result.Xt.Dot0(sxx, sxxx);
        Result.y.Dot0(sxxx, Result.gamma);
        Result.gamma.DotTr0(Result.Xt, Result.resid);
        Result.y.Subtract0(Result.resid, Result.resid);

        // variance of gamma
        if (Sizes.HasMa == false && !R) {
          Result.resid.DotTr0(Result.resid, Result.sigma2);
          Result.sigma2.Multiply_in(
              (Tv)1 / (Tv)T); // dof adjustment: (T- Result.gamma.ColsCount)
          sxx.Kron(Result.sigma2, Result.gammavar);
        }

        // re-estimate with ma part and restrictions

        sxx.Restructure0(f, f);
      }
      Result.Xt.Restructure0(T, f);

      if (Sizes.HasMa) { // new X
        row = Sizes.MaStart;
        for (Ti c = 0; c < Sizes.MaLength; c++) {
          auto l = Sizes.MaLags.at(c);
          Result.Xt.SetSub_t0(l, row, Result.resid, 0, 0, T - l, g);
          row += g;
        }
      }

      if (!R) {
        if (Sizes.HasMa == false) { // Unrestricted VAR
        } else {                    // Unrestricted VARMA
          // we should re-estimate with new X
          sxxx.Restructure0(T, f);
          Result.gamma.Restructure0(g, f);

          Result.Xt.TrDot0(Result.Xt, sxx);
          Result.cn = sxx.Norm('1');
          info = sxx.Inv0();
          if (info != 0) {
            throw exp_mat_sin; // Matrix<Tv> singularity
            return;
          }
          Result.cn *= sxx.Norm('1');
          if (Result.cn > maxCondNum)
            throw std::logic_error("Model check failed: Maximum CN");

          Result.Xt.Dot0(sxx, sxxx);
          Result.y.Dot0(sxxx, Result.gamma);
          Result.gamma.DotTr0(Result.Xt, Result.resid);
          Result.y.Subtract0(Result.resid, Result.resid);

          // variance of gamma
          Result.resid.DotTr0(Result.resid, Result.sigma2);
          Result.sigma2.Multiply_in((Tv)1 / (Tv)T);
          sxx.Kron(Result.sigma2, Result.gammavar);
        }
      } else { // Restricted VARMA
        // gamma
        Result.gamma.Restructure0(q, 1);
        Result.gammavar.Restructure0(q, q);

        Result.Xt.TrDot0(Result.Xt, sxx);
        sxx.KronIden(g, sxxoI);
        R->TrDot0(sxxoI, RsxxoI);
        RsxxoI.Dot0(*R, Result.gammavar);
        Result.cn = Result.gammavar.Norm('1');
        info = Result.gammavar.Inv0();

        if (info != 0) {
          throw exp_mat_sin; // Matrix<Tv> singularity
          return;
        }
        Result.cn *= Result.gammavar.Norm('1'); // TODO: is it valid?!
        if (Result.cn > maxCondNum)
          throw std::logic_error("Model check failed: Maximum CN");

        // TODO: variance of gamma is RsxxoIR and it might be wrong.
        // check it

        Result.y.CopyTo00(vecY);
        if (r) {
          Result.Xt.KronIden0(g, xtkronI);
          xtkronI.TrDot0(*r, xtkronr); // (X' o I)r
          vecY.Subtract0(xtkronr, vecY);
        }
        xtkronI.Restructure0(f * g, T * g); // (X o I)
        Result.Xt.TrKronIden0(g, xtkronI);
        R->TrDot0(xtkronI, rxkronI);
        rxkronI.Dot0(vecY, rxkronIy);
        Result.gammavar.Dot0(rxkronIy, Result.gamma);

        // residuals
        rxkronI.TrDot0(Result.gamma,
                       Result.resid);               // don't check dimensions
        vecY.Subtract0(Result.resid, Result.resid); // vec.resid

        // variance of regression
        Result.resid.DotTr0(Result.resid, Result.sigma2);
        Result.sigma2.Multiply_in((Tv)1 / (Tv)T);
      }
    }
  } else // pure MA case with no ma_resid
  {
    Result.Xt.SetValue(0);
    if (R) {
      Result.gamma.Restructure0(q, 1);
      Result.gammavar.Restructure0(q, q);
    }
    if (noestimation == false) {
      Result.gamma.SetValue(0);
      Result.resid.SetValue(0);
      Matrix<Tv>::Diagonal(Result.sigma2, INFINITY, 0); //? Inf or 0
      Matrix<Tv>::Diagonal(Result.gammavar, INFINITY, 0);
    }
  }
}

// #pragma endregion

// #pragma region ML

void MlUpdateResid(const Matrix<Tv> &gamma, Varma &model, const Matrix<Tv> *R,
                   const Matrix<Tv> *r, Matrix<Tv> &Pi, Matrix<Tv> &Xi,
                   Matrix<Tv> &Ri) {
  Ti g = model.Result.y.RowsCount;
  Ti n = model.Result.y.ColsCount;

  if (R && R->length() != 0) {
    R->Dot0(gamma, Pi); // no dim check
    if (r)
      Pi.Add0(*r, Pi);
  } else
    gamma.CopyTo00(Pi);

  Ti MaStart = model.Sizes.MaStart;
  Ti maLength = model.Sizes.MaLength;

  Ti l, row, c, i, j;
  if (g == 1) {
    for (i = 0; i < n; i++) {
      model.Result.Xt.GetRow0(i, Xi);  // k x 1
      Tv rr = Pi.VectorDotVector0(Xi); // 1xk kx1
      rr = model.Result.y.Data[i] - rr;
      model.Result.resid.Data[i] = rr;
      // update Xt with new residual
      row = MaStart;
      for (c = 0; c < maLength; c++) {
        l = model.Sizes.MaLags.at(c);
        if (i + l >= n)
          break;
        model.Result.Xt.Set(i + l, row, rr);
        row++;
      }
    }
  } else {
    for (i = 0; i < n; i++) {
      model.Result.Xt.GetRow0(i, Xi); // k x 1
      Pi.Dot0(Xi, Ri);                // gxk kx1

      // set i-th column in resid
      c = g * i;
      auto re = &model.Result.resid.Data[c];
      auto ye = &model.Result.y.Data[c];
      for (j = 0; j < g; j++)
        re[j] = ye[j] - Ri.Data[j];

      // update Xt with new residual
      row = MaStart;
      for (c = 0; c < maLength; c++) {
        l = model.Sizes.MaLags.at(c);
        if (i + l >= n)
          break;
        model.Result.Xt.SetSubRow0(i + l, row, re, g);
        row += g;
      }
    }
  }
}

Tv MlFunction(const Matrix<Tv> &gamma, Varma &model, const Matrix<Tv> *R,
              const Matrix<Tv> *r, Matrix<Tv> &Pi, Matrix<Tv> &Xi,
              Matrix<Tv> &Ri) {
  MlUpdateResid(gamma, model, R, r, Pi, Xi, Ri);
  Ti n = model.Result.y.ColsCount;
  model.Result.resid.DotTr0(model.Result.resid, model.Result.sigma2);
  model.Result.sigma2.Multiply_in((Tv)1 / (Tv)(n - gamma.ColsCount));
  return std::log(model.Result.sigma2.Det_pd0()) * n; // is it p.d.?
}

/*
void MlFunctionDerivativeNumerical(const Matrix<Tv>& gamma, Matrix<Tv>& storage,
    Varma& model,
    const Matrix<Tv>* R, const Matrix<Tv>* r,
    Matrix<Tv>& Pi, Matrix<Tv>& yi, Matrix<Tv>& Xi, Matrix<Tv>& Ri,
    Tv* dervWork, Derivative& derv) {

    std::function<Tv(const Matrix<Tv>&)> f = [model, R, r, Pi, yi, Xi, Ri](const
Matrix<Tv>& x) -> Tv { return MlFunction(x, model, R, r, Pi, Xi, Ri);
    };
    derv.CalculateFirst(f, gamma, storage.Data, dervWork);

}*/

void SetDetails(Varma &varma, const Matrix<Tv> *R) {
  if (R) {
    R->Dot0(varma.Result.gamma,
            varma.Result.coef); // don't check the dimension
    Tv *dig = new Tv[varma.Result.gammavar.RowsCount];
    auto mdig = Matrix<Tv>(dig, varma.Result.gammavar.RowsCount, 1);
    varma.Result.gammavar.GetDiag(mdig);
    R->Dot0(mdig, varma.Result.coefstd);
    delete[] dig;
  } else {
    varma.Result.gamma.CopyTo00(varma.Result.coef);
    varma.Result.gammavar.GetDiag(varma.Result.coefstd);
  }
  varma.Result.coefstd.Apply_in([](Tv d) -> Tv { return std::sqrt(d); });

  // t, prob
  auto Tg = static_cast<Tv>(
      varma.Result.y.length()); // if adjusted, (varma.Result.y.RowsCount -
                                // f) is degrees of freedom
  auto tdist = Distribution<DistributionType::kT>(Tg);
  varma.Result.coef.Apply0(
      varma.Result.coefstd, [](Tv c, Tv s) -> Tv { return c / s; },
      varma.Result.coeft);
  varma.Result.coeft.Apply0(
      [&](Tv t) -> Tv { return 2 * (1 - tdist.GetCdf(std::abs(t))); },
      varma.Result.coefprob);
}

void CalculateGoodness(Varma &varma, Ti T, Ti g, Ti f, Tv max_vf) {
  auto ll = (-(T * g * c_ln_2Pi_plus_one) - max_vf) / (Tv)2;
  auto aic = ((Tv)2 * g * f - (Tv)2 * ll) / (Tv)T;
  auto sic = (std::log(T) * g * f - (Tv)2 * ll) / (Tv)T;

  varma.Result.LogLikelihood = ll;
  varma.Result.Aic = aic;
  varma.Result.Sic = sic;

  // r2
  // auto ystat = Descriptive(varma.Result.y);
  // auto rstat = Descriptive(varma.Result.resid);
  // varma.Result.R2 = rstat.SumOfSquares(true) / ystat.SumOfSquares(true);
}

std::string Varma::ModelToString(VarmaSizes sizes) {
  return std::to_string(sizes.ArP) + std::string(",") +
         std::to_string(sizes.ArD) + std::string(",") +
         std::to_string(sizes.ArQ) + std::string(",") +
         std::to_string(sizes.MaP) + std::string(",") +
         std::to_string(sizes.MaD) + std::string(",") +
         std::to_string(sizes.MaQ) + std::string(",") +
         std::to_string(sizes.SeasonsCount) + std::string(",");
}

void Varma::EstimateMl(const Matrix<Tv> &data, const Matrix<Tv> *exoData,
                       Tv *work, Tv *storage, const Matrix<Tv> *R,
                       const Matrix<Tv> *r, Ti sampleEnd, bool useCurrentEstime,
                       Tv stdMultipler, Tv maxCondNum /*, bool recurs_init*/) {
  // SampleEnd = sampleEnd; it gets set in OLS
  useCurrentEstime = Sizes.HasMa ? useCurrentEstime : false;

  // estimate OLS
  // if (recurs_init && HasMa)
  //    estim_rec(false, data, exoData, R, r, Result, work, sampleEnd,
  //    useCurrentEstime, false);
  // else
  EstimateOls(data, exoData, R, r, work, storage, sampleEnd, useCurrentEstime,
              nullptr, maxCondNum);

  auto q = Sizes.NumParams;
  auto g = Sizes.EqsCount;
  auto f = Sizes.NumParamsEq;
  auto T = Sizes.T;
  if (R)
    q = R->ColsCount;

  if (Sizes.HasMa == false && !R) {
    if (mDoDetails) {
      SetDetails(*this, R);
      // logarithm Likelihood
      auto copsigma2 = new Tv[Result.sigma2.length()];
      auto mcopsigma2 = Matrix<Tv>(copsigma2, g, g);
      Result.sigma2.CopyTo(mcopsigma2);
      CalculateGoodness(*this, T, g, f,
                        std::log(mcopsigma2.Det_pd0()) * Result.y.ColsCount);
      delete[] copsigma2;
    }
    return;
  }

  // here, it has MA part

  // optimization
  auto derv =
      ldt::Derivative(q, true, mCalculateVarCoefs); // same as for 'count'

  // work
  Ti pos = 0;
  auto Pi = Matrix<Tv>(&work[pos], g, f);
  pos += g * f;
  auto Yi = Matrix<Tv>(&work[pos], g, 1);
  pos += g;
  auto Xi = Matrix<Tv>(&work[pos], f, 1);
  pos += f;
  auto Ri = Matrix<Tv>(&work[pos], g, 1);
  pos += g;
  auto lower =
      Matrix<Tv>(-std::numeric_limits<Tv>::infinity(), &work[pos], q, 1);
  pos += q;
  auto upper =
      Matrix<Tv>(std::numeric_limits<Tv>::infinity(), &work[pos], q, 1);
  pos += q;

  Tv *derv_work = &work[pos];
  pos += derv.WorkSize;
  Tv *gradient = &work[pos];
  pos += q; // a storage for the first derivative. It will be used in the
            // optim too. For second derivative, the storage is 'gammavar'
  Tv *optim_work = &work[pos]; // pos += optim.WorkSize;

  auto model = this;
  std::function<Tv(const Matrix<Tv> &)> fun = [&](const Matrix<Tv> &x) -> Tv {
    return MlFunction(x, *model, R, r, Pi, Xi, Ri);
  };
  std::function<void(const Matrix<Tv> &, Matrix<Tv> &)> gfun;

  bool analyticalDerv = false; // TODO
  if (analyticalDerv) {
    // for analytical derivative (size of WORK_derv is different):
    /*gf = [&](const Matrix<Tv>* x, Matrix<Tv>* storage) . void {
            estim_ml_derv(x, storage, &ols, R, r, Pi, Yi, Xi, Ri, &_lagsMA,
       work);
        };*/
  } else {
    gfun = [&](const Matrix<Tv> &x, Matrix<Tv> &grad) -> void {
      derv.CalculateFirst(fun, x, grad.Data,
                          derv_work); // It fills th 'grad' (the storage)
                                      // which is from the optim function
    };
  }

  // bounds
  for (Ti j = 0; j < q; j++) {
    lower.Data[j] = Result.gamma.Data[j] -
                    stdMultipler * std::sqrt(Result.gammavar.Get0(j, j));
    upper.Data[j] = Result.gamma.Data[j] +
                    stdMultipler * std::sqrt(Result.gammavar.Get0(j, j));
  }
  Result.Optim.Minimize(fun, gfun, Result.gamma, gradient, optim_work, &lower,
                        &upper);

  // variances
  if (mCalculateVarCoefs) {
    derv.CalculateSecond(fun, Result.gamma, Result.gammavar.Data,
                         derv_work); // note that in this case you should
                                     // have asked for second derivative
    Result.gammavar.Inv0();
    Result.gammavar.Multiply_in(2);

    // update values
    fun(Result.gamma);
  }

  // update sigma2, it is destroyed while calculating determinant
  Ti n = Result.y.ColsCount;
  Result.resid.DotTr0(Result.resid, Result.sigma2);
  Result.sigma2.Multiply_in((Tv)1 / (Tv)n);

  // set coefficient matrix (it is required in forecasting)
  if (R)
    R->Dot0(Result.gamma, Result.coef); // don't check the dimension
  else
    Result.gamma.CopyTo00(Result.coef);

  if (mDoDetails) {
    // logarithm Likelihood
    CalculateGoodness(*this, T, g, f, Result.Optim.FunctionValue);
    SetDetails(*this, R);
  }
}

// #pragma endregion
