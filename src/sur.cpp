/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the LGPL 3.0.
 See accompanying file LICENSE
*/

#include "sur.h"

using namespace ldt;

Sur::Sur(Ti N, Ti m, Ti k, bool is_restricted, bool do_details,
         Ti max_sig_search_iter) {

  if (max_sig_search_iter != 0) {
    is_restricted = true; // You need to initialize a restricted model for
                          // significance search.
    do_details = true;    // you need p-values
  }

  mIsRestricted = is_restricted;
  mDoDetails = do_details;
  mSigSearchMaxIter = max_sig_search_iter;

  // calculate sizes
  Ti km = k * m;
  // if restricted, let qStar be its maximum value: km

  StorageSize = 2 * km + km * km + 2 * N * m + m * m;
  if (mIsRestricted)
    WorkSize = k * k + 2 * km * km + 3 * N * m * km + N * m + km;
  else
    WorkSize = std::max(k * k + km, m * m);

  if (mDoDetails) {
    StorageSize += 3 * km;
    WorkSize = std::max(WorkSize, 2 * km * km);
  }
}

static Tv R2(const Matrix<Tv> &y, const Matrix<Tv> &resid) {
  Ti N = y.RowsCount;
  Ti m = y.ColsCount;

  auto vars = Matrix<Tv>(new double[m], m);
  y.ColumnsVariances(vars, false, false);
  Tv sum2_y = 0;
  for (Ti i = 0; i < m; i++)
    sum2_y += vars.Data[i] * N;
  delete[] vars.Data;

  // central (?) it will be different if there is no intercept
  Tv sum2_r = 0;
  for (Ti i = 0; i < resid.length(); i++)
    sum2_r += resid.Data[i] * resid.Data[i];

  return (Tv)1 - (sum2_r / sum2_y);
}

static Tv F(Tv r2, Ti N, Ti m, Ti qstar, Tv &pvalue) {
  Tv d1 = (Tv)(m * (N - 1));
  Tv d2 = (Tv)(m * N - qstar);
  Tv f = (r2 / d1) / (((Tv)1 - r2) / d2);
  if (f < 0)
    pvalue = NAN;
  else
    pvalue = (Tv)1 - Distribution<DistributionType::kF>(d1, d2).GetCdf(f);
  return f;
}

static double logL(Matrix<Tv> &resid_var_copy, Ti N, Ti m) {
  // see Greene p. 348
  auto W = Matrix<Tv>(new double[m * m], m, m);
  resid_var_copy.CopyTo00(W);
  Tv resid_var_det = W.Det_pd0();
  resid_var_copy.CopyTo00(W);
  W.Multiply_in((Tv)N);
  // resid.Dot_AtA(W);

  if (std::isnan(resid_var_det))
    throw std::logic_error("Determinant of residual variance is NAN");

  auto SW = Matrix<Tv>(new double[m * m], m, m);
  resid_var_copy.Inv0();
  resid_var_copy.Dot(W, SW);

  auto ll = (Tv)-0.5 * N * (m * c_ln_2Pi + std::log(resid_var_det)) -
            (Tv)0.5 * SW.Trace();

  delete[] W.Data;
  delete[] SW.Data;
  return ll;
  /*
  Tv _n_2 = -(N / (Tv)2);
  Tv part1 = _n_2 * m * c_ln_2Pi_plus_one;
  resid_var_det = resid_var_det / (Tv)std::pow(N, m);
  return part1 + std::log(resid_var_det) * _n_2;
  */
}

static Tv aic(Tv logL, Ti N, Ti k) { return (2 * k - 2 * logL) / (Tv)N; }

static Tv sic(Tv logL, Ti N, Ti k) {
  return (std::log(N) * k - 2 * logL) / (Tv)N;
}

static Tv hqic(Tv logL, Ti N, Ti k) {
  return (std::log(std::log(N)) * 2 * k - 2 * logL) / (Tv)N;
}

// static Tv aic_weight(Tv criterion) { return std::exp(((Tv)-0.5) * criterion);
// }

void Sur::calculate_details(Ti N, Ti m, Tv *work, bool just_probs,
                            bool force_unrestricted) {
  auto x = *pX;
  auto y = *pY;

  Ti k0 = x.ColsCount;
  Ti k0m = k0 * m;
  Ti q = 0;
  if (force_unrestricted == false && mIsRestricted) {
    auto qStar = gamma.length();
    auto Rgamma_var_diag = Matrix<Tv>(&work[q], k0m, qStar);
    q += k0m * qStar; // for considering restrictions
    auto Rgamma_var_diagRt = Matrix<Tv>(&work[q], k0m, k0m);
    q += k0m * k0m;

    pR->Dot(gamma_var, Rgamma_var_diag);
    Rgamma_var_diag.DotTr(*pR, Rgamma_var_diagRt);
    Rgamma_var_diagRt.GetDiag(e_beta_std);
  } else
    gamma_var.GetDiag0(e_beta_std);

  e_beta_std.Apply_in([](Tv x) -> double { return std::sqrt(x); });

  beta.Apply(
      e_beta_std, [](Tv x, Tv y) -> double { return x / y; },
      e_beta_t); // will generate NAN if std=0 (e.g. restricted)

  auto dis = Distribution<DistributionType::kT>((Tv)(N)); // or (N-k0)
  e_beta_t.Apply(
      [&dis](Tv x) -> double {
        return std::isnan(x) ? NAN : 2 * (1.0 - dis.GetCdf(std::abs(x)));
      },
      e_beta_prob);

  if (just_probs)
    return;

  // Goodness of Fit
  r2 = R2(y, resid);
  f = F(r2, N, m, k0 * m, f_prob);
  Aic = aic(logLikelihood, N, m);
  Sic = sic(logLikelihood, N, m);
  Hqic = hqic(logLikelihood, N, m);
}

void Sur::estim_un(Ti N, Ti m, Tv *work,
                   bool do_gamma_var) { // send x since it might be different
                                        // e.g. due to PCA analysis
  auto x = *pX;
  auto y = *pY;

  // Use it if regressors are identical in all equations (there is no
  // restriction) As if equations are estimated separately, except the off
  // diagonals of resid_var or off block-diagonal of beta_var beta =
  // (pX'pX)^{-1}pX'y  [gamma = vec(beta) = (pX'pX o I)^{-1}pX' o vecY]
  // resid_var = e'e/(N)
  // beta_var=resid_var Kron (pX'pX)^{-1}
  Ti k0 = x.ColsCount;
  Ti k0m = k0 * m;

  Ti q = 0;
  auto xtx = Matrix<Tv>(&work[q], k0, k0);
  q += k0 * k0;
  auto xty = Matrix<Tv>(&work[q], k0, m);
  q += k0m;

  x.Dot_AtA(xtx);                   // kxk
  condition_number = xtx.Norm('1'); // condition number
  auto info = xtx.Inv0();
  if (info != 0) {
    throw exp_mat_sin; // Matrix<Tv> singularity
    return;
  }
  condition_number *= xtx.Norm('1');

  x.TrDot(y, xty);
  xtx.Dot(xty, beta);
  beta.CopyTo00(gamma);

  x.Dot(beta, yhat);
  y.Subtract(yhat, resid);
  resid.Dot_AtA(resid_var);
  resid_var.Divide_in((
      Tv)(N)); // N-k0 ? don't bother finding the number of restrictions in each
               // equation. We are talking about asymptotic properties. Anyway,
               // if you change it, deal with k in 'calculate_details' too

  // A note:
  //   here you might want to distinct OLS from WLS or SUR, by setting the
  //   off-diagonals of resid_var to zero this will affect the log-likelihood,
  //   aic, etc. Of course, I don't get the difference between WLS and SUR here
  //   Anyway, we know that SUR estimator is the same as OLS when variables are
  //   identical It means that we can estimate the equations separately and get
  //   the same result However, what can we do with off-diagonal elements of
  //   resid-var and off-block-diagonals of gamma_var ?! they actually does not
  //   exist when we estimate equations separately again this affects the
  //   determinant of resid_var and therefor LogL So, I think it is better not
  //   to call the estimator OLS. This is actually GLS

  if (do_gamma_var)
    resid_var.Kron(xtx, gamma_var);
}

void Sur::estim_r(Ti N, Ti m, Tv *work) {
  auto x = *pX;
  auto y = *pY;

  // I use Luthkepol and VAR literature for the formula
  Ti k0 = x.ColsCount;
  Ti k0m = k0 * m;
  Ti Nm = N * m;
  Ti qStar = pR->ColsCount; // number of estimated parameters

  Ti q = 0;
  auto xtx = Matrix<Tv>(&work[q], k0, k0);
  q += k0 * k0;
  auto V_o_xtx = Matrix<Tv>(&work[q], k0m, k0m);
  q += k0m * k0m;
  auto RV_o_xtx = Matrix<Tv>(&work[q], qStar, k0m);
  q += qStar * k0m;
  // auto RV_o_xtxR = Matrix<Tv>(&work[q], qStar, qStar); q += qStar * qStar; I
  // will use gamma_var
  gamma_var.Restructure0(qStar, qStar);

  auto V_o_x = Matrix<Tv>(&work[q], Nm, k0m);
  q += Nm * k0m;
  auto V_o_xR = Matrix<Tv>(&work[q], Nm, qStar);
  q += Nm * qStar;

  auto I_o_x = Matrix<Tv>();
  auto I_o_xr = Matrix<Tv>();
  auto z = Matrix<Tv>();
  if (pr) {
    I_o_x.SetData(&work[q], Nm, k0m);
    q += Nm * k0m;
    I_o_xr.SetData(&work[q], Nm, 1);
    q += Nm;
    z.SetData(&work[q], Nm, 1);
    q += Nm;
  }
  auto V_o_xRtz = Matrix<Tv>(&work[q], qStar, (Ti)1);
  q += qStar;

  resid_var.Inv0();
  x.Dot_AtA(xtx); // kxk
  resid_var.Kron(xtx, V_o_xtx);
  pR->TrDot(V_o_xtx, RV_o_xtx);
  RV_o_xtx.Dot(*pR, gamma_var);

  condition_number = RV_o_xtx.Norm('1');
  auto info =
      gamma_var.Inv0(); //                      [R'(V^{-1} o pX'pX)R]^{-1}
  if (info != 0) {
    throw exp_mat_sin; // Matrix<Tv> singularity
    return;
  }
  condition_number *= gamma_var.Norm('1');

  // A note:
  //   like the note in the previous model, this is not OLS variance Matrix
  //   OLS is:  [R'(V^{-1} o pX'pX)R]^{-1} R'(V^{-1} o pX'pX)R [R'(V^{-1} o
  //   pX'pX)R]^{-1}  (see Luthkepol) this is FGLS estimator
  //     of course the estimator of resid_var can be different. It can be
  //     Identity, a diagonal Matrix, ...

  resid_var.Kron(x, V_o_x);
  V_o_x.Dot(*pR, V_o_xR);
  if (pr) {
    throw std::logic_error("not implemented (with r restriction)");
    // this is wrong. Kronecker is with I
    x.IdenKron(m, I_o_x);
    I_o_x.Dot(*pr, I_o_xr);
    y.Subtract0(I_o_xr, z);     // subtract0 for vec(y)
    V_o_xR.TrDot0(z, V_o_xRtz); // dot0 for vec(y)
  } else {
    V_o_xR.TrDot0(y, V_o_xRtz); // dot0 for vec(y)
  }

  gamma.Restructure0(qStar, (Ti)1);
  gamma_var.Dot(V_o_xRtz, gamma);

  // convert gamma to beta
  pR->Dot0(gamma, beta);
  if (pr) {
    beta.Add_in0(*pr);
  }

  // same as before:
  x.Dot(beta, yhat);
  y.Subtract(yhat, resid);
  resid.Dot_AtA(resid_var);
  resid_var.Divide_in((
      Tv)(N)); // N-k0 ? don't bother finding the number of restrictions in each
               // equation. We are talking about asymptotic properties. Anyway,
               // if you change it, deal with k in 'calculate_details' too
}

void Sur::estim_search(Ti N, Ti m, Tv *work, Tv sigSearchMaxProb) {
  auto x = *pX;
  auto y = *pY;

  // initialize by estimating the unrestricted model
  estim_un(N, m, work, true);
  calculate_details(N, m, work, true, true); // calculate probs

  Tv p;
  Ti k0 = x.ColsCount;
  Ti k0m = k0 * m;
  Ti q = 0, j;
  pR->SetValue(0);
  auto sig_inds = std::vector<Ti>();
  Ti cs = k0m;
  for (mSigSearchIter = 0; mSigSearchIter < mSigSearchMaxIter;
       mSigSearchIter++) {

    // get significant indexes
    sig_inds.clear();
    for (j = 0; j < k0m; j++) {
      p = e_beta_prob.Data[j];
      if (std::isnan(p)) {
      } // already restricted
      else if (p <= sigSearchMaxProb)
        sig_inds.push_back(j);
    }

    if ((Ti)sig_inds.size() == cs)
      return; // all coefficients are significant
    cs = (Ti)sig_inds.size();
    if (sig_inds.size() == 0)
      throw std::logic_error("All coefficients are insignificant");

    pR->Restructure0(k0m, (Ti)sig_inds.size());
    pR->SetValue(0);
    j = 0;
    for (auto i : sig_inds) {
      pR->Set(i, j, 1); // rows with significant coefficient must have 1
      j++;
    }

    // re-estimate with current restrictions
    estim_r(N, m, &work[q]);
    calculate_details(N, m, &work[q], true);
  }
}

void Sur::Calculate(const Matrix<Tv> &y, const Matrix<Tv> &x, Tv *storage,
                    Tv *work, Matrix<Tv> *R, Tv sigSearchMaxProb) {

  Ti N = y.RowsCount;
  Ti m = y.ColsCount;
  Ti k = x.ColsCount;
  Ti km = k * m;

  // check size
  auto temp = Sur(N, m, k, mIsRestricted, mDoDetails, mSigSearchMaxIter);
  if (temp.WorkSize > WorkSize || temp.StorageSize > StorageSize)
    throw std::logic_error("Inconsistent size (SUR estimation).");

  if (mSigSearchMaxIter != 0 &&
      (!R || R->RowsCount != km || R->ColsCount != km))
    throw std::logic_error(
        "R should be a 'km x km' Matrix, when you want a significant search.");
  else if (R && (R->RowsCount != km || R->ColsCount > km))
    throw std::logic_error("Restrictions are not valid.");

  pY = &y;
  pX = &x;
  pR = R;

  if (mSigSearchMaxIter != 0 && sigSearchMaxProb == 0)
    throw std::logic_error(
        "'max_sig_search_prob' must not be zero because "
        "'max_sig_search_iter' is not zero. If you don't want a significance "
        "search, don't set its iteration.");

  Ti p = 0;

  // initialize matrices
  gamma.SetData(&storage[p], km, 1);
  p += km;
  beta.SetData(&storage[p], k, m);
  p += km;
  gamma_var.SetData(&storage[p], km, km);
  p += km * km;
  yhat.SetData(&storage[p], N, m);
  p += N * m;
  resid.SetData(&storage[p], N, m);
  p += N * m;
  resid_var.SetData(&storage[p], m, m);
  p += m * m;
  if (mDoDetails) {
    e_beta_std.SetData(&storage[p], k, m);
    p += km;
    e_beta_t.SetData(&storage[p], k, m);
    p += km;
    e_beta_prob.SetData(&storage[p], k, m);
    p += km;
  }

  if (mIsRestricted) {
    if (mSigSearchMaxIter == 0) {
      // Matrix<Tv>::Diagonal(resid_var); //first, use identity as the
      // covariance Matrix
      estim_un(N, m, work, false); // I think we get a consistent estimator even
                                   // if we do not use the restrictions
      estim_r(N, m,
              work); // update by new variance Matrix. I think iterated version
                     // does not have any statistical justification
    } else
      estim_search(N, m, work, sigSearchMaxProb);
  } else
    estim_un(N, m, work, true);

  // calculate logarithm of likelihood
  auto resid_var_copy = Matrix<Tv>(work, m, m); // for calculating determinant
  resid_var.CopyTo00(resid_var_copy);
  logLikelihood = logL(resid_var_copy, N, m);

  if (mDoDetails)
    calculate_details(
        N, m, work); // if you let degrees of freedom in you calculations, you
                     // should deal with degrees of freedom in this method too
}
