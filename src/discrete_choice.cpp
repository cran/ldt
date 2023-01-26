/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the GPL 3.0.
 See accompanying file LICENSE
*/

#include "discrete_choice.h"

using namespace ldt;

template <DiscreteChoiceModelType modelType, DiscreteChoiceDistType distType>
DiscreteChoice<modelType, distType>::DiscreteChoice(Ti numObs, Ti numExo,
                                                    Ti numChoices,
                                                    bool doDetails) {
  if (numChoices < 1)
    throw std::logic_error("number of choices must be larger than 1");
  else if (numChoices == 2 && modelType == DiscreteChoiceModelType::kOrdered)
    throw std::logic_error("use binary model for 2 choices case");
  else if (numChoices > 2 && modelType == DiscreteChoiceModelType::kBinary)
    throw std::logic_error(
        "Don't use binary model when number of choices is larger than 2");

  this->mDoDetails = doDetails;
  Ti k = numExo + numChoices -
         2; // There are NumCutoff-1 cutoffs, starting from k0
  // y=0 -> y*<0          -> cutoff is restricted to be 0
  // y=1 -> 0<=y*<mu[0]   -> Beta[k0]
  // ...
  // y=NumCutoff -> y*>=my[NumCutoff-2]   -> Beta[k0+NumCutoff-2]
  // note that these are alphas (see Greene, p. 143)

  this->StorageSize = k + k * k + numChoices;
  if (doDetails)
    this->StorageSize += 3 * k;

  this->Optim = Newton(k);
  auto ols = Ols(numObs, 1, numExo, false, false);
  Ti priorWork = 2 * numObs + numChoices + numObs * numExo +
                 ols.WorkSize; // storage is beta
  this->WorkSize = std::max(priorWork, numObs + 2 * k + k * k +
                                           this->Optim.WorkSize + numChoices);
}

template <DiscreteChoiceModelType modelType, DiscreteChoiceDistType distType>
void DiscreteChoice<modelType, distType>::Calculate(
    const Matrix<Tv> &y, const Matrix<Tv> &x, const Matrix<Tv> *w, Tv *storage,
    Tv *work, Ti numChoices, bool olsInitial) {

  // check

  Ti numExo = x.ColsCount;
  Ti numObs = y.RowsCount;

  // auto n = y.length();

  if (x.RowsCount != numObs)
    throw std::logic_error("length of y is different from rows of x");
  if (w && w->RowsCount != numObs)
    throw std::logic_error("length of y is different from rows of x");

  if constexpr (modelType == DiscreteChoiceModelType::kBinary) {
    numChoices = 2;
    this->NumCutoff = 1;
  } else if constexpr (true) {
    if (numChoices <= 0) {
      this->NumCutoff = static_cast<Ti>(y.Maximum());
      numChoices = this->NumCutoff + 1;
    } else
      this->NumCutoff = numChoices - 1;
    if (this->NumCutoff <= 0)
      throw std::logic_error("Invalid dependent data");
  }
  this->NumChoices = numChoices;

  auto temp = DiscreteChoice<modelType, distType>(numObs, numExo, numChoices,
                                                  this->mDoDetails);
  if (temp.WorkSize > this->WorkSize || temp.StorageSize > this->StorageSize)
    throw std::logic_error("inconsistent arguments in discrete choice.");

  // set storage
  Ti k = numExo + this->NumCutoff - 1;
  Ti p = 0;
  this->Beta.SetData(&storage[p], k, 1);
  p += k;
  this->BetaVar.SetData(&storage[p], k, k);
  p += k * k;

  // resid =  Matrix<Tv>(n, 1);
  this->Counts.SetData(0, &storage[p], numChoices, 1);
  p += numChoices;
  if (this->mDoDetails) {
    this->BetaStd.SetData(&storage[p], k, 1);
    p += k;
    this->BetaZ.SetData(&storage[p], k, 1);
    p += k;
    this->BetaProb.SetData(&storage[p], k, 1);
    p += k;
  }

  if constexpr (modelType == DiscreteChoiceModelType::kBinary) {
    EstimateBinary(y, x, w, work, olsInitial);
  } else if constexpr (true) {
    EstimateOrdered(y, x, w, work, olsInitial);
  }
}

// #pragma region helpers

int get_constIndex(const Matrix<Tv> &x) {
  bool all1;
  for (int j = 0; j < x.ColsCount; j++) {
    if (x.Get0(0, j) == (Tv)1) {
      all1 = true;
      for (Ti i = 1; i < x.RowsCount; i++) {
        if (x.Get0(i, j) != (Tv)1) {
          all1 = false;
          break;
        }
      }
      if (all1)
        return j;
    }
  }
  return -1;
}

template <DiscreteChoiceModelType modelType, DiscreteChoiceDistType distType>
void setestimdetails(DiscreteChoice<modelType, distType> &model) {
  model.BetaVar.GetDiag(model.BetaStd);
  model.BetaStd.Apply_in([](Tv d) -> Tv { return std::sqrt(d); });

  // z, prob
  model.Beta.Apply0(
      model.BetaStd, [](Tv c, Tv s) -> Tv { return c / s; }, model.BetaZ);
  auto zdist = Distribution<DistributionType::kNormal>((Tv)0, (Tv)1);
  model.BetaZ.Apply0(
      [&](Tv t) -> Tv { return (Tv)2 * ((Tv)1 - zdist.GetCdf(std::abs(t))); },
      model.BetaProb);
  // z? see [section 21.4.3]
}

template <DiscreteChoiceModelType modelType, DiscreteChoiceDistType distType>
void calculate_goodness(DiscreteChoice<modelType, distType> &model) {

  // NOTE: If there is weight vector, while LogL can be the same as the
  // non-weight observations (e.g. by ungrouping), but Aic and Sic will differ

  auto k = model.Beta.length();
  model.Aic = (2 * k - 2 * model.LogL) / model.NumObs;
  model.Sic = (std::log(model.NumObs) * k - 2 * model.LogL) / model.NumObs;
}

static void ordermu(Ti k0, const Matrix<Tv> *Beta, Matrix<Tv> *mu, Ti NumCutoff,
                    bool restrictoffsets) {
  if (restrictoffsets) {
    // see Greene, kOrdered ..., p. 143
    mu->Data[0] = std::exp(Beta->Data[k0]);
    for (Ti i = 1; i < NumCutoff - 1; i++)
      mu->Data[i] = mu->Data[i - 1] + std::exp(Beta->Data[k0 + i]);
  } else {
    for (Ti i = 0; i < NumCutoff - 1; i++)
      mu->Data[i] = Beta->Data[k0 + i];
  }
}

// #pragma endregion

// #pragma region Binary

/// @brief Updates Beta matrix by estimating OLS
/// @tparam modelType
/// @tparam distType
/// @param y
/// @param x
/// @param w
/// @param work
template <DiscreteChoiceModelType modelType, DiscreteChoiceDistType distType>
void DiscreteChoice<modelType, distType>::EstimatePriorBinary(
    const Matrix<Tv> &y, const Matrix<Tv> &x, const Matrix<Tv> *w, Tv *work) {

  Ti n = y.length();
  Ti k0 = x.ColsCount;
  auto ols = Ols(n, 1, k0, false, false);

  Ti pos = 0;
  auto xb = Matrix<Tv>(&work[pos], n, 1);
  pos += n;
  auto y0 = Matrix<Tv>(&work[pos], n, 1);
  pos += n;
  auto x0 = Matrix<Tv>(&work[pos], n, k0);
  pos += n * k0;
  auto oglsW = &work[pos];

  Ti i, j;
  Tv xib, Pj, wi;
  Tv *d = nullptr;
  Tv *e = nullptr;

  // estimate OLS
  if (w) { // multiply rows by sqrt(weight)
    for (i = 0; i < n; i++) {
      wi = std::sqrt(w->Data[i]);
      y0.Data[i] = y.Data[i] * wi;
      d = &x.Data[i];
      e = &x0.Data[i];
      for (j = 0; j < k0; j++)
        e[n * j] = d[n * j] * wi;
    }

    ols.Calculate(y0, x0, this->Beta.Data, oglsW); // just beta
  } else
    ols.Calculate(y, x, this->Beta.Data, oglsW); // just beta

  x.DotVector0(this->Beta, xb); // don't check bounds
  if (distType == DiscreteChoiceDistType::kLogit) {
    for (i = 0; i < n; i++) {
      // Ti yi = static_cast<Ti>(y.Data[i]);
      xib = xb.Data[i];

      Pj = (Tv)1 / ((Tv)1 + std::exp(xib));
      Pj *= (Tv)1 - Pj;
      Pj = std::sqrt((w ? w->Data[i] : (Tv)1) /
                     Pj); // sqrt is required for weighted OLS
      y0.Data[i] = y.Data[i] * Pj;
      d = &x.Data[i];
      e = &x0.Data[i];
      for (j = 0; j < k0; j++)
        e[n * j] = d[n * j] * Pj;
    }
  } else if (distType == DiscreteChoiceDistType::kProbit) {
    for (i = 0; i < n; i++) {
      // Ti yi = static_cast<Ti>(y.Data[i]);
      xib = xb.Data[i];

      Pj = dist_normal_cdf(-xib, (Tv)0, (Tv)1);
      Pj *= (Tv)1 - Pj;
      Pj = std::sqrt((w ? w->Data[i] : (Tv)1) / Pj);

      y0.Data[i] = y.Data[i] * Pj;
      d = &x.Data[i];
      e = &x0.Data[i];
      for (j = 0; j < k0; j++)
        e[n * j] = d[n * j] * Pj;
    }
  }

  // use wighted data and estimate GLS

  ols.Calculate(y0, x0, this->Beta.Data, oglsW);
}

template <DiscreteChoiceModelType modelType, DiscreteChoiceDistType distType>
void DiscreteChoice<modelType, distType>::EstimateBinary(const Matrix<Tv> &y,
                                                         const Matrix<Tv> &x,
                                                         const Matrix<Tv> *w,
                                                         Tv *work,
                                                         bool olsInitial) {

  Ti n = y.length();
  this->NumObs = n;
  Ti k = x.ColsCount;
  // Ti k0 = k; // no cutoff

  if (w) {
    for (Ti i = 0; i < n; i++) {
      auto j = static_cast<Ti>(y.Data[i]);
      this->Counts.Data[j] += w->Data[i]; // + weight
    }
  } else {
    this->Counts.Data[1] = y.Sum();
    this->Counts.Data[0] = (Tv)y.length() - this->Counts.Data[1];
  }

  if (this->Counts.Data[0] == (Tv)0 || this->Counts.Data[1] == (Tv)0)
    throw std::logic_error("Dependent variable has no variance");

  // Initialize
  if (olsInitial || std::isnan(this->Beta.Data[0]))
    EstimatePriorBinary(y, x, w, work);

  Ti pos = 0;
  auto xb = Matrix<Tv>(&work[pos], n, 1);
  pos += n;
  auto xi = Matrix<Tv>(&work[pos], k, 1);
  pos += k;
  auto vg = Matrix<Tv>(&work[pos], k, 1);
  pos += k;
  auto Hi = Matrix<Tv>(&work[pos], k, k);
  pos += k * k;

  Tv *optim_WORK = &work[pos];
  pos += this->Optim.WorkSize;

  std::function<Tv(const Matrix<Tv> &)> fun;
  std::function<void(const Matrix<Tv> &, Matrix<Tv> &)> gfun;
  std::function<void(const Matrix<Tv> &, Matrix<Tv> &)> hfun;

  if constexpr (distType == DiscreteChoiceDistType::kLogit) {
    // equ. 21-17, p. 671, Greene 5th
    // of course, negative of log-likelihood
    // also, I use    e^a/(1+e^a) = 1/(1+e^-a)
    fun = [&](const Matrix<Tv> &Beta) -> Tv {
      Tv lnL = (Tv)0;
      x.Dot0(Beta, xb);
      for (Ti i = 0; i < n; i++)
        lnL +=
            (w ? w->Data[i] : (Tv)1) *
            (-std::log((Tv)1 + std::exp(xb.Data[i])) + y.Data[i] * xb.Data[i]);

      return -lnL;
    };

    // equ. 21-19, p. 671, Greene 5th
    gfun = [&](const Matrix<Tv> &Beta, Matrix<Tv> &stro) -> void {
      stro.SetValue((Tv)0);
      x.Dot0(Beta, xb, -(Tv)1);
      for (Ti i = 0; i < n; i++) {
        x.GetRow0(i, xi);
        xi.Multiply_in(
            (w ? w->Data[i] : (Tv)1) *
            (y.Data[i] -
             (Tv)1 /
                 ((Tv)1 + std::exp(xb.Data[i])))); // -1 is already multiplied
        stro.Subtract_in(xi);
      }
    };

    // equ. 21-22, p. 672, Greene 5th
    hfun = [&](const Matrix<Tv> &Beta, Matrix<Tv> &stro) -> void {
      Tv fxb;
      stro.SetValue((Tv)0);
      x.Dot0(Beta, xb);
      for (Ti i = 0; i < n; i++) {
        fxb = std::exp(xb.Data[i]);
        fxb = fxb / ((Tv)1 + fxb);

        x.GetRow0(i, xi);
        xi.DotTr0(xi, Hi, w ? w->Data[i] : (Tv)1);
        Hi.Multiply_in(fxb * ((Tv)1 - fxb));
        stro.Add_in(Hi);

        // resid->Data[i] = y.Data[i] - fxb;
      }
    };
  } else if constexpr (distType == DiscreteChoiceDistType::kProbit) { // PROBIT

    // equ. 21-17, p. 671, Greene 5th
    fun = [&](const Matrix<Tv> &Beta) -> Tv {
      Tv lnL = (Tv)0, yi, PHI;
      x.Dot0(Beta, xb);
      for (Ti i = 0; i < n; i++) {
        yi = y.Data[i];
        PHI = dist_normal_cdf(xb.Data[i], (Tv)0, (Tv)1);
        lnL += (w ? w->Data[i] : (Tv)1) *
               (yi * std::log(PHI) + ((Tv)1 - yi) * std::log((Tv)1 - PHI));
      }
      return -lnL;
    };

    // equ. 21-21, p. 672, Greene 5th
    gfun = [&](const Matrix<Tv> &Beta, Matrix<Tv> &stro) -> void {
      Tv qi, qix, yi;
      stro.SetValue((Tv)0);
      x.Dot0(Beta, xb);
      for (Ti i = 0; i < n; i++) {
        yi = y.Data[i];
        qi = (2 * yi - 1);
        qix = qi * xb.Data[i];
        x.GetRow0(i, xi);
        xi.Multiply_in((w ? w->Data[i] : (Tv)1) *
                       (qi * dist_normal_pdf(qix, (Tv)0, (Tv)1) /
                        dist_normal_cdf(qix, (Tv)0, (Tv)1)));
        stro.Subtract_in(xi);
      }
    };

    // equ. 21-23, p. 672, Greene 5th
    hfun = [&](const Matrix<Tv> &Beta, Matrix<Tv> &stro) -> void {
      Tv qi, yi, qix, Li, c;
      stro.SetValue((Tv)0);
      x.Dot0(Beta, xb);
      for (Ti i = 0; i < n; i++) {
        yi = y.Data[i];
        qi = (2 * yi - 1);
        qix = qi * xb.Data[i];
        c = dist_normal_cdf(qix, (Tv)0, (Tv)1);
        Li = qi * dist_normal_pdf(qix, (Tv)0, (Tv)1) / c;

        x.GetRow0(i, xi);
        xi.DotTr0(xi, Hi, w ? w->Data[i] : (Tv)1);
        Hi.Multiply_in(Li * (Li + xb.Data[i]));
        stro.Add_in(Hi);

        // can be more effiecient. You need it just the last time:
        // if (yi == 1)
        //	resid->Data[i] = yi - c;
        // else
        //	resid->Data[i] = yi - ((Tv)1 - c);
      }
    };

  } else if constexpr (true) {
    throw std::logic_error(
        "not implemented (discerete choice distribution type).");
    ;
  }

  this->Optim.Minimize2(fun, gfun, hfun, this->Beta, vg.Data,
                        this->BetaVar.Data, optim_WORK);

  // calculate variances from Hessian
  hfun(this->Beta, this->BetaVar); // Hessian gets destroyed in optimization
  auto ipiv = std::unique_ptr<int[]>(new int[k]);
  this->BetaVar.Inv00(ipiv.get(), Hi.Data);

  this->LogL = -this->Optim.FunctionValue;
  calculate_goodness(*this);
  // constIndex = get_constIndex(x);
  // if (constIndex != -1) {
  //	vg.SetValue((Tv)0);
  //	vg.Data[constIndex] = Beta.Data[constIndex];
  //	logL_b0 = -fun(&vg);
  // }

  if (this->mDoDetails)
    setestimdetails(*this);
}

// #pragma endregion

// #pragma region Ordered

/// @brief Updates Beta matrix by estimating OLS
/// @tparam modelType
/// @tparam distType
/// @param y
/// @param x
/// @param w
/// @param work
template <DiscreteChoiceModelType modelType, DiscreteChoiceDistType distType>
void DiscreteChoice<modelType, distType>::EstimatePriorOrdered(
    const Matrix<Tv> &y, const Matrix<Tv> &x, const Matrix<Tv> *w, Tv *work) {

  Ti n = y.length();
  Tv sum_w;
  if (w)
    sum_w = w->Sum();
  else
    sum_w = static_cast<Tv>(n);
  Ti k0 = x.ColsCount;
  Ti k = k0 + this->NumCutoff - 1;
  auto ols = Ols(n, 1, k0, false, false);

  Ti pos = 0;
  auto xb = Matrix<Tv>(&work[pos], n, 1);
  pos += n;
  auto mu = Matrix<Tv>(&work[pos], this->NumCutoff, 1);
  pos += this->NumCutoff;
  auto y0 = Matrix<Tv>(&work[pos], n, 1);
  pos += n;
  auto x0 = Matrix<Tv>(&work[pos], n, k0);
  pos += n * k0;
  auto oglsW = &work[pos];

  Ti i, j, yi;
  Tv tmp0 = (Tv)0, xib, Fj, Fj_1, Pj, wi;
  Tv *d = nullptr;
  Tv *e = nullptr;

  // calculate percentages
  // convert them to bounds by using quantile
  // for kLogit (inverse)
  ///   f(0-x)=p_0
  ///   f(mu_i - x) = p_i
  ///   mu_i=f^-1(p_i)+x
  ///   inverse: x=ln(p_i/(1-p_i))

  // calculate 'mu's
  tmp0 = (Tv)0;
  if (distType == DiscreteChoiceDistType::kLogit) {
    for (i = 0; i < this->NumCutoff; i++) {
      tmp0 += this->Counts.Data[i] /
              sum_w; // **** assuming that Counts are weight adjusted
      mu.Data[i] = std::log(tmp0 / ((Tv)1 - tmp0));
    }
  } else if (distType == DiscreteChoiceDistType::kProbit) {
    for (i = 0; i < this->NumCutoff; i++) {
      tmp0 += this->Counts.Data[i] / sum_w;
      mu.Data[i] = dist_normal_cdfInv(tmp0, (Tv)0, (Tv)1);
    }
  } else
    throw std::logic_error(
        "not implemented (discerete choice distribution type).");

  // estimate OLS
  if (w) { // multiply rows by sqrt(weight)
    for (i = 0; i < n; i++) {
      wi = std::sqrt(w->Data[i]);
      y0.Data[i] = y.Data[i] * wi;
      d = &x.Data[i];
      e = &x0.Data[i];
      for (j = 0; j < k0; j++)
        e[n * j] = d[n * j] * wi;
    }

    ols.Calculate(y0, x0, this->Beta.Data, oglsW); // just beta
  } else
    ols.Calculate(y, x, this->Beta.Data, oglsW); // just beta

  tmp0 = -this->Beta.Data[0] / mu.Data[0];
  for (i = 0; i < k0; i++)
    this->Beta.Data[i] /= tmp0;
  tmp0 = -mu.Data[0];
  for (i = k0; i < k; i++)
    this->Beta.Data[i] = mu.Data[i - k0 + 1] + tmp0;

  // Now, I use the estimated cutoffs and try to estimate GLS
  // I use the variance of multinomial distribution

  x.DotVector0(this->Beta, xb); // don't check bounds
  if (distType == DiscreteChoiceDistType::kLogit) {
    for (i = 0; i < n; i++) {
      yi = static_cast<Ti>(y.Data[i]);
      xib = xb.Data[i];
      if (yi == 0) {
        Fj = (Tv)1 / ((Tv)1 + std::exp(xib));
        Fj_1 = (Tv)0;
      } else if (yi == 1) {
        Fj = (Tv)1 / ((Tv)1 + std::exp(-this->Beta.Data[k0] + xib));
        Fj_1 = (Tv)1 / ((Tv)1 + std::exp(xib));
      } else if (yi == this->NumCutoff) {
        Fj = (Tv)1;
        Fj_1 = (Tv)1 / ((Tv)1 + std::exp(-this->Beta.Data[yi + k0 - 2] + xib));
      } else {
        Fj = (Tv)1 / ((Tv)1 + std::exp(-this->Beta.Data[yi + k0 - 1] + xib));
        Fj_1 = (Tv)1 / ((Tv)1 + std::exp(-this->Beta.Data[yi + k0 - 2] + xib));
      }

      Pj = Fj - Fj_1;
      Pj *= (Tv)1 - Pj;
      Pj = std::sqrt((w ? w->Data[i] : (Tv)1) / Pj);

      y0.Data[i] = y.Data[i] * Pj;
      d = &x.Data[i];
      e = &x0.Data[i];
      for (j = 0; j < k0; j++)
        e[n * j] = d[n * j] * Pj;
    }
  } else if (distType == DiscreteChoiceDistType::kProbit) {
    for (i = 0; i < n; i++) {
      yi = static_cast<Ti>(y.Data[i]);
      xib = xb.Data[i];
      if (yi == 0) { // Fj_1 = 0 and cutoff is 0
        Fj = dist_normal_cdf(-xib, (Tv)0, (Tv)1);
        Fj_1 = (Tv)0;
      } else if (yi == 1) { // Mu(-1) is fixed at 0
        Fj = dist_normal_cdf(this->Beta.Data[k0] - xib, (Tv)0, (Tv)1);
        Fj_1 = dist_normal_cdf(-xib, (Tv)0, (Tv)1);
      } else if (yi == this->NumCutoff) { // Fj = 1
        Fj = (Tv)1;
        Fj_1 =
            dist_normal_cdf(this->Beta.Data[yi + k0 - 2] - xib, (Tv)0, (Tv)1);
      } else {
        Fj = dist_normal_cdf(this->Beta.Data[yi + k0 - 1] - xib, (Tv)0, (Tv)1);
        Fj_1 =
            dist_normal_cdf(this->Beta.Data[yi + k0 - 2] - xib, (Tv)0, (Tv)1);
      }

      Pj = Fj - Fj_1;
      Pj *= (Tv)1 - Pj;
      Pj = std::sqrt((w ? w->Data[i] : (Tv)1) / Pj);

      y0.Data[i] = y.Data[i] * Pj;
      d = &x.Data[i];
      e = &x0.Data[i];
      for (j = 0; j < k0; j++)
        e[n * j] = d[n * j] * Pj;
    }
  }

  // use wighted data and estimate GLS
  ols.Calculate(y0, x0, this->Beta.Data, oglsW);

  // update the cutoffs like before
  tmp0 = -this->Beta.Data[0] / mu.Data[0];
  for (i = 0; i < k0; i++)
    this->Beta.Data[i] /= tmp0;
}

template <DiscreteChoiceModelType modelType, DiscreteChoiceDistType distType>
void DiscreteChoice<modelType, distType>::EstimateOrdered(const Matrix<Tv> &y,
                                                          const Matrix<Tv> &x,
                                                          const Matrix<Tv> *w,
                                                          Tv *work,
                                                          bool olsInitial) {

  auto n = y.length();
  this->NumObs = n;
  auto k0 = x.ColsCount;
  Ti k = k0 + this->NumCutoff - 1;

  // count and check variance of y
  if (w)
    for (Ti i = 0; i < n; i++) {
      auto j = static_cast<Ti>(y.Data[i]);
      this->Counts.Data[j] += w->Data[i]; // + weight
    }
  else
    for (Ti i = 0; i < n; i++) {
      auto j = static_cast<Ti>(y.Data[i]);
      this->Counts.Data[j]++;
    }
  for (Ti i = 0; i <= this->NumCutoff; i++)
    if (this->Counts.Data[i] < 1e-16)
      throw std::logic_error(
          "Number of data-points of at least one specific group is zero");

  // Initialize
  if (olsInitial || std::isnan(this->Beta.Data[0]))
    EstimatePriorOrdered(y, x, w, work);

  // work
  Ti pos = 0;
  auto xb = Matrix<Tv>(&work[pos], n, 1);
  pos += n; // you can use resid, instead
  auto xi = Matrix<Tv>(&work[pos], k0, 1);
  pos += k0;
  auto vg = Matrix<Tv>(&work[pos], k, 1);
  pos += k;
  auto Hi = Matrix<Tv>(&work[pos], k, k);
  pos += k * k;
  auto mu = Matrix<Tv>(&work[pos], this->NumCutoff - 1, 1);
  pos += this->NumCutoff - 1;

  Tv *optim_WORK = &work[pos];
  pos += this->Optim.WorkSize;
  Hi.Restructure0(k0, k0); // generally, I need k0xk0, however, I need a kxk
                           // Matrix<Tv> too. So I use restructure

  std::function<Tv(const Matrix<Tv> &)> fun;
  std::function<void(const Matrix<Tv> &, Matrix<Tv> &)> gfun;
  std::function<void(const Matrix<Tv> &, Matrix<Tv> &)> hfun;

  // see, Greene, Modeling Ordered Choices, section 5.9.5

  if constexpr (distType == DiscreteChoiceDistType::kLogit) {

    fun = [&](const Matrix<Tv> &Beta) -> Tv {
      ordermu(k0, &Beta, &mu, this->NumCutoff, false /*not tested*/);
      Tv lnL = (Tv)0, Fj, Fj_1, xib;
      Ti yi;
      x.DotVector0(Beta, xb); // don't check bounds

      for (Ti i = 0; i < n; i++) {
        yi = static_cast<Ti>(y.Data[i]);
        xib = xb.Data[i];
        if (yi == 0) { // Fj_1 = 0 and cutoff is 0
          Fj = (Tv)1 / ((Tv)1 + std::exp(xib));
          Fj_1 = (Tv)0;
        } else if (yi == 1) { // Mu(-1) is fixed at 0
          Fj = (Tv)1 / ((Tv)1 + std::exp(-mu.Data[0] + xib));
          Fj_1 = (Tv)1 / ((Tv)1 + std::exp(xib));
        } else if (yi == this->NumCutoff) { // Fj = 1
          Fj = (Tv)1;
          Fj_1 = (Tv)1 / ((Tv)1 + std::exp(-mu.Data[yi - 2] + xib));
        } else {
          Fj = (Tv)1 / ((Tv)1 + std::exp(-mu.Data[yi - 1] + xib));
          Fj_1 = (Tv)1 / ((Tv)1 + std::exp(-mu.Data[yi - 2] + xib));
        }
        lnL += (w ? w->Data[i] : (Tv)1) * (std::log(Fj - Fj_1));
      }
      return -lnL;
    };

    gfun = [&](const Matrix<Tv> &Beta, Matrix<Tv> &stro) -> void {
      ordermu(k0, &Beta, &mu, this->NumCutoff, false /*not tested*/);
      Tv Fj, Fj_1, Pj, xib, fj, fj_1;
      Ti yi;
      stro.SetValue((Tv)0);
      x.DotVector0(Beta, xb); // don't check bounds

      for (Ti i = 0; i < n; i++) {
        yi = static_cast<Ti>(y.Data[i]);
        xib = xb.Data[i];
        auto v = w ? w->Data[i] : (Tv)1;
        if (yi == 0) { // Fj_1 = 0 and cutoff is 0
          Fj = (Tv)1 / ((Tv)1 + std::exp(xib));
          Fj_1 = (Tv)0;
          fj = Fj * ((Tv)1 - Fj);
          fj_1 = (Tv)0;
          Pj = Fj - Fj_1;
          // it just updates Beta. mu[-1] is fixed at 0, mu[-2] is -inf
        } else if (yi == 1) {
          Fj = (Tv)1 / ((Tv)1 + std::exp(-mu.Data[0] + xib));
          Fj_1 = (Tv)1 / ((Tv)1 + std::exp(xib)); // mu0 is fixed at 0
          fj = Fj * ((Tv)1 - Fj);
          fj_1 = Fj_1 * ((Tv)1 - Fj_1);
          Pj = Fj - Fj_1;
          stro.Data[k0] -= v * (fj / Pj);
          // mu[-1] is fixed at 0
        } else if (yi == this->NumCutoff) { // Fj = 1
          Fj = (Tv)1;
          Fj_1 = (Tv)1 / ((Tv)1 + std::exp(-mu.Data[yi - 2] + xib));
          fj = (Tv)0;
          fj_1 = Fj_1 * ((Tv)1 - Fj_1);
          Pj = Fj - Fj_1;
          stro.Data[k0 + yi - 2] += v * (fj_1 / Pj); // its the last partition
                                                     // mu[NumCutoff-1] is +inf
        } else {
          Fj = (Tv)1 / ((Tv)1 + std::exp(-mu.Data[yi - 1] + xib));
          Fj_1 = (Tv)1 / ((Tv)1 + std::exp(-mu.Data[yi - 2] + xib));
          fj = Fj * ((Tv)1 - Fj);
          fj_1 = Fj_1 * ((Tv)1 - Fj_1);
          Pj = Fj - Fj_1;
          stro.Data[k0 + yi - 1] -= v * (fj / Pj);
          stro.Data[k0 + yi - 2] += v * (fj_1 / Pj);
        }

        x.GetRow0(i, xi);
        xi.Multiply_in(v * ((fj - fj_1) / Pj));
        for (Ti j = 0; j < k0; j++)
          stro.Data[j] += xi.Data[j];
      }
    };

    hfun = [&](const Matrix<Tv> &Beta, Matrix<Tv> &stro) -> void {
      ordermu(k0, &Beta, &mu, this->NumCutoff, false /*Not tested*/);
      Tv Fj, Fj_1, Pj, xib, fj, fj_1, ffj, ffj_1;
      Ti yi;
      Ti r1 = -1, r2 = -1;
      stro.SetValue((Tv)0);
      x.DotVector0(Beta, xb); // don't check bounds

      for (Ti i = 0; i < n; i++) {
        yi = static_cast<Ti>(y.Data[i]);
        xib = xb.Data[i];
        x.GetRow0(i, xi);
        xi.DotTr0(xi, Hi);
        if (yi == 0) { // Fj_1 = 0 and cutoff is 0
          Fj = (Tv)1 / ((Tv)1 + std::exp(xib));
          Fj_1 = (Tv)0;
          fj = Fj * ((Tv)1 - Fj);
          fj_1 = (Tv)0;
          ffj = fj * (1 - (Tv)2 * Fj);
          ffj_1 = (Tv)0;
          Pj = Fj - Fj_1;
          r1 = -1;
          r2 = -1;
          // it just updates Beta. mu[-1] is fixed at 0, mu[-2] is -inf
        } else if (yi == 1) {
          Fj = (Tv)1 / ((Tv)1 + std::exp(-mu.Data[0] + xib));
          Fj_1 = (Tv)1 / ((Tv)1 + std::exp(xib)); // mu0 is fixed at 0
          fj = Fj * ((Tv)1 - Fj);
          fj_1 = Fj_1 * ((Tv)1 - Fj_1);
          ffj = fj * (1 - (Tv)2 * Fj);
          ffj_1 = fj_1 * (1 - (Tv)2 * Fj_1);
          Pj = Fj - Fj_1;
          r1 = k0;
          r2 = -1;
          // mu[-1] is fixed at 0
        } else if (yi == this->NumCutoff) { // Fj = 1
          Fj = (Tv)1;
          Fj_1 = (Tv)1 / ((Tv)1 + std::exp(-mu.Data[yi - 2] + xib));
          fj = (Tv)0;
          fj_1 = Fj_1 * ((Tv)1 - Fj_1);
          ffj = (Tv)0;
          ffj_1 = fj_1 * (1 - (Tv)2 * Fj_1);
          Pj = Fj - Fj_1;
          r2 = k0 + yi - 2;
          r1 = -1; // its the last partition
          // mu[NumCutoff-1] is +inf
        } else {
          Fj = (Tv)1 / ((Tv)1 + std::exp(-mu.Data[yi - 1] + xib));
          Fj_1 = (Tv)1 / ((Tv)1 + std::exp(-mu.Data[yi - 2] + xib));
          fj = Fj * ((Tv)1 - Fj);
          fj_1 = Fj_1 * ((Tv)1 - Fj_1);
          ffj = fj * (1 - (Tv)2 * Fj);
          ffj_1 = fj_1 * (1 - (Tv)2 * Fj_1);
          Pj = Fj - Fj_1;

          r1 = k0 + yi - 1;
          r2 = k0 + yi - 2;
        }
        auto v = (w ? w->Data[i] : (Tv)1);
        if (r1 != -1) {
          xi.Multiply_in(ffj / Pj - (fj - fj_1) * fj / (Pj * Pj));
          for (Ti a = 0; a < k0; a++)
            stro.Set_Plus0(a, r1, v * xi.Data[a]);
          stro.Set_Minus0(r1, r1, v * (ffj / Pj - std::pow(fj / Pj, (Tv)2)));

          x.GetRow0(i, xi); // since it overrides it
        }
        if (r2 != -1) {
          xi.Multiply_in(-ffj_1 / Pj + (fj - fj_1) * fj_1 / (Pj * Pj));
          for (Ti a = 0; a < k0; a++)
            stro.Set_Plus0(a, r2, v * xi.Data[a]);
          stro.Set_Minus0(r2, r2,
                          v * (-ffj_1 / Pj - std::pow(fj_1 / Pj, (Tv)2)));
        }
        if (r1 != -1 && r2 != -1)
          stro.Set_Minus0(r2, r1, v * (fj * fj_1 / (Pj * Pj)));

        Hi.Multiply_in(
            v * ((ffj - ffj_1) / Pj - std::pow((fj - fj_1) / Pj, (Tv)2)));
        for (Ti j = 0; j < k0; j++)
          for (Ti a = 0; a < k0; a++) {
            if (j > a)
              continue;
            stro.Set_Minus0(j, a, Hi.Get0(j, a));
          }
        // keep resid
        // resid->Data[i] = (fj - fj_1) / Pj; // can be more efficient.
        // generally we need it the last time
      }

      // set lower triangle

      for (Ti j = 0; j < k; j++)
        for (Ti a = 0; a < k; a++) {
          if (j > a)
            continue;
          stro.Set0(a, j, stro.Get0(j, a));
        }
    };
  } else if constexpr (distType == DiscreteChoiceDistType::kProbit) { // PROBIT

    // Similar to kLogit. I just change the 'F, f, ff' parts

    fun = [&](const Matrix<Tv> &Beta) -> Tv {
      ordermu(k0, &Beta, &mu, this->NumCutoff, false /*not tested*/);
      Tv lnL = (Tv)0, Fj, Fj_1, xib;
      Ti yi;
      x.DotVector0(Beta, xb); // don't check bounds

      for (Ti i = 0; i < n; i++) {
        yi = static_cast<Ti>(y.Data[i]);
        xib = xb.Data[i];
        if (yi == 0) { // Fj_1 = 0 and cutoff is 0
          Fj = dist_normal_cdf(-xib, (Tv)0, (Tv)1);
          Fj_1 = (Tv)0;
        } else if (yi == 1) { // Mu(-1) is fixed at 0
          Fj = dist_normal_cdf(mu.Data[0] - xib, (Tv)0, (Tv)1);
          Fj_1 = dist_normal_cdf(-xib, (Tv)0, (Tv)1);
        } else if (yi == this->NumCutoff) { // Fj = 1
          Fj = (Tv)1;
          Fj_1 = dist_normal_cdf(mu.Data[yi - 2] - xib, (Tv)0, (Tv)1);
        } else {
          Fj = dist_normal_cdf(mu.Data[yi - 1] - xib, (Tv)0, (Tv)1);
          Fj_1 = dist_normal_cdf(mu.Data[yi - 2] - xib, (Tv)0, (Tv)1);
        }
        lnL += (w ? w->Data[i] : (Tv)1) * (std::log(Fj - Fj_1));
      }
      return -lnL;
    };

    gfun = [&](const Matrix<Tv> &Beta, Matrix<Tv> &stro) -> void {
      ordermu(k0, &Beta, &mu, this->NumCutoff, false /*not tested*/);
      Tv Fj, Fj_1, Pj, xib, fj, fj_1, d1, d2;
      Ti yi;
      stro.SetValue((Tv)0);
      x.DotVector0(Beta, xb); // don't check bounds
      for (Ti i = 0; i < n; i++) {
        yi = static_cast<Ti>(y.Data[i]);
        xib = xb.Data[i];
        auto v =
            w ? w->Data[i] : (Tv)1; // must multiply it whenever stro changes
        if (yi == 0) {              // Fj_1 = 0 and cutoff is 0
          Fj = dist_normal_cdf(-xib, (Tv)0, (Tv)1);
          Fj_1 = (Tv)0;
          fj = dist_normal_pdf(-xib, (Tv)0, (Tv)1);
          fj_1 = (Tv)0;
          Pj = Fj - Fj_1;
          // it just updates Beta. mu[-1] is fixed at 0, mu[-2] is -inf
        } else if (yi == 1) {
          d1 = mu.Data[0] - xib;
          Fj = dist_normal_cdf(d1, (Tv)0, (Tv)1);
          Fj_1 = dist_normal_cdf(-xib, (Tv)0, (Tv)1);
          fj = dist_normal_pdf(d1, (Tv)0, (Tv)1);
          fj_1 = dist_normal_pdf(-xib, (Tv)0, (Tv)1);
          Pj = Fj - Fj_1;
          stro.Data[k0] -= v * (fj / Pj);
          // mu[-1] is fixed at 0
        } else if (yi == this->NumCutoff) { // Fj = 1
          d1 = mu.Data[yi - 2] - xib;
          Fj = (Tv)1;
          Fj_1 = dist_normal_cdf(d1, (Tv)0, (Tv)1);
          fj = (Tv)0;
          fj_1 = dist_normal_pdf(d1, (Tv)0, (Tv)1);
          Pj = Fj - Fj_1;
          stro.Data[k0 + yi - 2] += v * (fj_1 / Pj); // its the last partition
                                                     // mu[NumCutoff-1] is +inf
          //  if Pj is zero, let DerivativeTv be zero  //??
        } else {
          d1 = mu.Data[yi - 1] - xib;
          d2 = mu.Data[yi - 2] - xib;
          Fj = dist_normal_cdf(d1, (Tv)0, (Tv)1);
          Fj_1 = dist_normal_cdf(d2, (Tv)0, (Tv)1);
          fj = dist_normal_pdf(d1, (Tv)0, (Tv)1);
          fj_1 = dist_normal_pdf(d2, (Tv)0, (Tv)1);
          Pj = Fj - Fj_1;
          stro.Data[k0 + yi - 1] -= v * (fj / Pj);
          stro.Data[k0 + yi - 2] += v * (fj_1 / Pj);
        }
        x.GetRow0(i, xi);
        xi.Multiply_in(v * ((fj - fj_1) / Pj));
        for (Ti j = 0; j < k0; j++)
          stro.Data[j] += xi.Data[j];
      }
    };

    hfun = [&](const Matrix<Tv> &Beta, Matrix<Tv> &stro) -> void {
      ordermu(k0, &Beta, &mu, this->NumCutoff, false /*not tested*/);
      Tv Fj, Fj_1, Pj, xib, fj, fj_1, ffj, ffj_1, d1, d2;
      Ti yi;
      Ti r1 = -1, r2 = -1;
      stro.SetValue((Tv)0);
      x.DotVector0(Beta, xb); // don't check bounds

      for (Ti i = 0; i < n; i++) {
        yi = static_cast<Ti>(y.Data[i]);
        xib = xb.Data[i];
        x.GetRow0(i, xi);
        xi.DotTr0(xi, Hi);
        if (yi == 0) { // Fj_1 = 0 and cutoff is 0
          Fj = dist_normal_cdf(-xib, (Tv)0, (Tv)1);
          Fj_1 = (Tv)0;
          fj = dist_normal_pdf(-xib, (Tv)0, (Tv)1);
          fj_1 = (Tv)0;
          ffj = fj * xib;
          ffj_1 = (Tv)0;
          Pj = Fj - Fj_1;
          r1 = -1;
          r2 = -1;
          // it just updates Beta. mu[-1] is fixed at 0, mu[-2] is -inf
        } else if (yi == 1) {
          d1 = mu.Data[0] - xib;
          Fj = dist_normal_cdf(d1, (Tv)0, (Tv)1);
          Fj_1 = dist_normal_cdf(-xib, (Tv)0, (Tv)1);
          fj = dist_normal_pdf(d1, (Tv)0, (Tv)1);
          fj_1 = dist_normal_pdf(-xib, (Tv)0, (Tv)1);
          ffj = -fj * d1;
          ffj_1 = fj_1 * xib;
          Pj = Fj - Fj_1;
          r1 = k0;
          r2 = -1;
          // mu[-1] is fixed at 0
        } else if (yi == this->NumCutoff) { // Fj = 1
          d1 = mu.Data[yi - 2] - xib;
          Fj = (Tv)1;
          Fj_1 = dist_normal_cdf(d1, (Tv)0, (Tv)1);
          fj = (Tv)0;
          fj_1 = dist_normal_pdf(d1, (Tv)0, (Tv)1);
          ffj = (Tv)0;
          ffj_1 = -fj_1 * d1;
          Pj = Fj - Fj_1;
          r2 = k0 + yi - 2;
          r1 = -1; // its the last partition
          // mu[NumCutoff-1] is +inf
        } else {
          d1 = mu.Data[yi - 1] - xib;
          d2 = mu.Data[yi - 2] - xib;
          Fj = dist_normal_cdf(d1, (Tv)0, (Tv)1);
          Fj_1 = dist_normal_cdf(d2, (Tv)0, (Tv)1);
          fj = dist_normal_pdf(d1, (Tv)0, (Tv)1);
          fj_1 = dist_normal_pdf(d2, (Tv)0, (Tv)1);
          ffj = -fj * d1;
          ffj_1 = -fj_1 * d2;
          Pj = Fj - Fj_1;

          r1 = k0 + yi - 1;
          r2 = k0 + yi - 2;
        }
        auto v = w ? w->Data[i] : (Tv)1;
        if (r1 != -1) {
          xi.Multiply_in(ffj / Pj - (fj - fj_1) * fj / (Pj * Pj));
          for (Ti a = 0; a < k0; a++)
            stro.Set_Plus0(a, r1, v * xi.Data[a]);
          stro.Set_Minus0(r1, r1, v * (ffj / Pj - std::pow(fj / Pj, (Tv)2)));

          x.GetRow0(i, xi); // since it overrides it
        }
        if (r2 != -1) {
          xi.Multiply_in(-ffj_1 / Pj + (fj - fj_1) * fj_1 / (Pj * Pj));
          for (Ti a = 0; a < k0; a++)
            stro.Set_Plus0(a, r2, v * xi.Data[a]);
          stro.Set_Minus0(r2, r2,
                          v * (-ffj_1 / Pj - std::pow(fj_1 / Pj, (Tv)2)));
        }
        if (r1 != -1 && r2 != -1)
          stro.Set_Minus0(r2, r1, v * (fj * fj_1 / (Pj * Pj)));

        Hi.Multiply_in(
            v * ((ffj - ffj_1) / Pj - std::pow((fj - fj_1) / Pj, (Tv)2)));
        for (Ti j = 0; j < k0; j++)
          for (Ti a = 0; a < k0; a++) {
            if (j > a)
              continue;
            stro.Set_Minus0(j, a, Hi.Get0(j, a));
          }
        // keep resid
        // resid->Data[i] = (fj - fj_1) / Pj; // can be more efficient.
        // generally we need it the last time
      }

      // set lower triangle

      for (Ti j = 0; j < k; j++)
        for (Ti a = 0; a < k; a++) {
          if (j > a)
            continue;
          stro.Set0(a, j, stro.Get0(j, a));
        }
    };

  } else if constexpr (true) {
    throw std::logic_error(
        "not implemented (discerete choice distribution type).");
  }

  this->Optim.Minimize2(fun, gfun, hfun, this->Beta, vg.Data,
                        this->BetaVar.Data, optim_WORK);

  hfun(this->Beta, this->BetaVar); // hessian gets destroyed in optimization
  auto ipiv = std::unique_ptr<int[]>(new int[k]);
  Hi.Restructure0(k, k);
  this->BetaVar.Inv00(ipiv.get(), Hi.Data);

  this->LogL = -this->Optim.FunctionValue;
  calculate_goodness(*this);
  // constIndex = get_constIndex(x);
  // if (constIndex != -1) {
  //	vg.SetValue((Tv)0);
  //	vg.Data[constIndex] = Beta.Data[constIndex];
  //	logL_b0 = -fun(&vg);
  // }

  if (this->mDoDetails)
    setestimdetails(*this);
}

// #pragma endregion

template <DiscreteChoiceModelType modelType, DiscreteChoiceDistType distType>
void DiscreteChoice<modelType, distType>::GetProbabilities(const Matrix<Tv> &x,
                                                           Matrix<Tv> &result,
                                                           Tv *work) {
  auto m = x.RowsCount;
  auto xb = Matrix<Tv>(work, m, 1);
  Ti k0 = x.ColsCount;

  auto JJ = static_cast<Ti>(this->NumCutoff);
  x.Dot0(this->Beta, xb);

  if constexpr (modelType == DiscreteChoiceModelType::kBinary) {
    if constexpr (distType == DiscreteChoiceDistType::kLogit) {
      Tv fxb;
      for (Ti i = 0; i < m; i++) {
        fxb = std::exp(xb.Data[i]);
        if (std::isinf(fxb))
          fxb = 1; // the limit (positive and nevative?)
        else
          fxb = fxb / ((Tv)1 + fxb);

        result.Set(i, 0, fxb);
        result.Set(i, 1, (Tv)1 - fxb);
      }
    } else if constexpr (distType == DiscreteChoiceDistType::kProbit) {
      Tv c;
      for (Ti i = 0; i < m; i++) {
        c = dist_normal_cdf(xb.Data[i], (Tv)0, (Tv)1);
        result.Set(i, 0, c);
        result.Set(i, 1, (Tv)1 - c);
      }
    } else if constexpr (true) {
      throw std::logic_error(
          "not implemented (discerete choice distribution type).");
    }
  } else if constexpr (modelType == DiscreteChoiceModelType::kOrdered) {
    Ti j;
    auto mu = Matrix<Tv>(&work[m], JJ - 1, 1);
    ordermu(k0, &this->Beta, &mu, JJ, false /*not tested*/);

    Tv preF, F, xib, mui;
    if constexpr (distType == DiscreteChoiceDistType::kLogit) {
      for (Ti i = 0; i < m; i++) {
        mui = (Tv)0;
        preF = (Tv)0;
        xib = xb.Data[i];
        for (j = 0; j < this->NumCutoff + 1; j++) {
          F = (Tv)1 / ((Tv)1 + std::exp(-mui + xib));

          // should we check infinity like the binary case ??!!

          result.Set(i, j, F - preF);
          // update
          if (j == this->NumCutoff - 1) {
            result.Set(i, j + 1, (Tv)1 - F);
            break;
          }
          preF = F;
          mui = mu.Data[j];
        }
      }
    } else if constexpr (distType == DiscreteChoiceDistType::kProbit) {
      for (Ti i = 0; i < m; i++) {
        mui = (Tv)0;
        preF = (Tv)0;
        xib = xb.Data[i];
        for (j = 0; j < this->NumCutoff + 1; j++) {
          F = dist_normal_cdf(mui - xib, (Tv)0, (Tv)1);
          result.Set(i, j, F - preF);
          // update
          if (j == this->NumCutoff - 1) {
            result.Set(i, j + 1, (Tv)1 - F);
            break;
          }
          preF = F;
          mui = mu.Data[j];
        }
      }
    } else if constexpr (true) {
      throw std::logic_error(
          "not implemented (discerete choice distribution type).");
    }
  } else if constexpr (true) {
    throw std::logic_error("not implemented (discerete choice model type).");
  }
}

DiscreteChoiceBase *
DiscreteChoiceBase::GetFromType(DiscreteChoiceModelType modelType,
                                DiscreteChoiceDistType distType, Ti numObs,
                                Ti numExo, Ti numChoices, bool doDetails) {
  DiscreteChoiceBase *d = nullptr;

  switch (modelType) {
  case ldt::DiscreteChoiceModelType::kBinary:
    switch (distType) {
    case ldt::DiscreteChoiceDistType::kLogit:
      d = new DiscreteChoice<DiscreteChoiceModelType::kBinary,
                             DiscreteChoiceDistType::kLogit>(
          numObs, numExo, numChoices, doDetails);
      break;
    case ldt::DiscreteChoiceDistType::kProbit:
      d = new DiscreteChoice<DiscreteChoiceModelType::kBinary,
                             DiscreteChoiceDistType::kProbit>(
          numObs, numExo, numChoices, doDetails);
      break;
    default:
      throw std::logic_error(
          "not implemented (distribution type in discrete choice model)");
    }
    break;
  case ldt::DiscreteChoiceModelType::kOrdered:
    switch (distType) {
    case ldt::DiscreteChoiceDistType::kLogit:
      d = new DiscreteChoice<DiscreteChoiceModelType::kOrdered,
                             DiscreteChoiceDistType::kLogit>(
          numObs, numExo, numChoices, doDetails);
      break;
    case ldt::DiscreteChoiceDistType::kProbit:
      d = new DiscreteChoice<DiscreteChoiceModelType::kOrdered,
                             DiscreteChoiceDistType::kProbit>(
          numObs, numExo, numChoices, doDetails);
      break;
    default:
      throw std::logic_error(
          "not implemented (distribution type in discrete choice model)");
    }
    break;
  default:
    throw std::logic_error("not implemented (discrete choice model type)");
  }
  d->mModelType = modelType;
  d->mDistType = distType;
  return d;
}

template class ldt::DiscreteChoice<DiscreteChoiceModelType::kBinary,
                                   DiscreteChoiceDistType::kLogit>;
template class ldt::DiscreteChoice<DiscreteChoiceModelType::kBinary,
                                   DiscreteChoiceDistType::kProbit>;

template class ldt::DiscreteChoice<DiscreteChoiceModelType::kOrdered,
                                   DiscreteChoiceDistType::kLogit>;
template class ldt::DiscreteChoice<DiscreteChoiceModelType::kOrdered,
                                   DiscreteChoiceDistType::kProbit>;
