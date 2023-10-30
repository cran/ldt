/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the GPL 3.0.
 See accompanying file LICENSE
*/

#include "varma.h"

using namespace ldt;

// #pragma region Model sizes

void ExpandPoly(Ti p, Ti P, Ti s, std::vector<Ti> &lags) {
  if (p == 0 && P == 0)
    return;
  for (Ti i = 1; i <= p; i++)
    lags.push_back(i);
  if (s > 0)
    for (Ti i = s; i <= s * P; i += s)
      lags.push_back(i);
}

Ti ExpandPolyDiff_ws(Ti d, Ti D, Ti s) {
  if (d == 0 && D == 0)
    return 0;

  Ti ws = 0;
  auto pw1 = PolynomialPower<Ti>(d, 1);
  auto pw2 = PolynomialPower<Ti>(D, s);
  if (d != 0)
    ws += pw1.WorkSize + pw1.StorageSize;

  if (D != 0)
    ws += s + 1 + pw2.WorkSize + pw2.StorageSize;

  if (d != 0 && D != 0) {
    auto mul = PolynomialMultiply<Ti>(pw1.StorageSize - 1,
                                      pw2.StorageSize -
                                          1); // storage size is the degree + 1
    ws += mul.StorageSize;
  }
  return ws;
}

void ExpandPolyDiff(Ti d, Ti D, Ti s, std::vector<Ti> &poly, Ti *work) {
  auto pw1 = PolynomialPower<Ti>(d, 1);
  auto pw2 = PolynomialPower<Ti>(D, s);

  Polynomial<Ti> *p1str = nullptr;
  Polynomial<Ti> *p2str = nullptr;

  Ti p = 0;
  if (d != 0) {
    Ti J[] = {1, -1};
    auto mat = Matrix<Ti>(J, 2, 1);
    auto p1 = Polynomial<Ti>();
    p1.Data(mat, false);

    pw1.Calculate(p1, d, work, &work[pw1.StorageSize]);
    p += pw1.StorageSize + pw1.WorkSize;
    p1str = &pw1.Result;
  }

  if (D != 0) {
    auto J = &work[p];
    p += s + 1;
    auto mat = Matrix<Ti>(0, J, s + 1, 1);
    mat.Data[0] = 1;
    mat.Data[s] = -1;
    auto p2 = Polynomial<Ti>();
    p2.Data(mat, false);

    pw2.Calculate(p2, D, &work[p], &work[p + pw2.StorageSize]);
    p += pw2.StorageSize + pw2.WorkSize;
    p2str = &pw2.Result;
  }

  if (d == 0 && D != 0) {
    for (Ti i = 0; i < p2str->Coefficients.length(); i++)
      poly.push_back(p2str->Coefficients.Data[i]);
  } else if (d != 0 && D == 0) {
    for (Ti i = 0; i < p1str->Coefficients.length(); i++)
      poly.push_back(p1str->Coefficients.Data[i]);
  } else {
    auto mul = PolynomialMultiply<Ti>(pw1.StorageSize - 1, pw2.StorageSize - 1);
    mul.Calculate(*p1str, *p2str, &work[p]);

    for (Ti i = 0; i < mul.Result.Coefficients.length(); i++)
      poly.push_back(mul.Result.Coefficients.Data[i]);
  }
}

VarmaSizes::VarmaSizes(Ti obsCount, Ti eqsCount, Ti exoCount, Ti arP, Ti arD,
                       Ti arQ, Ti maP, Ti maD, Ti maQ, Ti seasonsCount,
                       bool calculate) {
  if (seasonsCount < 2)
    seasonsCount = 0;

  if (arP < 0 || arD < 0 || maD < 0 || maP < 0 || arQ < 0 || maQ < 0 ||
      seasonsCount < 0)
    throw LdtException(ErrorType::kLogic, "varma-sizes",
                   "negative parameters: (p,d,q)x(P,D,Q)_m");
  if (seasonsCount == 0) {
    if (maP != 0 || maQ != 0 || maD != 0)
      throw LdtException(ErrorType::kLogic, "varma-sizes",
                     "invalid seasonal parameters");
  }
  if (arP == 0 && arQ == 0 && maP == 0 && maQ == 0)
    throw LdtException(ErrorType::kLogic, "varma-sizes", "all parameters zero");

  ObsCount = obsCount;
  EqsCount = eqsCount;
  ExoCount = exoCount;
  ArP = arP;
  ArD = arD;
  ArQ = arQ;
  MaP = maP;
  MaD = maD;
  MaQ = maQ;
  SeasonsCount = seasonsCount;

  WorkSizeI = ExpandPolyDiff_ws(ArD, MaD, SeasonsCount);

  ArLags = std::vector<Ti>();
  MaLags = std::vector<Ti>();
  DiffPoly = std::vector<Ti>();

  if (calculate)
    Calculate();
}

void VarmaSizes::Calculate() {
  auto W = std::make_unique<Ti[]>(WorkSizeI);
  Calculate(W.get());
}

void VarmaSizes::Calculate(Ti *workI) {
  ArLags.clear();
  MaLags.clear();
  DiffPoly.clear();

  // expnad
  ExpandPoly(ArP, MaP, SeasonsCount, ArLags);
  ExpandPoly(ArQ, MaQ, SeasonsCount, MaLags);

  if (ArD == 0 && MaD == 0)
    DiffPoly.push_back(1);
  else
    ExpandPolyDiff(ArD, MaD, SeasonsCount, DiffPoly, workI);

  // set values

  ArLength = (Ti)ArLags.size();
  MaLength = (Ti)MaLags.size();
  ArMax = ArLength == 0 ? 0 : ArLags.at(ArLength - 1);
  MaMax = MaLength == 0 ? 0 : MaLags.at(MaLength - 1);
  DiffDegree = DiffPoly.size() == 0 ? 0 : (Ti)(DiffPoly.size() - 1);
  ArMax_d = ArMax + DiffDegree;

  UpdateChanged();
}

void VarmaSizes::UpdateChanged() {
  HasArExo = ArLength != 0 || ExoCount != 0;
  HasAr = ArLength != 0;
  HasMa = MaLength != 0;
  HasDiff = DiffPoly.size() > 1;
  MaStart = ArLength * EqsCount + ExoCount;
  NumParamsEq = MaStart + MaLength * EqsCount;
  NumParams = NumParamsEq * EqsCount;
  T = ObsCount - ArMax_d;
}

// #pragma endregion

// #pragma region Storage

VarmaStorage::VarmaStorage(bool keepDetails, bool isRestricted,
                           const VarmaSizes &sizes, bool calculateVarCoefs,
                           LimitedMemoryBfgsbOptions *optimOptions) {
  mKeepDetails = keepDetails;

  auto q = sizes.NumParams; // it is less than this value in a restricted model
  auto g = sizes.EqsCount;
  auto f = sizes.NumParamsEq;
  auto T = sizes.T;

  gamma = Matrix<Tv>(q, 1);
  resid = Matrix<Tv>(g, T);
  y = Matrix<Tv>(g, T);
  Xt = Matrix<Tv>(T, f); // Use its transpose because I will expand its rows
  sigma2 = Matrix<Tv>(g, g);
  gammavar = Matrix<Tv>(q, q);
  coef = Matrix<Tv>(g, f);

  StorageSize = gamma.length() + resid.length() + y.length() + Xt.length() +
                sigma2.length() + gammavar.length() + coef.length();

  if (keepDetails) {
    coefstd = Matrix<Tv>(g, f);
    coeft = Matrix<Tv>(g, f);
    coefprob = Matrix<Tv>(g, f);

    StorageSize += 3 * coef.length();
  }

  // calculate work size
  WorkSize = 0;
  if (sizes.HasArExo) {
    WorkSize = f * f + f * g * f * g + g * T + f * g * T * g + q;
    WorkSize += T * f;
    if (isRestricted) {
      WorkSize += q * g * f + q * T * g;
      WorkSize += f * g; // if has 'r'
    }
  }
  if (sizes.HasDiff)
    WorkSize += g * (sizes.ObsCount - (Ti)sizes.DiffPoly.size() + 1);

  // maximum likelihood
  Ti mlW = 0;
  if (sizes.HasMa) {
    auto derv = Derivative(q, true,
                           calculateVarCoefs); // TODO: add 'count' as an option
    LimitedMemoryBfgsbOptions opop;
    if (optimOptions)
      opop = *optimOptions;
    Optim = LimitedMemoryBFGSB(q, opop.mMaxCorrections);
    Optim.Options.IterationMax = opop.IterationMax;
    Optim.Options.Factor = opop.Factor;
    Optim.Options.IterationPrint = opop.IterationPrint;
    Optim.Options.ProjectedGradientTol = opop.ProjectedGradientTol;
    mlW = g * f + 2 * g + f + 3 * q + derv.WorkSize;
    mlW += Optim.WorkSize;
  }
  WorkSize = std::max(mlW, WorkSize);
}

void VarmaStorage::SetStorage(Tv *storage, const VarmaSizes &sizes,
                              Ti sampleEnd) {
  auto q = sizes.NumParams; // it is less than this value in a restricted model
  auto g = sizes.EqsCount;
  auto f = sizes.NumParamsEq;
  auto T = sizes.T - sampleEnd;

  Ti pos = 0;

  gamma.SetData(&storage[pos], q, 1);
  pos += gamma.length();
  resid.SetData(&storage[pos], g, T);
  pos += resid.length();
  y.SetData(&storage[pos], g, T);
  pos += y.length();
  Xt.SetData(0, &storage[pos], T, f);
  pos += Xt.length();
  sigma2.SetData(&storage[pos], g, g);
  pos += sigma2.length();
  gammavar.SetData(&storage[pos], q, q);
  pos += gammavar.length();

  if (mKeepDetails) {
    coef.SetData(&storage[pos], g, f);
    pos += coef.length();
    coefstd.SetData(&storage[pos], g, f);
    pos += coefstd.length();
    coeft.SetData(&storage[pos], g, f);
    pos += coeft.length();
    coefprob.SetData(&storage[pos], g, f);
    pos += coefprob.length();
  }
}

void VarmaStorage::GetExpNames(const VarmaSizes &sizes,
                               std::vector<std::string> &endoNames,
                               std::vector<std::string> &exoNames,
                               std::vector<std::string> &result) {
  for (auto ar : sizes.ArLags)
    for (auto &e : endoNames) {
      result.push_back(e + std::string("(-") + std::to_string(ar) +
                       std::string(")"));
    }
  for (auto x : exoNames)
    result.push_back(x);

  for (auto ma : sizes.MaLags)
    for (auto &e : endoNames)
      result.push_back(e + std::string(": e(-") + std::to_string(ma) +
                       std::string(")"));
}

// #pragma endregion
