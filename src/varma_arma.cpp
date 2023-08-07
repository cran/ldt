/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the GPL 3.0.
 See accompanying file LICENSE
*/

#include "varma.h"

using namespace ldt;

VarmaArma::VarmaArma(const VarmaSizes &sizes, Ti maInfCount) {
  pSizes = &sizes;
  mMaInfCount = maInfCount;

  Ti m = sizes.EqsCount;
  Ti mm = m * m;
  WorkSize = 0;
  StorageSize = 0;

  // AR
  StorageSize = (sizes.ArMax + 1) * mm;

  // MA
  StorageSize += (sizes.MaMax + 1) * mm;

  // MA-Inf
  if (maInfCount >
      0) { // maInfCount is the length of polynomial (its degree is count-1)
    StorageSize += maInfCount * mm;

    if (sizes.HasDiff == false && sizes.HasAr == false) {
      // copy MA
    } else {

      Ti arDiffDegree = sizes.ArMax;
      if (sizes.HasDiff) { // multiply Ar and Diff (even if AR=0, convert 1x1 to
                           // matrix)
        WorkSize += (Ti)sizes.DiffPoly.size(); // copy vector
        auto mul1 = PolynomialMMultiply(sizes.EqsCount, sizes.ArMax,
                                        sizes.DiffDegree); // max count ?!
        WorkSize += mul1.StorageSize;
        arDiffDegree = sizes.ArMax + sizes.DiffDegree + 1; // new degree
      }

      auto inv = PolynomialMInvert(sizes.EqsCount, arDiffDegree, maInfCount);

      if (sizes.HasMa) { // multiply to Ma
        WorkSize += inv.WorkSize + inv.StorageSize;
        // auto mul2 = PolynomialMMultiply(sizes.EqsCount, arDiffDegree,
        // sizes.MaMax); WorkSize += mul2.StorageSize;
      } else {
        WorkSize += inv.WorkSize;
      }
    }
  }
}

void VarmaArma::Calculate(const Matrix<Tv> &Pi, Tv *storage, Tv *work) {

  auto sizes = *pSizes;
  if (Pi.ColsCount != sizes.NumParamsEq)
    throw LdtException(ErrorType::kLogic, "varma-arma", "inconsistent size");
  Ti m = sizes.EqsCount;
  // Ti mm = m * m;
  Ti p = 0, q = 0;

  // calculate Ar
  p += Ar.Data(sizes.ArMax, m, &storage[p]);

  Matrix<Tv>::Diagonal(*Ar.Coefficients.at(0));
  if (sizes.ArMax != 0) {
    std::function<Tv(Tv)> f = [](Tv d) -> Tv { return -d; };
    Ti i = 0;
    for (Ti k = 1; k <= sizes.ArMax; k++) {
      if (std::find(sizes.ArLags.begin(), sizes.ArLags.end(), k) !=
          sizes.ArLags.end()) {
        Pi.GetSub0(0, i, m, m, *Ar.Coefficients.at(k), 0, 0);
        Ar.Coefficients.at(k)->Apply_in(f);
        i += m;
      } else
        Ar.Coefficients.at(k)->SetValue(0);
    }
  }

  // Calculate Ma
  p += Ma.Data(sizes.MaMax, m, &storage[p]);

  Matrix<Tv>::Diagonal(*Ma.Coefficients.at(0));
  if (sizes.MaMax != 0) {

    Ti i = sizes.NumParamsEq - m * sizes.MaLength;
    for (Ti k = 1; k <= sizes.MaMax; k++) {
      if (std::find(sizes.MaLags.begin(), sizes.MaLags.end(), k) !=
          sizes.MaLags.end()) {
        Pi.GetSub0(0, i, m, m, *Ma.Coefficients.at(k), 0, 0);
        i += m;
      } else
        Ma.Coefficients.at(k)->SetValue(0);
    }
  }

  // MaInf
  if (mMaInfCount > 0) {
    MaInf.Data(mMaInfCount, m, &storage[p]);

    if (sizes.HasDiff == false && sizes.HasAr == false) { // copy MA

      Ti i = 0;
      for (auto &a : Ma.Coefficients) {
        a->CopyTo00(*MaInf.Coefficients.at(i));
        if (i == mMaInfCount)
          break;
        i++;
      }

      for (i; i <= mMaInfCount; i++)
        MaInf.Coefficients.at(i)->SetValue(0); // MA(i)=0 for i > madegree

    } else {
      // Equation 3.144 (check it) but I think we should multiply AR and Diff

      PolynomialM *arDiff = &Ar;
      Ti arDiffDegree = sizes.ArMax;

      auto mul1 =
          PolynomialMMultiply(sizes.EqsCount, sizes.ArMax, sizes.DiffDegree);
      if (sizes.HasDiff) { // multiply Ar and Diff
        auto diffPoly = Polynomial<Tv>();
        auto diffPolyData = Matrix<Tv>(&work[q], (Ti)sizes.DiffPoly.size(), 1);
        q += (Ti)sizes.DiffPoly.size();
        for (Ti i = 0; i < (Ti)sizes.DiffPoly.size(); i++)
          diffPolyData.Data[i] = (Tv)sizes.DiffPoly.at(i);
        diffPoly.Data(diffPolyData, false);

        mul1.Calculate(Ar, diffPoly, &work[q]);
        q += mul1.StorageSize;

        arDiffDegree = sizes.ArMax + sizes.DiffDegree + 1;
        arDiff = &mul1.Result;
      }

      auto inv = PolynomialMInvert(sizes.EqsCount, arDiffDegree, mMaInfCount);
      auto invS = &work[q];
      if (sizes.HasMa) {
        q += inv.StorageSize;
      } else {
        invS = &storage[p];
      }
      inv.Calculate(*arDiff, invS, &work[q], mMaInfCount);
      q += inv.WorkSize;

      if (sizes.HasMa) { // multiply to Ma

        auto mul2 = PolynomialMMultiply(sizes.EqsCount, inv.Result.GetDegree(),
                                        sizes.MaMax);
        mul2.Calculate(inv.Result, Ma, &storage[p], mMaInfCount);
      }
    }
  }
}
