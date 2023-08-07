/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the GPL 3.0.
 See accompanying file LICENSE
*/

#include "lbfgsb.h"
#include "matrix.h"
#include "optimization.h"
#include <algorithm>
#include <iostream>
#include <math.h>
#include <sstream>
#include <string>
#include <vector>

using namespace ldt;

LimitedMemoryBFGSB::LimitedMemoryBFGSB(Ti n, Ti maxCorrections) {

  Options.mMaxCorrections = maxCorrections;
  mN = n;

  GradientVector = Matrix<Tv>(n, 1);

  // calculate work-size

  WorkSize = 2 * maxCorrections * n + 11 * maxCorrections * maxCorrections +
             5 * n + 8 * maxCorrections;
  WorkSize += 2 * n; // for generating bounds, if needed
  WorkSize += 29;
  WorkSizeI = 4 * n + 44;

  StorageSize = n; // for gradient
}

void LimitedMemoryBFGSB::Minimize(
    const std::function<Tv(const Matrix<Tv> &)> &function,
    const std::function<void(const Matrix<Tv> &, Matrix<Tv> &)> &gradient,
    Matrix<Tv> &startPoint, Tv *storage, Tv *work, Matrix<Tv> *lower,
    Matrix<Tv> *upper) {

  Ti n = startPoint.length();
  auto uWorkI = std::unique_ptr<Ti[]>(new Ti[4 * n + 44]);
  auto workI = uWorkI.get();

  Minimize0(function, gradient, startPoint, storage, work, workI, lower, upper);
}

void LimitedMemoryBFGSB::Minimize0(
    const std::function<Tv(const Matrix<Tv> &)> &function,
    const std::function<void(const Matrix<Tv> &, Matrix<Tv> &)> &gradient,
    Matrix<Tv> &startPoint, Tv *storage, Tv *work, Ti *workI, Matrix<Tv> *lower,
    Matrix<Tv> *upper) {
  Ti n = startPoint.length();
  logical lsave[4];

  Minimize00(function, gradient, startPoint, storage, &work[29], workI,
             &workI[3 * n], lsave, &workI[4 * n] /*44*/, work /*29*/, lower,
             upper);
}

void LimitedMemoryBFGSB::Minimize00(
    const std::function<Tv(const Matrix<Tv> &)> &function,
    const std::function<void(const Matrix<Tv> &, Matrix<Tv> &)> &gradient,
    Matrix<Tv> &startPoint, Tv *storage, Tv *WORK, Ti *iWORK, Ti *nbd,
    logical *lsave, Ti *isave, Tv *dsave, Matrix<Tv> *lower,
    Matrix<Tv> *upper) {

  Ti n = startPoint.length();
  if (n > mN)
    throw LdtException(ErrorType::kLogic, "lmbfgsb",
                       "invalid size for 'lmbfgsb'");

  GradientVector.SetData(0, storage);
  GradientVector.Restructure0(n, 1);
  Xstar = &startPoint;

  auto lowerB = Matrix<Tv>(n, 1);
  auto upperB = Matrix<Tv>(n, 1);
  if (lower)
    lowerB.SetData(lower->Data);
  else
    lowerB.SetData(-INFINITY, WORK);
  if (upper)
    upperB.SetData(upper->Data);
  else
    upperB.SetData(INFINITY, &WORK[n]);

  for (int i = 0; i < n; i++) {
    bool lni = std::isinf(lowerB.Data[i]) && lowerB.Data[i] < 0;
    bool upi = std::isinf(upperB.Data[i]) && upperB.Data[i] > 0;
    nbd[i] = lni && upi ? 0 : lni ? 3 : upi ? 1 : 2;
  }

  FunctionValue = 0;
  Ti csave = 0;
  Task = 1;
  Iteration = 0;
  while (true) {
    if (Iteration >= Options.IterationMax)
      break;
    Iteration++;
    Options.IterationPrint = -1; //???????????

    setulb(&n, &Options.mMaxCorrections, startPoint.Data, lowerB.Data,
           upperB.Data, nbd, &FunctionValue, GradientVector.Data,
           &Options.Factor, &Options.ProjectedGradientTol, &WORK[2 * n], iWORK,
           &Task, &Options.IterationPrint, &csave, lsave, isave, dsave);

    if (Task >= 10 && Task <= 15) {
      FunctionValue = function(startPoint);
      gradient(startPoint, GradientVector);
    } else if (Task == 2) {
    } else
      break;
  }
}
