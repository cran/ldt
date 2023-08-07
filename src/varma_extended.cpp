/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the GPL 3.0.
 See accompanying file LICENSE
*/

#include "varma.h"

using namespace ldt;

VarmaExtended::VarmaExtended(
    const VarmaSizes &sizes,
    VarmaRestrictionType restriction, // we cannot get R or r directly, because
                                      // PCA changes the dimensions
    bool checkNan, bool doDetails, bool calcVariance, Ti fHorizon,
    PcaAnalysisOptions *pcaOptionsY, PcaAnalysisOptions *pcaOptionsX,
    LimitedMemoryBfgsbOptions *optimOptions) {
  mDoDetails = doDetails;
  mCheckNan = checkNan;
  mRestriction = restriction;
  mCalcVariance = calcVariance;
  mHasPcaY = pcaOptionsY && pcaOptionsY->IsEnabled();
  mHasPcaX = pcaOptionsX && pcaOptionsX->IsEnabled();
  mHorizon = fHorizon;

  Sizes = VarmaSizes(sizes); // it changes in the PCA

  StorageSize = 0;
  WorkSize = 0;

  Data = DatasetTs<
      false>(Sizes.ObsCount, Sizes.EqsCount + Sizes.ExoCount, checkNan, false /*for this, you should find a way to select column in forecast too*/);
  StorageSize += Data.StorageSize;

  if (Sizes.ExoCount > 0)
    StorageSize +=
        fHorizon * Sizes.ExoCount; // for creating out of sample exogenous data

  if (mHasPcaY) {
    pcaOptionsY->CheckValidity();
    pPcaOptionsY = pcaOptionsY;
    PcaY =
        PcaAnalysis(Sizes.ObsCount, Sizes.EqsCount, 0, true, true, true, true);
    StorageSize += PcaY.StorageSize;
    WorkSize = std::max(WorkSize, PcaY.WorkSize);
    // update number of equations
    Sizes.EqsCount =
        std::min(Sizes.EqsCount,
                 pcaOptionsY->CutoffCountMax +
                     pcaOptionsY->IgnoreFirstCount); // for estimating the model
  }

  if (mHasPcaX) {
    if (Sizes.ExoCount == 0)
      throw LdtException(ErrorType::kLogic, "varma-extended",
                         "PCA for X is given but there is no exogenous "
                         "variable");
    pcaOptionsX->CheckValidity();
    pPcaOptionsX = pcaOptionsX;
    PcaX = PcaAnalysis(Sizes.ObsCount, Sizes.ExoCount, fHorizon, true, true,
                       true, true);
    StorageSize += PcaX.StorageSize;
    WorkSize = std::max(WorkSize, PcaX.WorkSize);
    // update number of exogenous data
    Sizes.ExoCount =
        std::min(Sizes.ExoCount,
                 pcaOptionsX->CutoffCountMax +
                     pcaOptionsX->IgnoreFirstCount); // for estimating the model
  }
  if (mHasPcaY || mHasPcaX)
    Sizes.UpdateChanged(); // updata MAstart, etc.

  if (restriction == VarmaRestrictionType::kGeneral)
    throw LdtException(
        ErrorType::kLogic, "varma-extended",
        "'VarmaRestrictionType::kGeneral' not implemented"); // esp. with
                                                             // PCA
  auto r = VarmaRestriction(Sizes, restriction);
  StorageSize += r.StorageSize;

  Model = Varma(Sizes, r.IsRestricted, doDetails, calcVariance, optimOptions);
  StorageSize += Model.Result.StorageSize;
  WorkSize = std::max(WorkSize, Model.Result.WorkSize);

  if (fHorizon > 0) {
    Forecasts = VarmaForecast(Sizes, fHorizon);
    StorageSize += Forecasts.StorageSize;
    WorkSize = std::max(WorkSize, Forecasts.WorkSize);

    // ForecastError = Matrix<Tv>();
    // StorageSize += Sizes.EqsCount * std::min(mSampleEnd, fHorizon);
  }
}
void VarmaExtended::Calculate(Matrix<Tv> &data, Tv *storage, Tv *work,
                              bool useCurrentEstime, Ti horizon, Ti sampleEnd,
                              double maxCn, double stdMultiplier) {
  if (horizon > mHorizon)
    throw LdtException(
        ErrorType::kLogic, "varma-extended",
        "reserved maximum number of horizon is lower that the given horizon");

  auto temp = VarmaExtended(Sizes, mRestriction, mCheckNan, mDoDetails,
                            mCalcVariance, horizon, pPcaOptionsY, pPcaOptionsX,
                            &Model.Result.Optim.Options);
  if (temp.WorkSize > WorkSize || temp.StorageSize > StorageSize)
    throw LdtException(ErrorType::kLogic, "varma-extended",
                       "inconsistent arguments (in VarmaExtended)");

  Ti p = 0;

  // Prepare
  Data.Data(data);
  p += Data.StorageSize;
  Data.Update(nullptr, storage);
  if (Data.HasMissingData)
    throw LdtException(ErrorType::kLogic, "varma-extended",
                       "missing data is found in VARMA data");
  if (Data.Start > Data.End)
    throw LdtException(ErrorType::kLogic, "varma-extended",
                       "data is not valid");

  auto estimStorage =
      &storage[p]; // let it be here, so that you do not override gamma when
                   // useCurrentEstimation is true
  p += Model.Result.StorageSize;

  // create matrixes but note that we might update them by PCA

  Ti count = Data.Result.RowsCount; // note that sample end will be effective
                                    // in the Varma estimation
  Tv *d = Data.Result.Data;
  Y.SetData(d, count, Sizes.EqsCount); // since variables are in columns and
                                       // matrix is column-major
  if (Sizes.ExoCount > 0)
    X.SetData(&d[Sizes.EqsCount * count], count, Sizes.ExoCount);

  if (mHasPcaY) { // change data for Y
    pPcaOptionsY->CalculateForModel(PcaY, Y, work, &storage[p], nullptr);
    p += PcaY.StorageSize;
  }

  auto useExoForecast = Matrix<Tv>();
  if (Sizes.ExoCount > 0) { // change data for X
    if (horizon > 0) {
      if (data.RowsCount <= Data.End + horizon - sampleEnd)
        throw LdtException(
            ErrorType::kLogic, "varma-extended",
            "not enough exogenous data point exists in the given horizon");
      useExoForecast.SetData(&storage[p], horizon, X.ColsCount);
      p += horizon * X.ColsCount;
      useExoForecast.SetSub(0, 0, data, Data.End + 1 - sampleEnd,
                            Sizes.EqsCount, horizon, X.ColsCount);
    }
    if (mHasPcaX) {
      pPcaOptionsX->CalculateForModel(PcaX, X, work, &storage[p],
                                      horizon > 0 ? &useExoForecast : nullptr);
      p += PcaX.StorageSize;
    }
  }

  // restrictions

  auto res = VarmaRestriction(Sizes, mRestriction);
  res.Calculate(&storage[p]);
  p += res.StorageSize;

  // Transpose
  Y.Transpose();
  X.Transpose();
  if (Sizes.ExoCount > 0)
    useExoForecast.Transpose();

  // Estimate
  Model.EstimateMl(Y, Sizes.ExoCount > 0 ? &X : nullptr, work, estimStorage,
                   res.IsRestricted ? &res.R : nullptr,
                   res.r.Data ? &res.r : nullptr, sampleEnd, useCurrentEstime,
                   stdMultiplier);

  if (maxCn > 0) { // otherwise we do not check it
    if (Model.Result.cn > maxCn)
      throw LdtException(ErrorType::kLogic, "varma-extended",
                         "maximum condition number reached");
  }

  // predict
  if (horizon > 0) {
    Forecasts.Calculate(Model,
                        (Sizes.ExoCount == 0 ? nullptr : &useExoForecast), &Y,
                        &storage[p], work, horizon, true);
  }
}
