/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the GPL 3.0.
 See accompanying file LICENSE
*/

#include "pca.h"

#include <functional>
#include <iomanip>
#include <iostream>
#include <numeric>
#include <random>
#include <sstream>
#include <vector>

using namespace ldt;

PcaAnalysis::PcaAnalysis(Ti rows, Ti cols, Ti numForecast, bool doPCs,
                         bool checkZeroVar, bool center, bool scale) {
  if (scale == false)
    checkZeroVar = false;
  auto mn = std::min(rows, cols);
  auto svd = MatrixSvd<Tv>(rows, cols, 'N', 'S');
  WorkSize = svd.WorkSize;

  // initialize matrices
  StorageSize = cols * cols + mn * 2;
  if (center || scale) {
    DataS = MatrixStandardized<Tv>(rows, cols, checkZeroVar, center, scale);
    StorageSize += DataS.StorageSize;
  } else
    WorkSize += rows * cols; // copy data if there is no standardization
  if (numForecast > 0) {
    StorageSize += numForecast * cols;
    if (center || scale) {
      auto fpoData = MatrixStandardized<Tv>(
          numForecast, cols, checkZeroVar, center,
          scale); // we override mean and variances, but signal scale because of
                  // zero variance case
      WorkSize = std::max(WorkSize, fpoData.StorageSize);
    }
  }
  if (doPCs) {
    mDoPcs = true;
    StorageSize += rows * cols;
  }
}

void PcaAnalysis::Calculate(const Matrix<Tv> &mat, Tv *work, Tv *storage,
                            const Matrix<Tv> *Xforecast) {

  Ti rows = mat.RowsCount;
  Ti cols = mat.ColsCount;
  Ti numForecast = 0;
  if (Xforecast) {
    numForecast = Xforecast->RowsCount;
    if (Xforecast->ColsCount != cols)
      throw LdtException(ErrorType::kLogic, "pca",
                         "invalid 'Xforecast'. Different number of columns");
  }

  bool removeZeroVar = false;
  bool center = false;
  bool scale = false;
  if (DataS.StorageSize > 0) {
    removeZeroVar = DataS.mRemoveZeroVar;
    center = DataS.mCenter;
    scale = DataS.mScale;
  }
  auto temp = PcaAnalysis(rows, cols, numForecast, mDoPcs, removeZeroVar,
                          center, scale);
  if (temp.WorkSize > WorkSize || temp.StorageSize > StorageSize)
    throw LdtException(ErrorType::kLogic, "pca",
                       "Inconsistent size in 'PcaAnalysis'");

  Ti q = 0;
  Ti p = 0;
  Matrix<Tv> useMat = Matrix<Tv>(rows, cols);
  ;
  if (DataS.StorageSize > 0) {
    DataS.Calculate(mat, &storage[q]);
    q += DataS.StorageSize;
    useMat.SetData(DataS.Result.Data, DataS.Result.RowsCount,
                   DataS.Result.ColsCount);
  } else {
    useMat.Data = work;
    p += rows * cols;
    mat.CopyTo00(useMat);
  }
  Ti mn = std::min(rows, useMat.ColsCount);
  cols =
      useMat.ColsCount; // update number of columns, in case columns are removed

  auto svd = MatrixSvd<Tv>(rows, cols, 'N', 'S');
  auto svd_storage =
      &storage[q]; // I will reuse storage in SVD (don't increase q)
  Stds.SetData(&storage[q], mn, 1);
  q += mn;
  Directions.SetData(&storage[q], cols, cols);
  q += cols * cols;
  svd.Calculate(useMat, svd_storage, &work[p]);

  Stds2Ratios.SetData(&storage[q], mn, 1);
  q += mn;
  Tv r = 1.0 / std::sqrt(rows - 1);
  for (Ti i = 0; i < mn; i++)
    Stds.Data[i] *= r;

  // cumulative
  std::function<Tv(Tv)> f = [](Tv x) -> Tv { return x * x; };
  Stds.Apply(f, Stds2Ratios);
  auto varsum = Stds2Ratios.Sum();
  std::function<Tv(Tv)> g = [&varsum](Tv x) -> Tv { return x / varsum; };
  Stds2Ratios.Apply_in(g);

  // Calculate PCs
  if (mDoPcs) {
    PCs.SetData(&storage[q], rows, cols);
    q += rows * cols;
    useMat.DotTr0(Directions, PCs);
  }
  if (numForecast > 0) {
    Forecasts.SetData(&storage[q], numForecast, cols);
    q += numForecast;
    if (center || scale) {
      auto fpoData =
          MatrixStandardized<Tv>(numForecast,
                                 mat.ColsCount // don't use modified cols
                                 ,
                                 removeZeroVar, center, scale);
      fpoData.Calculate(*Xforecast, work,
                        DataS.ColumnMeans.Data ? &DataS.ColumnMeans : nullptr,
                        DataS.ColumnVars.Data ? &DataS.ColumnVars : nullptr);
      fpoData.Result.DotTr0(Directions, Forecasts);
    } else {
      Xforecast->DotTr0(Directions, Forecasts);
    }
  }
}

Ti PcaAnalysis::GetCutoffColumn(Tv CutoffRate) const {
  if (CutoffRate <= 0 || CutoffRate >= 1)
    throw LdtException(ErrorType::kLogic, "pca", "invalid cutoff rate");
  Ti col_cut;
  Tv cumsum = 0;
  for (col_cut = 1; col_cut <= Stds2Ratios.length(); col_cut++) {
    cumsum += Stds2Ratios.Data[col_cut - 1];
    if (cumsum > CutoffRate)
      break;
  }
  return col_cut;
}

void PcaAnalysisOptions::CalculateForModel(PcaAnalysis &model, Matrix<Tv> &data,
                                           Tv *work, Tv *storage,
                                           Matrix<Tv> *xForecast,
                                           bool throwIfConstant) const {

  throwIfConstant = true; // You need to test what happens if you ignore it

  // Ti numForecast = xForecast ? xForecast->RowsCount : 0;
  // Ti numObs = data.RowsCount;
  Ti numExo = data.ColsCount;
  if (xForecast && xForecast->ColsCount != numExo)
    throw LdtException(
        ErrorType::kLogic, "pca",
        "inconsistent number of variables in X and forecast in PCA for a "
        "model");

  if (IgnoreFirstCount >= numExo)
    throw LdtException(ErrorType::kLogic, "pca",
                       "invalid 'IgnoreFirstCount' in PCA options. It is "
                       ">= number of exogenous variables");
  auto start = IgnoreFirstCount;
  auto pcaMat = Matrix<Tv>(&data.Data[start * data.RowsCount], data.RowsCount,
                           numExo - IgnoreFirstCount);
  auto pcaFor = Matrix<Tv>();
  if (xForecast)
    pcaFor.SetData(&xForecast->Data[start * xForecast->RowsCount],
                   xForecast->RowsCount, numExo - IgnoreFirstCount);
  model.Calculate(pcaMat, work, storage, xForecast ? &pcaFor : nullptr);

  if (throwIfConstant && model.DataS.RemovedZeroVar.size() > 0)
    throw LdtException(ErrorType::kLogic, "pca",
                       "constant variable is found in PCA analysis");

  // copy required number of columns to the useMat
  auto cutoff_col = this->GetFinalCount(model);
  model.PCs.Restructure0(model.PCs.RowsCount, cutoff_col);
  pcaMat.CopyFrom00(model.PCs); // don't check size
  data.Restructure0(
      data.RowsCount,
      cutoff_col + IgnoreFirstCount -
          (Ti)model.DataS.RemovedZeroVar
              .size()); // these constant columns are removed from the data

  // do the same for forecast
  if (xForecast) {
    model.Forecasts.Restructure0(model.Forecasts.RowsCount, cutoff_col);
    pcaFor.CopyFrom00(model.Forecasts);
    xForecast->Restructure0(xForecast->RowsCount,
                            cutoff_col + IgnoreFirstCount);
  }
}

void PcaAnalysisOptions::CheckValidity() {
  if (IsEnabled() == false)
    return;

  if (IgnoreFirstCount < 0)
    throw LdtException(ErrorType::kLogic, "pca",
                       "invalid number of variables to ignore in PCA options");

  if (ExactCount > 0) {
    // everthing is OK
  } else {
    if (CutoffRate != 0) {
      if (CutoffRate <= 0 || CutoffRate >= 1)
        throw LdtException(ErrorType::kLogic, "pca",
                           "cutoff rate is not in [0,1]");
      if (CutoffCountMax == 0)
        throw LdtException(
            ErrorType::kLogic, "pca",
            "components are selected by the give cutoff rate, but it is "
            "restricted to 0 (param-name='CutoffCountMax')");
    }
  }
}

Ti PcaAnalysisOptions::GetFinalCount(const PcaAnalysis &model) const {
  {
    if (ExactCount != 0)
      return ExactCount;
    Ti c = model.GetCutoffColumn(CutoffRate);
    return std::min(c, (Ti)CutoffCountMax);
  }
}
