/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the LGPL 3.0.
 See accompanying file LICENSE
*/

#include "varma.h"

using namespace ldt;

/*


VarmaSimulationDetail::VarmaSimulationDetail() {
        Actuals = new Matrix<Tv>();
        Forecasts = new Matrix<Tv>();
        ForecastsStd = new Matrix<Tv>();
        IsValid = false;
        SampleEnds = -1;
}


VarmaSimulationDetail::~VarmaSimulationDetail() {
        delete Actuals;
        delete Forecasts;
        delete ForecastsStd;
}
*/

// #pragma region Simulation

VarmaSimulation::VarmaSimulation(const VarmaSizes &sizes, Ti count,
                                 const std::vector<Ti> &horizons,
                                 const std::vector<ScoringType> &measures,
                                 LimitedMemoryBfgsbOptions *optimOptions,
                                 bool isExtended,
                                 VarmaRestrictionType restriction,
                                 bool checkNan, PcaAnalysisOptions *pcaOptionsY,
                                 PcaAnalysisOptions *pcaOptionsX) {
  pSizes = &sizes;
  pHorizons = &horizons;
  pMeasures = &measures;
  mCount = count;
  auto count0 = mCount;
  if (count == 0 || count0 >= sizes.T)
    throw std::logic_error(
        std::string(
            "Invalid number of simulations. It is zero or larger than the "
            "number of observations.") +
        std::to_string(count0) + std::string("...") + std::to_string(sizes.T));

  IsExtended = isExtended;
  auto horizonMax = horizons.at(horizons.size() - 1);

  StorageSize = (Ti)(measures.size() * horizons.size() *
                     sizes.EqsCount); // for each measure and horizon and
                                      // variable, there is a value
  Results = std::vector<Matrix<Tv>>(measures.size());
  StorageSize += (Ti)measures.size() * sizes.EqsCount;
  WorkSize = 0;
  // calculate work size
  if (IsExtended) {
    EModel = VarmaExtended(sizes, restriction, checkNan, true, true, horizonMax,
                           pcaOptionsY, pcaOptionsX, optimOptions);
    WorkSize += EModel.WorkSize + EModel.StorageSize;
  } else {
    Model = Varma(sizes, true, true, true,
                  optimOptions); // we save space for details and variance
                                 // of coefficients
    WorkSize +=
        Model.Result.StorageSize; // TODO: it can be more efficient. we
                                  // need a partition of these observations
    Forecast = VarmaForecast(Model.Sizes, horizonMax, true,
                             false); // TODO: determine coefficients variance
    WorkSize += Forecast.StorageSize +
                std::max(Model.Result.WorkSize, Forecast.WorkSize);
  }

  //    others
  WorkSize += 5 * (sizes.EqsCount * (Ti)horizons.size()) + sizes.EqsCount;
}

void VarmaSimulation::Calculate(Tv *storage, Tv *work, Matrix<Tv> &data,
                                bool &cancel, Matrix<Tv> *exo,
                                const Matrix<Tv> *R, const Matrix<Tv> *r,
                                bool usePreviousEstim, Tv maxCondNum,
                                Tv stdMultipler, bool coefUncer,
                                Ti maxInvalidSim) {

  if (cancel)
    return;

  auto horizons = *pHorizons;
  auto measures = *pMeasures;
  auto sizes = *pSizes;
  Ti hh = (Ti)horizons.size();
  Ti hMin = horizons.at(0);
  Ti hMax = horizons.at(hh - 1);
  auto T = data.ColsCount;
  Ti mm = (Ti)measures.size();
  Ti yy = sizes.EqsCount; // number of endogenous data
  bool isRestricted = R && R->length() > 0;

  // check size
  auto tempm = VarmaSimulation(sizes, mCount, horizons, measures,
                               &Model.Result.Optim.Options);
  if (WorkSize < tempm.WorkSize || StorageSize < tempm.StorageSize)
    throw std::logic_error("inconsistent arguments in VARMA simulation");
  tempm = VarmaSimulation();

  auto count = mCount;

  if (T - count <= 0)
    throw std::logic_error(
        "invalid number of simulations. It is larger than available data");
  if (count == 0)
    throw std::logic_error(
        "invalid number of simulations. It is zero of negative");

  if (cancel)
    return;

  // set storage
  Ti pos = 0;
  for (int i = 0; i < mm; i++) {
    Results.at(i).SetData(0, &storage[pos], yy, hh);
    pos += yy * hh; // horizons are in columns
  }
  ResultAggs.SetData(0, &storage[pos], mm, yy);
  pos += mm * yy;

  // set work
  pos = 0;
  double *estimStorage = nullptr;
  double *forecastStorage = nullptr;
  double *estimForecastWork = nullptr;

  estimStorage = &work[pos];
  pos += Model.Result.StorageSize;
  forecastStorage = &work[pos];
  pos += Forecast.StorageSize;
  estimForecastWork = &work[pos];
  pos += std::max(Forecast.WorkSize, Model.Result.WorkSize);

  //      others
  Ti co = yy * hh;
  auto act = Matrix<Tv>(&work[pos], yy, hh);
  pos += co;
  auto forc = Matrix<Tv>(&work[pos], yy, hh);
  pos += co;
  auto err = Matrix<Tv>(&work[pos], yy, hh);
  pos += co;
  auto std = Matrix<Tv>(&work[pos], yy, hh);
  pos += co;
  auto temp = Matrix<Tv>(&work[pos], yy, hh);
  pos += co;
  auto last = Matrix<Tv>(&work[pos], yy, 1);
  pos += yy;

  ValidCounts = 0;
  Ti k, invalidCounts = 0;
  Ti forIndex;
  Ti colc;
  bool useCurrentEstims;
  Ti actIndex;
  Ti effectiveH;
  bool success = false;
  Ti row = 0;
  auto counters = std::vector<Ti>(horizons.size());
  Ti counter = 0;
  for (Ti se = count; se > 0; se--) {
    if (cancel)
      return;
    useCurrentEstims = usePreviousEstim && success;
    invalidCounts++;
    actIndex = T - se - 1;
    if (actIndex < 0)
      break;

    effectiveH = std::min(se, hMax);
    if (effectiveH < hMin)
      break;

    colc = 0;
    for (auto &h : horizons) {
      if (h <= effectiveH)
        colc++;
    }
    if (colc <= 0)
      break; // no more forecasts

    // estimate and forecast
    // update sizes by current sampleEnd. se is different which means Xt,
    // resid, etc are different
    auto forecast0 = &Forecast;
    try {
      Model.EstimateMl(data, exo, estimForecastWork, estimStorage,
                       isRestricted ? R : nullptr, isRestricted ? r : nullptr,
                       se, useCurrentEstims, stdMultipler, maxCondNum);
      if (cancel)
        return;
      Forecast.Calculate(Model, exo, &data, forecastStorage, estimForecastWork,
                         effectiveH);
      if (cancel)
        return;

    } catch (...) {
      row++;
      continue; // will be invalid
    }
    invalidCounts--;
    if (invalidCounts > maxInvalidSim)
      throw std::logic_error("Model check failed: Minimum Valid Simulations");

    auto forecast = *forecast0;

    forIndex = forecast.StartIndex - 1;
    data.GetColumn0(actIndex, last);

    k = 0;
    for (auto &h : horizons) {
      if (h <= effectiveH) {
        if (cancel)
          return;
        counter++;
        counters[k]++;
        act.SetColumnFromColumn0(k, data, actIndex + h);

        forc.SetColumnFromColumn0(k, forecast.Forecast, forIndex + h);
        if (forecast.Variance.Data)
          std.SetColumnFromColumn0(k, forecast.Variance, forIndex + h);
      } else {
        act.SetColumn0(k, NAN);
        forc.SetColumn0(k, NAN);
        std.SetColumn0(k, NAN);
      }
      k++;
    }
    if (forecast.Variance.Data)
      std.Apply_in([](Tv x) -> Tv { return std::sqrt(x); });
    act.Subtract0(forc, err);
    if (cancel)
      return;

    /*if (keepDetails) {
        details->sampleEnd = se;
        details->isValid = true;
        act.CopyTo00(details->actual);
        forc.CopyTo00(details->forecast);
        std.CopyTo00(details->stdforecast);
    }*/
    success = true;
    ValidCounts++;

    Ti c = 0;
    for (auto &eval : measures) {
      if (cancel)
        return;
      Scoring::GetScore(eval, temp, act, forc, err, std, last);

      // summation for calculating mean
      auto me = Results.at(c);
      k = 0;
      for (auto &h : horizons) {
        if (h > effectiveH)
          continue;
        for (Ti i = 0; i < temp.RowsCount; i++) {
          me.Set0(i, k, me.Get0(i, k) + temp.Get0(i, k));
          ResultAggs.Set0(c, i, ResultAggs.Get0(c, i) + temp.Get0(i, k));
        }
        k++;
      }

      // if (keepDetails) {
      //     temp.CopyTo(details->measures.at(c));
      // }
      c++;
    }

    row++;
  }

  if (counter == 0 || invalidCounts > maxInvalidSim)
    throw std::logic_error("Model check failed: Minimum Valid Simulations");

  if (cancel)
    return;

  // average:
  for (Ti j = 0; j < hh; j++) {
    auto o = (Tv)1 / (Tv)counters[j];
    for (Ti i = 0; i < mm; i++) {
      auto m = &Results.at(i);
      if (measures.at(i) == ScoringType::kRmse ||
          measures.at(i) == ScoringType::kScaledRmse) {
        for (k = 0; k < m->RowsCount; k++)
          m->Set0(k, j, std::sqrt(m->Get0(k, j) * o));
      } else {
        for (k = 0; k < m->RowsCount; k++)
          m->Set0(k, j, m->Get0(k, j) * o);
      }
    }
  }

  if (cancel)
    return;

  // average (aggregate ove horizons)
  for (Ti i = 0; i < mm; i++) {
    if (measures.at(i) == ScoringType::kRmse ||
        measures.at(i) == ScoringType::kScaledRmse) {
      for (k = 0; k < yy; k++)
        ResultAggs.Set0(i, k, std::sqrt(ResultAggs.Get0(i, k) / (Tv)counter));
    } else {
      for (k = 0; k < yy; k++)
        ResultAggs.Set0(i, k, ResultAggs.Get0(i, k) / (Tv)counter);
    }
  }
}

void VarmaSimulation::CalculateE(Tv *storage, Tv *work, Matrix<Tv> &data,
                                 Tv maxCondNum, Tv stdMultipler, bool coefUncer,
                                 bool usePreviousEstim) {
  auto horizons = *pHorizons;
  auto measures = *pMeasures;
  auto sizes = *pSizes;
  Ti hh = (Ti)horizons.size();
  Ti hMin = horizons.at(0);
  Ti hMax = horizons.at(hh - 1);

  // we should get t:
  auto D = DatasetTs<false>(data.RowsCount, data.ColsCount, true, false);
  D.Data(data);
  D.Update(nullptr, work);
  if (D.HasMissingData)
    throw std::logic_error("Missing data is found in VARMA data.");
  if (D.Start > D.End)
    throw std::logic_error("Data is not valid.");

  auto T = D.End - D.Start + 1;
  Ti mm = (Ti)measures.size();
  Ti yy = sizes.EqsCount; // number of endogenous data

  // check size
  auto tempm = VarmaSimulation(sizes, mCount, horizons, measures,
                               &EModel.Model.Result.Optim.Options, true,
                               EModel.mRestriction, EModel.mCheckNan,
                               EModel.pPcaOptionsY, EModel.pPcaOptionsX);
  if (WorkSize < tempm.WorkSize || StorageSize < tempm.StorageSize)
    throw std::logic_error("inconsistent arguments in VARMA simulation");
  tempm = VarmaSimulation();

  auto count = mCount;

  if (T - count <= 0)
    throw std::logic_error(
        "invalid number of simulations. It is larger than available data");
  if (count == 0)
    throw std::logic_error(
        "invalid number of simulations. It is zero of negative");

  // set storage
  Ti pos = 0;
  for (int i = 0; i < mm; i++) {
    Results.at(i).SetData(0, &storage[pos], yy, hh);
    pos += yy * hh; // horizons are in columns
  }
  ResultAggs.SetData(0, &storage[pos], mm, yy);
  pos += mm * yy;

  // set work
  pos = 0;
  double *estimStorage = nullptr;
  double *estimForecastWork = nullptr;

  estimStorage = &work[pos];
  pos += EModel.StorageSize;
  estimForecastWork = &work[pos];
  pos += EModel.WorkSize;

  //      others
  Ti co = yy * hh;
  auto act = Matrix<Tv>(&work[pos], yy, hh);
  pos += co;
  auto forc = Matrix<Tv>(&work[pos], yy, hh);
  pos += co;
  auto err = Matrix<Tv>(&work[pos], yy, hh);
  pos += co;
  auto std = Matrix<Tv>(&work[pos], yy, hh);
  pos += co;
  auto temp = Matrix<Tv>(&work[pos], yy, hh);
  pos += co;
  auto last = Matrix<Tv>(&work[pos], yy, 1);
  pos += yy;
  auto lastIndex = D.End;

  bool useCurrentEstims;
  Ti k;
  Ti forIndex;
  Ti colc;
  Ti actIndex;
  Ti effectiveH;
  bool success = false;
  Ti row = 0;
  auto counters = std::vector<Ti>(horizons.size());
  Ti counter = 0;
  for (Ti se = count; se > 0; se--) {
    useCurrentEstims = usePreviousEstim && success;
    actIndex = lastIndex - se;
    if (actIndex < 0)
      break;

    effectiveH = std::min(se, hMax);
    if (effectiveH < hMin)
      break;

    colc = 0;
    for (auto &h : horizons) {
      if (h <= effectiveH)
        colc++;
    }
    if (colc <= 0)
      break; // no more forecasts

    // estimate and forecast
    // update sizes by current sampleEnd. se is different which means Xt,
    // resid, etc are different
    auto forecast0 = &Forecast;
    try {
      EModel.Calculate(data, estimStorage, estimForecastWork, useCurrentEstims,
                       effectiveH, se, maxCondNum, stdMultipler);
      forecast0 = &EModel.Forecasts;
    } catch (std::exception &ex) {
      AddError(ex.what());
      row++;
      continue;
    } catch (std::string &ex) {
      AddError(ex.c_str());
      row++;
      continue;
    } catch (const char *ex) {
      AddError(ex);
      row++;
      continue;
    } catch (...) {
      AddError("unknown error!");
      row++;
      continue;
    }
    auto forecast = *forecast0;

    forIndex = forecast.StartIndex - 1;
    for (Ti b = 0; b < sizes.EqsCount; b++) // copy just the endogenous part
      last.Data[b] = data.Get0(actIndex, b);

    k = 0;
    for (auto &h : horizons) // set columns
    {
      if (h <= effectiveH) {
        counter++;
        counters[k]++;
        for (Ti b = 0; b < sizes.EqsCount; b++) // copy just the endogenous part
          act.Set0(b, k, data.Get0(actIndex + h, b));

        forc.SetColumnFromColumn0(k, forecast.Forecast, forIndex + h);
        if (forecast.Variance.Data)
          std.SetColumnFromColumn0(k, forecast.Variance, forIndex + h);
      } else {
        act.SetColumn0(k, NAN);
        forc.SetColumn0(k, NAN);
        std.SetColumn0(k, NAN);
      }
      k++;
    }
    if (forecast.Variance.Data)
      std.Apply_in([](Tv x) -> Tv { return std::sqrt(x); });
    act.Subtract0(forc, err);

    /*if (keepDetails) {
        details->sampleEnd = se;
        details->isValid = true;
        act.CopyTo00(details->actual);
        forc.CopyTo00(details->forecast);
        std.CopyTo00(details->stdforecast);
    }*/
    success = true;
    ValidCounts++;

    Ti c = 0;
    for (auto &eval : measures) {
      Scoring::GetScore(eval, temp, act, forc, err, std, last);

      // summation for calculating mean
      auto me = Results.at(c);
      k = 0;
      for (auto &h : horizons) {
        if (h > effectiveH)
          continue;
        for (Ti i = 0; i < temp.RowsCount; i++) {
          me.Set0(i, k, me.Get0(i, k) + temp.Get0(i, k));
          ResultAggs.Set0(c, i, ResultAggs.Get0(c, i) + temp.Get0(i, k));
        }
        k++;
      }

      // if (keepDetails) {
      //     temp.CopyTo(details->measures.at(c));
      // }
      c++;
    }

    row++;
  }

  // average:
  for (Ti j = 0; j < hh; j++) {
    auto o = (Tv)1 / (Tv)counters[j];
    for (Ti i = 0; i < mm; i++) {
      auto m = &Results.at(i);
      if (measures.at(i) == ScoringType::kRmse ||
          measures.at(i) == ScoringType::kScaledRmse) {
        for (k = 0; k < m->RowsCount; k++)
          m->Set0(k, j, std::sqrt(m->Get0(k, j) * o));
      } else {
        for (k = 0; k < m->RowsCount; k++)
          m->Set0(k, j, m->Get0(k, j) * o);
      }
    }
  }

  // average (aggregate ove horizons)
  for (Ti i = 0; i < mm; i++) {
    if (measures.at(i) == ScoringType::kRmse ||
        measures.at(i) == ScoringType::kScaledRmse) {
      for (k = 0; k < yy; k++)
        ResultAggs.Set0(i, k, std::sqrt(ResultAggs.Get0(i, k) / (Tv)counter));
    } else {
      for (k = 0; k < yy; k++)
        ResultAggs.Set0(i, k, ResultAggs.Get0(i, k) / (Tv)counter);
    }
  }
}

void VarmaSimulation::AddError(std::string state) {
  if (state.empty())
    return;
  if (Errors.find(state) != Errors.end())
    Errors.at(state)++;
  else {
    Errors.insert(std::pair<std::string, Ti>(state, 1));
  }
}

// #pragma endregion
