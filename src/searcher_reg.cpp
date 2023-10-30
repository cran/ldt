/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the GPL 3.0.
 See accompanying file LICENSE
*/

#include "searchers.h"

using namespace ldt;

SearcherReg::SearcherReg(const SearchData &data,
                         const SearchCombinations &combinations,
                         SearchOptions &options, const SearchItems &items,
                         const SearchMetricOptions &metrics,
                         const SearchModelChecks &checks,
                         const Ti &numPartitions, const bool &isInnerExogenous,
                         const std::vector<Ti> &innerIndices,
                         const Ti extraLength)
    : Searcher::Searcher(data, combinations, options, items, metrics, checks,
                         numPartitions, isInnerExogenous) {

  IsInnerExogenous = isInnerExogenous;
  mExtraLength = extraLength;
  auto w = data.HasWeight ? 1 : 0;
  InnerIndices = innerIndices;
  ColIndices = std::vector<Ti>(
      numPartitions + w +
      innerIndices.size()); // it gets updated in EstimateOne method

  if (isInnerExogenous) {
    // the second part of ColIndices is constant:
    for (Ti i = 0; i < (Ti)innerIndices.size(); i++)
      ColIndices.at(i + numPartitions + w) =
          InnerIndices.at(i) + w; //? check +w

    // weights are after endogenous:
    if (data.HasWeight)
      ColIndices.at(numPartitions) = data.NumEndo;
    // targets are in current indices and should be updated in the main loop
  } else {
    // put it in the first part of column indices
    for (Ti i = 0; i < (Ti)innerIndices.size(); i++)
      ColIndices.at(i) = innerIndices.at(i);

    // weights are after endogenous:
    if (this->pData->HasWeight)
      ColIndices.at(InnerIndices.size()) = this->pData->NumEndo;

    // we can set targets here
    for (auto &a : innerIndices) {
      if (a < items.LengthTargets)
        TargetsPositions.push_back(a);
    }
    if (TargetsPositions.size() == 0)
      throw LdtException(ErrorType::kLogic, "sur-modelset",
                         "a searcher with no target is not valid");
  }
}

std::string SearcherReg::EstimateOne(Tv *work, Ti *workI) {

  // update column and inner indices based on current indices

  if (IsInnerExogenous) {
    // update the first part of column indices
    for (Ti i = 0; i < (Ti)CurrentIndices.Vec.size(); i++)
      ColIndices.at(i) = CurrentIndices.Vec.at(i);

    // update target indices
    TargetsPositions.clear();
    for (auto &a : CurrentIndices.Vec) {
      if (a < pItems->LengthTargets)
        TargetsPositions.push_back(a);
    }
  } else {
    Ti w = pData->HasWeight ? 1 : 0;
    // update the second part of column indices
    for (Ti i = 0; i < (Ti)CurrentIndices.Vec.size(); i++)
      ColIndices.at(i + InnerIndices.size() + w) = CurrentIndices.Vec.at(i) + w;
  }

  auto numMeas = (Ti)(this->pMetrics->MetricsIn.size() +
                      this->pMetrics->MetricsOut.size());

  if (numMeas == 0)
    throw LdtException(ErrorType::kLogic, "searcher-reg", "No measures!");
  if (TargetsPositions.size() == 0)
    throw LdtException(ErrorType::kLogic, "searcher-reg", "No targets!");
  auto metrics = VMatrix<Tv>(numMeas, (Ti)TargetsPositions.size());

  auto extra = VMatrix<Ti>(mExtraLength);

  auto type1_mean =
      VMatrix<Tv>(this->pItems->Length1, (Ti)TargetsPositions.size());
  auto type1_var =
      VMatrix<Tv>(this->pItems->Length1, (Ti)TargetsPositions.size());
  // some length1 information are not present in the model, let the default
  // value be NAN. Its the derived class responsibility to handle length1
  // information order.
  type1_mean.Mat.SetValue(NAN);

  if (this->pOptions->RequestCancel)
    return "cancel";

  auto res = EstimateOneReg(work, workI, metrics, type1_mean, type1_var, extra);
  if (res.length() != 0)
    return res;

  if (this->pOptions->RequestCancel)
    return "cancel";

  // push:
  Ti i, j;
  bool allNan = true;
  for (i = 0; i < metrics.Mat.RowsCount; i++) { // metric index
    for (j = 0; j < metrics.Mat.ColsCount; j++) {

      // j is the target index (use TargetsPositions to interpret)
      Ti t_pos = TargetsPositions.at(j);

      auto metric = metrics.Mat.Get0(i, j);
      if (std::isnan(metric))
        continue;
      allNan = false;

      Tv weight = NAN, min_metric = 0;
      if (i < (Ti)this->pMetrics->MetricsIn.size()) {
        auto gf = pMetrics->MetricsIn.at(i);
        if (this->pMetrics->MinMetricIn.find(gf) !=
            this->pMetrics->MinMetricIn.end())
          min_metric = this->pMetrics->MinMetricIn.at(gf).at(t_pos);
        weight = GoodnessOfFit::ToWeight(gf, metric, min_metric);
      } else {
        auto sr = pMetrics->MetricsOut.at(i - this->pMetrics->MetricsIn.size());
        if (this->pMetrics->MinMetricOut.find(sr) !=
            this->pMetrics->MinMetricOut.end())
          min_metric = this->pMetrics->MinMetricOut.at(sr).at(t_pos);
        weight = Scoring::ToWeight(sr, metric, min_metric);
      }

      // add model information:
      if (this->pItems->KeepModelEvaluations) {
        std::shared_ptr<EstimationKeep> ek;
        if (IsInnerExogenous)
          ek = std::make_shared<EstimationKeep>(metric, weight, InnerIndices,
                                                extra.Vec,
                                                this->CurrentIndices.Vec);
        else
          ek = std::make_shared<EstimationKeep>(metric, weight,
                                                this->CurrentIndices.Vec,
                                                extra.Vec, InnerIndices);
        this->Push0(ek, i, t_pos);
      }

      if (this->pItems->Length1 > 0) { // Add coefficients
        for (Ti i1 = 0; i1 < type1_mean.Mat.RowsCount; i1++) {

          std::shared_ptr<EstimationKeep> ek;
          auto m = type1_mean.Mat.Get(i1, j);
          if (std::isnan(m))
            continue;
          auto v = type1_var.Mat.Get(i1, j);

          if (IsInnerExogenous)
            ek = std::make_shared<EstimationKeep>(
                metric, weight, InnerIndices, extra.Vec,
                this->CurrentIndices.Vec, m, v);
          else
            ek = std::make_shared<EstimationKeep>(
                metric, weight, this->CurrentIndices.Vec, extra.Vec,
                InnerIndices, m, v);

          this->Push1(ek, i, t_pos, i1);
        }
      }

      if (this->pItems->Length2 > 0) { // TODO: for saving projections
      }
    }
  }

  if (allNan) {
    throw LdtException(
        ErrorType::kLogic, "searcher-reg",
        "all metrics are NaN"); // +
                                // Sur::ModelToString(DModel.Sizes).c_str();
                                // // weights are all nan
  }

  return "";
}
