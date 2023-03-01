/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the GPL 3.0.
 See accompanying file LICENSE
*/

#include "functionx.h"
#include "scoring.h"

using namespace ldt;

template <bool hasWeight, bool hasCost> ROC<hasWeight, hasCost>::ROC() {}

template <bool hasWeight, bool hasCost> ROC<hasWeight, hasCost>::ROC(Ti n) {}

template <bool hasWeight, bool hasCost>
void ROC<hasWeight, hasCost>::Calculate(const Matrix<Tv> &y,
                                        const Matrix<Tv> &scores,
                                        const Matrix<Tv> *weights,
                                        const RocOptions &options) {
  bool isPartial = std::isnan(options.LowerThreshold) == false &&
                   std::isnan(options.UpperThreshold) == false;
  bool normalizePoints = options.NormalizePoints;
  if (isPartial) {
    normalizePoints =
        true; // we should normalize the points before using thresholds
    if (options.UpperThreshold < options.LowerThreshold ||
        options.UpperThreshold > 1 || options.LowerThreshold < 0)
      throw std::logic_error("Invalid bounds in partial AUC.");
  }

  if constexpr (hasCost) {
    if (!options.CostMatrix.Data || options.CostMatrix.RowsCount != 2 ||
        options.CostMatrix.ColsCount != 2)
      throw std::logic_error("Missing or invalid cost matrix.");
  }

  Ti n = y.length();
  if (n == 0)
    throw std::logic_error("zero number of observations in calculating ROC.");

  // first column must be the scores (i.e., probability of negative
  // observations) score=1 => It is definitely negative
  auto sortedIndexes = std::vector<Ti>();
  SortIndexes(scores.Data, n, sortedIndexes, true);

  bool isNeg = false;
  Ti ind;
  Tv yi, th, thPre = scores.Data[sortedIndexes[0]], w = 1, sumFP = 0, horiz = 0,
             sumTP = 0, verti = 0;
  Points.clear();
  Points.push_back(std::make_tuple<Tv, Tv>(0, 0)); // start from origin
  for (Ti i = 0; i < n; i++) {
    ind = sortedIndexes[i];
    th = scores.Data[ind];
    yi = y.Data[ind];
    w = 1;
    if constexpr (hasWeight) {
      w = weights->Data[ind];
    }

    if (std::abs(th - thPre) > options.Epsilon) { // push new point(s)
      sumTP += verti; // overall vertical move (for the rectangle)
      sumFP += horiz;
      if (options.Pessimistic)
        Points.push_back(std::make_tuple(sumFP, 0)); // no vertical move
      Points.push_back(std::make_tuple(sumFP, sumTP));
      thPre = th;
      horiz = 0;
      verti = 0;
    }

    //    At the current threshold: this observation and all
    //    previous observations are predicted to be positive

    isNeg = yi == 0;

    if constexpr (hasCost) {
      Tv xi = options.Costs.Data ? options.Costs.Data[ind] : 1;
      Tv b_tp = options.CostMatrix.Data[0] * xi - options.CostMatrix.Data[2];
      if (b_tp < 0)
        throw std::logic_error("Invalid cost matrix: benefit of TP is "
                               "negative. Check the first row.");
      Tv c_fp = -(options.CostMatrix.Data[1] * xi - options.CostMatrix.Data[3]);
      if (c_fp < 0)
        throw std::logic_error("Invalid cost matrix: cost of FP is negative. "
                               "Check the second row.");
      if (isNeg)
        horiz += w * c_fp;
      else
        verti += w * b_tp;

    } else if constexpr (true) {

      if (isNeg) // A false-positive -> a horizontal move in ROC
        horiz += w;
      else // a true-positive -> a vertical move
        verti += w;
    }

    // for pessimistic: see
    // https://link.springer.com/article/10.1007/s00357-019-09345-1
  }

  sumTP += verti;
  sumFP += horiz;
  Points.push_back(std::make_tuple(sumFP, sumTP));

  if (normalizePoints) {
    for (auto &p : Points) {
      std::get<0>(p) /= sumFP;
      std::get<1>(p) /= sumTP;
    }
    if (isPartial) { // TODO: create a Bounded AUC class and move this logic
                     // to that class
      // throw std::logic_error("Partial AUC is not implemented.");
      std::vector<std::tuple<Tv, Tv>> newPoints;
      Tv x, y, x0 = 0, y0 = 0, slope;
      for (auto &p : Points) {
        x = std::get<0>(p);
        y = std::get<1>(p);
        slope = (y - y0) / (x - x0);

        if (x >= options.LowerThreshold && x0 <= options.UpperThreshold) {
          if (x > options.LowerThreshold &&
              x0 < options.LowerThreshold) // don't miss the first point
            newPoints.push_back(
                std::make_tuple(options.LowerThreshold,
                                y0 + (options.LowerThreshold - x0) * slope));

          if (x >= options.LowerThreshold && x <= options.UpperThreshold)
            newPoints.push_back(std::make_tuple(x, y));

          if (x > options.UpperThreshold &&
              x0 < options.UpperThreshold) // don't miss the last point
            newPoints.push_back(
                std::make_tuple(options.UpperThreshold,
                                y - (x - options.UpperThreshold) * slope));
        }

        x0 = x;
        y0 = y;
      }
      AucPoints<false> a(newPoints, 0);
      Result = a.Result / (options.UpperThreshold - options.LowerThreshold);
    } else {
      AucPoints<false> a(Points, 0);
      Result = a.Result;
    }
  } else {
    AucPoints<false> a(Points, 0);
    Result = a.Result / (sumFP * sumTP); // normalize
  }
}

template class ldt::ROC<true, true>;
template class ldt::ROC<true, false>;
template class ldt::ROC<false, true>;
template class ldt::ROC<false, false>;
