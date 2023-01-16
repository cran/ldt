/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the LGPL 3.0.
 See accompanying file LICENSE
*/

#include "scoring.h"

using namespace ldt;

Tv Scoring::GetScoreCrpsNormal(Tv y, Tv mean, Tv std) {
  y = y - mean;
  if (std == 0)
    return std::abs(y); // MAE ?!

  auto dist = Distribution<DistributionType::kNormal>(0, std);
  Tv z = y / std;
  return y * (2 * dist.GetCdf(y) - 1) +
         std * (c_sqrt2 * exp(-0.5 * pow(z, 2)) - 1) / c_sqrt_pi;
}

Tv Scoring::GetScoreCrpsLogNormal(Tv y, Tv meanLog, Tv stdLog) {
  auto var = pow(stdLog, 2);

  auto dist1 = Distribution<DistributionType::kLogNormal>(meanLog, stdLog);
  auto dist2 =
      Distribution<DistributionType::kLogNormal>(meanLog + var, stdLog);
  auto dist3 = Distribution<DistributionType::kNormal>(0, 1);

  // auto a1 = dist1.GetCdf(y);
  // auto a2 = dist2.GetCdf(y);
  // auto a3 = dist3.GetCdf(stdLog / c_sqrt2);

  return y * (2 * dist1.GetCdf(y) - 1) -
         (2 * exp(meanLog + 0.5 * var) *
          (dist2.GetCdf(y) + dist3.GetCdf(stdLog / c_sqrt2) - 1));
}

void Scoring::GetScore(ScoringType type, Matrix<Tv> &result, Matrix<Tv> &act,
                       Matrix<Tv> &forc, Matrix<Tv> &err, Matrix<Tv> &std,
                       Matrix<Tv> &last_m) {
  Ti i, j;
  switch (type) {
  case ScoringType::kDirection:
    Tv d, f, a, l;
    for (i = 0; i < act.RowsCount; i++) {
      for (j = 0; j < act.ColsCount; j++) {
        d = 0;
        f = forc.Get0(i, j);
        a = act.Get0(i, j);
        l = last_m.Data[i];

        if (std::isnan(f))
          d = NAN;
        else if (a > l) {
          if (f > l)
            d = 1;
        } else if (a < l) {
          if (f < l)
            d = 1;
        } else if (a == l) {
          if (f == l)
            d = 1;
        }
        result.Set0(i, j, d);
      }
    }
    break;

  case ScoringType::kSign:

    act.Apply0(
        forc,
        [](Tv a, Tv f) -> Tv {
          if (std::isnan(f))
            return NAN;
          else if (a == 0 || f == 0)
            return 0.5; // award of 0 is 0.5
          else if ((a > 0 && f < 0) || (a < 0 && f > 0))
            return 0.0;
          return 1.0;
        },
        result);
    break;

  case ScoringType::kMae:
    err.Apply0([](Tv x) -> Tv { return std::abs(x); }, result);
    break;
  case ScoringType::kScaledMae:
    for (i = 0; i < act.length(); i++) {
      if (act.Data[i] == 0)
        result.Data[i] = NAN; //??
      else
        result.Data[i] = std::abs(err.Data[i]) / std::abs(act.Data[i]);
    }
    break;

  case ScoringType::kRmse:
    err.Apply0([](Tv x) -> Tv { return x * x; },
               result); // note that a sqrt is required for aggregating
    break;
  case ScoringType::kScaledRmse:
    for (i = 0; i < act.length(); i++) {
      if (act.Data[i] == 0)
        result.Data[i] = NAN; //??
      else
        result.Data[i] =
            err.Data[i] * err.Data[i] / (act.Data[i] * act.Data[i]);
    }
    break;

  case ScoringType::kCrps:
    err.Apply0(
        std,
        [](Tv e, Tv s) -> Tv { return Scoring::GetScoreCrpsNormal(e, 0, s); },
        result);
    break;

  default:
    throw std::logic_error("not implemented");
  }
}

bool Scoring::RequiresVariance(const ScoringType &type) {
  switch (type) {
  case ScoringType::kMae:
  case ScoringType::kScaledMae:
  case ScoringType::kRmse:
  case ScoringType::kScaledRmse:
    return false;
  default:
    return true;
  }
  return true;
}

Tv GoodnessOfFit::ToWeight(const GoodnessOfFitType &type, const Tv &measure) {
  switch (type) {
  case GoodnessOfFitType::kAic:
  case GoodnessOfFitType::kSic:
    return std::exp(-0.5 * measure);
  case GoodnessOfFitType::kAuc:
    return measure;
  case GoodnessOfFitType::kCostMatrix:
    return 1 - measure;
  default:
    throw std::logic_error("not implemented goodness-of-fit type to weight");
  }
}

Tv GoodnessOfFit::FromWeight(const GoodnessOfFitType &type, const Tv &weight) {
  switch (type) {
  case GoodnessOfFitType::kAic:
  case GoodnessOfFitType::kSic:
    return -2 * std::log(weight);
  case GoodnessOfFitType::kAuc:
    return weight;
  case GoodnessOfFitType::kCostMatrix:
    return 1 - weight;
  default:
    throw std::logic_error("not implemented goodness-of-fit type to weight");
  }
}

Tv Scoring::ToWeight(const ScoringType &type, const Tv &measure) {
  switch (type) {
  case ScoringType::kDirection:
  case ScoringType::kSign:
    return measure;

  case ScoringType::kMae:
  case ScoringType::kScaledMae:
  case ScoringType::kRmse:
  case ScoringType::kScaledRmse:
    return (Tv)1 / measure; // ????

  case ScoringType::kCrps:
    return (Tv)1 / measure; // ????

  default:
    throw std::logic_error("not implemented scoring type to weight");
  }
}

Tv Scoring::FromWeight(const ScoringType &type, const Tv &weight) {
  switch (type) {
  case ScoringType::kDirection:
  case ScoringType::kSign:
    return weight;

  case ScoringType::kMae:
  case ScoringType::kScaledMae:
  case ScoringType::kRmse:
  case ScoringType::kScaledRmse:
    return (Tv)1 / weight; // ????

  case ScoringType::kCrps:
    return (Tv)1 / weight; // ????

  default:
    throw std::logic_error("not implemented scoring type to weight");
  }
}
