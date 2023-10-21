/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the GPL 3.0.
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

bool Scoring::RequiresVariance(const ScoringType &type) {
  switch (type) {
  case ScoringType::kMae:
  case ScoringType::kMape:
  case ScoringType::kRmse:
  case ScoringType::kRmspe:
    return false;
  default:
    return true;
  }
  return true;
}

Tv GoodnessOfFit::ToWeight(const GoodnessOfFitType &type, const Tv &metric,
                           const double &minMetric) {
  switch (type) {
  case GoodnessOfFitType::kAic:
  case GoodnessOfFitType::kSic:
  case GoodnessOfFitType::kBrier:
    return std::exp(-0.5 * (metric - minMetric));
    // Note that exp(-0.5 * x) transformation is invariant to translation. so we
    // can use the running algorithm

  case GoodnessOfFitType::kAuc:
    return metric;
  case GoodnessOfFitType::kFrequencyCost:
    return 1 - metric;

  default:
    throw LdtException(ErrorType::kLogic, "scoring",
                       "not implemented goodness-of-fit type to weight");
  }
}

Tv GoodnessOfFit::FromWeight(const GoodnessOfFitType &type, const Tv &weight,
                             const double &minMetric) {
  switch (type) {
  case GoodnessOfFitType::kAic:
  case GoodnessOfFitType::kSic:
  case GoodnessOfFitType::kBrier:
    return -2.0 * std::log(weight) + minMetric;
  case GoodnessOfFitType::kAuc:
    return weight;
  case GoodnessOfFitType::kFrequencyCost:
    return 1 - weight;

  default:
    throw LdtException(ErrorType::kLogic, "scoring",
                       "not implemented goodness-of-fit type to weight");
  }
}

bool GoodnessOfFit::IsPositiveOriented(const GoodnessOfFitType &type) {
  switch (type) {
  case GoodnessOfFitType::kAic:
  case GoodnessOfFitType::kSic:
  case GoodnessOfFitType::kBrier:
  case GoodnessOfFitType::kFrequencyCost:
    return false;
  case GoodnessOfFitType::kAuc:
    return true;
  default:
    throw LdtException(ErrorType::kLogic, "scoring",
                       "not implemented goodness-of-fit orientation");
  }
}

Tv Scoring::ToWeight(const ScoringType &type, const Tv &metric,
                     const double &minMetric) {
  switch (type) {
  case ScoringType::kDirection:
  case ScoringType::kSign:
    return metric;

  case ScoringType::kMae:
  case ScoringType::kRmse:
  case ScoringType::kBrier:
  case ScoringType::kCrps:
    return std::exp(-0.5 * (metric - minMetric));

  case ScoringType::kMape:
  case ScoringType::kRmspe:
    return std::exp(-0.5 * (metric - minMetric) / 100);

  case ScoringType::kAuc:
    return metric;

  case ScoringType::kFrequencyCost:
    return 1 - metric;

  default:
    throw LdtException(ErrorType::kLogic, "scoring",
                       format("given scoring type (value={}) is "
                              "whether invalid or not implemented.",
                              (int)type));
  }
}

Tv Scoring::FromWeight(const ScoringType &type, const Tv &weight,
                       const double &minMetric) {
  switch (type) {
  case ScoringType::kDirection:
  case ScoringType::kSign:
    return weight;

  case ScoringType::kMae:
  case ScoringType::kRmse:
  case ScoringType::kCrps:
  case ScoringType::kBrier:
    return -2 * std::log(weight) + minMetric;

  case ScoringType::kMape:
  case ScoringType::kRmspe:
    return -2 * std::log(weight) * 100 + minMetric;

  case ScoringType::kAuc:
    return weight;

  case ScoringType::kFrequencyCost:
    return 1 - weight;

  default:
    throw LdtException(ErrorType::kLogic, "scoring",
                       "not implemented scoring type to weight");
  }
}

bool Scoring::IsPositiveOriented(const ScoringType &type) {
  switch (type) {
  case ScoringType::kDirection:
  case ScoringType::kSign:
  case ScoringType::kAuc:
    return true;

  case ScoringType::kMae:
  case ScoringType::kRmse:
  case ScoringType::kCrps:
  case ScoringType::kBrier:
  case ScoringType::kMape:
  case ScoringType::kRmspe:
  case ScoringType::kFrequencyCost:
    return false;

  default:
    throw LdtException(ErrorType::kLogic, "scoring",
                       "not implemented scoring orientation");
  }
}
