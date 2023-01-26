/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the GPL 3.0.
 See accompanying file LICENSE
*/

#include "distributions.h"
#include "running.h"
#include <numeric>

using namespace ldt;

DistributionMixtureType gettype(std::vector<DistributionBase *> *dists) {
  bool all = true;
  Ti siz = (Ti)dists->size();
  for (Ti i = 0; i < siz; i++) {
    if (dists->at(i)->IsDiscrete() == false) {
      all = false;
      break;
    }
  }
  if (all)
    return DistributionMixtureType::kDiscrete;

  all = true;
  for (Ti i = 0; i < siz; i++) {
    if (dists->at(i)->IsDiscrete() == true) {
      all = false;
      break;
    }
  }

  if (all)
    return DistributionMixtureType::kContinuous;

  return DistributionMixtureType::kBoth;
}

DistributionMixture::DistributionMixture(
    std::vector<Tv> &weights, std::vector<DistributionBase *> &dists) {
  if (weights.size() != dists.size())
    throw std::logic_error("inconsistent size.");
  for (auto &w : weights)
    if (w <= 0.0)
      throw std::logic_error("Zero/negative weight in mixture distribution");
  if (weights.size() == 0)
    throw std::logic_error("zero number of distributions.");

  pWeights = &weights;
  pDistributions = &dists;
  pType = gettype(&dists);
}

Tv DistributionMixture::GetMinimum() {
  Tv min = INFINITY;
  Tv m;
  for (auto &c : *pDistributions) {
    m = c->GetMinimum();
    if (m < min)
      min = m;
  }
  return min;
}

Tv DistributionMixture::GetMaximum() {
  Tv max = -INFINITY;
  Tv m;
  for (auto &c : *pDistributions) {
    m = c->GetMaximum();
    if (m > max)
      max = m;
  }
  return max;
}

void DistributionMixture::GetMoments(Tv &mean, Tv &variance, Tv &skewness,
                                     Tv &kurtosis) {

  // Fr�hwirth-Schnatter, S. (2006) page. 10

  // first calculate and keep central moments.
  // TODO: it might be more efficient to provide one function for all 4 moments,
  // like this method
  Ti count = (Ti)pWeights->size();
  auto means = std::vector<Tv>(count);
  auto varis = std::vector<Tv>(count);
  auto skews = std::vector<Tv>(count);
  auto kurts = std::vector<Tv>(count);

  Ti i;
  for (i = 0; i < count; i++) {
    means[i] = pDistributions->at(i)->GetMean();
    varis[i] = pDistributions->at(i)->GetVariance();
    skews[i] = pDistributions->at(i)->GetSkewness();
    kurts[i] = pDistributions->at(i)->GetKurtosis();
  }

  // first central moment (mean) is weighted average of means
  RunningWeightedMean wmean = RunningWeightedMean();
  i = 0;
  for (auto &w : *pWeights) {
    wmean.PushNew(means[i], w);
    i++;
  }
  mean = wmean.GetMean();

  // eq. 1.21 for variance
  RunningWeightedMean wvari = RunningWeightedMean();
  i = 0;
  for (auto &w : *pWeights) {
    wvari.PushNew(std::pow(means[i], 2) + varis[i], w);
    i++;
  }
  variance = wvari.GetMean() - (mean * mean);

  // skewness, 1.22
  Tv mdiff;
  RunningWeightedMean wskew = RunningWeightedMean();
  i = 0;
  for (auto &w : *pWeights) {
    mdiff = means[i] - mean;
    wskew.PushNew(std::pow(mdiff, 3) + 3 * mdiff * varis[i] +
                      skews[i] * std::pow(varis[i], 1.5),
                  w);
    i++;
  }
  skewness = wskew.GetMean() / std::pow(variance, 1.5);

  // kurtosis, 1.22
  RunningWeightedMean wkurt = RunningWeightedMean();
  i = 0;
  for (auto &w : *pWeights) {
    mdiff = means[i] - mean;
    wkurt.PushNew(std::pow(mdiff, 4) + 6 * std::pow(mdiff, 2) * varis[i] +
                      4 * mdiff * skews[i] * std::pow(varis[i], 1.5) +
                      (kurts[i] + 3.0) * std::pow(varis[i], 2.0),
                  w);
    i++;
  }
  kurtosis = wkurt.GetMean() / std::pow(variance, 2.0) - 3.0;
}

void DistributionMixture::GetMomentsNormal(Tv &mean, Tv &variance, Tv &skewness,
                                           Tv &kurtosis) {

  // first calculate and keep central moments.
  // TODO: it might be more efficient to provide one function for all 4 moments,
  // like this method
  Ti count = (Ti)pWeights->size();
  auto means = std::vector<Tv>(count);
  auto varis = std::vector<Tv>(count);
  auto skews = std::vector<Tv>(count);
  auto kurts = std::vector<Tv>(count);

  Ti i;
  for (i = 0; i < count; i++) {
    means[i] = pDistributions->at(i)->GetMean();
    varis[i] = pDistributions->at(i)->GetVariance();
    skews[i] = pDistributions->at(i)->GetSkewness();
    kurts[i] = pDistributions->at(i)->GetKurtosis();
  }

  // first central moment (mean) is weighted avarage of means
  RunningWeightedMean wmean = RunningWeightedMean();
  i = 0;
  for (auto &w : *pWeights) {
    wmean.PushNew(means[i], w);
    i++;
  }
  mean = wmean.GetMean();

  // eq. 1.21 for variance
  RunningWeightedMean wvari = RunningWeightedMean();
  i = 0;
  for (auto &w : *pWeights) {
    wvari.PushNew(std::pow(means[i], 2) + varis[i], w);
    i++;
  }
  variance = wvari.GetMean() - (mean * mean);

  // skewness, the formula after 1.22
  Tv mdiff;
  RunningWeightedMean wskew = RunningWeightedMean();
  i = 0;
  for (auto &w : *pWeights) {
    mdiff = means[i] - mean;
    wskew.PushNew((std::pow(mdiff, 2) + 3.0 * varis[i]) * mdiff, w);
    i++;
  }
  skewness = wskew.GetMean() / std::pow(variance, 1.5);

  // kurtosis, the formula after 1.22
  RunningWeightedMean wkurt = RunningWeightedMean();
  i = 0;
  for (auto &w : *pWeights) {
    mdiff = means[i] - mean;
    wkurt.PushNew(std::pow(mdiff, 4) + 6.0 * std::pow(mdiff, 2) * varis[i] +
                      3 * std::pow(varis[i], 2),
                  w);
    i++;
  }
  kurtosis = wkurt.GetMean() / std::pow(variance, 2.0) - 3.0;
}

void DistributionMixture::GetMoments(Tv &mean, Tv &variance) {
  Ti count = (Ti)pWeights->size();
  auto means = std::vector<Tv>(count);
  auto varis = std::vector<Tv>(count);
  auto skews = std::vector<Tv>(count);
  auto kurts = std::vector<Tv>(count);

  Ti i;
  for (i = 0; i < count; i++) {
    means[i] = pDistributions->at(i)->GetMean();
    varis[i] = pDistributions->at(i)->GetVariance();
    skews[i] = pDistributions->at(i)->GetSkewness();
    kurts[i] = pDistributions->at(i)->GetKurtosis();
  }

  // first central moment (mean) is weighted avarage of means
  RunningWeightedMean wmean = RunningWeightedMean();
  i = 0;
  for (auto &w : *pWeights) {
    wmean.PushNew(means[i], w);
    i++;
  }
  mean = wmean.GetMean();

  // eq. 1.21 for variance
  RunningWeightedMean wvari = RunningWeightedMean();
  i = 0;
  for (auto &w : *pWeights) {
    wvari.PushNew(std::pow(means[i], 2) + varis[i], w);
    i++;
  }
  variance = wvari.GetMean() - (mean * mean);
}

Tv DistributionMixture::GetCdf(Tv x) {
  RunningWeightedMean wm = RunningWeightedMean();
  Ti i = 0;
  if (pWeights) {
    for (auto &w : *pWeights) {
      wm.PushNew(pDistributions->at(i)->GetCdf(x), w);
      i++;
    }
  }
  return wm.GetMean();
}

Ti DistributionMixture::GetPmfSupportSize(Tv &min, Tv &max) {

  // as long as the support is the integers, we calculate global min and max and
  // enumerate if there is a distribution where the support is different, throw
  // not implemented or handle it differently
  // TODO: check
  // for (Ti i = 0; i < pDistributions->size(); i++)
  //	pDistributions->at(i)->GetPmfSupportIncrement();
  Tv min0 = std::numeric_limits<Tv>::max();
  Tv max0 = std::numeric_limits<Tv>::min();
  Ti siz = (Ti)pDistributions->size();
  for (Ti i = 0; i < siz; i++) {
    min0 = std::fmin(min0, pDistributions->at(i)->GetMinimum());
    max0 = std::fmax(max0, pDistributions->at(i)->GetMaximum());
  }
  min = fmax(min, min0);
  max = fmin(max, max0);

  return static_cast<Ti>(max - min) + 1;
}

void DistributionMixture::GetPmfSupport(Tv *x, Tv *Value, bool log, Ti length,
                                        bool for_continuous_plot, Tv min,
                                        Tv max) {
  if (length <= 0)
    throw std::logic_error("invalid length for support of distribution.");
  if (pType != DistributionMixtureType::kDiscrete)
    throw std::logic_error("Use it when all distributions are discrete.");

  // TODO: check increment

  GetPmfSupportSize(min, max); // update min/max

  if (for_continuous_plot) {
    Tv xx;
    length /= 3;
    Ti j = 0;
    for (Ti i = 0; i < length; i++) {
      xx = min + i;
      x[j] = xx;
      x[j + 1] = xx;
      x[j + 2] = xx;
      Value[j] = 0;
      Value[j + 1] = log ? GetPdfOrPmfLog(xx) : GetPdfOrPmf(xx);
      Value[j + 2] = 0;
      j += 3;
    }
  } else {
    Tv xx = min;
    for (Ti i = 0; i < length; i++) {
      xx = min + i;
      x[i] = xx;
      Value[i] = log ? GetPdfOrPmfLog(xx) : GetPdfOrPmf(xx);
    }
  }
}

Tv DistributionMixture::GetPdfOrPmf(Tv x) {

  if (pType == DistributionMixtureType::kBoth)
    throw std::logic_error(
        "PDF/PMF of a mixture of discrete and continuous distributions is "
        "not supported");

  RunningWeightedMean wm = RunningWeightedMean();
  Ti i = 0;
  if (pWeights) {
    for (auto &w : *pWeights) {
      wm.PushNew(pDistributions->at(i)->GetPdfOrPmf(x), w);
      i++;
    }
  }

  return wm.GetMean();
}

Tv DistributionMixture::GetPdfOrPmfLog(Tv x) {
  return std::log(GetPdfOrPmf(x));
}

void DistributionMixture::GetSample(Tv *storage, Ti length, unsigned int seed) {
  std::mt19937 eng;

  // should we sample from discrete cases? aren't they zero probability event
  // right now I think sampling from them is valid

  if (seed != 0)
    eng = std::mt19937(seed);
  else {
    std::random_device rdev{};
    eng = std::mt19937(rdev());
  }

  std::vector<Tv> Sum = std::vector<Tv>(pWeights->size());
  std::partial_sum(pWeights->begin(), pWeights->end(), Sum.begin(),
                   std::plus<Tv>());

  Ti cs = (Ti)pDistributions->size();
  Tv max = Sum.at(Sum.size() - 1);
  Tv un;
  Ti j;
  std::uniform_real_distribution<Tv> dist(0.0, max);
  for (Ti i = 0; i < length; i++) {
    un = dist(eng);
    j = 0;
    for (auto &a : Sum) {
      if (un < a)
        break;
      j++;
    }
    if (j < cs)
      // sample from j-th distribution
      storage[i] = pDistributions->at(j)->GetSample1(eng);
    else {
      j -= cs;
      storage[i] = pDistributions->at(j)->GetSample1(eng);
    }
  }
}
