#pragma once

#include "array.h"
#include <cmath>

namespace ldt {

template <Ti moments, bool skipNAN = true, bool isWeighted = true,
          class Tw = Tv>
class LDT_EXPORT RunningMoments {
private:
  Tw mMean = 0, mM2 = 0, mM3 = 0, mM4 = 0;

  Tw w = 0; //, delta = 0, R = 0, t1 = 0, t2 = 0, temp = 0;

public:
  /// @brief Determines the number of pushes. It is not related to the
  /// statistical part, just the operational part.
  Ti Count = 0;

  /// @brief Sum of the weights
  Tw SumWeights = 0;

  /// @brief Initializes a new instance of the class
  RunningMoments() {}

  /// @brief Resets the calculations
  void Reset() {
    Count = 0;
    SumWeights = 0;
    mMean = 0;
    mM2 = 0;
    mM3 = 0;
    mM4 = 0;
  }

  /// @brief Use this function to get the mean
  /// @return NAN if sum of weights is zero, otherwise mean
  Tw GetMean() { return SumWeights == 0 ? NAN : mMean; }

  /// @brief Use this function to get the population variance
  /// @return NAN if sum of weights is zero or \par moments is 1, otherwise
  /// variance
  Tw GetVariance() {
    if constexpr (moments >= 2) {
      return SumWeights == 0 ? NAN : mM2 / SumWeights;
    } else if constexpr (true) {
      return NAN;
    }
  }

  /// @brief Use this function to get the population skewness
  /// @return NAN if sum of weights is zero or \par moments is less than 2,
  /// otherwise skewness
  Tw GetSkewness() {
    if constexpr (moments >= 3) {
      return SumWeights == 0 ? NAN : sqrt(SumWeights) * mM3 / pow(mM2, 1.5);
    } else if constexpr (true) {
      return NAN;
    }
  }

  /// @brief Use this function to get the population kurtosis
  /// @return NAN if sum of weights is zero or \par moments is less than 3,
  /// otherwise kurtosis. Note that it is not excess kurtosis
  Tw GetKurtosis() {
    if constexpr (moments == 4) {
      return SumWeights == 0 ? NAN : SumWeights * mM4 / (mM2 * mM2) - 3;
    } else if constexpr (true) {
      return NAN;
    }
  }

  /// @brief Update the moments by adding a new observation
  /// @param value The new observation
  /// @param weight The weight of the observation
  void PushNew(const Tw &value, const Tw &weight) {

    if constexpr (skipNAN) {
      if (std::isnan(value))
        return;
    }

    Count++;
    Tw ww = weight;
    if constexpr (isWeighted == false) {
      ww = 1;
    }

    Array<Tw>::template update_single_pass<isWeighted, moments>(
        value, ww, SumWeights, mMean, mM2, mM3, mM4);
  }

  /// @brief Combines another set of moments with current set
  /// @param other The second set
  void Combine(const RunningMoments<moments, skipNAN, isWeighted, Tw> &other) {

    if constexpr (skipNAN) { // don't let other destroy any moments
      if (std::isnan(other.mMean))
        return;
      if constexpr (moments >= 2) {
        if (std::isnan(other.mM2))
          return;
      }
      if constexpr (moments >= 3) {
        if (std::isnan(other.mM3))
          return;
      }
      if constexpr (moments == 4) {
        if (std::isnan(other.mM4))
          return;
      }
    }

    Tw mean = 0, m2 = 0, m3 = 0, m4 = 0, sw = 0;
    Array<Tw>::template update_single_pass_combine<moments>(
        other.SumWeights, other.mMean, other.mM2, other.mM3, other.mM4,
        SumWeights, mMean, mM2, mM3, mM4, sw, mean, m2, m3, m4);

    if constexpr (skipNAN) {
      if (std::isnan(mean))
        return;
      if constexpr (moments >= 2) {
        if (std::isnan(m2))
          return;
      }
      if constexpr (moments >= 3) {
        if (std::isnan(m3))
          return;
      }
      if constexpr (moments == 4) {
        if (std::isnan(m4))
          return;
      }
    }

    mMean = mean;
    mM2 = m2;
    mM3 = m3;
    mM4 = m4;
    SumWeights = sw;
    Count += other.Count;
  }

  /// @brief Combines current moments with another set of moments
  /// @param mean mean of the second set
  /// @param var variance of the second set (population formula)
  /// @param skew skewness of the second set (population formula)
  /// @param kurt kurtosis of the second set (population formula). It should not
  /// be excess kurtosis.
  /// @param weight Relative weight of the second set.
  /// @param count An operational value to update the value of \par Count
  /// in the class.
  void Combine(const Tw &mean, const Tw &var, const Tw &skew, const Tw &kurt,
               const Tw &weight, Ti count = 1) {

    if constexpr (skipNAN) { // don't let other destroy any moments
      if (std::isnan(mean))
        return;
    }

    Tw m2, m3, m4;

    // convert to moments
    if constexpr (moments >= 2) {
      m2 = var * weight;
      if constexpr (skipNAN) {
        if (std::isnan(m2))
          return;
      }
    }

    if constexpr (moments >= 3) {
      m3 = pow(m2, 1.5) * skew / sqrt(weight);
      if constexpr (skipNAN) {
        if (std::isnan(m3))
          return;
      }
    }

    if constexpr (moments == 4) {
      m4 = (kurt + 3) * (m2 * m2) / weight;
      if constexpr (skipNAN) {
        if (std::isnan(m4))
          return;
      }
    }

    Tw mean_ = 0, m2_ = 0, m3_ = 0, m4_ = 0, sw_ = 0;
    Array<Tw>::template update_single_pass_combine<moments>(
        weight, mean, m2, m3, m4, SumWeights, mMean, mM2, mM3, mM4, sw_, mean_,
        m2_, m3_, m4_);

    if constexpr (skipNAN) {
      if (std::isnan(mean_))
        return;
      if constexpr (moments >= 2) {
        if (std::isnan(m2_))
          return;
      }
      if constexpr (moments >= 3) {
        if (std::isnan(m3_))
          return;
      }
      if constexpr (moments == 4) {
        if (std::isnan(m4_))
          return;
      }
    }

    mMean = mean_;
    mM2 = m2_;
    mM3 = m3_;
    mM4 = m4_;
    SumWeights = sw_;

    Count += count;
  }
};

} // namespace ldt
