#pragma once

#include "matrix.h"
#include <cmath>

namespace ldt {

/// @brief A class for calculating running weighted mean. Source: Math.NET
/// source code
class LDT_EXPORT RunningWeightedMean {
private:
  Ti mCount = 0;
  Tv mSumWeights = 0.0;
  Tv mM1 = 0.0;

public:
  /// @brief Initializes a new instance of the class
  RunningWeightedMean();

  /// @brief Initializes a new instance of the class
  /// @param data Initial data
  /// @param weight Weight of the initial data
  RunningWeightedMean(const Matrix<Tv> &data, Tv weight);

  /// @brief Resets the class. Sets count and sum of weights to zero.
  void Reset() {
    mCount = 0;
    mSumWeights = 0;
  }

  /// @brief Gets the number of counted observations
  /// @return count
  Ti GetCount() { return mCount; }

  /// @brief Gets summation of the weights
  /// @return Sum of the weights
  Tv GetSumOfWeights() { return mSumWeights; }

  /// @brief Gets the mean
  /// @return The mean. If count is 0, NAN
  Tv GetMean() { return mCount == 0 ? NAN : mM1; }

  /// @brief Add new data
  /// @param value New data
  /// @param weight Weight of the new data
  void PushNew(Tv value, Tv weight);

  /// @brief Add a range of new data
  /// @param data New data
  /// @param weight Weights of the new data
  void PushNewRange(const Matrix<Tv> &data, Tv weight);

  /// @brief Combines two classes
  /// @param b The second class
  void Combine(RunningWeightedMean &b);
};

/// @brief A class for calculating running weighted variance. Source: Math.NET
/// source code
class LDT_EXPORT RunningWeightedVariance {
private:
  Ti mCount = 0;
  Tv mSumWeights = 0.0;
  Tv mM1 = 0.0;
  Tv mM2 = 0.0;

public:
  /// @brief Initializes a new instance of the class
  RunningWeightedVariance();

  /// @brief Initializes a new instance of the class
  /// @param data Initial data
  /// @param weight Weight of the initial data
  RunningWeightedVariance(const Matrix<Tv> &data, Tv weight);

  /// @brief Resets the class. Sets count and sum of weights to zero.
  void Reset() {
    mCount = 0;
    mSumWeights = 0;
  }

  /// @brief Gets the number of counted observations
  /// @return count
  Ti GetCount() { return mCount; }

  /// @brief Gets summation of the weights
  /// @return Sum of the weights
  Tv Sum() { return mSumWeights; }

  /// @brief Gets the mean
  /// @return The mean. If count is 0, NAN
  Tv GetMean() { return mCount == 0 ? NAN : mM1; }

  /// @brief Gets the variance (population)
  /// @return The variance. If count is less than 1, NAN
  Tv GetVariancePopulation() { return mCount <= 1 ? NAN : mM2 / mSumWeights; }

  /// @brief Add new data
  /// @param value New data
  /// @param weight Weight of the new data
  void PushNew(Tv value, Tv weight);

  /// @brief Add a range of new data
  /// @param data New data
  /// @param weight Weights of the new data
  void PushNewRange(const Matrix<Tv> &data, Tv weight);

  /// @brief Combines two classes
  /// @param b The second class
  void Combine(RunningWeightedVariance b);
};

/// @brief /// @brief A class for calculating running weighted mean, variance,
/// skewness and kurtosis. Source: Math.NET source code
class LDT_EXPORT RunningWeighted4 {
private:
  Ti mCount = 0;
  Tv mSumWeights = 0.0;
  Tv mM1 = 0.0;
  Tv mM2 = 0.0;
  Tv mM3 = 0.0;
  Tv mM4 = 0.0;

public:
  /// @brief Initializes a new instance of the class
  RunningWeighted4();

  /// @brief Initializes a new instance of the class
  /// @param data Initial data
  /// @param weight Weight of the initial data
  RunningWeighted4(const Matrix<Tv> &data, Tv weight);

  /// @brief Resets the class. Sets count and sum of weights to zero.
  void Reset() {
    mCount = 0;
    mSumWeights = 0;
  }

  /// @brief Gets the number of counted observations
  /// @return count
  Ti GetCount() { return mCount; }

  /// @brief Gets summation of the weights
  /// @return Sum of the weights
  Tv Sum() { return mSumWeights; }

  /// @brief Gets the mean
  /// @return The mean. If count is 0, NAN
  Tv GetMean() { return mCount == 0 ? NAN : mM1; }

  /// @brief Gets the variance (population)
  /// @return The variance. If count is less than 1, NAN
  Tv GetVariancePopulation() { return mCount <= 1 ? NAN : mM2 / mSumWeights; }

  /// @brief Gets the skewness (population)
  /// @return The skewness. If count is less than 1, NAN
  Tv GetSkewnessPopulation() {
    return mCount <= 1 ? NAN
                       : std::sqrt(mSumWeights) * mM3 / std::pow(mM2, (Tv)1.5);
  }

  /// @brief Gets the kurtosis (population)
  /// @return The kurtosis. If count is less than 2, NAN
  Tv GetKurtosisPopulation() {
    return mCount <= 2 ? NAN : mSumWeights * mM4 / (mM2 * mM2) - (Tv)3;
  }

  /// @brief Add new data
  /// @param value New data
  /// @param weight Weight of the new data
  void PushNew(Tv value, Tv weight);

  /// @brief Add a range of new data
  /// @param data New data
  /// @param weight Weights of the new data
  void PushNewRange(const Matrix<Tv> &data, Tv weight);

  /// @brief Combines two classes
  /// @param b The second class
  void Combine(RunningWeighted4 &b);

  /// @brief Add a distribution as a data
  /// @param mean Mean of the distribution
  /// @param variance Variance of the distribution
  /// @param skewness Skewness of the distribution
  /// @param kurtosis Kurtosis of the distribution
  /// @param weight Weight of the distribution
  /// @param count Sets the \ref mCount (Note that if two distributions
  /// are merged and \ref mCount is 2, then kurtosis will be NAN. Therefore, it
  /// is recommended to set it larger than 1. It generally does not affect the
  /// calculations)
  void PushNewDistribution(Tv mean, Tv variance, Tv skewness = 0,
                           Tv kurtosis = 0, Tv weight = 1, Ti count = 1);
};

} // namespace ldt
