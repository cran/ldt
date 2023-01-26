#pragma once

#include "ldt_base.h"
#include "matrix.h"

#include <algorithm>
#include <iostream>
#include <math.h>
#include <random>
#include <set>
#include <sstream>
#include <string>
#include <vector>

namespace ldt {

/// @brief A class to calculate descriptive statistics for a vector
class LDT_EXPORT Descriptive {

public:
  /// @brief A reference to the given data
  const Matrix<Tv> *pArray = nullptr;

  /// @brief Initializes a new instance of the class
  /// @param mat The data as a vector (i.e., dimension is not used, just the
  /// inner array)
  Descriptive(const Matrix<Tv> *mat);

  /// @brief Minimum value
  /// @return If length is zero, it returns NAN. It ignores NAN. Use
  /// 'HasNaN()' if you expect NAN. If all NAN, it returns +inf
  double Minimum();

  /// @brief Minimum value if data is sorted
  /// @return The first element as the minimum
  double MinimumSorted();

  /// @brief Maximum value
  /// @return If length is zero, it returns NAN. It ignores NAN. Use
  /// 'HasNaN()' if you expect NAN. If all NAN, it returns -inf
  double Maximum();

  /// @brief Maximum value if data is sorted
  /// @return The last element as the maximum
  double MaximumSorted();

  /// @brief Sum of the elements
  /// @return Sum
  double Sum();

  /// @brief Calculates mean and variance
  /// @param sample If true, it uses the sample statistics
  /// @return mean and variance
  std::tuple<double, double> MeanVariance(bool sample = true);

  /// @brief Calculates mean, variance, skewness and kurtosis
  /// @param sample If true, it uses the sample statistics
  /// @return mean, variance, skewness and kurtosis
  std::tuple<double, double, double, double>
  MeanVarianceKurtosisSkewness(bool sample = true);

  /// @brief Recursive filter. i.e., y_i = x_i + c_0*y_{i-1} + ... +
  /// c_p*y_{i-p-1} where y is the filtered series, x is current data, c are the
  /// coefficients.
  /// @param coefficients Same size as the current array
  /// @param storage Same size as the current array
  void FilterAr(const Matrix<Tv> *coefficients, Matrix<Tv> *storage);

  /// @brief Convolution filter. i.e., y_i = c_0*x_{i} + ... + c_p*x_{i-p} where
  /// y is
  ///  the filtered series, x is current data, c are the coefficients.
  /// If filter is centered, an offset of floor(k/2) (where k is the length of
  /// coefficients) will be used. If k is even, the end of the result will have
  /// one more NaN.
  /// @param coefficients
  /// @param centered
  /// @param storage
  void FilterMa(const Matrix<Tv> &coefficients, bool centered,
                Matrix<Tv> &storage);

  /// @brief The results are independent of 'start' (e.g. if data starts from
  /// 2nd season, you should just interpret the results accordingly).
  /// @param storage_trend
  /// @param storage_seasonal
  /// @param storage_resid
  /// @param storage_means
  /// @param trendCoefs
  /// @param seasonCount
  /// @param isMultiplicative
  /// @param do_resid
  /// @param trend_centred
  void SeasonalDecompositionMa(Matrix<Tv> &storage_trend,
                               Matrix<Tv> &storage_seasonal,
                               Matrix<Tv> &storage_resid,
                               Matrix<Tv> &storage_means,
                               Matrix<Tv> *trendCoefs = {}, Ti seasonCount = 4,
                               bool isMultiplicative = false,
                               bool do_resid = true, bool trend_centred = true);

  /// @brief Estimates a linear trend regression(y = a + bt) and returns
  /// intercept and slope
  /// @param storage2 An array of size 2
  void RegressionTrend(double *storage2);

  /// @brief Calculates the sum of square
  /// @param central If true, it centers data
  /// @return sum of square
  double SumOfSquares(bool central = true);

  /// @brief Quantile of a sorted array
  /// @param tau
  /// @return
  double QuantileSorted(double tau);
};

/// @brief Ranks the values of the columns of a matrix
class LDT_EXPORT Rank {
public:
  /// @brief Initializes a new instance of the class
  /// @param rows Maximum expected number of rows
  /// @param cols Maximum expected number of columns
  Rank(Ti rows, Ti cols);

  /// @brief Gets the required size of the storage array
  Ti StorageSize = 0;

  /// @brief Gets the required size of the work array (real)
  Ti WorkSize = 0;

  /// @brief After \ref Calculate, it contains the result
  Matrix<Tv> Result;

  /// @brief Calculate the results
  /// @param mat Data
  /// @param work Work array of size \ref WorkSize
  /// @param storage Storage array of size \ref StorageSize
  /// @param ascending Determines the order of the ranks
  void Calculate(const Matrix<Tv> &mat, Tv *work, Tv *storage,
                 bool ascending = true);
};

/// @brief A simple OLS estimator class
class LDT_EXPORT Ols {
  bool mDoResid = false, mDoSigma = false;

public:
  /// @brief Initializes a new instance of the class
  /// @param N Maximum expected number of observations (i.e., rows in y)
  /// @param m Maximum expected number of equations (i.e., columns in y)
  /// @param k Maximum expected number of explanatory variables (i.e., columns
  /// in x)
  /// @param resid If true, residuals are also calculated
  /// @param sigma If true, variance of the regression is also calculated. If
  /// true, \p resid =true is used
  Ols(Ti N, Ti m, Ti k, bool resid, bool sigma);

  /// @brief Gets the required size of the storage array
  Ti StorageSize = 0;

  /// @brief Gets the required size of the work array (real)
  Ti WorkSize = 0;

  /// @brief After \ref Calculate, gets the estimated coefficients
  Matrix<Tv> Beta;

  /// @brief After \ref Calculate and if requested, gets the estimated residuals
  Matrix<Tv> Resid;

  /// @brief After \ref Calculate and if requested, gets the estimated variance
  /// of the regression
  Matrix<Tv> Sigma;

  /// @brief Calculates the results
  /// @param y Endogenous data
  /// @param x Exogenous data
  /// @param storage Storage array of size \ref StorageSize
  /// @param work Work array of size \ref WorkSize
  void Calculate(const Matrix<Tv> &y, const Matrix<Tv> &x, Tv *storage,
                 Tv *work);
};

/// @brief A simple GLS estimator class
class LDT_EXPORT Gls {
  bool mDoResid = false, mDoSigma = false;

public:
  /// @brief Gets the value \p isOmegaInv given to the constructor. It
  /// determines the type of the \p omega function in \ref Calculate
  bool mIsOmegaInv = false;

  /// @brief Initializes
  /// @param N Maximum expected number of observations (i.e., rows in y)
  /// @param m Maximum expected number of equations (i.e., columns in y)
  /// @param k Maximum expected number of explanatory variables (i.e., columns
  /// in x)
  /// @param resid If true, residuals are also calculated
  /// @param sigma If true, variance of the regression is also calculated. If
  /// true, \p resid =true is used
  /// @param isOmegaInv If true, \p omega matrix given to \ref Calculate is
  /// inverse
  Gls(Ti N, Ti m, Ti k, bool resid, bool sigma, bool isOmegaInv = false);

  /// @brief Gets the required size of the storage array
  Ti StorageSize = 0;

  /// @brief Gets the required size of the work array (real)
  Ti WorkSize = 0;

  /// @brief After \ref Calculate, gets the estimated coefficients
  Matrix<Tv> Beta;

  /// @brief After \ref Calculate and if requested, gets the estimated residuals
  Matrix<Tv> Resid;

  /// @brief After \ref Calculate and if requested, gets the estimated variance
  /// of the regression
  Matrix<Tv> Sigma;

  /// @brief Calculates the results
  /// @param y Endogenous data
  /// @param x Exogenous data
  /// @param omega Initialized residual variance for Feasible GLS (It can be the
  /// inverse if declared in the constructor)
  /// @param storage Storage array of size \ref StorageSize
  /// @param work Work array of size \ref WorkSize
  void Calculate(const Matrix<Tv> &y, const Matrix<Tv> &x, Matrix<Tv> &omega,
                 Tv *storage, Tv *work);
};

} // namespace ldt
