#pragma once

#include "ldt_base.h"
#include "matrix.h"
#include <functional>
#include <iostream>
#include <iterator>
#include <vector>

namespace ldt {

/// @brief A class for calculating numerical derivative
class LDT_EXPORT Derivative {

private:
  Ti mCount = 5, mN = 0;
  bool mDoFirstDerivative = true, mDoSecondDerivative = true;

public:
  /// @brief Gets the required storage size for the first derivative (which is
  /// 'Result1' of size 'n')
  Ti StorageSize1 = 0;

  /// @brief Gets the required storage size for the first derivative (which is
  /// 'Result2' of size 'n x n')
  Ti StorageSize2 = 0;

  /// @brief Gets the required work size
  Ti WorkSize = 0;

  /// @brief First derivative (if asked for, otherwise empty). It is 'n x 1'. It
  /// has the 'storage' of \ref CalculateFirst
  Matrix<Tv> Result1;

  /// @brief Second derivative (if asked for, otherwise empty). It is 'n x n'.
  /// It has the 'storage' of \ref CalculateSecond
  Matrix<Tv> Result2;

  /// @brief Initializes a new instance of the class
  /// @param n Maximum expected dimension of 'x'
  /// @param doFirstDerivative If true, first derivative is expected
  /// @param doSecondDerivative If true, second derivative is expected
  /// @param count Maximum number of evaluations
  Derivative(Ti n, bool doFirstDerivative = true,
             bool doSecondDerivative = true, Ti count = 5);

  /// @brief Step size for |x| < \ref Epsilon
  Tv Step0 = 1e-5;

  /// @brief Step size for |x| > \ref Epsilon
  Tv Step1 = 1e-4;

  /// @brief Determines zero in \ref Step0
  Tv Epsilon = 1e-7;

  /// @brief t parameter in Richardson extrapolation
  Tv T = 2;

  /// @brief Calculates the first derivative of a function at a specific point
  /// using Richardson extrapolation
  /// @param f The function
  /// @param x The specific point
  /// @param storage Storage array of size \ref StorageSize1
  /// @param work Work array of size \ref WorkSize
  void CalculateFirst(const std::function<Tv(const Matrix<Tv> &)> &f,
                      const Matrix<Tv> &x, Tv *storage, Tv *work);

  /// @brief Calculates the first derivative of a function at a specific point
  /// using Richardson extrapolation
  /// @param f The function
  /// @param x The specific point
  /// @param storage Storage array of size \ref StorageSize2
  /// @param work Work array of size \ref WorkSize
  void CalculateSecond(const std::function<Tv(const Matrix<Tv> &)> &f,
                       const Matrix<Tv> &x, Tv *storage, Tv *work);
};

} // namespace ldt
