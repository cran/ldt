#pragma once

#include "helpers.h"
#include "ldt_base.h"
#include <algorithm> // std::sort, std::stable_sort
#include <functional>
#include <numeric> // std::iota
#include <string>
#include <vector>

namespace ldt {

/// @brief A range with an start and end indices
class LDT_EXPORT IndexRange {

public:
  /// @brief Starting index of the range
  Ti StartIndex = 0;

  /// @brief Ending index of the range
  Ti EndIndex = 0;

  /// @brief Initializes a new instance of the class
  IndexRange(){};

  /// @brief Initializes a new instance of the class
  /// @param start Starting index of the range
  /// @param end Ending index of the range
  IndexRange(Ti start, Ti end);

  /// @brief Determines whether this range is invalid (start > end, start < 0,
  /// end < 0)
  /// @return true if the range is not valid, false otherwise
  bool IsNotValid() const;

  /// @brief Number of elements in this range. It is 1 if start==end
  /// @return Number of elements in this range
  Ti Count() const;
};

/// @brief Function Types
enum class FunctionType {

  /// @brief Identity function
  kId,
  kPow2,
  kPow3,
  kPow4,
  kExp,
  kLog,
  kLog10,
  kSqrt,
  kSin,
  kCos,
  kTan,
  kAsin,
  kAcos,
  kAtan,
  kSinh,
  kCosh,
  kTanh,
  kAbs,
  kCeil,
  kFloor
};

inline const char *ToString(FunctionType type) {

  switch (type) {
  case FunctionType::kId:
    return "Identity";
  case FunctionType::kPow2:
    return "Power2";
  case FunctionType::kPow3:
    return "Power3";
  case FunctionType::kPow4:
    return "Power4";
  case FunctionType::kExp:
    return "Exponential";
  case FunctionType::kLog:
    return "Natural Logarithm";
  case FunctionType::kLog10:
    return "Base-10 Logarithm";
  case FunctionType::kSqrt:
    return "Square Root";
  case FunctionType::kSin:
    return "Sine";
  case FunctionType::kCos:
    return "Cosine";
  case FunctionType::kTan:
    return "Tangent";
  case FunctionType::kAsin:
    return "Inverse Sine";
  case FunctionType::kAcos:
    return "Inverse Cosine";
  case FunctionType::kAtan:
    return "Inverse Tangent";
  case FunctionType::kSinh:
    return "Hyperbolic Sine";
  case FunctionType::kCosh:
    return "Hyperbolic Cosine";
  case FunctionType::kTanh:
    return "Hyperbolic Tangent";
  case FunctionType::kAbs:
    return "Absolute Value";
  case FunctionType::kCeil:
    return "Ceiling";
  case FunctionType::kFloor:
    return "Floor";
  default:
    return "[Unknown FunctionType]";
  }
}

inline FunctionType FromString_FunctionType(const char *name) {

  if (std::strcmp(name, "id") == 0) {
    return FunctionType::kId;
  } else if (std::strcmp(name, "pow2") == 0) {
    return FunctionType::kPow2;
  } else if (std::strcmp(name, "pow3") == 0) {
    return FunctionType::kPow3;
  } else if (std::strcmp(name, "pow4") == 0) {
    return FunctionType::kPow4;
  } else if (std::strcmp(name, "exp") == 0) {
    return FunctionType::kExp;
  } else if (std::strcmp(name, "log") == 0) {
    return FunctionType::kLog;
  } else if (std::strcmp(name, "log10") == 0) {
    return FunctionType::kLog10;
  } else if (std::strcmp(name, "sqrt") == 0) {
    return FunctionType::kSqrt;
  } else if (std::strcmp(name, "sin") == 0) {
    return FunctionType::kSin;
  } else if (std::strcmp(name, "cos") == 0) {
    return FunctionType::kCos;
  } else if (std::strcmp(name, "tan") == 0) {
    return FunctionType::kTan;
  } else if (std::strcmp(name, "asin") == 0) {
    return FunctionType::kAsin;
  } else if (std::strcmp(name, "acos") == 0) {
    return FunctionType::kAcos;
  } else if (std::strcmp(name, "atan") == 0) {
    return FunctionType::kAtan;
  } else if (std::strcmp(name, "sinh") == 0) {
    return FunctionType::kSinh;
  } else if (std::strcmp(name, "cosh") == 0) {
    return FunctionType::kCosh;
  } else if (std::strcmp(name, "tanh") == 0) {
    return FunctionType::kTanh;
  } else if (std::strcmp(name, "abs") == 0) {
    return FunctionType::kAbs;
  } else if (std::strcmp(name, "ceil") == 0) {
    return FunctionType::kCeil;
  } else if (std::strcmp(name, "floor") == 0) {
    return FunctionType::kFloor;
  } else {
    throw LdtException(ErrorType::kLogic, "array.h", "invalid function type");
  }
}

/// @brief Descriptive type
enum class DescriptiveType {

  kMin,
  kMax,
  kMean,

  kVariance,
  kVariancePop,
  kStd,
  kStdPop,

  kSkewness,
  kSkewnessPop,

  kKurtosis,
  kKurtosisPop,

  kLast,
  kFirst

};

/// @brief Converts a value of \ref DescriptiveType to string
/// @param v The value
/// @return The string
inline const char *ToString(DescriptiveType v) {
  switch (v) {
  case DescriptiveType::kMin:
    return "Minimum";
  case DescriptiveType::kMax:
    return "Maximum";
  case DescriptiveType::kMean:
    return "Mean";
  case DescriptiveType::kVariance:
    return "Variance";
  case DescriptiveType::kStd:
    return "Std";
  case DescriptiveType::kVariancePop:
    return "Variance (pop)";
  case DescriptiveType::kStdPop:
    return "Std (pop)";
  case DescriptiveType::kSkewness:
    return "Skewness";
  case DescriptiveType::kSkewnessPop:
    return "Skewness (pop)";
  case DescriptiveType::kKurtosis:
    return "Kurtosis";
  case DescriptiveType::kKurtosisPop:
    return "Kurtosis (pop)";
  case DescriptiveType::kFirst:
    return "First";
  case DescriptiveType::kLast:
    return "Last";

  default:
    return "[Unknown DescriptiveType]";
  }
}

/// @brief Converts a string into a value of \ref DescriptiveType
/// @param v The string
/// @return The value
inline DescriptiveType FromString_DescriptiveType(const char *v) {

  if (StartsWith("min", v))
    return DescriptiveType::kMin;
  else if (StartsWith("max", v))
    return DescriptiveType::kMax;
  else if (StartsWith("mea", v) || StartsWith("ave", v))
    return DescriptiveType::kMean;
  else if (StartsWith("var", v)) {
    if (EndsWith("pop", v))
      return DescriptiveType::kVariancePop;
    else
      return DescriptiveType::kVariance;
  } else if (StartsWith("std", v)) {
    if (EndsWith("pop", v))
      return DescriptiveType::kStdPop;
    else
      return DescriptiveType::kStd;
  } else if (StartsWith("ske", v)) {
    if (EndsWith("pop", v))
      return DescriptiveType::kSkewnessPop;
    else
      return DescriptiveType::kSkewness;
  } else if (StartsWith("kur", v)) {
    if (EndsWith("pop", v))
      return DescriptiveType::kKurtosisPop;
    else
      return DescriptiveType::kKurtosis;
  } else if (StartsWith("last", v))
    return DescriptiveType::kLast;
  else if (StartsWith("firs", v))
    return DescriptiveType::kFirst;

  throw LdtException(ErrorType::kLogic, "array.h", "invalid descriptive type");
}

/// @brief A class for static array operations, that are for example needed both
/// in matrix and variable
/// @tparam Tw type of data in the matrix array
template <class Tw = Tv> class LDT_EXPORT Array {

public:
  /// @brief Determines where non-NA data starts and ends
  /// @param data The data
  /// @param length Length of array
  /// @param hasMissing On exit, it determines whether missing observation is
  /// located or not.
  /// @return Range of data.
  static IndexRange GetRange(const Tw *data, const Ti &length,
                             bool &hasMissing);

  /// @brief Similar to GetRange, but it does not search for missing data points
  /// (is faster)
  /// @param data
  /// @param length
  /// @return
  static IndexRange GetRange(const Tw *data, const Ti &length);

  /// @brief Interpolates missing observations
  /// @param data
  /// @param length
  /// @param count
  /// @return
  static IndexRange Interpolate(Tw *data, const Ti &length, Ti &count);

  /// @brief Converts a vector into sub-vectors of equal size
  /// @param data The data
  /// @param result A place to store the result
  /// @param size The length of each sub-vector
  /// @param fromEnd It is effective if the length of \par data is not divisible
  /// by \par size. If true, the length of the first partition might have less
  /// observation. IF false, length of the last partition might have less
  /// observation.
  static void PartitionEqual(const std::vector<Tw> &data,
                             std::vector<std::vector<Tw>> &result, Ti size,
                             bool fromEnd);

  // MATH FUNCTIONS

  /// @brief Applies a function on a scalar
  /// @tparam T The function
  /// @param x The scalar
  template <FunctionType T> void Function(Tw &x) {
    if constexpr (T == FunctionType::kId) {
      // x = x;
    } else if constexpr (T == FunctionType::kPow2) {
      x = std::pow(x, 2);
    } else if constexpr (T == FunctionType::kPow3) {
      x = std::pow(x, 3);
    } else if constexpr (T == FunctionType::kPow4) {
      x = std::pow(x, 4);
    } else if constexpr (T == FunctionType::kExp) {
      x = std::exp(x);
    } else if constexpr (T == FunctionType::kLog) {
      x = std::log(x);
    } else if constexpr (T == FunctionType::kLog10) {
      x = std::log10(x);
    } else if constexpr (T == FunctionType::kSqrt) {
      x = std::sqrt(x);
    } else if constexpr (T == FunctionType::kSin) {
      x = std::sin(x);
    } else if constexpr (T == FunctionType::kCos) {
      x = std::cos(x);
    } else if constexpr (T == FunctionType::kTan) {
      x = std::tan(x);
    } else if constexpr (T == FunctionType::kAsin) {
      x = std::asin(x);
    } else if constexpr (T == FunctionType::kAcos) {
      x = std::acos(x);
    } else if constexpr (T == FunctionType::kAtan) {
      x = std::atan(x);
    } else if constexpr (T == FunctionType::kSinh) {
      x = std::sinh(x);
    } else if constexpr (T == FunctionType::kCosh) {
      x = std::cosh(x);
    } else if constexpr (T == FunctionType::kTanh) {
      x = std::tanh(x);
    } else if constexpr (T == FunctionType::kAbs) {
      x = std::abs(x);
    } else if constexpr (T == FunctionType::kCeil) {
      x = std::ceil(x);
    } else if constexpr (T == FunctionType::kFloor) {
      x = std::floor(x);
    }
  }

  /// @brief Applies a function on the elements of an array
  /// @tparam T The function
  /// @param data The array
  /// @param length Length of the array
  template <FunctionType T> void Function(Tw *data, const Ti &length) {

    for (Ti i = 0; i < length; i++) {
      if constexpr (T == FunctionType::kId) {
        // data[i] = data[i];
      } else if constexpr (T == FunctionType::kPow2) {
        data[i] = std::pow(data[i], 2);
      } else if constexpr (T == FunctionType::kPow3) {
        data[i] = std::pow(data[i], 3);
      } else if constexpr (T == FunctionType::kPow4) {
        data[i] = std::pow(data[i], 4);
      } else if constexpr (T == FunctionType::kExp) {
        data[i] = std::exp(data[i]);
      } else if constexpr (T == FunctionType::kLog) {
        data[i] = std::log(data[i]);
      } else if constexpr (T == FunctionType::kLog10) {
        data[i] = std::log10(data[i]);
      } else if constexpr (T == FunctionType::kSqrt) {
        data[i] = std::sqrt(data[i]);
      } else if constexpr (T == FunctionType::kSin) {
        data[i] = std::sin(data[i]);
      } else if constexpr (T == FunctionType::kCos) {
        data[i] = std::cos(data[i]);
      } else if constexpr (T == FunctionType::kTan) {
        data[i] = std::tan(data[i]);
      } else if constexpr (T == FunctionType::kAsin) {
        data[i] = std::asin(data[i]);
      } else if constexpr (T == FunctionType::kAcos) {
        data[i] = std::acos(data[i]);
      } else if constexpr (T == FunctionType::kAtan) {
        data[i] = std::atan(data[i]);
      } else if constexpr (T == FunctionType::kSinh) {
        data[i] = std::sinh(data[i]);
      } else if constexpr (T == FunctionType::kCosh) {
        data[i] = std::cosh(data[i]);
      } else if constexpr (T == FunctionType::kTanh) {
        data[i] = std::tanh(data[i]);
      } else if constexpr (T == FunctionType::kAbs) {
        data[i] = std::abs(data[i]);
      } else if constexpr (T == FunctionType::kCeil) {
        data[i] = std::ceil(data[i]);
      } else if constexpr (T == FunctionType::kFloor) {
        data[i] = std::floor(data[i]);
      }
    }
  }

  // ................ DESCRIPTIVES

  template <bool skipNAN>
  static void GetDescriptive(const Tw *data, const Ti &length,
                             const DescriptiveType &type, Tw &result) {

    switch (type) {
    case DescriptiveType::kMin:
      Min<skipNAN>(data, length, result);
      break;
    case DescriptiveType::kMax:
      Max<skipNAN>(data, length, result);
      break;
    case DescriptiveType::kLast:
      Last<skipNAN>(data, length, result);
      break;
    case DescriptiveType::kFirst:
      First<skipNAN>(data, length, result);
      break;
    case DescriptiveType::kMean:
      result = Mean<skipNAN>(data, length);
      break;
    case DescriptiveType::kVariance: {
      result = Variance<skipNAN, true>(data, length); // sample statistics
    } break;
    case DescriptiveType::kVariancePop: {
      result = Variance<skipNAN, false>(data, length);
    } break;
    case DescriptiveType::kStd: {
      result = Variance<skipNAN, true>(data, length);
      result = std::sqrt(result);
    } break;
    case DescriptiveType::kStdPop: {
      result = Variance<skipNAN, false>(data, length);
      result = std::sqrt(result);
    } break;

    case DescriptiveType::kSkewness: {
      result = Skewness<skipNAN, true>(data, length); // sample statistics
    } break;
    case DescriptiveType::kSkewnessPop: {
      result = Skewness<skipNAN, false>(data, length);
    } break;

    case DescriptiveType::kKurtosis: {
      result = Kurtosis<skipNAN, true>(data, length); // sample statistics
    } break;
    case DescriptiveType::kKurtosisPop: {
      result = Kurtosis<skipNAN, false>(data, length);
    } break;

    default:
      throw LdtException(ErrorType::kLogic, "array.h",
                     "invalid or not-implemented descriptive statistics");
    }
  }

  template <bool skipNAN>
  static void Min(const Tw *data, const Ti &length, Tw &min) {
    if (length == 0)
      min = NAN;
    else {
      min = INFINITY;
      Tw d;
      for (Ti i = 0; i < length; i++) {
        d = data[i];

        if constexpr (skipNAN) {
          if (std::isnan(d))
            continue;
        }

        if (d < min)
          min = d;
      }
    }
  }

  template <bool skipNAN>
  static void MinIndex(const Tw *data, const Ti &length, Tw &min, Ti &index) {
    if (length == 0) {
      min = NAN;
      index = -1;
    } else {
      index = -1;
      min = INFINITY;
      Tw d;
      for (Ti i = 0; i < length; i++) {
        d = data[i];

        if constexpr (skipNAN) {
          if (std::isnan(d))
            continue;
        }

        if (d < min) {
          min = d;
          index = i;
        }
      }
    }
  }

  template <bool skipNAN>
  static void Max(const Tw *data, const Ti &length, Tw &max) {
    if (length == 0)
      max = NAN;
    else {
      max = -INFINITY;
      Tw d;
      Ti i;
      for (i = 0; i < length; i++) {
        d = data[i];

        if constexpr (skipNAN) {
          if (std::isnan(d))
            continue;
        }

        if (d > max)
          max = d;
      }
    }
  }

  template <bool skipNAN>
  static void MaxIndex(const Tw *data, const Ti &length, Tw &max, Ti &index) {
    if (length == 0) {
      max = NAN;
      index = -1;
    } else {
      index = -1;
      max = -INFINITY;
      Tw d;
      Ti i;
      for (i = 0; i < length; i++) {
        d = data[i];

        if constexpr (skipNAN) {
          if (std::isnan(d))
            continue;
        }

        if (d > max) {
          max = d;
          index = i;
        }
      }
    }
  }

  template <Ti moments>
  inline static void
  update_single_pass_combine(const Tw &sumW_1, const Tw &mean_1, const Tw &M2_1,
                             const Tw &M3_1, const Tw &M4_1, const Tw &sumW_2,
                             const Tw &mean_2, const Tw &M2_2, const Tw &M3_2,
                             const Tw &M4_2, Tw &sumW, Tw &mean, Tw &M2, Tw &M3,
                             Tw &M4) {

    auto temp = sumW_1 + sumW_2;
    Tv delta = mean_2 - mean_1;
    Tv delta2 = delta * delta;

    mean = (sumW_1 * mean_1 + sumW_2 * mean_2) / temp;

    if constexpr (moments == 4) { // Pebay (2008). Formulas for Robust, One-Pass
                                  // Parallel..., Eq. 1.6
      M4 = M4_1 + M4_2 +
           delta2 * delta2 * sumW_1 * sumW_2 *
               (sumW_1 * sumW_1 - sumW_1 * sumW_2 + sumW_2 * sumW_2) /
               (temp * temp * temp) +
           6 * delta2 * (sumW_1 * sumW_1 * M2_2 + sumW_2 * sumW_2 * M2_1) /
               (temp * temp) +
           4 * delta * (sumW_1 * M3_2 - sumW_2 * M3_1) / temp;
    }

    if constexpr (moments >= 3) { // Pebay (2008). Formulas for Robust, One-Pass
                                  // Parallel..., Eq. 1.5
      M3 =
          M3_1 + M3_2 +
          delta * delta2 * sumW_1 * sumW_2 * (sumW_1 - sumW_2) / (temp * temp) +
          3 * delta * (sumW_1 * M2_2 - sumW_2 * M2_1) / temp;
    }

    if constexpr (moments >= 2) {
      M2 = M2_1 + M2_2 + delta2 * sumW_1 * sumW_2 / temp; // Eq. 1.4
    }

    sumW = temp;
  }

  template <bool isWeighted, Ti moments>
  inline static void update_single_pass(const Tw &x, const Tw &w, Tw &sumW,
                                        Tw &mean, Tw &M2, Tw &M3, Tw &M4) {

    Tw ww = w;
    if constexpr (isWeighted == false) {
      ww = 1;
    }

    auto delta = x - mean;
    auto delta2 = delta * delta;
    auto temp = sumW + ww;

    mean = (sumW * mean + ww * x) / temp;

    if constexpr (moments == 4) {
      M4 += delta2 * delta2 * sumW * ww * (sumW * sumW - sumW * ww + ww * ww) /
                (temp * temp * temp) +
            6 * delta2 * (ww * ww * M2) / (temp * temp) +
            4 * delta * (-ww * M3) / temp;
    }

    if constexpr (moments >= 3) {
      M3 += delta * delta2 * sumW * ww * (sumW - ww) / (temp * temp) +
            3 * delta * (-ww * M2) / temp;
    }

    if constexpr (moments >= 2) {
      M2 += delta2 * sumW * ww / temp;
    }

    sumW = temp;
  }

  /// @brief Calculates the population mean, variance, skewness, and kurtosis of
  /// an array in a single pass.
  /// @tparam isWighted If true, calculates weighted statistics and a vector of
  /// weights is expected.
  /// @tparam skipNAN If true, checks for NAN values and skips them.
  /// @tparam moments moments Determines whether to calculate just the mean (if
  /// its value is 1), the mean and variance (if its value is 2), the mean,
  /// variance, and skewness (if its value is 3), or all four moments (if its
  /// value is 4).
  /// @param data An array of doubles.
  /// @param length The length of the array.
  /// @param weights The weights vector.
  /// @param mean The calculated mean
  /// @param variance IF not null, on exit it contains the variance
  /// @param skewness IF not null, on exit it contains the skewness
  /// @param kurtosis IF not null, on exit it contains the variance (it is not
  /// excess kurtosis. I mean for random normal data it is app. 3)
  template <bool isWeighted, bool skipNAN, Ti moments>
  static void Moments(const Tw *data, Ti length, const Tw *weights, Tw &mean,
                      Tw *variance = nullptr, Tw *skewness = nullptr,
                      Tw *kurtosis = nullptr) {
    // I tried to add 'isSample' template, but it is not easy to implement for
    // the weighted version. I think Bessel's correction is different for
    // weighted formula an you cannot just use 'sumW-1'. It seems tha: The
    // formula for weighted sample variance with Bessel’s correction is given
    // by: s ^ 2 = (V1 / (V1 ^ 2 - V2)) * Sum(wi * (xi - mu *)^2)
    // where V1 is the sum of the weights, V2 is the sum of squared weights, wi
    // is the weight of observation i, xi is observation i, and mu *is the
    // weighted mean 2. See http://re-design.dimiter.eu/?p=290
    // note that weighted algorithm is important in ldt, because of the model
    // weights.

    if constexpr (isWeighted) {
      if (!weights)
        throw LdtException(ErrorType::kLogic, "array.h, moments",
                       "weights are missing");
    }

    Tw sumW = 0, m2 = 0, m3 = 0, m4 = 0, w = 1;
    mean = 0;
    for (Ti i = 0; i < length; i++) {

      if constexpr (skipNAN) {
        if (std::isnan(data[i]))
          continue;
      }

      if constexpr (isWeighted) {
        update_single_pass<isWeighted, moments>(data[i], weights[i], sumW, mean,
                                                m2, m3, m4);
      } else if constexpr (true) {
        update_single_pass<isWeighted, moments>(data[i], w, sumW, mean, m2, m3,
                                                m4);
      }
    }

    if (sumW == 0) {
      mean = NAN;
      *variance = NAN;
      *skewness = NAN;
      *kurtosis = NAN;
    } else {

      if constexpr (moments >= 2) {
        *variance = m2 / sumW;
      }

      if constexpr (moments >= 3) {
        *skewness = sqrt(sumW) * m3 / pow(m2, 1.5);
      }

      if constexpr (moments == 4) {
        *kurtosis = sumW * m4 / (m2 * m2) - 3;
      }
    }
  }

  /// @brief Calculates the mean in a single pass
  /// @tparam isWighted If true, calculates weighted statistics and a vector of
  /// weights is expected.
  /// @tparam skipNAN If true, checks for NAN values and skips them.
  /// @param data An array of doubles.
  /// @param length The length of the array.
  /// @param weights The weights vector.
  /// @return The mean
  template <bool isWeighted, bool skipNAN>
  static Tw Mean(const Tw *data, Ti length, const Tw *weights) {
    Tw mean = 0;
    Moments<isWeighted, skipNAN, 1>(data, length, weights, mean);
    return mean;
  }

  /// @brief Calculates the variance in a single pass
  /// @tparam isWighted If true, calculates weighted statistics and a vector of
  /// weights is expected.
  /// @tparam skipNAN If true, checks for NAN values and skips them.
  /// @param data An array of doubles.
  /// @param length The length of the array.
  /// @param weights The weights vector.
  /// @return The variance
  template <bool isWeighted, bool skipNAN>
  static Tw Variance(const Tw *data, Ti length, const Tw *weights) {
    Tw mean = 0, variance = 0;
    Moments<isWeighted, skipNAN, 2>(data, length, weights, mean, &variance);
    return variance;
  }

  /// @brief Calculates the skewness in a single pass
  /// @tparam isWighted If true, calculates weighted statistics and a vector of
  /// weights is expected.
  /// @tparam skipNAN If true, checks for NAN values and skips them.
  /// @param data An array of doubles.
  /// @param length The length of the array.
  /// @param weights The weights vector.
  /// @return The skewness
  template <bool isWeighted, bool skipNAN>
  static Tw Skewness(const Tw *data, Ti length, const Tw *weights) {
    Tw mean = 0, variance = 0, skewness = 0;

    Moments<isWeighted, skipNAN, 3>(data, length, weights, mean, &variance,
                                    &skewness);
    return skewness;
  }

  /// @brief Calculates the kurtosis in a single pass
  /// @tparam isWighted If true, calculates weighted statistics and a vector of
  /// weights is expected.
  /// @tparam skipNAN If true, checks for NAN values and skips them.
  /// @param data An array of doubles.
  /// @param length The length of the array.
  /// @param weights The weights vector.
  /// @return The kurtosis
  template <bool isWeighted, bool skipNAN>
  static Tw Kurtosis(const Tw *data, Ti length, const Tw *weights) {
    Tw mean = 0, variance = 0, skewness = 0, kurtosis = 0;
    Moments<isWeighted, skipNAN, 4>(data, length, weights, mean, &variance,
                                    &skewness, &kurtosis);
    return kurtosis;
  }

  template <bool skipNAN>
  static void Last(const Tw *data, const Ti &length, Tw &last) {

    if (length == 0)
      last = NAN;
    else {

      if constexpr (skipNAN) {
        Tw d;
        last = NAN;
        for (Ti i = length - 1; i >= 0; i--) {
          d = data[i];
          if (std::isnan(d) == false) {
            last = d;
            break;
          }
        }
      } else if constexpr (true) {
        last = data[length - 1];
      }
    }
  }

  template <bool skipNAN>
  static void First(const Tw *data, const Ti &length, Tw &first) {

    if (length == 0)
      first = NAN;
    else {

      if constexpr (skipNAN) {
        Tw d;
        first = NAN;
        for (Ti i = 0; i < length; i++) {
          d = data[i];
          if (std::isnan(d) == false) {
            first = d;
            break;
          }
        }
      } else if constexpr (true) {
        first = data[0];
      }
    }
  }
};

extern template class ldt::Array<Tv>;

} // namespace ldt
