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

/// @brief Correlation type
enum class DescriptiveType {

  kMin,
  kMax,
  kMean,
  kVariance,
  kVariancePop,
  kStd,
  kStdPop,

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
  case DescriptiveType::kFirst:
    return "First";
  case DescriptiveType::kLast:
    return "Last";

  default:
    return "[Unknown DescriptiveType Method]";
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
  } else if (StartsWith("last", v))
    return DescriptiveType::kLast;
  else if (StartsWith("firs", v))
    return DescriptiveType::kFirst;

  throw std::logic_error("Invalid enum name: 'DescriptiveType'.");
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
  static void PartitionEqual(const std::vector<Tv> &data,
                             std::vector<std::vector<Tv>> &result, Ti size,
                             bool fromEnd);

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
      Mean<skipNAN>(data, length, result);
      break;
    case DescriptiveType::kVariance: {
      Tw d;
      MeanVariance<skipNAN>(data, length, d, result,
                            true); // sample statistics
    } break;
    case DescriptiveType::kVariancePop: {
      Tw d;
      MeanVariance<skipNAN>(data, length, d, result, false);
    } break;
    case DescriptiveType::kStd: {
      Tw d;
      MeanVariance<skipNAN>(data, length, d, result, true);
      result = std::sqrt(result);
    } break;
    case DescriptiveType::kStdPop: {
      Tw d;
      MeanVariance<skipNAN>(data, length, d, result, false);
      result = std::sqrt(result);
    } break;
    default:
      throw std::logic_error(
          "Invalid or not-implemented type of descriptive statistics.");
    }
  }

  template <bool skipNAN>
  static void Min(const Tw *data, const Ti &length, Tw &min) {
    if (length == 0)
      min = NAN;
    else {
      min = INFINITY;
      Tv d;
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
      Tv d;
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
      double d;
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
      double d;
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

  template <bool skipNAN>
  static void Mean(const Tw *data, const Ti &length, Tw &mean) {
    if (length == 0) {
      mean = NAN;
    } else if (length == 1) {
      mean = data[0];
    } else {
      double d;
      mean = 0.0;
      Ti count;
      if constexpr (skipNAN) {
        count = 0;
      }
      for (Ti i = 0; i < length; i++) {
        d = data[i];

        if constexpr (skipNAN) {
          if (std::isnan(d))
            continue;
          count++;
        }

        mean += d;
      }
      if constexpr (skipNAN) {
        mean = count == 0 ? NAN : mean / (Tw)count;
      } else if constexpr (true) {
        mean = mean / length;
      }
    }
  }

  template <bool skipNAN>
  static void MeanVariance(const Tw *data, const Ti &length, Tw &mean,
                           Tw &variance, bool sample = true) {

    if (length == 0) {
      mean = NAN;
      variance = NAN;
    } else if (length == 1) {
      mean = data[0];
      variance = NAN;
    } else {
      double d;
      double diff;
      mean = 0.0;
      double s;
      variance = 0.0;
      Ti count;
      if constexpr (skipNAN) {
        count = 0;
      }
      for (Ti i = 0; i < length; i++) {
        d = data[i];

        if constexpr (skipNAN) {
          if (std::isnan(d))
            continue;
          count++;
        }

        diff = d - mean;
        s = diff / (i + 1);
        mean += s;
        variance += diff * s * i;
      }
      if constexpr (skipNAN) {
        variance = sample ? variance / (count - 1) : variance / count;
      } else if constexpr (true) {
        variance = sample ? variance / (length - 1) : variance / length;
      }
    }
  }

  template <bool skipNAN>
  static void Last(const Tw *data, const Ti &length, Tw &last) {

    if (length == 0)
      last = NAN;
    else {

      if constexpr (skipNAN) {
        Tv d;
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
        Tv d;
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
