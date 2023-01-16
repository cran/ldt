#pragma once

#include "ldt_base.h"
#include <algorithm> // std::sort, std::stable_sort

#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/join.hpp>
#include <boost/math/special_functions/beta.hpp>
#include <boost/math/special_functions/erf.hpp>
#include <boost/math/special_functions/gamma.hpp>
#include <boost/range/adaptor/transformed.hpp>

#include <cmath>
#include <cstring>
#include <functional>
#include <iostream>
#include <iterator>
#include <numeric> // std::iota
#include <type_traits>
#include <vector>

// #pragma region Constants

/// calculations are from https://keisan.casio.com/calculator

constexpr double c_pi = 3.14159265358979323846264338327950288;

constexpr double c_sqrt_pi =
    1.7724538509055160272981674833411451827975494561224;

constexpr double c_sqrt_pi_inv =
    0.564189583547756286948079451560772585844050629329;

constexpr double c_sqrt_2Pi =
    2.5066282746310005024157652848110452530069867406099;

constexpr double c_ln_sqrt2Pi =
    0.91893853320467274178032973640561763986139747363778;

constexpr double c_sqrt2 = 1.414213562373095048801688724209698078569671875377;

constexpr double c_ln2 = 0.69314718055994530941723212145817656807550013436026;

constexpr double c_pi_pow_2 =
    9.8696044010893586188344909998761511353136994072408;

constexpr double c_pi_pow_4 =
    97.409091034002437236440332688705111249727585672685;

constexpr double c_zeta3 = 1.2020569031595942853997381615114499907649862923405;

const double c_ln_2Pi = 1.8378770664093454835606594728112352797227949472756;

const double c_ln_2Pi_plus_one =
    2.8378770664093454835606594728112352797227949472756;

// #pragma endregion

// #pragma region Statistics

double dist_normal_pdf(double x, double mean = 0.0, double std = 1.0);

double dist_normal_pdf_ln(double x, double mean = 0.0, double std = 1.0);

double dist_normal_cdf(double x, double mean = 0.0, double std = 1.0);

double dist_normal_cdfInv(double p, double mean, double std);

// #pragma endregion

// #pragma region Functions

template <typename Tw = Tv> constexpr Tw Math_ConstantEuler() {
  return boost::math::constants::euler<Tw>();
}

template <typename Tw = Tv> Tw Math_DiGamma(Tw x) {
  return boost::math::digamma(x);
}

template <typename Tj = Ti, typename Tw = Tv> Tw Math_PolyGamma(Tj n, Tw x) {
  return boost::math::polygamma(n, x);
}

template <typename Tw = Tv> Tw Math_Beta(Tw x, Tw y) {
  return boost::math::beta(x, y);
}

template <typename Tw = Tv> Tw Math_iBeta(Tw x, Tw y, Tw z) {
  return boost::math::ibeta(x, y, z);
}

template <typename Tw = Tv> Tw Math_iBetaInv(Tw x, Tw y, Tw z) {
  return boost::math::ibeta_inv(x, y, z);
}

template <typename Tw = Tv> Tw Math_GammaPInv(Tw x, Tw y) {
  return boost::math::gamma_p_inv(x, y);
}

template <typename Tw = Tv> Tw Math_GammaP(Tw x, Tw y) {
  return boost::math::gamma_p(x, y);
}

template <typename Tw = Tv> Tw Math_ErfInv(Tw x) {
  return boost::math::erf_inv(x);
}

template <typename Tw = Tv>
Tw Math_BinomialCoefficient(unsigned int x, unsigned int y) {
  return boost::math::binomial_coefficient<Tw>(x, y);
}

// #pragma endregion

// #pragma region Exception

/// @brief A centralized method for rethrowing a exception with details
/// @param msg A header message
/// @param logic true for 'logic_error_, false for 'runtime_error'.
LDT_EXPORT void Rethrow(const char *msg, bool logic = true);

// #pragma endregion

// #pragma region Other

/// @brief Sorts an array and keeps the sorting indices  (for a description,
/// see: https://stackoverflow.com/a/12399290/5615980)
/// @tparam Tw Type of data
/// @param v The array
/// @param length Length of the array
/// @param result A place to keep the sorting indices
template <typename Tw = Tv>
void SortIndexes(const Tw *v, Ti length, std::vector<Ti> &result) {
  result.resize(length);
  std::iota(result.begin(), result.end(), 0);
  std::stable_sort(result.begin(), result.end(),
                   [&v](Ti i1, Ti i2) { return v[i1] < v[i2]; });
}

/// @brief Sorts a vector and keeps the sorting indices (for a description,
/// see: https://stackoverflow.com/a/12399290/5615980)
/// @tparam Tw Type of data
/// @param v The vector
/// @param result A place to keep the sorting indices
template <typename Tw = Tv>
void SortIndexes(const std::vector<Tw> &v, std::vector<Ti> &result) {
  result.resize(v.size());
  std::iota(result.begin(), result.end(), 0);
  std::stable_sort(result.begin(), result.end(),
                   [&v](Ti i1, Ti i2) { return v.at(i1) < v.at(i2); });
}

inline static void Split(const std::string &str, const std::string &delim,
                         std::vector<std::string> &result) {
  size_t start = 0;
  size_t end = str.find(delim);
  while (end != std::string::npos) {
    result.push_back(str.substr(start, end - start));
    start = end + delim.length();
    end = str.find(delim, start);
  }
  result.push_back(str.substr(start, end - start));
}

/// @brief Splits a string by multiple characters
/// @param str The string
/// @param delims The multiple characters
/// @param result The parts
inline static void SplitMultiple(const std::string &str,
                                 const std::string &delims,
                                 std::vector<std::string> &result) {
  size_t start = 0;
  size_t end = str.find_first_of(delims);
  while (end != std::string::npos) {
    result.push_back(str.substr(start, end - start));
    start = end + 1;
    end = str.find_first_of(delims, start);
  }
  result.push_back(str.substr(start, end - start));
}

/// @brief Joins the vectors of element with a delimiter
/// @tparam T Type of data
/// @param values The vector
/// @param delimiter The delimiter
/// @return The string
template <typename T = std::string>
inline static std::string Join(const std::vector<T> values,
                               const std::string &delimiter,
                               std::function<std::string(T)> &fun) {
  return boost::algorithm::join(boost::adaptors::transform(values, fun),
                                delimiter);
}

/// @brief Finds the first index of an element in a vector
/// @tparam T Type of data
/// @param vector The vector
/// @param element The element
//  / @return Index of element in vector. It is -1 if element is not found.
template <typename T> int IndexOf(const std::vector<T> &vector, T element) {
  // Find given element in vector
  auto it = std::find(vector.begin(), vector.end(), element);
  if (it != vector.end())
    return (int)std::distance(vector.begin(), it);
  else
    return -1;
}

/// @brief converts an array to CSV string
/// @tparam T type of the array
/// @param vec the array
/// @param size the length of the array
/// @param sep separator
/// @return a CSV string
template <typename T>
std::string VectorToCsv(const T *vec, int size, char sep = ',') {
  std::ostringstream str;
  str << "Vector (size=" << size << "): ";
  if (size == 0) {
    str << "empty!";
    return str.str();
  }
  for (auto i = 0; i < size; i++) {
    str << vec[i];
    if (i != size - 1) {
      str << sep << ' ';
    }
  }
  return str.str();
}

/// @brief converts a vector to CSV string
/// @tparam T type of the vector
/// @param sep seperator
/// @return a CSV string
template <typename T>
std::string VectorToCsv(const std::vector<T> &vec, char sep = ',') {
  int size = (int)vec.size();
  std::ostringstream str;
  str << "Vector(size=" << size << "): ";
  if (size == 0) {
    str << "empty!";
    return str.str();
  }
  int i = -1;
  for (auto &s : vec) {
    i++;
    str << s;
    if (i != size - 1) {
      str << sep << ' ';
    }
  }
  return str.str();
}

/// @brief Determines if vector contains an element (using 'std::find' function)
/// @tparam T type of the vector
/// @param vec the vector
/// @param element the element
/// @return true if element is found, false otherwise
template <typename T>
bool Contains(const std::vector<T> &vec, const T &element) {
  if (std::find(vec.begin(), vec.end(), element) != vec.end())
    return true;
  return false;
}

/// @brief Determines if a character array starts with another (smaller)
/// character array
/// @param code smaller character array
/// @param str the original array
/// @return true if \p str starts with \p code, false otherwise
LDT_EXPORT bool StartsWith(const char *code, const char *str);

LDT_EXPORT bool AreEqual_i(const char *first, const char *second);

LDT_EXPORT bool AreEqual(const char *first, const char *second);

// #pragma endregion
