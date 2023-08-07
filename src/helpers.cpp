/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the GPL 3.0.
 See accompanying file LICENSE
*/

#include "helpers.h"

using namespace ldt;

bool StartsWith(const char *code, const char *str) {
  return boost::starts_with(str, code);
}

bool EndsWith(const char *code, const char *str) {
  return boost::ends_with(str, code);
}

bool AreEqual_i(const char *first, const char *second) {
  return boost::iequals(first, second);
}

bool AreEqual(const char *first, const char *second) {
  return boost::equals(first, second);
}

double dist_normal_pdf(double x, double mean, double std) {
  auto n = (x - mean) / std;
  return std::exp(-0.5 * n * n) / (c_sqrt_2Pi * std);
}
double dist_normal_pdf_ln(double x, double mean, double std) {
  auto n = (x - mean) / std;
  return (-0.5 * n * n) - std::log(std) - c_ln_sqrt2Pi;
}
double dist_normal_cdf(double x, double mean, double std) {
  if (std::isinf(x) && x > 0)
    return 1.0;
  if (std::isinf(x) && x < 0)
    return 0.0;
  return 0.5 * erfc((mean - x) / (std * c_sqrt2));
}
double dist_normal_cdfInv(double p, double mean, double std) {
  if (p == 0)
    return -INFINITY;
  if (p == 1.0)
    return INFINITY;
  return mean + (std * c_sqrt2 * Math_ErfInv(2.0 * p - 1.0));
}
