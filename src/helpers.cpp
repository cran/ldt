/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the LGPL 3.0.
 See accompanying file LICENSE
*/

#include "helpers.h"

LDT_EXPORT bool StartsWith(const char *code, const char *str) {
  return boost::starts_with(str, code);
}

bool AreEqual_i(const char *first, const char *second) {
  return boost::iequals(first, second);
}

bool AreEqual(const char *first, const char *second) {
  return boost::equals(first, second);
}

void Rethrow(const char *msg, bool logic) {

  auto f = [&msg, &logic](const char *e_what,
                          const std::string &details_header) -> void {
    auto msg0 = std::string(msg) + std::string("[") +
                std::string(details_header) + std::string(": ") +
                std::string(e_what) + std::string("]");
    if (logic)
      throw std::logic_error(msg0);
    else
      throw std::runtime_error(msg0);
  };

  try {
    throw;
  } catch (const std::out_of_range &e) {
    f(e.what(), "out of range");
  } catch (const std::logic_error &e) {
    f(e.what(), "logic");
  } catch (const std::system_error &e) {
    f(e.what(), "system");
  } catch (const std::runtime_error &e) {
    f(e.what(), "runtime");
  } catch (const std::exception &e) {
    f(e.what(), "generic");
  } catch (const std::string &e) {
    f(e.c_str(), "general");
  } catch (const char *e) {
    f(e, "general");
  } catch (...) {
    f("an unknown error occurred.", "general");
  }
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
