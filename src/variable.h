#pragma once

#include "frequency.h"
#include "helpers.h"
#include "ldt_base.h"
#include <map>
#include <random>
#include <string>
#include <vector>

namespace ldt {

/// @brief A class for representing vector of data with name, frequency and
/// field
/// @tparam Tw Type of data
template <class Tw = Tv> class LDT_EXPORT Variable {

public:
  /// @brief Data of the variable.
  /// array
  std::vector<Tw> Data;

  /// @brief A pointer to the frequency of the first data-point.
  std::unique_ptr<Frequency> StartFrequency;

  /// @brief Name of the variable
  std::string Name = std::string("V");

  /// @brief Fields of the variable. Keys must not contain ';' or parsing fails
  std::map<std::string, std::string> Fields;

  /// @brief Initializes a new instance of this class
  Variable();

  /// @brief Gets the end frequency
  /// @return End frequency
  std::unique_ptr<Frequency> GetEndFrequency() const;

  bool IsEqualTo(Variable<Tw> &other, const Tv &epsilon = 1e-14) const;

  /// @brief Converts this variable to a string (compact form)
  /// @return The string
  std::string ToString() const;

  /// @brief Parses a string (compact form) to a variable
  /// @param str The string
  /// @param variable On exit, the updated variable
  /// @param listItemsString On exit, if frequency is of type string list, it is
  /// its items.
  /// @param listItemsDate n exit, if frequency is of type string list, it is
  /// its items.
  static void Parse(const std::string &str, Variable<Tw> &result,
                    std::vector<std::string> &listItemsString,
                    std::vector<boost::gregorian::date> &listItemsDate);

  /// @brief Generates a list of variables with all available frequencies
  /// (generally to be used in testing)
  /// @param values On exit, it contains the examples
  /// @param f_values On exit, it contains the frequency of the examples
  /// @param listItemsString On exit, it contains the list items for the string
  /// list frequency
  /// @param listItemsDate On exit, it contains the list items for the date list
  /// frequency
  /// @param end rnd
  static void Examples(std::vector<std::unique_ptr<Variable<Tw>>> &values,
                       std::vector<std::unique_ptr<Frequency>> &f_values,
                       std::vector<std::string> &listItemsString,
                       std::vector<boost::gregorian::date> &listItemsDate,
                       std::mt19937 &eng);
};

/// @brief A class for grouping a list of variables with similar frequencies
/// @tparam Tw Type of data
template <class Tw = Tv> class LDT_EXPORT Variables {

public:
  /// @brief Data of the variable. You can create a column major matrix from it
  /// array
  std::vector<Tw> Data;

  /// @brief A pointer to the frequency of the first data-point.
  std::unique_ptr<Frequency> StartFrequency;

  /// @brief Number of observations
  Ti NumObs = 0;

  /// @brief Names of the variables
  std::vector<std::string> Names;

  /// @brief Initializes a new instance of this class
  /// @param vars List of variables
  Variables(const std::vector<Variable<Tw> *> vars);
};

extern template class ldt::Variable<Tv>;
// extern template class ldt::Variable < Ti>;

extern template class ldt::Variables<Tv>;
// extern template class ldt::Variable < Ti>;

} // namespace ldt
