#pragma once

#include "ldt_base.h"
#include "matrix.h"
#include <string>

namespace ldt {

/// @brief Correlation type
enum class CorrelationType {
  /// @brief Correlation (i.e., vectors are standardized)
  kCorrelation,

  /// @brief Covariance (i.e., vectors are demeaned)
  kCovariance,

};

/// @brief Correlation method
enum class CorrelationMethod {

  /// @brief Pearson correlation
  kPearson,

  /// @brief Spearman correlation
  kSpearman,

};

/// @brief Converts a value of \ref CorrelationMethod to string
/// @param v The value
/// @return The string
inline const char *ToString(CorrelationMethod v) {
  switch (v) {
  case CorrelationMethod::kPearson:
    return "Pearson";
  case CorrelationMethod::kSpearman:
    return "Spearman";
  default:
    return "[Unknown Correlation Method]";
  }
}

/// @brief Converts a string into a value of \ref CorrelationMethod
/// @param v The string
/// @return The value
inline CorrelationMethod FromString_CorrelationMethod(const char *v) {
  if (StartsWith("pea", v))
    return CorrelationMethod::kPearson;
  else if (StartsWith("spe", v))
    return CorrelationMethod::kSpearman;

  throw LdtException(ErrorType::kLogic, "correlation.h",
                 "invalid or not implemented correlation method");
}

/// @brief A base class for correlation
class LDT_EXPORT CorrelationBase {
protected:
  Ti mRows = 0, mCols = 0;

public:
  /// @brief Gets the required storage size
  Ti StorageSize = 0;

  /// @brief Gets the required work size
  Ti WorkSize = 0;

  /// @brief Means of columns/rows in data used to Calculate the directions
  Matrix<Tv> Means;

  /// @brief Variances of columns/rows in data used to Calculate the directions
  Matrix<Tv> Variances;

  /// @brief After calculate, given an (M x N) Matrix, it is whether (M x M)
  /// (\ref mByColumn=false) or (N x N) (\ref mByColumn=true)
  Matrix<Tv> Result;

  /// @brief If check_nan is true, it shows the number of data points used in
  /// the calculations
  MatrixSym<true> ResultCounts;

  CorrelationBase(){};

  virtual ~CorrelationBase(){};

  virtual void Calculate(const Matrix<Tv> &mat, Tv *work, Tv *storage,
                         bool adjustDoF, bool setLower) = 0;
};

/// @brief Correlation class
/// @tparam checkNaN If true, data is checked for NAN (can be slower)
/// @tparam type Type of the correlation
/// @tparam method Method of the correlation
template <bool checkNaN = false,
          CorrelationType type = CorrelationType::kCorrelation,
          CorrelationMethod method = CorrelationMethod::kPearson>
class LDT_EXPORT Correlation : public CorrelationBase {

public:
  /// @brief If true, correlation between the columns are requested. Otherwise,
  /// correlation between the rows.
  bool mByColumn = true;

  /// @brief Initializes a new instance of this class
  /// @param rows Maximum expected number of rows
  /// @param cols Maximum expected number of columns
  /// @param byColumn It true, it calculates correlations between the column.
  /// (otherwise is not implemented yet)
  Correlation(Ti rows, Ti cols, bool byColumn = true);

  /// @brief Calculates the correlations
  /// @param mat An (M x N) matrix
  /// @param work Work array of size \ref WorkSize
  /// @param storage Storage array of size \ref StorageSize
  /// @param adjustDoF If true, it adjust degrees of freedom (length-1) in
  /// calculating covariances
  /// @param setLower If true, copies the calculated values from the upper part
  /// of the Result to the lower part
  virtual void Calculate(const Matrix<Tv> &mat, Tv *work, Tv *storage,
                         bool adjustDoF = false, bool setLower = true);

private:
  void pearson(const Matrix<Tv> &mat, Tv *work, bool adjustDoF, bool setLower);
  void spearman(const Matrix<Tv> &mat, Tv *work, Tv *storage, bool adjustDoF,
                bool setLower);
};

// Pearson
extern template class ldt::Correlation<false, CorrelationType::kCorrelation,
                                       CorrelationMethod::kPearson>;
extern template class ldt::Correlation<false, CorrelationType::kCovariance,
                                       CorrelationMethod::kPearson>;
extern template class ldt::Correlation<true, CorrelationType::kCorrelation,
                                       CorrelationMethod::kPearson>;
extern template class ldt::Correlation<true, CorrelationType::kCovariance,
                                       CorrelationMethod::kPearson>;

// Spearman:
extern template class ldt::Correlation<false, CorrelationType::kCorrelation,
                                       CorrelationMethod::kSpearman>;
extern template class ldt::Correlation<false, CorrelationType::kCovariance,
                                       CorrelationMethod::kSpearman>;
extern template class ldt::Correlation<true, CorrelationType::kCorrelation,
                                       CorrelationMethod::kSpearman>;
extern template class ldt::Correlation<true, CorrelationType::kCovariance,
                                       CorrelationMethod::kSpearman>;

} // namespace ldt
