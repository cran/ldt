#pragma once

#include <functional>
#include <iostream>
#include <iterator>
#include <vector>

#include "distributions.h"
#include "ldt_base.h"
#include "matrix.h"

namespace ldt {

/// @brief Types of in-sample measures
enum class GoodnessOfFitType {

  /// @brief Akaike information criterion
  kAic = 50,

  /// @brief Schwartz information criterion
  kSic = 51,

  /// @brief Custom cost matrix, e.g., in binary case
  kCostMatrix = 100,

  /// @brief Area under ROC
  kAuc = 110,
};

/// @brief Converts a \ref GoodnessOfFitType to string
/// @param v The type
/// @param abr If true, an abbreviation is return. Otherwise, a longer
/// description is returned
/// @return String representation of the type
inline const char *ToString(GoodnessOfFitType v, bool abr = true) {
  switch (v) {
  case GoodnessOfFitType::kAic:
    return abr ? "aic" : "Akaike Information Criterion";
  case GoodnessOfFitType::kSic:
    return abr ? "sic" : "Schwartz Information Criterion";
  case GoodnessOfFitType::kCostMatrix:
    return abr ? "costMatrixIn" : "Custom Cost Matrix";
  case GoodnessOfFitType::kAuc:
    return abr ? "aucIn" : "Area Under ROC (In-Sample)";
  default:
    return "[Unknown 'GoodnessOfFitType']";
  }
}

/// @brief Converts a string to a \ref GoodnessOfFitType
/// @param v The string
/// @return The type
inline GoodnessOfFitType FromString_GoodnessOfFitType(const char *v) {
  if (StartsWith("aic", v))
    return GoodnessOfFitType::kAic;
  else if (StartsWith("sic", v))
    return GoodnessOfFitType::kSic;
  else if (StartsWith("costm", v))
    return GoodnessOfFitType::kCostMatrix;
  else if (StartsWith("auc", v))
    return GoodnessOfFitType::kAuc;

  throw std::logic_error(
      std::string("Invalid enum name: 'GoodnessOfFit Type'. value=") +
      std::string(v));
}

/// @brief A helper class for \ref GoodnessOfFitType related methods
class LDT_EXPORT GoodnessOfFit {
public:
  /// @brief Converts a \ref GoodnessOfFitType measure to weight
  /// @param type The type
  /// @param measure The measure
  /// @return The weight
  static Tv ToWeight(const GoodnessOfFitType &type, const Tv &measure);

  /// @brief Converts a a \ref GoodnessOfFitType weight to measure
  /// @param type The type
  /// @param weight The weight
  /// @return The measure
  static Tv FromWeight(const GoodnessOfFitType &type, const Tv &weight);
};

/// @brief Types of out-of-sample measures
enum class ScoringType {

  /// @brief Direction of change (in ordered data such as time-series data)
  kDirection = 0,

  /// @brief Sign
  kSign = 1,

  /// @brief Mean Absolute Error
  kMae = 5,

  /// @brief Similar to 'MAE' but error is divided by the actual value
  kScaledMae = 6,

  /// @brief Root Mean Squared Error
  kRmse = 10,

  /// @brief Similar to 'RMAE', but error is divided by the actual value
  kScaledRmse = 11,

  /// @brief Continuous Ranked Probability Score
  kCrps = 20,

  /// @brief Custom cost matrix, e.g., in binary case
  kCostMatrix = 100,

  /// @brief Area under ROC
  kAuc
};

/// @brief Converts a \ref ScoringRule to string
/// @param v The type
/// @param abr If true, an abbreviation is return. Otherwise, a longer
/// description is returned
/// @return String representation of the type
inline const char *ToString(ScoringType v, bool abr = true) {
  switch (v) {
  case ScoringType::kDirection:
    return "direction";
  case ScoringType::kSign:
    return "sign";

  case ScoringType::kMae:
    return abr ? "mae" : "Mean Absolute Error";
  case ScoringType::kScaledMae:
    return abr ? "scaledMae" : "Scaled Mean Absolute Error";
  case ScoringType::kRmse:
    return abr ? "rmse" : "Root Mean Squared Error";
  case ScoringType::kScaledRmse:
    return abr ? "scaledRmse" : "Scaled Root Mean Squared Error";

  case ScoringType::kCrps:
    return abr ? "crps" : "Continuous Ranked Probability Score";

  case ScoringType::kCostMatrix:
    return abr ? "costMatrixOut" : "Custom Cost Matrix<Tv> (Out-of-Sample)";

  case ScoringType::kAuc:
    return abr ? "aucOut" : "Area Under ROC";

  default:
    return "[Unknown 'ScoringType']";
  }
}

/// @brief Converts a string to a \ref ScoringRule
/// @param v The string
/// @return The type
inline ScoringType FromString_ScoringType(const char *v) {
  if (StartsWith("dir", v))
    return ScoringType::kDirection;
  else if (StartsWith("sig", v))
    return ScoringType::kSign;

  else if (StartsWith("mae", v))
    return ScoringType::kMae;
  else if (StartsWith("scaledmae", v) || StartsWith("maescaled", v))
    return ScoringType::kScaledMae;
  else if (StartsWith("rms", v))
    return ScoringType::kRmse;
  else if (StartsWith("scaledrmse", v) || StartsWith("rmsescaled", v))
    return ScoringType::kScaledRmse;

  else if (StartsWith("crp", v))
    return ScoringType::kCrps;

  else if (StartsWith("costm", v))
    return ScoringType::kCostMatrix;

  else if (StartsWith("auc", v))
    return ScoringType::kAuc;

  throw std::logic_error(
      std::string("Invalid enum name: 'Scoring Type'. value=") +
      std::string(v));
}

/// @brief A helper class for \ref GoodnessOfFitType related methods
class LDT_EXPORT Scoring {
private:
  // DistributionBase* mpDistribution = nullptr;

public:
  /// @brief Calculates the CRPS of a value assuming normal distribution
  /// @param y The value
  /// @param mean Mean of the distribution
  /// @param std Standard error of the distribution
  /// @return CRPS value
  static Tv GetScoreCrpsNormal(Tv y, Tv mean, Tv std);

  /// @brief Calculates the CRPS of a value assuming log-normal distribution
  /// @param y The value
  /// @param mean Mean of the distribution
  /// @param std Standard error of the distribution
  /// @return CRPS value
  static Tv GetScoreCrpsLogNormal(Tv y, Tv meanLog, Tv stdLog);

  /// @brief Gets a score given a type
  /// @param type The scoring type
  /// @param result A place to keep the results
  /// @param act Actual values
  /// @param means Predictions or projections
  /// @param err Errors
  /// @param std Standard errors of the predictions/projections
  /// @param last_m Last values in an ordered data for calculating the
  /// directions
  static void GetScore(ScoringType type, Matrix<Tv> &result, Matrix<Tv> &act,
                       Matrix<Tv> &means, Matrix<Tv> &err, Matrix<Tv> &std,
                       Matrix<Tv> &last_m);

  /// @brief Gets whether calculating a scoring rule requires variance of a
  /// distribution
  /// @param type Type of the scoring rules
  /// @return
  static bool RequiresVariance(const ScoringType &type);

  /// @brief Converts a \ref ScoringRule measure to weight
  /// @param type The type
  /// @param measure The measure
  /// @return The weight
  static Tv ToWeight(const ScoringType &type, const Tv &measure);

  /// @brief Converts a a \ref ScoringRule weight to measure
  /// @param type The type
  /// @param weight The weight
  /// @return The measure
  static Tv FromWeight(const ScoringType &type, const Tv &weight);
};

/// @brief a base class for CostMatrix
class LDT_EXPORT CostMatrixBase {
public:
  /// @brief Gets the required size of the storage array
  Ti StorageSize = 0;

  /// @brief After calculate, total cost of each cost matrix. Length: number of
  /// cost matrices
  Matrix<Tv> CostSums;

  /// @brief After calculate, sum of the weights for each cost matrix. Length:
  /// number of cost matrices
  Matrix<Tv> CostCounts;

  /// @brief After calculate, average cost for all cost matrices
  Tv AverageRatio = 0;

  /// @brief Calculates the results
  /// @param costs List of cost matrices. Length = M
  /// @param y Actual labels. They should start from 0. Length: N
  /// @param scores Calculated scores for each label. Dimension : N x P, where P
  /// is the number of unique choices (E.g., for binary case it must have 2
  /// columns for 0 and 1). An actual label is an index (i.e., starts from 0.
  /// E.g., if 2, the element at the 3-rd column is the score for this label)
  /// @param weights Weight of each label. Length: N
  /// @param storage Storage array of size \ref StorageSize
  virtual void Calculate(const std::vector<Matrix<Tv>> &costs,
                         const Matrix<Tv> &y, const Matrix<Tv> &scores,
                         const Matrix<Tv> *weights, Tv *storage) = 0;

  virtual ~CostMatrixBase(){};
};

/// @brief A class to calculate expected cost based on a cost matrix
/// @tparam hasWeight
template <bool hasWeight> class LDT_EXPORT CostMatrix : public CostMatrixBase {
public:
  /// @brief Initializes a new instance of the class
  CostMatrix(){};

  /// @briefInitializes a new instance of the class
  /// @param count Maximum expected number of cost matrixes
  CostMatrix(Ti count);

  /// @brief Calculates the results
  /// @param costs List of cost matrices. Length = M
  /// @param y Actual labels. They should start from 0. Length: N
  /// @param scores Calculated scores for each label. Dimension : N x P, where P
  /// is the number of unique choices (E.g., for binary case it must have 2
  /// columns for 0 and 1). An actual label is an index (i.e., starts from 0.
  /// E.g., if 2, the element at the 3-rd column is the score for this label)
  /// @param weights Weight of each label. Length: N
  /// @param storage Storage array of size \ref StorageSize
  virtual void Calculate(const std::vector<Matrix<Tv>> &costs,
                         const Matrix<Tv> &y, const Matrix<Tv> &scores,
                         const Matrix<Tv> *weights, Tv *storage) override;

  /// @brief Checks the validity of a cost matrix
  /// @param costMatrix The cost matrix
  /// @param numChoices Number of choices in the actual data
  static void Check(const Matrix<Tv> costMatrix, const Ti &numChoices);
};

extern template class ldt::CostMatrix<true>;
extern template class ldt::CostMatrix<false>;

/// @brief A base class for AUC
class LDT_EXPORT AucBase {
public:
  /// @brief After \ref Calculate, it is AUC
  Tv Result = -1;

  /// @brief Calculate the result
  /// @param y Actual labels. Length = N
  /// @param scores Calculated scores for each label. Dimension : N x P, where P
  /// is the number of unique choices (E.g., for binary case it must have 2
  /// columns for 0 and 1). An actual label is an index (i.e., starts from 0.
  /// E.g., if 2, the element at the 3-rd column is the score for this label)
  /// @param weights Weight of each label. Length: N. It should be null if this
  /// is not a weighted class.
  /// @param multi_class_weights_ See LightGBM source code (It is not currently
  /// implemented)
  virtual void Calculate(Matrix<Tv> &y, Matrix<Tv> &scores, Matrix<Tv> *weights,
                         Matrix<Tv> *multi_class_weights_) = 0;
  virtual ~AucBase(){};
};

/// @brief A class to calculate AUC. Original source: LightGBM source code
/// @tparam hasWeight If true, labels are weighted
/// @tparam isBinary If true, labels are 0 and 1, otherwise, it has more than 1
/// number of choices.
template <bool hasWeight, bool isBinary> class LDT_EXPORT AUC : public AucBase {
public:
  /// @brief Initializes a new instance of the class
  AUC(){};

  /// @brief Initializes a new instance of the class
  /// @param n Number of the observations
  AUC(Ti n){};

  /// @brief Calculate the result
  /// @param y Actual labels. Length = N
  /// @param scores Calculated scores for each label. Dimension : N x P, where P
  /// is the number of unique choices (E.g., for binary case it must have 2
  /// columns for 0 and 1). An actual label is an index (i.e., starts from 0.
  /// E.g., if 2, the element at the 3-rd column is the score for this label)
  /// @param weights Weight of each label. Length: N. It should be null if this
  /// is not a weighted class.
  /// @param multi_class_weights_ See LightGBM source code (It is not currently
  /// implemented)
  virtual void Calculate(Matrix<Tv> &y, Matrix<Tv> &scores, Matrix<Tv> *weights,
                         Matrix<Tv> *multi_class_weights_) override;
};

extern template class ldt::AUC<true, true>;
extern template class ldt::AUC<true, false>;
extern template class ldt::AUC<false, true>;
extern template class ldt::AUC<false, false>;

} // namespace ldt
