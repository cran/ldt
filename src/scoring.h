#pragma once

#include <iterator>
#include <vector>

#include "distributions.h"
#include "ldt_base.h"
#include "matrix.h"

namespace ldt {

/// @brief Types of in-sample metrics
enum class GoodnessOfFitType {

  /// @brief Akaike information criterion
  kAic = 50,

  /// @brief Schwartz information criterion
  kSic = 51,

  /// @brief Custom frequency cost matrix, e.g., in binary case
  kFrequencyCost = 100,

  /// @brief Area under ROC
  kAuc = 110,

  /// @brief Brier score
  kBrier = 115
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
  case GoodnessOfFitType::kFrequencyCost:
    return abr ? "frequencyCostIn" : "Custom Frequency Cost";
  case GoodnessOfFitType::kAuc:
    return abr ? "aucIn" : "Area Under ROC (In-Sample)";
  case GoodnessOfFitType::kBrier:
    return abr ? "brierIn" : "Brier Score (In-Sample)";
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
  else if (StartsWith("freq", v))
    return GoodnessOfFitType::kFrequencyCost;
  else if (StartsWith("auc", v))
    return GoodnessOfFitType::kAuc;
  else if (StartsWith("bri", v))
    return GoodnessOfFitType::kBrier;

  throw LdtException(
      ErrorType::kLogic, "scoring.h",
      format("invalid or not implemented goodness of fit (name={})", v));
}

/// @brief A helper class for \ref GoodnessOfFitType related methods
class LDT_EXPORT GoodnessOfFit {
public:
  /// @brief Converts a \ref GoodnessOfFitType metricsIn to weight
  /// @param type The type
  /// @param metricsIn The metric
  /// @param minMetrics minimum value for metric to be used in AIC weight
  /// formula: exp(-0.5*(metric-minMetric))
  /// @return The weight
  static Tv ToWeight(const GoodnessOfFitType &type, const Tv &metric,
                     const double &minMetric = NAN);

  /// @brief Converts a a \ref GoodnessOfFitType weight to metric
  /// @param type The type
  /// @param weight The weight
  /// @param minMetrics minimum value for metric used in calculating the
  /// weight based on AIC weight formula: exp(-0.5*(metric-minMetric))
  /// @return The metric
  static Tv FromWeight(const GoodnessOfFitType &type, const Tv &weight,
                       const double &minMetric = NAN);

  static bool IsPositiveOriented(const GoodnessOfFitType &type);
};

/// @brief Types of out-of-sample metrics
enum class ScoringType {

  /// @brief Direction of change (in ordered data such as time-series data)
  kDirection = 0,

  /// @brief Sign
  kSign = 1,

  /// @brief Mean Absolute Error
  kMae = 5,

  /// @brief Mean Absolute Percentage Error
  kMape = 6,

  /// @brief Root Mean Squared Error
  kRmse = 10,

  /// @brief Root Mean Squared Percentage Error
  kRmspe = 11,

  /// @brief Continuous Ranked Probability Score
  kCrps = 20,

  /// @brief Custom frequency cost matrix, e.g., in binary case
  kFrequencyCost = 100,

  /// @brief Area under ROC
  kAuc = 110,

  /// @brief Brier score
  kBrier = 115
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
  case ScoringType::kMape:
    return abr ? "mape" : "Mean Absolute Percentage Error";
  case ScoringType::kRmse:
    return abr ? "rmse" : "Root Mean Squared Error";
  case ScoringType::kRmspe:
    return abr ? "rmspe" : "Root Mean Squared Percentage Error";

  case ScoringType::kCrps:
    return abr ? "crps" : "Continuous Ranked Probability Score";

  case ScoringType::kFrequencyCost:
    return abr ? "frequencyCostOut"
               : "Custom Frequency Cost<Tv> (Out-of-Sample)";

  case ScoringType::kAuc:
    return abr ? "aucOut" : "Area Under ROC (Out-of-Sample)";

  case ScoringType::kBrier:
    return abr ? "brierOut" : "Brier Score (Out-of-Sample)";

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
  else if (StartsWith("map", v))
    return ScoringType::kMape;
  else if (StartsWith("rms", v))
    return ScoringType::kRmse;
  else if (StartsWith("rmsp", v))
    return ScoringType::kRmspe;

  else if (StartsWith("crp", v))
    return ScoringType::kCrps;

  else if (StartsWith("freq", v))
    return ScoringType::kFrequencyCost;

  else if (StartsWith("auc", v))
    return ScoringType::kAuc;

  else if (StartsWith("bri", v))
    return ScoringType::kBrier;

  throw LdtException(ErrorType::kLogic, "scoring.h",
                     format("invalid or not implemented scoring (name={})", v));
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

  /// @brief Gets whether calculating a scoring rule requires variance of a
  /// distribution
  /// @param type Type of the scoring rules
  /// @return
  static bool RequiresVariance(const ScoringType &type);

  /// @brief Converts a \ref ScoringRule metric to weight
  /// @param type The type
  /// @param metric The metric
  /// @param minMetrics minimum value for metric to be used in AIC weight
  /// formula: exp(-0.5*(metric-minMetric))
  /// @return The weight
  static Tv ToWeight(const ScoringType &type, const Tv &metric,
                     const double &minMetric = NAN);

  /// @brief Converts a a \ref ScoringRule weight to metric
  /// @param type The type
  /// @param weight The weight
  /// @param minMetrics minimum value for metric used in calculating the
  /// weight based on AIC weight formula: exp(-0.5*(metric-minMetric))
  /// @return The metric
  static Tv FromWeight(const ScoringType &type, const Tv &weight,
                       const double &minMetric = NAN);

  static bool IsPositiveOriented(const ScoringType &type);
};

/// @brief a base class for FrequencyCost
class LDT_EXPORT FrequencyCostBase {
public:
  /// @brief Gets the required size of the storage array
  Ti StorageSize = 0;

  /// @brief After calculate, total cost of each frequency cost matrix. Length:
  /// number of cost matrices
  Matrix<Tv> CostSums;

  /// @brief After calculate, sum of the weights for each frequency cost matrix.
  /// Length: number of cost matrices
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

  virtual ~FrequencyCostBase(){};
};

/// @brief A class to calculate expected cost based on a frequency cost matrix
/// @tparam hasWeight
template <bool hasWeight>
class LDT_EXPORT FrequencyCost : public FrequencyCostBase {
public:
  /// @brief Initializes a new instance of the class
  FrequencyCost(){};

  /// @briefInitializes a new instance of the class
  /// @param count Maximum expected number of frequency cost matrixes
  FrequencyCost(Ti count);

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

  /// @brief Checks the validity of a frequency cost matrix
  /// @param frequencyCost The frequency cost matrix
  /// @param numChoices Number of choices in the actual data
  static void Check(const Matrix<Tv> frequencyCost, const Ti &numChoices);
};

extern template class ldt::FrequencyCost<true>;
extern template class ldt::FrequencyCost<false>;

struct LDT_EXPORT RocOptions {

  /// @brief If false, AUC is calculated without normalizing
  /// \par Points (It is faster, but you can't draw the ROC properly)
  bool NormalizePoints = true;

  /// @brief Lower bound for calculating partial AUC
  Tv LowerThreshold = NAN;

  /// @brief Upper bound for calculating partial AUC
  Tv UpperThreshold = NAN;

  /// @brief A value to ignore small floating point differences in comparing
  /// scores.
  Tv Epsilon = 0;

  /// @brief If true, sequences of equally scored instances are
  /// treated differently and a pessimistic metric is calculated (see Fawcett
  /// (2006) An introduction to roc analysis, fig. 6).
  bool Pessimistic = false;

  /// @brief (N x 1) cost of observations (If its Data is null, cost of all
  /// observations will be 1)
  Matrix<Tv> Costs;

  /// @brief  (2 x 2) cost matrix in which: (1,1) is cost of TN,
  /// (2,2) is cost of TP, (1,2) is cost of FP and (2,1) is cost of FN. First
  /// column is multiplied by the corresponding value in \par costs vector (see
  /// Fawcett (2006), ROC graphs with instance-varying costs). Its Data must not
  /// be null if \par Costs's Data is not.
  Matrix<Tv> CostMatrix;
};

/// @brief A base class for ROC
class LDT_EXPORT RocBase {
public:
  /// @brief After \ref Calculate, it is AUC or partial AUC (divided by
  /// length of the bound)
  Tv Result = -1;

  /// @brief After \ref Calculate, it contains the curve points
  std::vector<std::tuple<Tv, Tv>> Points;

  virtual void Calculate(const Matrix<Tv> &y, const Matrix<Tv> &scores,
                         const Matrix<Tv> *weights,
                         const RocOptions &options) = 0;
  virtual ~RocBase(){};
};

/// @brief A class to calculate ROC points and AUC for binary model.
/// @tparam hasWeight If true, labels are weighted
/// @tparam hasCost If true, a vector of variable costs and a cost matrix is
/// expected
template <bool hasWeight, bool hasCost> class LDT_EXPORT ROC : public RocBase {
public:
  /// @brief Initializes a new instance of the class
  ROC();

  /// @brief Initializes a new instance of the class
  /// @param n Number of the observations
  ROC(Ti n);

  /// @brief Calculate the result
  /// @param y Actual labels. Length = N
  /// @param scores (N x 1) Calculated probabilities for negative observations
  /// (y=0).
  /// @param weights Weight of each label. Length: N. It should be null if this
  /// is not a weighted class.
  /// @param options Options in calculating AUC
  virtual void Calculate(const Matrix<Tv> &y, const Matrix<Tv> &scores,
                         const Matrix<Tv> *weights,
                         const RocOptions &options) override;
};

extern template class ldt::ROC<true, true>;
extern template class ldt::ROC<true, false>;
extern template class ldt::ROC<false, true>;
extern template class ldt::ROC<false, false>;

} // namespace ldt
