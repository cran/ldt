#pragma once

#include "correlation.h"
#include "helpers.h"
#include "ldt_base.h"
#include "matrix.h"

#include <set>
#include <string>

namespace ldt {

/// @brief Methods for calculating distance
enum class DistanceMethod {

  /// @brief sum((x_i-y_i)^2)
  kEuclidean = 0,

  /// @brief sum(|x_i-y_i|)
  kManhattan = 1,

  /// @brief Maximum distance
  kMaximum = 2,

  /// @brief correlation Distance. sqrt(0.5*(1-r)) see Van Dongen, S., &
  /// Enright, A. J. (2012). Metric Distances derived from cosine similarity and
  /// Pearson and Spearman correlations. arXiv preprint arXiv:1208.3145
  kCorrelation = 3,

  /// @brief Absolute correlation Distance. sqrt(1-r^2) see Van Dongen, S., &
  /// Enright,
  /// A. J. (2012). Metric Distances derived from cosine similarity and Pearson
  /// and Spearman correlations. arXiv preprint arXiv:1208.3145
  kAbsCorrelation = 4
};

/// @brief Converts a value of \ref DistanceMethod to string
/// @param v The value
/// @return The string
inline const char *ToString(DistanceMethod v) {
  switch (v) {
  case ldt::DistanceMethod::kEuclidean:
    return "Euclidean";
  case ldt::DistanceMethod::kManhattan:
    return "Manhattan";
  case ldt::DistanceMethod::kMaximum:
    return "Maximum";
  case ldt::DistanceMethod::kCorrelation:
    return "Correlation";
  case ldt::DistanceMethod::kAbsCorrelation:
    return "Absolute Correlation";
  default:
    return "[Unknown Distance Method]";
  }
}

/// @brief Converts a string to a value of \ref DistanceMethod
/// @param v The string
/// @return The value
inline DistanceMethod FromString_DistanceMethod(const char *v) {
  if (StartsWith("euc", v))
    return DistanceMethod::kEuclidean;
  else if (StartsWith("man", v))
    return DistanceMethod::kManhattan;
  else if (StartsWith("max", v))
    return DistanceMethod::kMaximum;
  else if (StartsWith("abs", v))
    return DistanceMethod::kAbsCorrelation;
  else if (StartsWith("cor", v))
    return DistanceMethod::kCorrelation;

  throw LdtException(ErrorType::kLogic, "clustering.h",
                     "invalid or not implemented distance method");
}

/// @brief A base class for calculating distance
class LDT_EXPORT DistanceBase {

public:
  /// @brief Gets the required storage size
  Ti StorageSize = 0;

  /// @brief Gets the required work size
  Ti WorkSize = 0;

  DistanceBase(){};

  /// @brief After \ref Calculate, it contains the result
  MatrixSym<false> Result;

  /// @brief Get distance class from the types
  /// @param checkNan A value for the parameter in the function
  /// @param distMethod A value for the parameter in the function
  /// @param corrMethod A value for the parameter in the function
  /// @param rows Maximum expected number of rows
  /// @param cols Maximum expected number of columns
  /// @return A pointer to the class
  static std::unique_ptr<DistanceBase> GetFromType(bool checkNan,
                                                   DistanceMethod distMethod,
                                                   CorrelationMethod corrMethod,
                                                   Ti rows, Ti cols);

  virtual void Calculate(const Matrix<Tv> &data, Tv *storage, Tv *work) = 0;
};

/// @brief A class to calculate distance
/// @tparam checkNan If true, it checks for NAN data
/// @tparam method Distance method
/// @tparam corrMethod Correlation method if distance method is related to
/// correlation
template <bool checkNan = false,
          DistanceMethod method = DistanceMethod::kEuclidean,
          CorrelationMethod corrMethod = CorrelationMethod::kPearson>
class LDT_EXPORT Distance : public DistanceBase {

public:
  /// @brief Initializes a new instance of the class
  /// @param rows Maximum expected number of rows
  /// @param cols Maximum expected number of columns
  Distance(Ti rows, Ti cols);

  /// @brief Calculates the results
  /// @param data Data
  /// @param storage Storage array of size \ref StorageSize
  /// @param work Work array of size \ref WorkSize
  virtual void Calculate(const Matrix<Tv> &data, Tv *storage,
                         Tv *work) override;
};

/// @brief The linkage criterion. Determines how Distances are calculated in a
/// left-right node merge
enum class HClusterLinkage {

  /// @brief Minimum of the left and right Distances
  kSingle = 0,

  /// @brief Maximum of the left and right Distances
  kComplete = 1,

  /// @brief See https://en.wikipedia.org/wiki/UPGMA
  kAverageU = 2,

  /// @brief See https://en.wikipedia.org/wiki/WPGMA
  kAverageW = 3,

  /// @brief Ward's minimum variance criterion (I use Lance-Williams formula see
  /// https://en.wikipedia.org/wiki/Ward%27s_method)
  kWard = 6
};

/// @brief Converts a value of \ref HClusterLinkage to string
/// @param v The value
/// @return The string
inline const char *ToString(HClusterLinkage v) {
  switch (v) {
  case ldt::HClusterLinkage::kSingle:
    return "Single";
  case ldt::HClusterLinkage::kComplete:
    return "Complete";
  case ldt::HClusterLinkage::kAverageU:
    return "AverageU (UPGMA)";
  case ldt::HClusterLinkage::kAverageW:
    return "AverageW (WPGMA)";
  case ldt::HClusterLinkage::kWard:
    return "Ward";
  default:
    return "[Unknown Cluster Linkage]";
  }
}

/// @brief Converts a string to a value of \ref HClusterLinkage
/// @param v The string
/// @return The value
inline HClusterLinkage FromString_HClusterLinkage(const char *v) {
  if (StartsWith("sin", v))
    return HClusterLinkage::kSingle;
  else if (StartsWith("com", v))
    return HClusterLinkage::kComplete;
  else if (AreEqual_i("averageu", v) || StartsWith("uav", v) ||
           StartsWith("upg", v))
    return HClusterLinkage::kAverageU;
  else if (AreEqual_i("averagew", v) || StartsWith("wav", v) ||
           StartsWith("wpg", v))
    return HClusterLinkage::kAverageW;
  else if (StartsWith("war", v))
    return HClusterLinkage::kWard;

  throw LdtException(ErrorType::kLogic, "clustering.h",
                     "invalid or not implemented h-cluster linkage");
}

/// @brief A cluster node
struct HClusterNode {
  /// @brief Unique id for this node
  Ti id = 0;

  /// @brief Id of the left node
  Ti idLeft = 0;

  /// @brief Id of the right node
  Ti idRight = 0;

  /// @brief Number of single Nodes in this node. must start from 1
  Ti nodesWithin = 1;

  /// @brief Index from which we extract the Distance
  Ti distanceIndex = 0;

  /// @brief If true, it means this node is merged
  bool isMerged = false;

  /// @brief Distance between left and right Nodes
  Tv leftDistanceRight = 0;
};

/// @brief A base class for hierarchical clustering
class LDT_EXPORT HClusterBase {

public:
  HClusterBase(){};
  virtual ~HClusterBase() = default;

  /// @brief Array of Nodes. At first it has n nodes. After calculation, n-1
  /// more merged Nodes are added
  std::vector<std::unique_ptr<HClusterNode>> Nodes;

  /// @brief Gets the class from a type
  /// @param linkage The type
  /// @param n Number rows (or columns) in the distance matrix
  /// @return
  static std::unique_ptr<HClusterBase> GetFromType(HClusterLinkage linkage,
                                                   Ti n);

  virtual void Calculate(MatrixSym<false> &distances) = 0;

  virtual void
  Group(std::vector<std::unique_ptr<std::vector<Ti>>> &map) const = 0;

  virtual void MergeR(Matrix<Ti> &merge, Matrix<Tv> &heights,
                      std::vector<Ti> &order) const = 0;
};

template <HClusterLinkage method = HClusterLinkage::kComplete>
class LDT_EXPORT HCluster : public HClusterBase {
private:
  /// @brief rows (cols) of the Distance Matrix: D:nxn
  Ti n = 0;

  /// @brief A reference to the given matrix of distances
  MatrixSym<false> *Distances = nullptr;

  /// @brief Merges to nodes and pushes the merged node in to the nodes array
  /// @param n_i
  /// @param leftNode
  /// @param rightNode
  /// @param leftDistanceRight Distance between left & right nodes
  /// @return
  HClusterNode *Merge2(Ti &n_i, HClusterNode &leftNode, HClusterNode &rightNode,
                       Tv leftDistanceRight);

  /// @brief Finds the minimum Distance neighbor
  /// @param node
  /// @param distance
  /// @return
  HClusterNode *GetNearestNeighbor(const HClusterNode &node, Tv &distance);

  /// @brief
  /// @param nLeft number of single Nodes within left
  /// @param nRight number of single Nodes within right
  /// @param nNode number of single Nodes within this node
  /// @param disLeft Distance between this node and left
  /// @param disRight Distance between this node and right
  /// @param leftDisRight Distance between left and right Nodes
  /// @return
  Tv CalculateDistance(Ti nLeft, Ti nRight, Ti nNode, Tv disLeft, Tv disRight,
                       Tv leftDisRight);

public:
  /// @brief Initializes a new instance of this class
  /// @param n
  HCluster(Ti n);

  /// @brief Calculates the results
  /// @param distances The distance matrix
  virtual void Calculate(MatrixSym<false> &distances) override;

  /// @brief After calculate, it groups the data
  /// @param map Its size determines the number of groups
  virtual void
  Group(std::vector<std::unique_ptr<std::vector<Ti>>> &map) const override;

  /// @brief Similar merge and height matrixes with the R 'hclust' function
  /// (not complete yet. the sorting remains)
  /// @param merge
  /// @param heights
  /// @param order
  virtual void MergeR(Matrix<Ti> &merge, Matrix<Tv> &heights,
                      std::vector<Ti> &order) const override;
};

/// @brief A base class for grouping data
class LDT_EXPORT GroupDataBase {

public:
  /// @brief Gets the required size of the work array
  Ti WorkSize = 0;

  /// @brief After \ref Calculate, it contains the result
  std::vector<std::unique_ptr<std::vector<Ti>>> Groups;

  /// @brief After \ref Calculate, it shows removed columns
  std::set<size_t> Removed;

  /// @brief If true, NAN distance is found
  bool NaNDistanceFound = false;

  GroupDataBase(){};

  virtual void Calculate(const Matrix<Tv> &mat, Tv *work, Ti groupCount,
                         Tv removeThreshold = 0) = 0;

  /// @brief Gets an initialized class from the types
  /// @param linkMethod A value to be passed
  /// @param distMethod A value to be passed
  /// @param corrMethod A value to be passed
  /// @param rows Maximum expected number of rows
  /// @param cols Maximum expected number of columns
  /// @return The initialized class
  static std::unique_ptr<GroupDataBase>
  GetFromType(HClusterLinkage linkMethod, DistanceMethod distMethod,
              CorrelationMethod corrMethod, Ti rows, Ti cols);
};

/// @brief A class for grouping data
/// @tparam linkMethod Type of link method
/// @tparam distMethod Type of distribution method
/// @tparam corrMethod Type of correlation method if distribution method is
/// related to correlation
template <HClusterLinkage linkMethod, DistanceMethod distMethod,
          CorrelationMethod corrMethod>
class LDT_EXPORT GroupData : public GroupDataBase {
public:
  /// @brief Initializes an instance of this class
  /// @param rows Maximum expected number of rows
  /// @param cols Maximum expected number of columns
  GroupData(Ti rows, Ti cols);

  /// @brief Calculates the results
  /// @param mat Data
  /// @param work Work array of size \ref WorkSize
  /// @param groupCount Number of groups
  /// @param removeThreshold If distance is less than this value, the first
  /// column is removed
  virtual void Calculate(const Matrix<Tv> &mat, Tv *work, Ti groupCount,
                         Tv removeThreshold = 0) override;
};

extern template class ldt::Distance<false, DistanceMethod::kEuclidean,
                                    CorrelationMethod::kPearson>;
extern template class ldt::Distance<false, DistanceMethod::kManhattan,
                                    CorrelationMethod::kPearson>;
extern template class ldt::Distance<false, DistanceMethod::kMaximum,
                                    CorrelationMethod::kPearson>;

extern template class ldt::Distance<true, DistanceMethod::kEuclidean,
                                    CorrelationMethod::kPearson>;
extern template class ldt::Distance<true, DistanceMethod::kManhattan,
                                    CorrelationMethod::kPearson>;
extern template class ldt::Distance<true, DistanceMethod::kMaximum,
                                    CorrelationMethod::kPearson>;

// Pearson correlation
extern template class ldt::Distance<false, DistanceMethod::kCorrelation,
                                    CorrelationMethod::kPearson>;
extern template class ldt::Distance<false, DistanceMethod::kAbsCorrelation,
                                    CorrelationMethod::kPearson>;
extern template class ldt::Distance<true, DistanceMethod::kCorrelation,
                                    CorrelationMethod::kPearson>;
extern template class ldt::Distance<true, DistanceMethod::kAbsCorrelation,
                                    CorrelationMethod::kPearson>;

// Spearman
extern template class ldt::Distance<false, DistanceMethod::kCorrelation,
                                    CorrelationMethod::kSpearman>;
extern template class ldt::Distance<false, DistanceMethod::kAbsCorrelation,
                                    CorrelationMethod::kSpearman>;
extern template class ldt::Distance<true, DistanceMethod::kCorrelation,
                                    CorrelationMethod::kSpearman>;
extern template class ldt::Distance<true, DistanceMethod::kAbsCorrelation,
                                    CorrelationMethod::kSpearman>;

// Group
// Correlation
extern template class ldt::GroupData<HClusterLinkage::kAverageU,
                                     DistanceMethod::kEuclidean,
                                     CorrelationMethod::kPearson>;

extern template class ldt::GroupData<HClusterLinkage::kAverageU,
                                     DistanceMethod::kCorrelation,
                                     CorrelationMethod::kPearson>;
extern template class ldt::GroupData<HClusterLinkage::kAverageW,
                                     DistanceMethod::kCorrelation,
                                     CorrelationMethod::kPearson>;
extern template class ldt::GroupData<HClusterLinkage::kComplete,
                                     DistanceMethod::kCorrelation,
                                     CorrelationMethod::kPearson>;
extern template class ldt::GroupData<HClusterLinkage::kSingle,
                                     DistanceMethod::kCorrelation,
                                     CorrelationMethod::kPearson>;
extern template class ldt::GroupData<HClusterLinkage::kWard,
                                     DistanceMethod::kCorrelation,
                                     CorrelationMethod::kPearson>;

extern template class ldt::GroupData<HClusterLinkage::kAverageU,
                                     DistanceMethod::kCorrelation,
                                     CorrelationMethod::kSpearman>;
extern template class ldt::GroupData<HClusterLinkage::kAverageW,
                                     DistanceMethod::kCorrelation,
                                     CorrelationMethod::kSpearman>;
extern template class ldt::GroupData<HClusterLinkage::kComplete,
                                     DistanceMethod::kCorrelation,
                                     CorrelationMethod::kSpearman>;
extern template class ldt::GroupData<HClusterLinkage::kSingle,
                                     DistanceMethod::kCorrelation,
                                     CorrelationMethod::kSpearman>;
extern template class ldt::GroupData<HClusterLinkage::kWard,
                                     DistanceMethod::kCorrelation,
                                     CorrelationMethod::kSpearman>;

// Abs Correlation

extern template class ldt::GroupData<HClusterLinkage::kAverageU,
                                     DistanceMethod::kAbsCorrelation,
                                     CorrelationMethod::kPearson>;
extern template class ldt::GroupData<HClusterLinkage::kAverageW,
                                     DistanceMethod::kAbsCorrelation,
                                     CorrelationMethod::kPearson>;
extern template class ldt::GroupData<HClusterLinkage::kComplete,
                                     DistanceMethod::kAbsCorrelation,
                                     CorrelationMethod::kPearson>;
extern template class ldt::GroupData<HClusterLinkage::kSingle,
                                     DistanceMethod::kAbsCorrelation,
                                     CorrelationMethod::kPearson>;
extern template class ldt::GroupData<HClusterLinkage::kWard,
                                     DistanceMethod::kAbsCorrelation,
                                     CorrelationMethod::kPearson>;

extern template class ldt::GroupData<HClusterLinkage::kAverageU,
                                     DistanceMethod::kAbsCorrelation,
                                     CorrelationMethod::kSpearman>;
extern template class ldt::GroupData<HClusterLinkage::kAverageW,
                                     DistanceMethod::kAbsCorrelation,
                                     CorrelationMethod::kSpearman>;
extern template class ldt::GroupData<HClusterLinkage::kComplete,
                                     DistanceMethod::kAbsCorrelation,
                                     CorrelationMethod::kSpearman>;
extern template class ldt::GroupData<HClusterLinkage::kSingle,
                                     DistanceMethod::kAbsCorrelation,
                                     CorrelationMethod::kSpearman>;
extern template class ldt::GroupData<HClusterLinkage::kWard,
                                     DistanceMethod::kAbsCorrelation,
                                     CorrelationMethod::kSpearman>;

// Cluster

extern template class ldt::HCluster<HClusterLinkage::kAverageU>;
extern template class ldt::HCluster<HClusterLinkage::kAverageW>;
extern template class ldt::HCluster<HClusterLinkage::kComplete>;
extern template class ldt::HCluster<HClusterLinkage::kSingle>;
extern template class ldt::HCluster<HClusterLinkage::kWard>;

} // namespace ldt
