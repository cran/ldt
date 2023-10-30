#pragma once

#include "ldt_base.h"
#include "matrix.h"
#include <random>
#include <string>
#include <vector>

namespace ldt {

/// @brief Splits data into train and test sample (for discrete data)
class LDT_EXPORT DataSplitDiscrete {
  Ti mNumChoices = 0;
  Matrix<Tv> Y;

  std::vector<Ti> Counts;
  std::vector<Ti> CountsSortedIndexes;

  Tv mTrainRatio = 0;
  Ti mTrainFixSize = 0;

public:
  /// @brief size of the storage array
  Ti StorageSize = 0;

  /// @brief size of the work array
  Ti WorkSizeI = 0;

  /// @brief size: number of choices. Each element consist index of the rows in
  /// the given data
  std::vector<std::unique_ptr<std::vector<Ti>>> Rows;

  /// @brief Training sample
  Matrix<Tv> Sample0;

  /// @brief Test sample
  Matrix<Tv> Sample1;

  /// @brief initializes the class
  /// @param rows maximum expected number of rows in the data
  /// @param cols maximum expected number of columns in the data
  /// @param numChoices number of unique labels in the data
  DataSplitDiscrete(Ti rows, Ti cols, Ti numChoices);

  /// @brief Allocates storage and prepares the class. After this, call
  /// \ref Shuffle, e.g. in a loop
  /// @param data data
  /// @param storage storage array (Tv)
  /// @param trainRatio relative size of the training sample
  /// @param trainFixSize Use zero to disable and use \p trainRatio.
  void Calculate(const Matrix<Tv> &data, Tv *storage, Tv trainRatio,
                 Ti trainFixSize);

  /// @brief Shuffles the data
  /// @param data data
  /// @param workI work array (Ti)
  /// @param eng pseudo-random number generator
  void Shuffle(const Matrix<Tv> &data, Ti *workI, std::mt19937 &eng);
};

/// @brief Split data into train and test sample
class LDT_EXPORT DataSplit {
  Tv mTrainRatio = 0;
  Ti mTrainFixSize = 0;

public:
  /// @brief Size of the storage array
  Ti StorageSize = 0;

  /// @brief Size of the work array
  Ti WorkSizeI = 0;

  /// @brief Training sample
  Matrix<Tv> Sample0;

  /// @brief Test sample
  Matrix<Tv> Sample1;

  /// @brief initializes the class
  DataSplit(){};

  /// @brief initializes the class
  /// @param rows maximum expected number of rows in the data
  /// @param cols maximum expected number of columns in the data
  DataSplit(Ti rows, Ti cols);

  /// @brief Allocates storage and prepares the class. After this, call
  /// \ref Shuffle, e.g. in a loop
  /// @param data data
  /// @param storage storage array (Tv)
  /// @param trainRatio relative size of the training sample
  /// @param trainFixSize Use zero to disable and use \p trainRatio.
  void Calculate(const Matrix<Tv> &data, Tv *storage, Tv trainRatio,
                 Ti trainFixSize);

  /// @brief Shuffles the data
  /// @param data data
  /// @param workI work array (Ti)
  /// @param eng pseudo-random number generator
  void Shuffle(const Matrix<Tv> &data, Ti *workI, std::mt19937 &eng);
};

} // namespace ldt
