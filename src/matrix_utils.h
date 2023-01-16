#pragma once

#include "helpers.h"
#include "ldt_base.h"
#include "matrix.h"
#include <algorithm> // std::sort, std::stable_sort
#include <functional>
#include <numeric> // std::iota
#include <string>
#include <vector>

namespace ldt {

/// @brief An extended matrix class that can deal with removing NANs and
/// selecting a subset of columns
/// @tparam Tw Type of data
template <class Tw = Tv> class LDT_EXPORT Dataset {
  bool mHasNaN;
  bool mSelectColumn;

public:
  /// @brief initializes the class
  Dataset(){};

  /// @brief Initializes a new instance of the class
  /// @param rows Expected number of rows
  /// @param cols Expected number of columns
  /// @param hasNan If true, NaN is expected in the data
  /// @param selectColumn If true, a subset of columns might be selected.
  Dataset(Ti rows, Ti cols, bool hasNan = true,
          bool selectColumn = true /*, bool interpolate = false*/);

  /// @brief Gets the storage size
  Ti StorageSize = 0;

  /// @brief After \ref Calculate, it is a matrix with the given columns and no
  /// NAN
  Matrix<Tw> Result;

  /// @brief Calculates the \ref Result
  /// @param data Data
  /// @param colIndexes If not null, it selects these columns
  /// @param storage An array of length \ref StorageSize for keeping the result
  void Calculate(const Matrix<Tw> &data, std::vector<Ti> *colIndexes,
                 Tw *storage);
};

/// @brief An extended matrix class to deal with filtering rows of a time-series
/// data
/// @tparam Tw Type of data
/// @tparam byRow If true, variables are in the rows and time is in the columns
template <bool byRow = true, class Tw = Tv> class LDT_EXPORT DatasetTs {
  bool mHasNaN = true;
  bool mSelect = false;
  bool mInterpolate = false;
  Ti mAdjustLeadLagsCount = 0;

public:
  /// @brief A pointer to the given data in \ref Calculate
  Matrix<Tw> *pData = nullptr;

  /// @brief Initializes a new instance of the class
  DatasetTs(){};

  /// @brief Initializes a new instance of the class
  /// @param rows Expected number of rows
  /// @param cols Expected number of columns
  /// @param hasNan If true, NaN is expected in the data
  /// @param selectColumn If true, a subset of columns or rows might be
  /// selected.
  /// @param interpolate If true, missing data might exist in which case
  /// interpolation is used
  /// @param adjustLeadLagsCount If not zero, leads and lags are adjusted by
  /// removing the given number of extra data at the end of the variables.
  DatasetTs(Ti rows, Ti cols, bool hasNan = true, bool select = true,
            bool interpolate = false, Ti adjustLeadLagsCount = 0);

  /// @brief Gets the storage size
  Ti StorageSize = 0;

  /// @brief After \ref Update, if false and \ref WithMissingIndexes is not
  /// empty, it means interpolation
  bool HasMissingData = false;

  /// @brief After \ref Update, Gets the start index of the final data
  Ti Start = 0;

  /// @brief After \ref Update, Gets the end index of the final data
  Ti End = 0;

  std::vector<Ti> WithMissingIndexes;

  std::vector<IndexRange> Ranges;

  std::vector<Ti> InterpolationCounts;

  /// @brief After \ref Update, it is variables with leads relative to the first
  /// variable
  std::vector<Ti> WithLeads;

  /// @brief After \ref Update, it is variables with lags relative to the first
  /// variable
  std::vector<Ti> WithLags;

  /// @brief After \ref Update, a matrix with given rows and no NaN
  Matrix<Tw> Result;

  void Data(Matrix<Tw> &data);

  void Update(std::vector<Ti> *indexes, Tw *storage);
};

/// @brief An extended matrix class to deal with removing mean and variance from
/// the columns of a matrix
/// @tparam Tw Type of data
template <class Tw = Tv> class LDT_EXPORT MatrixStandardized {
public:
  /// @brief Gets the storage size
  Ti StorageSize = 0;

  /// @brief Initializes a new instance of the class
  MatrixStandardized(){};

  /// @brief Initializes a new instance of the class
  /// @param rows Expected number of rows
  /// @param cols Expected number of columns
  /// @param removeZeroVar If true, columns with zero variance are removed from
  /// the result
  /// @param center If true, it centers the data
  /// @param scale If true, it scales the data
  MatrixStandardized(Ti rows, Ti cols, bool removeZeroVar = true,
                     bool center = true, bool scale = true);

  /// @brief Gets the center argument
  bool mCenter;
  /// @brief Gets the scale argument
  bool mScale;
  /// @brief Gets the removeZeroVar argument
  bool mRemoveZeroVar;

  /// @brief Gets or sets whether sample statistics should be calculated
  bool Sample = true;

  /// @brief Gets or sets whether NAN exists and should be omitted in the
  /// calculations
  bool CheckNan = false;

  /// @brief Column means used for centering
  Matrix<Tw> ColumnMeans;

  /// @brief Column variances used for scaling
  Matrix<Tw> ColumnVars;

  /// @brief Indexes of zero-variance columns. Note that such columns are not
  /// removed from 'poColumnMeans' and 'poColumnVars'
  std::vector<Ti> RemovedZeroVar;

  /// @brief Standardized matrix. Might have less column than the given data if
  /// you asked to remove zero variance columns.
  Matrix<Tw> Result;

  /// @brief Calculates the result
  /// @param mat The data
  /// @param storage An array of size \ref StorageSize to keep the results
  /// @param overrideMean If not null, it is used for centering the data
  /// @param overrideVariance If not null, it is used for scaling the data
  void Calculate(const Matrix<Tw> &mat, Tw *storage,
                 const Matrix<Tw> *overrideMean = nullptr,
                 const Matrix<Tw> *overrideVariance = nullptr);
};

/// @brief A helper class for SVD decomposition
/// @tparam Tw
template <class Tw = Tv> class LDT_EXPORT MatrixSvd {
  Ti W_svd;
  char mJobU;
  char mJobVT;

public:
  /// @brief Gets the required storage size
  Ti StorageSize = 0;

  /// @brief Gets the required work size
  Ti WorkSize = 0;

  /// @brief Initializes a new instance of the class
  /// @param rows Expected number of rows
  /// @param cols Expected number of columns
  /// @param jobU Determines how \ref U is calculated. It Can be 'N' (not
  /// calculated), 'A' (everything calculated), and 'S'
  /// @param jobVT Determines how \ref V is calculated. It Can be 'N' (not
  /// calculated), 'A' (everything calculated), and 'S'
  MatrixSvd(Ti rows, Ti cols, char jobU = 'A', char jobVT = 'A');

  /// @brief U in A = U*S*VT. Given an 'n x m' matrix, if 'jobU==A', it is n x
  /// n. If 'jobU=N', it is null. If 'jobU=S', it is still 'n x n', but the
  /// first min(n,m) columns in U is calculated. You can restructure it. Note
  /// that if n<m, no invalid column exists.
  Matrix<Tw> U;

  /// @brief vectorized version of S in A = U*S*VT. Given an 'n x m' matrix, it
  /// is min(n,m) x 1
  Matrix<Tw> S;

  /// @brief VT in A = U*S*VT. Given an 'n x m' matrix, if 'jobU==A', it is m x
  /// m. If 'jobU=N', it is null. If 'jobU=S', it is still 'm x m', but the
  /// first min(n,m) rows in V is calculated. You can NOT use restructure
  /// because this is column major. Note that if m<n, no invalid row exists.
  Matrix<Tw> VT;

  /// @brief Calculate \ref U, \ref S and \ref VT
  /// @param mat Data
  /// @param storage Array of size \ref StorageSize
  /// @param work Array of size \ref WorkSize
  void Calculate(const Matrix<Tw> &mat, Tw *storage, Tw *work);

  /// @brief Similar to \ref Calculate without checking the WorkSize and
  /// StorageSize
  /// @param mat
  /// @param storage
  /// @param work
  void Calculate0(const Matrix<Tw> &mat, Tw *storage, Tw *work);
};

extern template class ldt::Dataset<Tv>;
extern template class ldt::Dataset<Ti>;

extern template class ldt::DatasetTs<true, Tv>;
extern template class ldt::DatasetTs<true, Ti>;
extern template class ldt::DatasetTs<false, Tv>;
extern template class ldt::DatasetTs<false, Ti>;

extern template class ldt::MatrixStandardized<Tv>;
extern template class ldt::MatrixStandardized<Ti>;

extern template class ldt::MatrixSvd<Tv>;

} // namespace ldt
