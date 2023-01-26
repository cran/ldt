#pragma once

#include "helpers.h"
#include "ldt_base.h"
#include <algorithm> // std::sort, std::stable_sort
#include <functional>
#include <numeric> // std::iota
#include <string>
#include <vector>

// #pragma region Exceptions

/// @brief inversion of a singular Matrix
static const char exp_mat_sin[] = "mat_sin";

// #pragma endregion

namespace ldt {

/// @brief A range with an start and end indices
class LDT_EXPORT IndexRange {

public:
  /// @brief Starting index of the range
  Ti StartIndex = 0;

  /// @brief Ending index of the range
  Ti EndIndex = 0;

  /// @brief Initializes a new instance of the class
  IndexRange(){};

  /// @brief Initializes a new instance of the class
  /// @param start Starting index of the range
  /// @param end Ending index of the range
  IndexRange(Ti start, Ti end);

  /// @brief Determines whether this range is invalid (start > end, start < 0,
  /// end < 0)
  /// @return true if the range is not valid, false otherwise
  bool IsNotValid() const;

  /// @brief Number of elements in this range. It is 1 if start==end
  /// @return Number of elements in this range
  Ti Count() const;
};

/// @brief A matrix with an array
/// @tparam Tw type of data in the matrix array
template <class Tw = Tv> class LDT_EXPORT Matrix {

public:
  /// @brief Number of rows of the matrix
  Ti RowsCount = 0;

  /// @brief Number of columns of the matrix
  Ti ColsCount = 0;

  /// @briefValues in the matrix. It gets populated column-wise (such that 1st
  /// column is populated first, then second column, and so on ). Also note that
  /// the class does not own this array. It is a reference to the provided data.
  Tw *Data = nullptr;

  // #pragma region Constructors

  /// @brief Initializes a new instance of the class
  Matrix<Tw>();

  /// @brief Initializes an m x n matrix with no data
  /// @param m number of rows
  /// @param n number of columns
  Matrix<Tw>(Ti m, Ti n = 1);

  /// @brief Initializes an m x n matrix (column-wise)
  /// @param values data for the matrix. see \ref Data.
  /// @param m number of rows
  /// @param n number of columns
  Matrix<Tw>(Tw *values, Ti m, Ti n = 1);

  /// @brief Initializes a vector (values.size() x 1)
  /// @param values values of the vector. In this case, \ref Data is a reference
  /// to the vector's array.
  Matrix<Tw>(std::vector<Tw> *values);

  /// @brief Initializes an m x n matrix (column-wise)
  /// @param values data for the matrix. see \ref Data.
  /// @param m number of rows
  /// @param n number of columns
  Matrix<Tw>(std::vector<Tw> *values, Ti m, Ti n = 1);

  /// @brief Initializes an m x n matrix (column-wise)
  /// @param defaultValue default value of the matrix
  /// @param values data for the matrix. see \ref Data.
  /// @param m number of rows
  /// @param n number of columns
  Matrix<Tw>(Tw defaultValue, Tw *values, Ti m, Ti n = 1);

  /// @brief Initializes an m x n matrix (column-wise)
  /// @param defaultValue default value of the matrix
  /// @param values data for the matrix. see \ref Data.
  /// @param m number of rows
  /// @param n number of columns
  Matrix<Tw>(Tw defaultValue, std::vector<Tw> &values, Ti m, Ti n = 1);

  ~Matrix<Tw>();

  // #pragma endregion

  // #pragma region Data

  /// @brief changes the number of rows and columns of the Matrix. Old and new
  /// size must be equal (\p newRows * \p newCols == oldRows * oldCols)
  /// @param newRows new number of rows
  /// @param newCols new number of columns
  void Restructure(Ti newRows, Ti newCols);

  /// @brief similar to \ref restructure without checking the number of the
  /// elements
  /// @param newRows new number of rows
  /// @param newCols new number of columns
  void Restructure0(Ti newRows, Ti newCols);

  /// @brief length of the inner vector: \ref RowsCount * \ref ColsCount
  /// @return the length
  Ti length() const;

  /// @brief sets or changes the reference to the inner data (\ref Data)
  /// @param data a pointer to the new data
  /// @param newRows new number of rows (if -1, current value is used)
  /// @param newCols new number of columns (if -1, current value is used)
  void SetData(Tw *data, Ti newRows = -1, Ti newCols = -1);

  /// @brief sets or changes the reference to the inner data (\ref Data)
  /// @param defaultValue the default value
  /// @param data a pointer to the new data
  /// @param newRows new number of rows (if -1, current value is used)
  /// @param newCols new number of columns (if -1, current value is used)
  void SetData(Tw defaultValue, Tw *data, Ti newRows = -1, Ti newCols = -1);

  /// @brief Determines if this is a vector, i.e., if the number of columns is 1
  /// (note that 0 x 1 returns true)
  /// @return true if this is a vector, false otherwise
  bool IsVector() const;

  /// @brief Determines if this is an empty matrix; i.e., if whether the number
  /// of rows or columns is zero (0 x 0), (0 x 1), (1 x 0)
  /// @return true if the matrix is empty
  bool IsEmpty() const;

  /// @brief Determines if this is a square matrix; i.e., rows == columns
  /// @return true if the matrix is square
  bool IsSquare() const;

  /// @brief Determines if this is a symmetric matrix
  /// @param epsilon a value to ignore small floating point differences
  /// @return true if the matrix is symmetric
  bool IsSymmetric(Tw epsilon = 0) const;

  /// @brief Determines if the matrix has NAN
  /// @return true if any NAN is found in \ref Data
  bool HasNaN() const;

  // #pragma endregion

  // #pragma region Index

  /// @brief translate an index to its position (row and column index)
  /// @param index index
  /// @param rowIndex row index (updated on exit)
  /// @param colIndex column index (updated on exit)
  void TranslateIndex(Ti index, Ti &rowIndex, Ti &colIndex) const;

  /// @brief gets the (i,j)-th element of the Matrix (checks the bounds)
  /// @param i row index
  /// @param j column index
  /// @return the value at the given position
  Tw Get(Ti i, Ti j) const;

  /// @brief gets the i-th element of the inner array (i.e. the vectorized
  /// array) (checks the bounds)
  /// @param i index of the element
  /// @return the value at the given position
  Tw Get(Ti i) const;

  /// @brief gets the i-th element of the inner array (throws error if it is not
  /// a vector) (checks the bounds)
  /// @param i index of the element
  /// @return the value at the given position
  Tw GetVector(Ti i) const;

  /// @brief similar to \ref Get (DOES NOT check the bounds)
  /// @param i row index
  /// @param j column index
  /// @return the value at the given position
  Tw Get0(Ti i, Ti j) const;

  /// @brief sets the (i,j)-th element of the Matrix (checks the bounds)
  /// @param i row index
  /// @param j column index
  void Set(Ti i, Ti j, Tw value);

  /// @brief sets the i-th element of the inner array (i.e. the vectorized
  /// array) (checks the bounds)
  /// @param i
  /// @param value
  void Set(Ti i, Tw value);

  /// @brief similar to \ref Set (DOES NOT check the bounds)
  /// @param i row index
  /// @param j column index
  void Set0(Ti i, Ti j, Tw value);

  /// @brief sets the i-th element of a vector (it throws error if it is not a
  /// vector) (checks the bounds)
  /// @param i
  /// @param value
  void SetVector(Ti i, Tw value);

  /// @brief data[i,j] += value (checks the bounds)
  /// @param i row index
  /// @param j column index
  /// @param value value
  void Set_Plus(Ti i, Ti j, Tw value);

  /// @brief Similar to \ref Set_Plus (DOES NOT check the bounds)
  /// @param i row index
  /// @param j column index
  /// @param value value
  void Set_Plus0(Ti i, Ti j, Tw value);

  /// @brief data[i,j] -= value (checks the bounds)
  /// @param i row index
  /// @param j column index
  /// @param value value
  void Set_Minus(Ti i, Ti j, Tw value);

  /// @brief Similar to \ref Set_Minus (DOES NOT check the bounds)
  /// @param i row index
  /// @param j column index
  /// @param value value
  void Set_Minus0(Ti i, Ti j, Tw value);

  // #pragma endregion

  // #pragma region Equality

  /// @brief compares the elements of this Matrix with a given value
  /// @param b Given value
  /// @param epsilon A value to ignore small floating point differences
  /// @param nansAreEqual If true, NAN==NAN is true. default is false.
  /// @param ignoreNan If true, NANs are skipped
  /// @return true if the all elements are equal to \p b
  bool EqualsValue(Tw b, Tw epsilon = 0, bool nansAreEqual = false,
                   bool ignoreNan = false) const;

  /// @brief Similar to \ref EqualsValue but for a specific column
  /// @param colIndex Column index
  /// @param b Given value
  /// @param epsilon A value to ignore small floating point differences
  /// @param nansAreEqual If true, NAN==NAN is true
  /// @param ignoreNan If true, NANs are skipped
  /// @return true if the all elements are equal to \p b
  bool EqualsValueColumn(Ti colIndex, Tw b, Tw epsilon = 0,
                         bool nansAreEqual = false,
                         bool ignoreNan = false) const;

  /// @brief compare the elements of this matrix with a second one
  /// @param b second matrix
  /// @param epsilon A value to ignore small floating point differences
  /// @param throwForSize if true, it throws error if sizes are not equal
  /// @param nansAreEqual If true, NAN==NAN is true
  /// @return true if two matrices are equal, false otherwise
  bool Equals(const Matrix<Tw> &b, Tw epsilon = 0, bool throwForSize = true,
              bool nansAreEqual = false) const;

  /// @brief compare the elements of this matrix with a second one
  /// @param m second matrix
  /// @param abs_diff absolute difference between the values (updated on exit)
  /// @param epsilon A value to ignore small floating point differences
  /// @param throwForSize if true, it throws error if sizes are not equal
  /// @param nansAreEqual If true, NAN==NAN is true
  /// @return true if two matrices are equal, false otherwise
  bool Equals(const Matrix<Tw> &m, Tw &abs_diff, Tw epsilon,
              bool throwForSize = true, bool nansAreEqual = false) const;

  // #pragma endregion

  // #pragma region NAN

  /// @brief Gets the range of data for a specific column
  /// @param hasMissing (updated on exit) true if missing observation is found
  /// @param j Column index
  /// @return range of data in the column
  IndexRange GetRangeColumn(bool &hasMissing, Ti j = 0) const;

  /// @brief Gets the range of data for a specific row
  /// @param hasMissing (updated on exit) true if missing observation is found
  /// @param i Row index
  /// @return range of data in the row
  IndexRange GetRangeRow(bool &hasMissing, Ti i = 0) const;

  /// @brief Gets the range of data in the columns
  /// @param anyColumnHasMissing (updated on exit) true if missing observation
  /// is found in any column
  /// @return range of data in all the columns
  IndexRange GetRange(bool &anyColumnHasMissing) const;

  /// @brief Gets indices of the rows where there is no NAN
  /// @param rowIndexes A vector to be filled with the result
  /// @param checkInfinity If true, infinity is considered to be NAN
  /// @param colIndexes If not null, it restricts the search to the given
  /// columns
  void GetAnyNanRow(std::vector<Ti> &rowIndexes, bool checkInfinity = false,
                    std::vector<Ti> *colIndexes = nullptr) const;

  // #pragma endregion

  // #pragma region Convert

  /// @brief Copies (casts) the values of a vector to a matrix
  /// @tparam t_value1 type of the vector
  /// @param data source vector
  /// @param storage a matrix to keep the results
  template <typename t_value1>
  static void Convert(std::vector<t_value1> &data, Matrix<Tw> &storage) {
    if (storage.length() != data->size())
      throw std::logic_error("wrong size: storage");
    Ti i = 0;
    for (auto &d : *data) {
      storage.Data[i] = static_cast<Tw>(d);
      i++;
    }
  }

  /// @brief Copies (casts) the values of this matrix to a vector
  /// @tparam t_value1 type of the vector
  /// @param storage a vector with equal size to save the results
  template <typename t_value1> void Convert(std::vector<t_value1> &storage) {
    if (storage.size() != length())
      throw std::logic_error("wrong size: storage");
    for (Ti i = 0; i < length(); i++) {
      storage.at(i) = static_cast<Tw>(Data[i]);
    }
  }

  /// @brief Removes NAN data-points in this vector and restructures (storage is
  /// kept)
  /// @param removeInf If true, infinity is also removed
  void RemoveNanVector_in(bool removeInf = false);

  /// @brief Removes columns with any NAN and restructures (storage is kept)
  /// @param removeInf If true, columns with any infinity are also removed
  void RemoveColumnsAnyNan_in(bool removeInf = false);

  /// @brief Removes the columns by index and restructures (storage is kept)
  /// @param cols Column indices to be removed
  void RemoveColumnsIn(std::vector<Ti> &cols);

  /// @brief Remove NaN from a vector
  /// @param data the vector
  /// @param storage where results are saved. If storage.Data is null, it count
  /// non-NAN values and returns it
  /// @return number of non-NAN values if storage.Data is null
  static Ti RemoveNanVector(Matrix<Tw> &data, Matrix<Tw> &storage);

  // #pragma endregion

  // #pragma region Linq - like functions

  /// @brief Gets the last element
  /// @return the last element
  Tw Last();

  /// @brief Gets the first element
  /// @return the first element
  Tw First();

  /// @brief Gets the indices of a value in the inner array
  /// @param value the value (it can be NAN)
  /// @param vec a storage for the results
  void IndicesOfVector(Tw value, std::vector<Ti> &vec) const;

  /// @brief Determines if any element equals to a value
  /// @param value given value (it can be NAN)
  /// @return true if any equal element is found
  bool Any(Tw value) const;

  /// @brief Determines if all element equal to a value
  /// @param value Given value (it can be NAN)
  /// @return true if all are equal to the \p value
  bool All(Tw value) const;

  /// @brief Sorts the values in each column
  /// @param storage A matrix of equal size (on exit, columns are sorted)
  /// @param ascending Determines the order
  void Sort(Matrix<Tw> &storage, bool ascending) const;

  /// @brief Sorts a vector, given an array of indices
  /// @param storage A vector to store the result
  /// @param indices Array of indices for the sort
  void SortByVector(Matrix<Tw> &storage, std::vector<Ti> &indices);

  /// @brief similar to \ref sortByVector without checking the bounds
  /// @param storage A vector to store the result
  /// @param indices Array of indices for the sort
  void SortByVector0(Matrix<Tw> &storage, std::vector<Ti> &indices);

  /// @brief Sort rows based on a given array of row indices
  /// @param storage a matrix of equal size for the results
  /// @param rowIndices array of row indices
  void SortRowsBy(Matrix<Tw> &storage, std::vector<Ti> &rowIndices);

  /// @brief Similar to SortRowsBy without checking the bounds
  /// @param storage a matrix of equal size for the results
  /// @param rowIndices array of row indices
  void SortRowsBy0(Matrix<Tw> &storage, std::vector<Ti> &rowIndices);

  /// @brief Sort columns based on a given array of row indices
  /// @param storage a matrix of equal size for the results
  /// @param colIndices array of column indices
  void SortColumnsBy(Matrix<Tw> &storage, std::vector<Ti> &colIndices);

  /// @brief Similar to \ref SortColumnsBy without checking the bounds
  /// @param storage a matrix of equal size for the results
  /// @param colIndices array of column indices
  void SortColumnsBy0(Matrix<Tw> &storage, std::vector<Ti> &colIndices);

  /// @brief Gets the sorting indices in a vector
  /// @param indices a storage for the result
  /// @param ascending Determines the order of the sort
  void SortIndicesVector(std::vector<Ti> &indices, bool ascending = true) const;

  // #pragma endregion

  // #pragma region Apply

  /// @brief Sets the elements of the matrix to be a value
  /// @param value the value
  void SetValue(Tw value);

  /// @brief Sets the elements to be a sequence
  /// @param start start of the sequence
  /// @param step increment in the sequence
  void SetSequence(Tw start = 0, Tw step = 1);

  /// @brief Sets the value of the diagonal
  /// @param vDiag a value
  void SetValueDiag(Tw vDiag);

  /// @brief Sets the value of the diagonal and off
  /// diagonal diagonal
  /// @param vDiag value for the diagonal
  /// @param vOffDiag value for the off-diagonal
  void SetValueDiag(Tw vDiag, Tw vOffDiag);

  /// @brief Sets the value of the off-diagonal
  /// @param vOffDiag value of the off-diagonal
  void SetValueOffDiag(Tw vOffDiag);

  /// @brief Applies a function to each elements of the inner array
  /// @param func the function
  void Apply_in(std::function<Tw(Tw)> func);

  /// @brief Applies a function the the elements the matrix, by using the
  /// element of another matrix
  /// @param B the second matrix
  /// @param func the function
  void Apply_in(const Matrix<Tw> &B, std::function<Tw(Tw, Tw)> func);

  /// @brief Applies a function to the elements of a specific row
  /// @param i Row index
  /// @param func the function
  void ApplyRow_in(Ti i, std::function<Tw(Tw)> func);

  /// @brief Applies a function to the elements of a specific column
  /// @param j Column index
  /// @param func the function
  void ApplyColumn_in(Ti j, std::function<Tw(Tw)> func);

  /// @brief Applies a function to each elements of the inner vector and stores
  /// the result in a new Matrix
  /// @param func The function
  /// @param storage A place to save the result
  void Apply(std::function<Tw(Tw)> func, Matrix<Tw> &storage) const;

  /// @brief Similar to \ref Apply without checking bounds
  /// @param func The function
  /// @param storage  A place to save the result
  void Apply0(std::function<Tw(Tw)> func, Matrix<Tw> &storage) const;

  /// @brief Applies a function to each elements of this and a second matrix and
  /// saves the result in the third matrix
  /// @param B Second matrix
  /// @param func The function
  /// @param storage A place to save the result
  void Apply(Matrix<Tw> &B, std::function<Tw(Tw, Tw)> func,
             Matrix<Tw> &storage) const;

  /// @brief Similar to \ref Apply without checking the bounds
  /// @param B Second matrix
  /// @param func The function
  /// @param storage A place to save the result
  void Apply0(Matrix<Tw> &B, std::function<Tw(Tw, Tw)> func,
              Matrix<Tw> &storage) const;

  // #pragma endregion

  // #pragma region Copy

  /// @brief Copies the elements
  /// @param storage A matrix with equal size to save the result
  void CopyTo(Matrix<Tw> &storage) const;

  /// @brief Similar to \ref CopyTo without checking the dimension, but checks
  /// the length
  /// @param storage A matrix with equal length to save the result
  void CopyTo0(Matrix<Tw> &storage) const;

  /// @brief Similar to \ref CopyTo without checking dimension or length
  /// @param storage A matrix to save the result
  void CopyTo00(Matrix<Tw> &storage) const;

  /// @brief Sets the values of this matrix from a source
  /// @param source The source
  void CopyFrom(Matrix<Tw> &source);

  /// @brief Similar to \ref CopyFrom, without checking the dimension, but
  /// checks the length
  /// @param source The source
  void CopyFrom0(Matrix<Tw> &source);

  /// @brief Similar to \ref CopyFrom without checking the dimension or the
  /// length
  /// @param source The source
  void CopyFrom00(const Matrix<Tw> &source);

  // #pragma endregion

  // #pragma region Sub

  /// @brief Sets a sub-matrix
  /// @param rowStart From this row
  /// @param colStart From this column
  /// @param source Gets data from this source
  /// @param sourceRowStart From this row in the source
  /// @param sourceColStart From this column in the source
  /// @param rowCount This number of rows in the source
  /// @param colCount This number of columns in the source
  void SetSub(Ti rowStart, Ti colStart, const Matrix<Tw> &source,
              Ti sourceRowStart, Ti sourceColStart, Ti rowCount, Ti colCount);

  /// @brief Similar to \ref SetSub without checking the bounds
  /// @param rowStart From this row
  /// @param colStart From this column
  /// @param source Gets data from this source
  /// @param sourceRowStart From this row in the source
  /// @param sourceColStart From this column in the source
  /// @param rowCount This number of rows in the source
  /// @param colCount This number of columns in the source
  void SetSub0(Ti rowStart, Ti colStart, const Matrix<Tw> &source,
               Ti sourceRowStart, Ti sourceColStart, Ti rowCount, Ti colCount);

  /// @brief Sets a sub-matrix from a transposed source
  /// @param rowStart From this row
  /// @param colStart From this column
  /// @param source Gets data from the transpose of this source
  /// @param sourceRowStart From this row in the transposed source
  /// @param sourceColStart From this column in the transposed source
  /// @param rowCount This number of rows in the transposed source
  /// @param colCount This number of columns in the transposed source
  void SetSub_t(Ti rowStart, Ti colStart, const Matrix<Tw> &source,
                Ti sourceRowStart, Ti sourceColStart, Ti rowCount, Ti colCount);

  /// @brief Similar to \ref SetSub_t without checking the bounds
  /// @param rowStart From this row
  /// @param colStart From this column
  /// @param source Gets data from the transpose of this source
  /// @param sourceRowStart From this row in the transposed source
  /// @param sourceColStart From this column in the transposed source
  /// @param rowCount This number of rows in the transposed source
  /// @param colCount This number of columns in the transposed source
  void SetSub_t0(Ti rowStart, Ti colStart, const Matrix<Tw> &source,
                 Ti sourceRowStart, Ti sourceColStart, Ti rowCount,
                 Ti colCount);

  /// @brief Sets a sub-vector
  /// @param start From this index
  /// @param source Gets data from this source
  /// @param sourceStart From this index in the source
  /// @param count This number of elements in the source
  void SetSubVector(Ti start, const Matrix<Tw> &source, Ti sourceStart,
                    Ti count);

  /// @brief Similar to \ref SetSubVector without checking the bounds
  /// @param start From this index
  /// @param source Gets data from this source
  /// @param sourceStart From this index in the source
  /// @param count This number of elements in the source
  void SetSubVector0(Ti start, const Matrix<Tw> &source, Ti sourceStart,
                     Ti count);

  /// @brief Sets a sub-row
  /// @param row In this row
  /// @param colStart Starts from this column
  /// @param source Gets data from this source
  /// @param length This number of elements from the source
  void SetSubRow(Ti row, Ti colStart, Matrix<Tw> &source, Ti length);

  /// @brief Similar to \ref SetSubRow without checking the bounds
  /// @param row In this row
  /// @param colStart Starts from this column
  /// @param source Gets data from this source
  /// @param length This number of elements from the source
  void SetSubRow0(Ti row, Ti colStart, Tw *source, Ti length);

  /// @brief Gets a sub-matrix
  /// @param rowStart From this row of the matrix
  /// @param colStart From this column of the matrix
  /// @param rowCount This number of rows
  /// @param colCount This number of columns
  /// @param storage Put data in this matrix
  /// @param storageRowStart From this row in the storage
  /// @param storageColStart From this column in the storage
  void GetSub(Ti rowStart, Ti colStart, Ti rowCount, Ti colCount,
              Matrix<Tw> &storage, Ti storageRowStart = 0,
              Ti storageColStart = 0) const;

  /// @brief Similar to \ref GetSub without checking the bounds
  /// @param rowStart From this row of the matrix
  /// @param colStart From this column of the matrix
  /// @param rowCount This number of rows
  /// @param colCount This number of columns
  /// @param storage Put data in this matrix
  /// @param storageRowStart From this row in the storage
  /// @param storageColStart From this column in the storage
  void GetSub0(Ti rowStart, Ti colStart, Ti rowCount, Ti colCount,
               Matrix<Tw> &storage, Ti storageRowStart = 0,
               Ti storageColStart = 0) const;

  /// @brief Gets a sub-matrix
  /// @param firstStart From this row/column of the matrix
  /// @param firstCount This number of rows/columns
  /// @param secondIndices From this row/column indices
  /// @param firstIsRow If true, \p firstStart and \p firstCount belong to the
  /// rows and \p secondIndices belong to the columns
  /// @param storage Put data in this matrix
  /// @param storageRowStart From this row in the storage
  /// @param storageColStart From this column in the storage
  /// @param exclude_indexes If true, it removes the rows/columns given in \p
  /// secondIndices
  void GetSub(Ti firstStart, Ti firstCount,
              const std::vector<Ti> &secondIndices, bool firstIsRow,
              Matrix<Tw> &storage, Ti storageRowStart = 0,
              Ti storageColStart = 0, bool exclude_indexes = false) const;

  /// @brief Similar to GetSub without checking the bounds
  /// @param firstStart From this row/column of the matrix
  /// @param firstCount This number of rows/columns
  /// @param secondIndices From this row/column indices
  /// @param firstIsRow If true, \p firstStart and \p firstCount belong to the
  /// rows and \p secondIndices belong to the columns
  /// @param storage Put data in this matrix
  /// @param storageRowStart From this row in the storage
  /// @param storageColStart From this column in the storage
  /// @param exclude_indexes If true, it removes the rows/columns given in \p
  /// secondIndices
  void GetSub0(Ti firstStart, Ti firstCount,
               const std::vector<Ti> &secondIndices, bool firstIsRow,
               Matrix<Tw> &storage, Ti storageRowStart = 0,
               Ti storageColStart = 0, bool exclude_indexes = false) const;

  /// @brief Gets a sub-matrix
  /// @param rowIndices From these row indices
  /// @param colIndices From these column indices
  /// @param storage Put data in this matrix
  /// @param storageRowStart From this row in the storage
  /// @param storageColStart From this column in the storage
  void GetSub(std::vector<Ti> &rowIndices, std::vector<Ti> &colIndices,
              Matrix<Tw> &storage, Ti storageRowStart = 0,
              Ti storageColStart = 0) const;

  /// @brief Similar to GetSub without checking the bounds
  /// @param rowIndices From these row indices
  /// @param colIndices From these column indices
  /// @param storage Put data in this matrix
  /// @param storageRowStart From this row in the storage
  /// @param storageColStart From this column in the storage
  void GetSub0(std::vector<Ti> &rowIndices, std::vector<Ti> &colIndices,
               Matrix<Tw> &storage, Ti storageRowStart = 0,
               Ti storageColStart = 0) const;

  /// @brief Gets a sub-vector
  /// @param start From this index
  /// @param count This number of elements
  /// @param storage Put data in this vector
  /// @param storageStart From this index in the storage
  void GetSubVector(Ti start, Ti count, Matrix<Tw> &storage,
                    Ti storageStart) const;

  /// @brief Similar to \ref GetSubVector without checking the bounds
  /// @param start From this index
  /// @param count This number of elements
  /// @param storage Put data in this vector
  /// @param storageStart From this index in the storage
  void GetSubVector0(Ti start, Ti count, Matrix<Tw> &storage,
                     Ti storageStart) const;

  /// @brief Sets a row
  /// @param i Row index
  /// @param source From this vector
  void SetRow(Ti i, const Matrix<Tw> &source);

  /// @brief Similar to \ref SetRow without checking the bounds
  /// @param i Row index
  /// @param source From this vector
  void SetRow0(Ti i, const Matrix<Tw> &source);

  /// @brief Sets a row to a value
  /// @param i Row index
  /// @param value The value
  void SetRow(Ti i, Tw value);

  /// @brief Similar to \ref SetRow without checking the bounds
  /// @param i Row index
  /// @param value The value
  void SetRow0(Ti i, Tw value);

  /// @brief Adds a value to the elements of a row
  /// @param i row index
  /// @param value The value
  void SetRow_plus(Ti i, Tw value);

  /// @brief Subtracts a value from the elements of a row
  /// @param i Row index
  /// @param value The value
  void SetRow_minus(Ti i, Tw value);

  /// @brief Similar to \ref SetRow_plus without checking the bounds
  /// @param i Row index
  /// @param value The value
  void SetRow_plus0(Ti i, Tw value);

  /// @brief Similar to \ref SetRow_minus without checking the bounds
  /// @param i Row index
  /// @param value The value
  void SetRow_minus0(Ti i, Tw value);

  /// @brief Gets a row
  /// @param i Row index
  /// @param storage Puts data in this vector
  void GetRow(Ti i, Matrix<Tw> &storage) const;

  /// @brief Similar to \ref GetRow without checking the bounds
  /// @param i Row index
  /// @param storage Puts data in this vector
  void GetRow0(Ti i, Matrix<Tw> &storage) const;

  /// @brief Sets a column
  /// @param j Column index
  /// @param source From this vector
  void SetColumn(Ti j, const Matrix<Tw> &source);

  /// @brief Similar to \ref SetColumn without checking the bounds
  /// @param j Column index
  /// @param source From this vector
  void SetColumn0(Ti j, const Matrix<Tw> &source);

  /// @brief Sets a column to a value
  /// @param j Column index
  /// @param value The value
  void SetColumn(Ti j, Tw value);

  /// @brief Similar to \ref SetColumn without checking the bounds
  /// @param j Column index
  /// @param value The value
  void SetColumn0(Ti j, Tw value);

  /// @brief Adds a value to the elements of a column
  /// @param j Column index
  /// @param value The value
  void SetColumn_plus(Ti j, Tw value);

  /// @brief Subtracts a value from the elements of a column
  /// @param j Column index
  /// @param value The value
  void SetColumn_minus(Ti j, Tw value);

  /// @brief Similar to \ref SetColumn_plus without checking the bounds
  /// @param j Column index
  /// @param value The value
  void SetColumn_plus0(Ti j, Tw value);

  /// @brief Similar to \ref SetColumn_minus without checking the bounds
  /// @param j Column index
  /// @param value The value
  void SetColumn_minus0(Ti j, Tw value);

  /// @brief Gets a column
  /// @param j Column index
  /// @param storage Puts data in this vector
  void GetColumn(Ti j, Matrix<Tw> &storage) const;

  /// @brief Similar to \ref getcolumn without checking the bounds
  /// @param j Column index
  /// @param storage Puts data in this vector
  void GetColumn0(Ti j, Matrix<Tw> &storage) const;

  /// @brief Gets diagonal
  /// @param storage a matrix with length = min(rows,cols)
  void GetDiag(Matrix<Tw> &storage) const;

  /// @brief Similar to \ref GetDiag without checking the bounds
  /// @param storage
  void GetDiag0(Matrix<Tw> &storage) const;

  /// @brief Sets a row from a row of another matrix
  /// @param i Row index in this matrix
  /// @param source Gets data from this matrix
  /// @param j Row index in the source
  void SetRowFromRow(Ti i, const Matrix<Tw> &source, Ti j);

  /// @brief Similar to \ref SetRowFromRow without checking the bounds
  /// @param i Row index in this matrix
  /// @param source Gets data from this matrix
  /// @param j Row index in the source
  void SetRowFromRow0(Ti i, const Matrix<Tw> &source, Ti j);

  /// @brief Sets a column from a row of another matrix
  /// @param i Column index in this matrix
  /// @param source Gets data from this matrix
  /// @param j Row index in the source
  void SetColumnFromRow(Ti i, const Matrix<Tw> &source, Ti j);

  /// @brief Similar to \ref SetColumnFromRow without checking the bounds
  /// @param i Column index in this matrix
  /// @param source Gets data from this matrix
  /// @param j Row index in the source
  void SetColumnFromRow0(Ti i, const Matrix<Tw> &source, Ti j);

  /// @brief Sets a column from a column of another matrix
  /// @param i Column index in this matrix
  /// @param source Gets data from this matrix
  /// @param j Column index in the source
  void SetColumnFromColumn(Ti i, const Matrix<Tw> &source, Ti j);

  /// @brief Similar to \ref SetColumnFromColumn without checking the bounds
  /// @param i Column index in this matrix
  /// @param source Gets data from this matrix
  /// @param j Column index in the source
  void SetColumnFromColumn0(Ti i, const Matrix<Tw> &source, Ti j);

  /// @brief Sets a column from a diagonal of another matrix
  /// @param i Column index in this matrix
  /// @param source Gets data from this matrix
  void SetColumnFromDiag(Ti i, const Matrix<Tw> &source);

  /// @brief Similar to \ref SetColumnFromDiag without checking the bounds
  /// @param i Column index in this matrix
  /// @param source Gets data from this matrix
  void SetColumnFromDiag0(Ti i, const Matrix<Tw> &source);

  /// @brief Sets a row from a diagonal of another matrix
  /// @param i Row index in this matrix
  /// @param source Gets data from this matrix
  void SetRowFromDiag(Ti i, const Matrix<Tw> &source);

  /// @brief Similar to \ref SetRowFromDiag0 without checking the bounds
  /// @param i Row index in this matrix
  /// @param source Gets data from this matrix
  void SetRowFromDiag0(Ti i, const Matrix<Tw> &source);

  // #pragma endregion

  // #pragma region Helpers

  /// @brief Creates a diagonal Matrix
  /// @param storage A square matrix to store the result
  /// @param vDiag Value for the diagonal
  /// @param vOffDiag Value for the off-diagonal
  static void Diagonal(Matrix<Tw> &storage, Tw vDiag = 1.0, Tw vOffDiag = 0.0);

  /// @brief Determines whether the given Matrix is a diagonal one
  /// @param mat The matrix
  /// @param vDiag Value of the diagonal
  /// @param vOffDiag Value of the off-diagonal
  /// @param epsilon A value to ignore small floating point differences
  /// @return true if the matrix is a diagonal one
  static bool IsDiagonal(Matrix<Tw> &mat, Tw vDiag = 1.0, Tw vOffDiag = 0.0,
                         Tw epsilon = 1e-8);

  /// @brief Uses a vector of elements and generates a triangular Matrix. (some
  /// options might not be implemented)
  /// @param storage A matrix of size n x n
  /// @param elements vector of elements of size n(n+1)/2 or n(n-1)/2
  /// @param up If 0, it populate both upper and lower triangles. If 1, it
  /// populates just the upper triangle. If -1, it just populates the lower
  /// triangle
  /// @param diag if true, \p elements has the value of the diagonal (its size
  /// is n(n+1)/2)
  /// @param byrow determines how elements populate the Matrix
  static void MakeTriangular(Matrix<Tw> &storage, Matrix<Tw> &elements,
                             int up = 0, bool diag = true, bool byrow = true);

  /// @brief Similar to \ref tiangular without checking the sizes or the bounds
  /// @param storage
  /// @param elements
  /// @param up
  /// @param diag
  /// @param byrow
  static void MakeTriangular0(Matrix<Tw> &storage, Matrix<Tw> &elements,
                              int up = 0, bool diag = true, bool byrow = true);

  /// @brief Fills a matrix with randomly generated data from a normal
  /// distribution
  /// @param storage A vector for the results
  /// @param seed a seed for RNG
  /// @param mean First parameter of the distribution
  /// @param variance Second parameter of the distribution
  static void FillRandom_normal(Matrix<Tw> &storage, unsigned int seed,
                                Tw mean = 0, Tw variance = 1);

  /// @brief Fills a matrix with randomly generated data from a uniform
  /// distribution
  /// @param storage A vector for the results
  /// @param seed a seed for RNG
  /// @param min First parameter of the distribution
  /// @param max Second parameter of the distribution
  static void FillRandom_uniform(Matrix<Tw> &storage, unsigned int seed,
                                 Tw min = 0, Tw max = 1);

  // #pragma endregion

  // #pragma region String

  /// @brief Converts the matrix into a string
  /// @param colSep Column separator
  /// @param rowSep Row separator
  /// @param precession Number of decimal points
  /// @return String representation of the matrix
  std::string ToString(char colSep = '\t', char rowSep = '\n',
                       std::streamsize precession = 16) const;

  /// @brief Similar to \ref ToString but without a header that shows the size
  /// of the matrix
  /// @param colSep Column separator
  /// @param rowSep Row separator
  /// @param precession Number of decimal points
  /// @return String representation of the matrix
  std::string ToString0(char colSep = '\t', char rowSep = '\n',
                        std::streamsize precession = 16) const;

  /// @brief Converts the matrix to a string to be used in R
  /// @param precession Number of decimal points
  /// @param line_count Number of lines in the result (if small, some long lines
  /// might be returned)
  /// @param start Name of the matrix
  /// @param breakDim If true, dimension is added to a separate line
  /// @return A string that can create this matrix in R
  std::string ToString_R_Matrix(std::streamsize precession = 16,
                                Ti line_count = 50,
                                std::string start = "data <- ",
                                bool breakDim = true) const;

  // #pragma endregion

  // #pragma region Linear Algebra

  // #pragma region Element - wise

  /// @brief  [s_i] = [a_i + b]
  /// @param b The scalar
  /// @param storage A matrix to keep the results
  void Add(Tw b, Matrix<Tw> &storage) const;

  /// @brief Similar to \ref Add without checking the bounds
  /// @param b The scalar
  /// @param storage A matrix to keep the results
  void Add0(Tw b, Matrix<Tw> &storage) const;

  /// @brief [s_i] = [a_i * b + beta*storage_i]
  /// @param b The scalar
  /// @param storage A matrix to keep the results
  /// @param beta An scalar that is multiplied to the current values of the
  void Multiply(Tw b, Matrix<Tw> &storage, Tw beta = 0.0) const;

  /// @brief Similar to \ref Multiply without checking the bounds
  /// @param b The scalar
  /// @param storage A matrix to keep the results
  /// @param beta An scalar that is multiplied to the current values of the
  void Multiply0(Tw b, Matrix<Tw> &storage, Tw beta = 0.0) const;

  /// @brief [s_i] = [a_i - b]
  /// @param b The scalar
  /// @param storage A matrix to keep the results
  void Subtract(Tw b, Matrix<Tw> &storage) const;

  /// @brief Similar to \ref Subtract without checking the bounds
  /// @param b
  /// @param storage
  void Subtract0(Tw b, Matrix<Tw> &storage) const;

  /// @brief [s_i] = [a_i / b]
  /// @param b The scalar
  /// @param storage A matrix to keep the results
  void Divide(Tw b, Matrix<Tw> &storage) const;

  /// @brief Similar to \ref Divide without checking the bounds
  /// @param b
  /// @param storage
  void Divide0(Tw b, Matrix<Tw> &storage) const;

  /// @brief Element-wise addition: [s_i] = [a_i + b_i]
  /// @param B The second matrix
  /// @param storage A matrix to keep the results
  void Add(const Matrix<Tw> &B, Matrix<Tw> &storage) const;

  /// @brief Similar to \ref Add without checking the bounds
  /// @param B
  /// @param storage
  void Add0(const Matrix<Tw> &B, Matrix<Tw> &storage) const;

  /// @brief Element-wise multiplication: [s_i] = [a_i * b_i + beta*storage_i]
  /// @param B The second matrix
  /// @param storage A matrix to keep the results
  /// @param beta An scalar that is multiplied to the current values of the
  void Multiply(const Matrix<Tw> &B, Matrix<Tw> &storage, Tw beta = 0.0) const;

  /// @brief Similar to \ref Multiply without checking the bounds
  /// @param B
  /// @param storage
  /// @param beta
  void Multiply0(const Matrix<Tw> &B, Matrix<Tw> &storage, Tw beta = 0.0) const;

  /// @brief Element-wise subtraction: [s_i] = [a_i - b_i]
  /// @param B The second matrix
  /// @param storage A matrix to keep the results
  void Subtract(const Matrix<Tw> &B, Matrix<Tw> &storage) const;

  /// @brief Similar to \ref Subtract without checking the bounds
  /// @param B
  /// @param storage
  void Subtract0(const Matrix<Tw> &B, Matrix<Tw> &storage) const;

  /// @brief Element-wise division: [s_i] = [a_i / b_i]
  /// @param B The second matrix
  /// @param storage A matrix to keep the results
  void Divide(const Matrix<Tw> &B, Matrix<Tw> &storage) const;

  /// @brief Similar to \ref Divide without checking the bounds
  /// @param B
  /// @param storage
  void Divide0(const Matrix<Tw> &B, Matrix<Tw> &storage) const;

  /// @brief [a_i] = [a_i + b]
  /// @param b The scalar
  void Add_in(Tw b);

  /// @brief [a_i] = [a_i * b]
  /// @param b The scalar
  void Multiply_in(Tw b);

  /// @brief [a_i] = [a_i ^ b]
  /// @param b The scalar
  void Power_in(Tw b);

  /// @brief [a_i] = [a_i - b]
  /// @param b The scalar
  void Subtract_in(Tw b);

  /// @brief [a_i] = [a_i / b]
  /// @param b The scalar
  void Divide_in(Tw b);

  /// @brief [a_i] = [a_i + b_i]
  /// @param B The second matrix
  void Add_in(const Matrix<Tw> &B);

  /// @brief Similar to \ref Add_in without checking the bounds
  /// @param B
  void Add_in0(const Matrix<Tw> &B);

  /// @brief [a_i] = [a_i * b_i]
  /// @param B The second matrix
  void Multiply_in(const Matrix<Tw> &B);

  /// @brief Similar to \ref Multiply_in without checking the bounds
  /// @param B
  void Multiply_in0(const Matrix<Tw> &B);

  /// @brief [a_i] = [a_i - b_i]
  /// @param B The second matrix
  void Subtract_in(const Matrix<Tw> &B);

  /// @brief Similar to \ref Subtract_in without checking the bounds
  /// @param B
  void Subtract_in0(const Matrix<Tw> &B);

  /// @brief [a_i] = [a_i / b_i]
  /// @param B The second matrix
  void Divide_in(const Matrix<Tw> &B);

  /// @brief Similar to \ref Divide_in without checking the bounds
  /// @param B
  void Divide_in0(const Matrix<Tw> &B);

  // #pragma endregion

  // #pragma region Dot vector - vector

  /// @brief a'b (vector dot)
  /// @param b The second vector
  /// @return The result of the operation
  Tw VectorDotVector(const Matrix<Tw> &b) const;

  /// @brief Similar to VectorDotVector without checking the bounds
  /// @param b
  /// @return
  Tw VectorDotVector0(const Matrix<Tw> &b) const;

  // #pragma endregion

  // #pragma region Dot Matrix - vector

  /// @brief Matrix-vector multiplication (Ab): C = alpha * Ab + beta*C (by
  /// default alpha=1, beta=0)
  /// @param b The vector
  /// @param storage A place for storing the results
  /// @param alpha An scalar
  /// @param beta An scalar
  void DotVector(const Matrix<Tw> &b, Matrix<Tw> &storage, Tw alpha = 1.0,
                 Tw beta = 0.0) const;

  /// @brief Similar to \ref DotVector without checking the bounds
  /// @param b
  /// @param storage
  /// @param alpha
  /// @param beta
  void DotVector0(const Matrix<Tw> &b, Matrix<Tw> &storage, Tw alpha = 1.0,
                  Tw beta = 0.0) const;

  /// @brief Transposed Matrix-vector multiplication (A'b) (the first Matrix is
  /// transposed): C = alpha * A'b + beta*C (by default alpha=1, beta=0)
  /// @param b The vector
  /// @param storage A place for storing the results
  /// @param alpha An scalar
  /// @param beta An scalar
  void tDotVector(const Matrix<Tw> &b, Matrix<Tw> &storage, Tw alpha = 1.0,
                  Tw beta = 0.0) const;

  /// @brief Similar to \ref tDotVector without checking the bounds
  /// @param b
  /// @param storage
  /// @param alpha
  /// @param beta
  void tDotVector0(const Matrix<Tw> &b, Matrix<Tw> &storage, Tw alpha = 1.0,
                   Tw beta = 0.0) const;

  // #pragma endregion

  // #pragma region Dot Matrix - Matrix

  /// @brief Matrix-Matrix multiplication (AB): C = alpha * AB + beta*C
  /// @param B The second matrix
  /// @param storage A place for storing the results
  /// @param alpha An scalar
  /// @param beta An scalar
  void Dot(const Matrix<Tw> &B, Matrix<Tw> &storage, Tw alpha = 1.0,
           Tw beta = 0.0) const;

  /// @brief Similar to \ref Dot without checking bounds
  /// @param B
  /// @param storage
  /// @param alpha
  /// @param beta
  void Dot0(const Matrix<Tw> &B, Matrix<Tw> &storage, Tw alpha = 1.0,
            Tw beta = 0.0) const;

  /// @brief Matrix-Transposed Matrix multiplication (AB') (the second Matrix is
  /// transposed): C = alpha * AB' + beta*C (by default alpha=1, beta=0)
  /// @param B The second matrix
  /// @param storage A place for storing the results
  /// @param alpha An scalar
  /// @param beta An scalar
  void DotTr(const Matrix<Tw> &B, Matrix<Tw> &storage, Tw alpha = 1.0,
             Tw beta = 0.0) const;

  /// @brief similar to \ref DotTr without checking the bounds
  /// @param B
  /// @param storage
  /// @param alpha
  /// @param beta
  void DotTr0(const Matrix<Tw> &B, Matrix<Tw> &storage, Tw alpha = 1.0,
              Tw beta = 0.0) const;

  /// @brief Transposed Matrix-Matrix multiplication (A'B) (the first Matrix is
  /// transposed): C = alpha * A'B + beta*C (by default alpha=1, beta=0)
  /// @param B The second matrix
  /// @param storage A place for storing the results
  /// @param alpha An scalar
  /// @param beta An scalar
  void TrDot(const Matrix<Tw> &B, Matrix<Tw> &storage, Tw alpha = 1.0,
             Tw beta = 0.0) const;

  /// @brief Similar to \ref TrDot without checking the bounds
  /// @param B
  /// @param storage
  /// @param alpha
  /// @param beta
  void TrDot0(const Matrix<Tw> &B, Matrix<Tw> &storage, Tw alpha = 1.0,
              Tw beta = 0.0) const;

  /// @brief Transposed Matrix-Transposed Matrix multiplication (A'B') (both
  /// matrices are transposed): C = alpha * A'B' + beta*C (by default alpha=1,
  /// beta=0)
  /// @param B The second matrix
  /// @param storage A place for storing the results
  /// @param alpha An scalar
  /// @param beta An scalar
  void TrDotTr(const Matrix<Tw> &B, Matrix<Tw> &storage, Tw alpha = 1.0,
               Tw beta = 0.0) const;

  /// @brief similar to \ref TrDotTr without checking the bounds
  /// @param B
  /// @param storage
  /// @param alpha
  /// @param beta
  void TrDotTr0(const Matrix<Tw> &B, Matrix<Tw> &storage, Tw alpha = 1.0,
                Tw beta = 0.0) const;

  /// @brief C = alpha * A*A' + beta*C
  /// @param storage A place for storing the results
  /// @param setLower If true, the lower triangular is also populated
  /// @param alpha An scalar
  /// @param beta An scalar
  void Dot_AAt(Matrix<Tw> &storage, bool setLower = true, Tw alpha = 1,
               Tw beta = 0) const;

  /// @brief Similar to \ref Dot_AAt without checking the bounds
  /// @param storage
  /// @param setLower
  /// @param alpha
  /// @param beta
  void Dot_AAt0(Matrix<Tw> &storage, bool setLower = true, Tw alpha = 1,
                Tw beta = 0) const;

  /// @brief C = alpha * A'*A + beta*C
  /// @param storage A place for storing the results
  /// @param setLower If true, the lower triangular is also populated
  /// @param alpha An scalar
  /// @param beta An scalar
  void Dot_AtA(Matrix<Tw> &storage, bool setLower = true, Tw alpha = 1,
               Tw beta = 0) const;

  /// @brief Similar to \ref Dot_AtA without checking the bounds
  /// @param storage
  /// @param setLower
  /// @param alpha
  /// @param beta
  void Dot_AtA0(Matrix<Tw> &storage, bool setLower = true, Tw alpha = 1,
                Tw beta = 0) const;

  /// @brief Similar to \ref Dot_AtA but checks the NAN (It uses loops and is
  /// slower)
  /// @param storage A place for storing the results of the multiplication
  /// @param counts_storage A place for storing the lengths of each
  /// multiplication
  /// @param setLower If true, the lower triangular is also populated
  void Dot_AtA_nan(Matrix<Tw> &storage, Matrix<Tw> &counts_storage,
                   bool setLower = true) const;

  /// @brief Similar to \ref Dot_AtA_nan without checking the bounds
  /// @param storage
  /// @param counts_storage
  /// @param setLower
  void Dot_AtA_nan0(Matrix<Tw> &storage, Matrix<Tw> &counts_storage,
                    bool setLower = true) const;

  /// @brief Matrix-Symmetric Matrix multiplication: C = alpha * A * B +
  /// beta * C (where A is symmetric)
  /// @param B The second matrix
  /// @param storage A place for storing the results
  /// @param upperTriangular If the matrix is upper-triangular, let it be true.
  /// Otherwise, false.
  /// @param alpha An scalar
  /// @param beta An scalar
  void SymDot(const Matrix<Tw> &B, Matrix<Tw> &storage,
              bool upperTriangular = true, Tw alpha = 1.0, Tw beta = 0.0) const;

  /// @brief Similar to \ref SymDot without checking the bounds
  /// @param B
  /// @param storage
  /// @param upperTriangular
  /// @param alpha
  /// @param beta
  void SymDot0(const Matrix<Tw> &B, Matrix<Tw> &storage,
               bool upperTriangular = true, Tw alpha = 1.0,
               Tw beta = 0.0) const;

  /// @brief Symmetric Matrix-Matrix multiplication: C = alpha * A * B +
  /// beta * C (where B is symmetric)
  /// @param B The second matrix (which is symmetric)
  /// @param storage A place for storing the results
  /// @param upperTriangular If the matrix is upper-triangular, let it be true.
  /// Otherwise, false.
  /// @param alpha An scalar
  /// @param beta An scalar
  void DotSym(const Matrix<Tw> &B, Matrix<Tw> &storage,
              bool upperTriangular = true, Tw alpha = 1.0, Tw beta = 0.0) const;

  /// @brief Similar to \ref DotSym without checking the bounds
  /// @param B
  /// @param storage
  /// @param upperTriangular
  /// @param alpha
  /// @param beta
  void DotSym0(const Matrix<Tw> &B, Matrix<Tw> &storage,
               bool upperTriangular = true, Tw alpha = 1.0,
               Tw beta = 0.0) const;

  // #pragma endregion

  // #pragma region Dot Matrix - diagonal Matrix

  /// @brief Matrix-Diagonal Matrix multiplication
  /// @param b Diagonal matrix as a vector
  /// @param storage A place for storing the result
  void DotDiag(const Matrix<Tw> &b, Matrix<Tw> &storage) const;

  /// @brief Similar to \ref DotDiag without checking the bounds
  /// @param b
  /// @param storage
  void DotDiag0(const Matrix<Tw> &b, Matrix<Tw> &storage) const;

  /// @brief Diagonal Matrix-Matrix multiplication
  /// @param b Diagonal matrix as a vector
  /// @param storage A place for storing the result
  void DiagDot(const Matrix<Tw> &b, Matrix<Tw> &storage) const;

  /// @brief Similar to \ref DiagDot without checking bounds
  /// @param b
  /// @param storage
  void DiagDot0(const Matrix<Tw> &b, Matrix<Tw> &storage) const;

  // #pragma endregion

  // #pragma region Others

  /// @brief Transposes this Matrix
  void Transpose();

  /// @brief Transposes a matrix
  /// @param storage A place for storing the result
  void Transpose(Matrix<Tw> &storage) const;

  /// @brief Similar to \ref Transpose without checking the bounds
  /// @param storage
  void Transpose0(Matrix<Tw> &storage) const;

  /// @brief Kronecker product of two matrices
  /// @param B The second matrix
  /// @param storage A place for storing the result
  void Kron(const Matrix<Tw> &B, Matrix<Tw> &storage) const;

  /// @brief Similar to \ref Kron without checking the bounds
  /// @param B
  /// @param storage
  void Kron0(const Matrix<Tw> &B, Matrix<Tw> &storage) const;

  /// @brief Kronecker product of a Matrix with an identity Matrix
  /// @param m Size of the identity matrix
  /// @param storage A place for storing the result
  void KronIden(Ti m, Matrix<Tw> &storage) const;

  /// @brief Similar to \ref KronIden without checking the bounds
  /// @param m
  /// @param storage
  void KronIden0(Ti m, Matrix<Tw> &storage) const;

  /// @brief Kronecker product of an identity Matrix with this Matrix
  /// @param m Size of the identity matrix
  /// @param storage A place for storing the result
  void IdenKron(Ti m, Matrix<Tw> &storage) const;

  /// @brief Similar to \ref IdenKron without checking the bounds
  /// @param m
  /// @param storage
  void IdenKron0(Ti m, Matrix<Tw> &storage) const;

  /// @brief Kronecker product of a transposed matrix with an identity matrix
  /// @param m Size of the identity matrix
  /// @param storage A place for storing the result
  void TrKronIden(Ti m, Matrix<Tw> &storage) const;

  /// @brief Similar to \ref tIkron without checking the bounds
  /// @param m
  /// @param storage
  void TrKronIden0(Ti m, Matrix<Tw> &storage) const;

  // #pragma endregion

  // #pragma region decompositions

  /// @brief Calculates the determinant of a positive definite Matrix (it uses
  /// Cholesky decomposition) and destroys the matrix
  /// @return NAN if Cholesky decomposition fails. Otherwise, the determinant
  Tw Det_pd0();

  /// @brief Finds the inverse of this 2x2 Matrix
  /// @return -1 if determinant is zero. Otherwise, 0.
  Ti Inv2x2();

  /// @brief returns the norm of the matrix
  /// @param norm norm='1': one norm; ='F': Frobenius norm; ='I': infinity norm;
  /// ='M': largest absolute value
  /// @return The norm
  Tw Norm(const char norm) const;

  /// @brief Finds the inverse of this Matrix
  /// @param storage A place for storing the result
  /// @return 0 if successful. Negative value (-i) if i-th argument had
  /// an illegal value. positive value if it is a singular matrix
  Ti Inv(Matrix<Tw> &storage) const;

  /// @brief Similar to \ref inv without checking the bounds
  /// @return
  Ti Inv0();

  /// @brief Similar to \ref inv0 with required working arrays
  /// @param ipiv For an MxM matrix, it is an M+1 array
  /// @param work For an MxM matrix, it is an M*M array
  /// @return
  Ti Inv00(int *ipiv, Tw *work);

  /// @brief Cholesky decomposition of a symmetric Matrix
  /// @param storage A matrix to store upper or lower triangular matrix.
  /// @param upper If true lower triangle is zero and C=U'U, otherwise upper
  /// triangle is zero and C=LL'
  /// @return 0 means success. if -i, the i - th argument had an illegal value.
  /// if i, the leading minor of order i is not positive definite,
  /// and the factorization could not be completed.
  Ti Chol(Matrix<Tw> &storage, bool upper = true) const;

  /// @brief Similar to \ref Chol without any checks
  /// @param upper
  /// @return
  Ti Chol0(bool upper);

  /// @brief Calculates determinant using LU decomposition. The matrix will be
  /// destroyed.
  /// @return The determinant
  Tw Det();

  /*
  int lu(Matrix<Tw>& L, Matrix<Tw>& U);

  int lu0();


  int lu00(Ti* ipiv);
  */

  /// @brief QR decomposition
  /// @param Q A storage for the Q
  /// @param R A storage for the R
  /// @return
  Ti QR(Matrix<Tw> &Q, Matrix<Tw> &R);

  /// @brief Similar to qr without any checks
  /// @param tau
  /// @return
  Ti QR0(Tw *tau);

  /// @brief Solves a triangular system of linear equations
  /// @param b b in AX=b. On exit, it contains the solution.
  /// @param upper Upper triangle of A is stored, otherwise Lower
  /// triangle of A is stored
  /// @param transpose The system is A'X=b
  /// @param unitDiag Set true if the diagonal of A is 1
  /// @return 0 for successful exit. If -i, the i-th argument had an illegal
  /// value. If i, the i-th diagonal element of A is zero, indicating that the
  /// matrix is singular and the solutions have not been computed.
  Ti SolveTrian(Matrix<Tw> &b, bool upper = true, bool transpose = false,
                bool unitDiag = false);

  /// @brief Similar to \ref SolveTrian without any checks
  /// @param b
  /// @param upper
  /// @param transpose
  /// @param unitDiag
  /// @return
  Ti SolveTrian0(Matrix<Tw> &b, bool upper = true, bool transpose = false,
                 bool unitDiag = false);

  /// @brief Solves AX=b or A'X=b where A (this matrix) is positive definite
  /// (and therefore symmetric)
  /// @param b b in AX=b. On exit, it contains the solution
  /// @param upper Upper triangle of A is stored, otherwise Lower triangle of A
  /// is stored
  /// @return
  Ti SolvePos(Matrix<Tw> &b, bool upper = true);

  /// @brief Similar to \ref SolvePos without any checks
  /// @param b
  /// @param upper
  /// @return
  Ti SolvePos0(Matrix<Tw> &b, bool upper = true);

  /// @brief Calculates singular value decomposition
  /// @param Data Matrix as an array
  /// @param M
  /// @param N
  /// @param WORK
  /// @param lwork
  /// @param U
  /// @param S
  /// @param VT
  /// @param jobU
  /// @param jobVT
  /// @return
  static Ti SVD0(Tw *Data, const Ti M, const Ti N, Tw *WORK, Ti lwork,
                 Matrix<Tw> &U, Matrix<Tw> &S, Matrix<Tw> &VT, const char jobU,
                 const char jobVT);

  // #pragma endregion

  // #pragma endregion

  // #pragma region Statistics

  /// @brief Finds the maximum
  /// @return The maximum
  Tw Maximum() const;

  /// @brief Finds the minimum
  /// @return The minimum
  Tw Minimum() const;

  /// @brief Finds the maximum and its position
  /// @param rowIndex On exit, row index of the maximum
  /// @param colIndex On exit, column index of the maximum
  /// @return The maximum
  Tw max(Ti &rowIndex, Ti &colIndex) const;

  /// @brief Finds the minimum and its position
  /// @param rowIndex On exit, row index of the minimum
  /// @param colIndex On exit, column index of the minimum
  /// @return The minimum
  Tw min(Ti &rowIndex, Ti &colIndex) const;

  /// @brief Finds the maximum value in a specific row and its column index (1st
  /// occurrence)
  /// @param i Row index
  /// @param colIndex On exit, column index of the maximum
  /// @return the maximum
  Tw MaximumInRow(Ti i, Ti &colIndex) const;

  /// @brief Finds the minimum value in a specific row and its column index (1st
  /// occurrence)
  /// @param i Row index
  /// @param colIndex On exit, column index of the minimum
  /// @return the minimum
  Tw MinimumInRow(Ti i, Ti &colIndex) const;

  /// @brief Finds the maximum value in a specific column and its row index (1st
  /// occurrence)
  /// @param i Column index
  /// @param colIndex On exit, row index of the maximum
  /// @return the maximum
  Tw MaximumInColumn(Ti j, Ti &rowIndex) const;

  /// @brief Finds the minimum value in a specific column and its row index (1st
  /// occurrence)
  /// @param i Column index
  /// @param colIndex On exit, row index of the minimum
  /// @return the minimum
  Tw MinimumInColumn(Ti j, Ti &rowIndex) const;

  /// @brief Calculates the trace of the matrix
  /// @return The trace
  Tw Trace() const;

  /// @brief Calculates the sum of all elements
  /// @return The sum
  Tw Sum() const;

  /// @brief Calculates the mean of all elements
  /// @param check_nan If true, it ignores the NANs
  /// @return The mean
  Tw Mean(bool check_nan = false) const;

  /// @brief Calculates the variance of all elements
  /// @param mean On exit, it is the mean
  /// @param sample If true, it uses sample statistics
  /// @param check_nan If true, it ignores the NANs
  /// @return The variance
  Tw Variance(Tw &mean, bool sample = false, bool check_nan = false) const;

  /// @brief Calculates the variance of a column
  /// @param j Column index
  /// @param mean On exit, it is the mean
  /// @param count On exit, it is the number of elements used to calculate the
  /// value
  /// @param sample If true, it uses sample statistics
  /// @param check_nan If true, it ignores the NANs
  /// @return The variance
  Tw VarianceColumn(Ti j, Tw &mean, Ti &count, bool sample,
                    bool check_nan) const;

  /// @brief Calculates covariance between two columns
  /// @param j1 First column index
  /// @param j2 Second column index
  /// @param mean1 On exit, it is the mean of the first column
  /// @param mean2 On exit, it is the mean of the second column
  /// @param count On exit, it is the number of elements used to calculate the
  /// value
  /// @param sample If true, it uses sample statistics
  /// @param check_nan If true, it ignores the NANs
  /// @return The covariance
  Tw CovarianceColumn(Ti j1, Ti j2, Tw &mean1, Tw &mean2, Ti &count,
                      bool sample, bool check_nan) const;

  /// @brief Calculates the correlation coefficient between two columns
  /// @param j1 First column index
  /// @param j2 Second column index
  /// @param mean1 On exit, it is the mean of the first column
  /// @param mean2 On exit, it is the mean of the second column
  /// @param var1 On exit, it is the variance of the first column
  /// @param var2 On exit, it is the variance of the second column
  /// @param count On exit, it is the number of elements used to calculate the
  /// value
  /// @param sample If true, it uses sample statistics
  /// @param check_nan If true, it ignores the NANs
  /// @return The correlation coefficient
  Tw CorrelationColumn(Ti j1, Ti j2, Tw &mean1, Tw &mean2, Tw &var1, Tw &var2,
                       Ti &count, bool sample, bool check_nan) const;

  /// @brief Calculates the sum of the elements in the rows
  /// @param storage A vector to keep the results
  /// @param rowIndices Row indices. Uses all rows if empty
  void RowsSum(Matrix<Tw> &storage, std::vector<Ti> &rowIndices) const;

  /// @brief Calculates the sum of the elements in the columns
  /// @param storage A vector to keep the results
  /// @param colIndices Column indices. Uses all columns if empty
  void ColumnsSum(Matrix<Tw> &storage, std::vector<Ti> &colIndices) const;

  /// @brief Calculates the mean of the elements in the columns
  /// @param storage A vector to keep the results
  /// @param colIndices Column indices. Uses all columns if empty
  void ColumnsMean(Matrix<Tw> &storage, std::vector<Ti> &colIndices) const;

  /// @brief Calculates the variance matrix of the columns
  /// @param storage m x m matrix where m is the number of given column indices
  /// @param colIndices Column indices. Uses all columns if empty
  /// @param sample If true, it uses sample statistics
  void ColumnsVariance(Matrix<Tw> &storage, std::vector<Ti> &colIndices,
                       bool sample = true) const;

  /// @brief Standardizes the columns
  /// @param means If null, it does not demeans the columns
  /// @param stds If null, it does not divide the columns by anything
  /// @param is_var True means you gave the array of variances instead of
  /// standard errors
  void ColumnsStandard(const Matrix<Tw> *means,
                       const Matrix<Tw> *stds = nullptr, bool is_var = false);

  /// @brief Calculates the columns means
  /// @param storage_mean A place to keep the result
  /// @param check_nan If true, it checks and ignores the NANs
  void ColumnsMeans(Matrix<Tw> &storage_mean, bool check_nan = false) const;

  /// @brief Calculates the columns variances
  /// @param storage_var A place to keep the result
  /// @param sample If true, it uses sample statistics
  /// @param check_nan If true, it checks and ignores the NANs
  void ColumnsVariances(Matrix<Tw> &storage_var, bool sample = true,
                        bool check_nan = false) const;

  /// @brief Calculates the columns means and variances
  /// @param storage_mean A place to keep the calculated means
  /// @param storage_var A place to keep the calculated variances
  /// @param sample If true, it uses sample statistics
  /// @param check_nan If true, it checks and ignores the NANs
  void ColumnsMeansVariances(Matrix<Tw> &storage_mean, Matrix<Tw> &storage_var,
                             bool sample = true, bool check_nan = false) const;

  // #pragma endregion
};

/// @brief Upper triangle of a square Matrix. If M x M, its storage is M*(M-1)/2
/// (without diagonal) or M*(M+1)/2 (with diagonal)
/// @tparam Tw Type of the data
/// @tparam has_diag If true, the matrix has a diagonal (If affects the length
/// of the inner array).
template <bool has_diag = true, class Tw = Tv> class LDT_EXPORT MatrixSym {

public:
  /// @brief Number of rows (or columns)
  Ti RowsCount = 0;

  /// @brief Inner data array
  Tw *Data = nullptr;

  /// @brief Initializes a new instance of the class
  MatrixSym() {}

  /// @brief Initializes a new instance of the class with null data
  /// @param rows Number of rows
  MatrixSym(Ti rows);

  /// @brief Initializes a new instance of the class
  /// @param values Data of the upper triangle in the matrix. If M x M, its
  /// storage is M*(M-1)/2 (without diagonal) or M*(M+1)/2 (with diagonal)
  /// @param rows Number of rows
  MatrixSym(Tw *values, Ti rows);

  /// @brief Calculates the length of the inner array from the \ref RowsCount
  /// @return The length
  Ti length_array() const;

  /// @brief Sets the inner data
  /// @param data Pointer to the new data
  /// @param newRows New dimension. If -1, current dimension is unchanged.
  void SetData(Tw *data, Ti newRows = -1);

  /// @brief Sets the inner data
  /// @param defaultValue A value for all elements of the matrix
  /// @param data Pointer to the new data
  /// @param newRows New dimension. If -1, current dimension is unchanged.
  void SetData(Tw defaultValue, Tw *data, Ti newRows = -1);

  /// @brief Gets the (i,j)-th element
  /// @param i Row index
  /// @param j Column index
  /// @return Value at the given position
  Tw Get(Ti i, Ti j) const;

  /// @brief Similar to \ref Get without checking the bounds
  /// @param i
  /// @param j
  /// @return
  Tw Get0(Ti i, Ti j) const;

  /// @brief Sets the (i,j)-th element to a value
  /// @param i Row index
  /// @param j Column index
  /// @param value The value
  void Set(Ti i, Ti j, Tw value);

  /// @brief Similar to \ref Set without checking the bounds
  /// @param i
  /// @param j
  /// @param value
  void Set0(Ti i, Ti j, Tw value);

  /// @brief Converts the inner data to string
  /// @param colSep Column separator
  /// @param rowSep Row separator
  /// @param precession A value to ignore small floating point differences
  /// @return String representation of this matrix
  std::string ToString(char colSep = '\t', char rowSep = '\n',
                       std::streamsize precession = 16) const;

  /// @brief Determines if any value is equal to the given value
  /// @param value The given value
  /// @return true, if any equal value is found
  bool Any(Tw value) const;

  /// @brief Determines whether all values are equal to the given value
  /// @param value The given value
  /// @return true, if all values are equal to the given value
  bool All(Tw value) const;
};

extern template class ldt::Matrix<Ti>;
extern template class ldt::Matrix<Tv>;

extern template class ldt::MatrixSym<true, Tv>;
extern template class ldt::MatrixSym<true, Ti>;

extern template class ldt::MatrixSym<false, Tv>;
extern template class ldt::MatrixSym<false, Ti>;

} // namespace ldt
