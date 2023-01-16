/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the LGPL 3.0.
 See accompanying file LICENSE
*/

#include "matrix_utils.h"

using namespace ldt;

template <typename Tw>
MatrixSvd<Tw>::MatrixSvd(Ti rows, Ti cols, char jobU, char jobVT) {
  if (cols <= 0 || rows <= 0)
    throw std::logic_error("invalid size in 'svd'.");
  mJobU = jobU;
  mJobVT = jobVT;
  auto mn = std::min(rows, cols);
  S = Matrix<Tw>();
  StorageSize = mn;

  if (jobU != 'N') {
    U = Matrix<Tw>();
    StorageSize += rows * rows;
  }
  if (jobVT != 'N') {
    VT = Matrix<Tw>();
    StorageSize += cols * cols;
  }

  W_svd = 2 * // ?!  // see the 'DGESVD' documentation
          std::max(3 * std::min(rows, cols) + std::max(rows, cols),
                   5 * std::min(rows, cols));
  WorkSize = rows * cols + W_svd;
}

template <typename Tw>
void MatrixSvd<Tw>::Calculate(const Matrix<Tw> &mat, Tw *storage, Tw *work) {

  auto temp = MatrixSvd(mat.RowsCount, mat.ColsCount, mJobU, mJobVT);
  if (temp.StorageSize > StorageSize || temp.WorkSize > WorkSize)
    throw std::logic_error("inconsistent arguments in 'MatrixSvd'.");
  Calculate0(mat, storage, work);
}

template <typename Tw>
void MatrixSvd<Tw>::Calculate0(const Matrix<Tw> &mat, Tw *storage, Tw *work) {

  Ti rows = mat.RowsCount;
  Ti cols = mat.ColsCount;
  auto mn = std::min(rows, cols);

  Ti q = 0;
  if (mJobU != 'N') {
    U.SetData(&storage[q], rows, rows);
    q += rows * rows;
  }
  S.SetData(&storage[q], mn, 1);
  q += mn;
  if (mJobVT != 'N') {
    VT.SetData(&storage[q], cols, cols);
    q += cols * cols;
  }

  auto copy_mat = Matrix<Tw>(work, rows, cols);
  mat.CopyTo00(copy_mat);

  // Ti lwork = 0;
  auto info = Matrix<Tw>::SVD0(copy_mat.Data, rows, cols, &work[rows * cols],
                               W_svd, U, S, VT, mJobU, mJobVT);
  if (info != 0)
    throw std::logic_error("svd failed");
}

template class ldt::MatrixSvd<Tv>;
