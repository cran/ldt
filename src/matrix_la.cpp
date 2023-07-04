/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the GPL 3.0.
 See accompanying file LICENSE
*/

#include "matrix.h"

#include "blas.h"
#include "lapack.h"

#include <functional>
#include <iomanip>
#include <iostream>
#include <numeric>
#include <random>
#include <sstream>
#include <vector>

using namespace ldt;

template <typename Tw> void Matrix<Tw>::CopyTo00(Matrix<Tw> &storage) const {

  Ti N = length();
  Ti incx = 1;
  Ti incy = 1;

  if constexpr (std::is_same<Tw, double>()) {
    dcopy_(&N, Data, &incx, storage.Data, &incy);
  } else if constexpr (std::is_same<Tw, float>()) {
    scopy_(&N, Data, &incx, storage.Data, &incy);
  } else if constexpr (true) {
    for (Ti i = 0; i < length(); i++)
      storage.Data[i] = Data[i];
  }
}

template <typename Tw>
Tw Matrix<Tw>::VectorDotVector0(const Matrix<Tw> &b) const {

  Ti n = length();
  Ti incx = 1;
  Ti incy = 1;

  if constexpr (std::is_same<Tw, double>()) {
    return ddot_(&n, Data, &incx, b.Data, &incy);
  } else if constexpr (std::is_same<Tw, float>()) {
    return sdot_(&n, Data, &incx, b.Data, &incy);
  } else if constexpr (true) {
    throw std::logic_error("not implemented: ?dot");
  }
}

template <typename Tw>
void Matrix<Tw>::DotVector0(const Matrix<Tw> &b, Matrix<Tw> &storage, Tw alpha,
                            Tw beta) const {
  // y := alpha*A*x + beta*y

  const Tw *opA = Data;
  const Tw *opB = b.Data;
  Tw *C = storage.Data;

  Ti M = this->RowsCount;
  Ti N = this->ColsCount;
  Ti incx = 1;
  Ti incy = 1;

  char transpose = 'N';

  if constexpr (std::is_same<Tw, double>()) {
    dgemv_(&transpose, &M, &N, &alpha, opA, &M, opB, &incx, &beta, C, &incy);
  } else if constexpr (std::is_same<Tw, float>()) {
    sgemv_(&transpose, &M, &N, &alpha, opA, &M, opB, &incx, &beta, C, &incy);
  } else if constexpr (true) {
    throw std::logic_error("not implemented: ?gemv");
  }
}

template <typename Tw>
void Matrix<Tw>::tDotVector0(const Matrix<Tw> &b, Matrix<Tw> &storage, Tw alpha,
                             Tw beta) const {
  // y := alpha*A*x + beta*y

  const Tw *opA = Data;
  const Tw *opB = b.Data;
  Tw *C = storage.Data;

  Ti M = this->RowsCount;
  Ti N = this->ColsCount;
  Ti incx = 1;
  Ti incy = 1;

  auto transpose = 'T';

  if constexpr (std::is_same<Tw, double>()) {
    dgemv_(&transpose, &M, &N, &alpha, opA, &M, opB, &incx, &beta, C, &incy);
  } else if constexpr (std::is_same<Tw, float>()) {
    sgemv_(&transpose, &M, &N, &alpha, opA, &M, opB, &incx, &beta, C, &incy);
  } else if constexpr (true) {
    throw std::logic_error("not implemented: ?gemv");
  }
}

template <typename Tw>
void Matrix<Tw>::Dot0(const Matrix<Tw> &b, Matrix<Tw> &storage, Tw alpha,
                      Tw beta) const {
  // C := alpha*op( A )*op( B ) + beta*C

  const Tw *opA = Data;   // M x K
  const Tw *opB = b.Data; // K x N
  Tw *C = storage.Data;   // M x N

  auto transpose_a = 'N';
  auto transpose_b = 'N';

  Ti M = this->RowsCount;
  Ti N = b.ColsCount;
  Ti K = this->ColsCount;
  // Ti incx = 1;
  // Ti incy = 1;

  if constexpr (std::is_same<Tw, double>()) {
    dgemm_(&transpose_a, &transpose_b, &M, &N, &K, &alpha, opA, &M, opB, &K,
           &beta, C, &M);
  } else if constexpr (std::is_same<Tw, float>()) {
    sgemm_(&transpose_a, &transpose_b, &M, &N, &K, &alpha, opA, &M, opB, &K,
           &beta, C, &M);
  } else if constexpr (true) {
    throw std::logic_error("not implemented: ?gemm");
  }
}

template <typename Tw>
void Matrix<Tw>::DotTr0(const Matrix<Tw> &b, Matrix<Tw> &storage, Tw alpha,
                        Tw beta) const {
  // C := alpha*op( A )*op( B ) + beta*C

  const Tw *opA = Data;   // M x K
  const Tw *opB = b.Data; // K x N
  Tw *C = storage.Data;   // M x N

  auto transpose_a = 'N';
  auto transpose_b = 'T';

  Ti M = this->RowsCount;
  Ti N = b.RowsCount;
  Ti K = this->ColsCount;
  // Ti incx = 1;
  // Ti incy = 1;

  if constexpr (std::is_same<Tw, double>()) {
    dgemm_(&transpose_a, &transpose_b, &M, &N, &K, &alpha, opA, &M, opB, &N,
           &beta, C, &M);
  } else if constexpr (std::is_same<Tw, float>()) {
    sgemm_(&transpose_a, &transpose_b, &M, &N, &K, &alpha, opA, &M, opB, &N,
           &beta, C, &M);
  } else if constexpr (true) {
    throw std::logic_error("not implemented: ?gemm");
  }
}

template <typename Tw>
void Matrix<Tw>::TrDot0(const Matrix<Tw> &b, Matrix<Tw> &storage, Tw alpha,
                        Tw beta) const {
  // C := alpha*op( A )*op( B ) + beta*C

  const Tw *opA = Data;
  const Tw *opB = b.Data;
  Tw *C = storage.Data;

  auto transpose_a = 'T';
  auto transpose_b = 'N';

  Ti M = this->ColsCount;
  Ti N = b.ColsCount;
  Ti K = this->RowsCount;
  // Ti incx = 1;
  // Ti incy = 1;

  if constexpr (std::is_same<Tw, double>()) {
    dgemm_(&transpose_a, &transpose_b, &M, &N, &K, &alpha, opA, &K, opB, &K,
           &beta, C, &M);
  } else if constexpr (std::is_same<Tw, float>()) {
    sgemm_(&transpose_a, &transpose_b, &M, &N, &K, &alpha, opA, &K, opB, &K,
           &beta, C, &M);
  } else if constexpr (true) {
    throw std::logic_error("not implemented: ?gemm");
  }
}

template <typename Tw>
void Matrix<Tw>::TrDotTr0(const Matrix<Tw> &b, Matrix<Tw> &storage, Tw alpha,
                          Tw beta) const {
  // C := alpha*op( A )*op( B ) + beta*C

  const Tw *opA = Data;
  const Tw *opB = b.Data;
  Tw *C = storage.Data;

  auto transpose_a = 'T';
  auto transpose_b = 'T';

  Ti M = this->ColsCount;
  Ti N = b.RowsCount;
  Ti K = this->RowsCount;
  // Ti incx = 1;
  // Ti incy = 1;

  if constexpr (std::is_same<Tw, double>()) {
    dgemm_(&transpose_a, &transpose_b, &M, &N, &K, &alpha, opA, &K, opB, &N,
           &beta, C, &M);
  } else if constexpr (std::is_same<Tw, float>()) {
    sgemm_(&transpose_a, &transpose_b, &M, &N, &K, &alpha, opA, &K, opB, &N,
           &beta, C, &M);
  } else if constexpr (true) {
    throw std::logic_error("not implemented: ?gemm");
  }
}

template <typename Tw>
void Matrix<Tw>::DotDiag0(const Matrix<Tw> &b, Matrix<Tw> &storage) const {
  for (Ti j = 0; j < RowsCount; j++) {
    for (Ti i = 0; i < RowsCount; i++)
      storage.Set0(i, j, b.Data[j] * Get0(i, j));
  }
}

template <typename Tw>
void Matrix<Tw>::DiagDot0(const Matrix<Tw> &b, Matrix<Tw> &storage) const {
  for (Ti i = 0; i < length(); i++)
    for (Ti j = 0; j < b.ColsCount; j++)
      storage.Set0(i, j, Data[i] * b.Get0(i, j));
}

template <typename Tw>
void Matrix<Tw>::Dot_AAt0(Matrix<Tw> &storage, bool setLower, Tw alpha,
                          Tw beta) const {

  const Tw *A = Data;
  Tw *C = storage.Data;

  Ti M = this->RowsCount;
  Ti N = this->ColsCount;
  auto uplo = 'U';
  auto transpose = 'N';

  if constexpr (std::is_same<Tw, double>()) {
    dsyrk_(&uplo, &transpose, &M, &N, &alpha, A, &M, &beta, C, &M);
  } else if constexpr (std::is_same<Tw, float>()) {
    ssyrk_(&uplo, &transpose, &M, &N, &alpha, A, &M, &beta, C, &M);
  } else if constexpr (true) {
    throw std::logic_error("not implemented: ?ssyrk");
  }

  // set lower part of the storage
  if (setLower) {
    for (Ti i = 0; i < RowsCount; i++)
      for (Ti j = 0; j < RowsCount; j++) {
        if (i <= j)
          continue;
        storage.Set0(i, j, storage.Get0(j, i));
      }
  }
}

template <typename Tw>
void Matrix<Tw>::Dot_AtA0(Matrix<Tw> &storage, bool setLower, Tw alpha,
                          Tw beta) const {

  const Tw *A = Data;
  Tw *C = storage.Data;

  Ti M = this->RowsCount;
  Ti N = this->ColsCount;
  auto uplo = 'U';
  auto transpose = 'T';

  if constexpr (std::is_same<Tw, double>()) {
    dsyrk_(&uplo, &transpose, &N, &M, &alpha, A, &M, &beta, C, &N);
  } else if constexpr (std::is_same<Tw, float>()) {
    ssyrk_(&uplo, &transpose, &N, &M, &alpha, A, &M, &beta, C, &N);
  } else if constexpr (true) {
    throw std::logic_error("not implemented: ?ssyrk");
  }

  // set lower part of the storage
  if (setLower) {
    for (Ti i = 0; i < ColsCount; i++)
      for (Ti j = 0; j < ColsCount; j++) {
        if (i <= j)
          continue;
        storage.Set0(i, j, storage.Get0(j, i));
      }
  }
}

template <typename Tw>
void Matrix<Tw>::Dot_AtA_nan0(Matrix<Tw> &storage, Matrix<Tw> &counts_storage,
                              bool set_lower) const {
  if constexpr (std::numeric_limits<Tw>::has_quiet_NaN == false) {
    throw std::logic_error(
        "invalid operation. no NAN for this type. Dot_AtA_nan");
  } else if constexpr (true) {
    Ti c, i, j, k;
    Tw s, v;
    Tw *col1, *col2;

    for (i = 0; i < ColsCount; i++) {
      col1 = &Data[i * RowsCount];
      for (j = 0; j < ColsCount; j++) {
        col2 = &Data[j * RowsCount];
        s = 0;
        c = 0;
        for (k = 0; k < RowsCount; k++) {
          v = col1[k] * col2[k];
          if (std::isnan(v))
            continue;
          c++;
          s += v;
        }
        storage.Set0(j, i, s);
        counts_storage.Set0(j, i, c);
        if (set_lower) {
          storage.Set0(i, j, s);
          counts_storage.Set0(i, j, c);
        }
      }
    }
  }
}

template <typename Tw>
void Matrix<Tw>::SymDot0(const Matrix<Tw> &b, Matrix<Tw> &storage,
                         bool uppertrig, Tw alpha, Tw beta) const {

  const Tw *A = Data;
  const Tw *B = b.Data;
  Tw *C = storage.Data;

  auto uplo = uppertrig ? 'U' : 'L';
  auto side = 'L';

  Ti M = this->RowsCount;
  Ti N = b.ColsCount;
  Ti K = this->ColsCount;

  if constexpr (std::is_same<Tw, double>()) {
    dsymm_(&side, &uplo, &M, &N, &alpha, A, &M, B, &K, &beta, C, &M);
  } else if constexpr (std::is_same<Tw, float>()) {
    ssymm_(&side, &uplo, &M, &N, &alpha, A, &M, B, &K, &beta, C, &M);
  } else if constexpr (true) {
    throw std::logic_error("not implemented: ?symm");
  }
}

template <typename Tw>
void Matrix<Tw>::DotSym0(const Matrix<Tw> &b, Matrix<Tw> &storage,
                         bool uppertrig, Tw alpha, Tw beta) const {

  const Tw *B = Data;
  const Tw *A = b.Data;
  Tw *C = storage.Data;

  auto uplo = uppertrig ? 'L' : 'U'; // check the test
  auto side = 'R';

  Ti M = storage.RowsCount;
  Ti N = storage.ColsCount;

  if constexpr (std::is_same<Tw, double>()) {
    dsymm_(&side, &uplo, &M, &N, &alpha, A, &N, B, &M, &beta, C, &M);
  } else if constexpr (std::is_same<Tw, float>()) {
    ssymm_(&side, &uplo, &M, &N, &alpha, A, &N, B, &M, &beta, C, &M);
  } else if constexpr (true) {
    throw std::logic_error("not implemented: ?symm");
  }
}

template <typename Tw> void Matrix<Tw>::Transpose0(Matrix<Tw> &storage) const {
  for (Ti i = 0; i < RowsCount; i++) {
    for (Ti j = 0; j < ColsCount; j++) {
      storage.Set0(j, i, Get0(i, j));
    }
  }
}

template <typename Tw> void Matrix<Tw>::Transpose() {
  Ti c, a;
  if (RowsCount == ColsCount) {
    for (Ti i = 0; i < RowsCount; i++) {
      for (Ti j = i + 1; j < ColsCount; j++) {
        c = i + RowsCount * j;
        a = j + RowsCount * i;
        std::swap(Data[a], Data[c]);
      }
    }
  } else {
    // see: https://en.wikipedia.org/wiki/In-place_matrix_transposition

    const Ti N = length() - 1;
    std::vector<bool> visited(N);
    for (c = 0; c < N; c++) {
      if (visited.at(c))
        continue;
      a = c;
      while (true) {
        a = (ColsCount * a) % N;
        visited.at(a) = true;
        std::swap(Data[a], Data[c]);
        if (a == c)
          break;
      }
    }
    std::swap(RowsCount, ColsCount);
  }
}

template <typename Tw>
void Matrix<Tw>::Kron0(const Matrix<Tw> &B, Matrix<Tw> &storage) const {
  auto m = RowsCount;
  auto n = ColsCount;
  auto p = B.RowsCount;
  auto q = B.ColsCount;

  Ti row = 0;
  for (Ti i = 0; i < m; i++) {
    for (Ti ii = 0; ii < p; ii++) {
      Ti col = 0;
      for (Ti j = 0; j < n; j++) {
        for (Ti jj = 0; jj < q; jj++) {
          storage.Set0(row, col, Get0(i, j) * B.Get0(ii, jj));
          col++;
        }
      }
      row++;
    }
  }
}

template <typename Tw>
void Matrix<Tw>::KronIden0(Ti h,
                           Matrix<Tw> &storage) const { // TODO: use 'setsub'
  auto m = RowsCount;
  auto n = ColsCount;

  Ti row = 0;
  for (Ti i = 0; i < m; i++) {
    for (Ti ii = 0; ii < h; ii++) {
      Ti col = 0;
      for (Ti j = 0; j < n; j++) {
        for (Ti jj = 0; jj < h; jj++) {
          storage.Set0(row, col, Get0(i, j) * (ii == jj ? 1 : 0));
          col++;
        }
      }
      row++;
    }
  }
}

template <typename Tw>
void Matrix<Tw>::IdenKron0(Ti h,
                           Matrix<Tw> &storage) const { // TODO: use 'setsub'
  auto m = RowsCount;
  auto n = ColsCount;

  Ti row = 0;
  for (Ti ii = 0; ii < h; ii++) {
    for (Ti i = 0; i < m; i++) {
      Ti col = 0;
      for (Ti jj = 0; jj < h; jj++) {
        for (Ti j = 0; j < n; j++) {

          storage.Set0(row, col, Get0(i, j) * (ii == jj ? 1 : 0));
          col++;
        }
      }
      row++;
    }
  }
}

template <typename Tw>
void Matrix<Tw>::TrKronIden0(Ti h,
                             Matrix<Tw> &storage) const { // TODO: use 'setsub'
  auto m = ColsCount;
  auto n = RowsCount;

  Ti row = 0;
  for (Ti i = 0; i < m; i++) {
    for (Ti ii = 0; ii < h; ii++) {
      Ti col = 0;
      for (Ti j = 0; j < n; j++) {
        for (Ti jj = 0; jj < h; jj++) {
          storage.Set0(row, col, Get0(j, i) * (ii == jj ? 1 : 0));
          col++;
        }
      }
      row++;
    }
  }
}

template <typename Tw> Ti Matrix<Tw>::Inv2x2() {
  if constexpr (std::is_floating_point<Tw>()) {

    Tw a = Data[0];
    Tw b = Data[1];
    Tw c = Data[2];
    Tw d = Data[3];
    Tw idet;
    idet = static_cast<Tw>((Tw)1 / (a * d - b * c));

    if constexpr (std::numeric_limits<Tw>::has_infinity) {
      if (std::isinf(idet))
        return -1;
    }

    Data[0] = static_cast<Tw>(idet * d);
    Data[1] = static_cast<Tw>(idet * (-b));
    Data[2] = static_cast<Tw>(idet * (-c));
    Data[3] = static_cast<Tw>(idet * a);
    return 0;
  } else
    throw std::logic_error("not implemented"); // integer division?!
}

template <typename Tw> Tw Matrix<Tw>::Norm(const char norm) const {

  const Ti M = RowsCount;
  const Ti N = ColsCount;
  const Tw *A = const_cast<const Tw *>(Data);
  auto WORK0 = std::unique_ptr<Tw[]>(new Tw[norm == 'I' ? M : 0]);
  auto WORK = WORK0.get();

  Tw d;

  if constexpr (std::is_same<Tw, double>()) {
    d = dlange_(&norm, &M, &N, A, &M, WORK);
  } else if constexpr (std::is_same<Tw, float>()) {
    d = slange_(&norm, &M, &N, A, &M, WORK);
  } else if constexpr (true) {
    throw std::logic_error("not implemented: ?lange");
  }

  return d;
}

template <typename Tw> Ti Matrix<Tw>::Inv00(int *ipiv, Tw *work) {
  auto M = this->RowsCount;
  auto lwork = M * M;
  Tw *A = Data;

  Ti info = (Ti)0;

  if constexpr (std::is_same<Tw, double>()) {
    dgetrf_(&M, &M, A, &M, ipiv, &info); // LU decomposition
  } else if constexpr (std::is_same<Tw, float>()) {
    sgetrf_(&M, &M, A, &M, ipiv, &info); // LU decomposition
  } else if constexpr (true) {
    throw std::logic_error("not implemented: ?getrf");
  }

  if (info != 0)
    return info;
  /*INFO is Ti
            = 0:  successful exit
            < 0:  if INFO = -i, the i-th argument had an illegal value
            > 0:  if INFO = i, U(i,i) is exactly zero. The factorization
                          has been completed, but the factor U is exactly
                          singular, and division by zero will occur if it is
     used to solve a system of equations.*/

  if constexpr (std::is_same<Tw, double>()) {
    dgetri_(&M, A, &M, ipiv, work, &lwork, &info);
  } else if constexpr (std::is_same<Tw, float>()) {
    sgetri_(&M, A, &M, ipiv, work, &lwork, &info);
  } else if constexpr (true) {
    throw std::logic_error("not implemented: ?getri");
  }

  return info;
  /*INFO is Ti
            = 0:  successful exit
            < 0:  if INFO = -i, the i-th argument had an illegal value
            > 0:  if INFO = i, U(i,i) is exactly zero; the Matrix<Tw> is
                          singular and its inverse could not be computed.*/
}

template <typename Tw> Ti Matrix<Tw>::Chol0(bool upper) {

  // This is the unblocked version of the algorithm, calling Level 2 BLAS.
  auto UPLO = upper ? 'U' : 'L';
  const Ti N = ColsCount;
  Tw *A = Data;
  Ti info = (Ti)0;

  if constexpr (std::is_same<Tw, double>()) {
    dpotrf2_(&UPLO, &N, A, &N, &info);
  } else if constexpr (std::is_same<Tw, float>()) {
    spotrf2_(&UPLO, &N, A, &N, &info);
  } else if constexpr (true) {
    throw std::logic_error("not implemented: ?potrf2");
  }

  if (info != 0)
    return info;

  if (upper)
    for (Ti i = 0; i < ColsCount; i++)
      for (Ti j = 0; j < ColsCount; j++) {
        if (i > j)
          Set0(i, j, 0);
      }
  else
    for (Ti i = 0; i < ColsCount; i++)
      for (Ti j = 0; j < ColsCount; j++) {
        if (i < j)
          Set0(i, j, 0);
      }

  return info;
}

template <typename Tw> Tw Matrix<Tw>::Det() {
  if (ColsCount != RowsCount)
    throw std::logic_error("Matrix<Tw> is not square.");
  const Ti M = static_cast<Ti>(this->RowsCount);
  Tw *A = Data;
  Ti info = (Ti)0;
  auto ipiv0 = std::unique_ptr<int[]>(new int[M + 1]);
  auto ipiv = ipiv0.get();

  if constexpr (std::is_same<Tw, double>()) {
    dgetrf_(&M, &M, A, &M, ipiv, &info);
  } else if constexpr (std::is_same<Tw, float>()) {
    sgetrf_(&M, &M, A, &M, ipiv, &info);
  } else if constexpr (true) {
    throw std::logic_error("not implemented: ?pgetrf");
  }

  if (info != 0)
    throw std::invalid_argument("LU decomposition failed with code: " +
                                std::to_string(info));

  Tw res = 1;
  for (Ti i = 0; i < RowsCount; i++)
    res *= Get0(i, i);

  // determinant of P
  if constexpr (std::is_unsigned<Tw>() == false) {
    for (Ti j = 0; j < M; j++)
      if (j + 1 != ipiv[j])
        res = -res;
  } else if constexpr (true) {
    throw std::logic_error("not implemented");
  }

  return res;
}

template <typename Tw> Ti Matrix<Tw>::QR0(Tw *tau) {

  Ti M = RowsCount;
  Ti N = ColsCount;
  Tw *A = Data;
  Ti info = 0;

  Ti lwork = -1;
  Tw dtemp;

  if constexpr (std::is_same<Tw, double>()) {
    dgeqrf_(&M, &N, A, &M, tau, &dtemp, &lwork, &info);
  } else if constexpr (std::is_same<Tw, float>()) {
    sgeqrf_(&M, &N, A, &M, tau, &dtemp, &lwork, &info);

  } else if constexpr (true) {
    throw std::logic_error("not implemented");
  }

  if (info != 0)
    return info;

  lwork = static_cast<Ti>(dtemp);
  // auto work0 = std::unique_ptr<int[]>(new int[lwork]);
  // auto work = work0.get();

  if constexpr (std::is_same<Tw, double>()) {
    dgeqrf_(&M, &N, A, &M, tau, &dtemp, &lwork, &info);
  } else if constexpr (std::is_same<Tw, float>()) {
    sgeqrf_(&M, &N, A, &M, tau, &dtemp, &lwork, &info);
  } else if constexpr (true) {
    throw std::logic_error("not implemented");
  }

  return info;
}

template <typename Tw>
Ti Matrix<Tw>::SolveTrian0(Matrix<Tw> &b, bool upper, bool transpose,
                           bool unitdiag) {
  const char UPLO = upper ? 'U' : 'L';
  const char TRANS = transpose ? 'T' : 'N';
  const char DIAG = unitdiag ? 'U' : 'N';
  const Ti N = ColsCount;
  const Ti NRHS = static_cast<Ti>(b.ColsCount);
  Tw *A = Data;
  Tw *B = b.Data;
  Ti info = (Ti)0;

  if constexpr (std::is_same<Tw, double>()) {
    dtrtrs_(&UPLO, &TRANS, &DIAG, &N, &NRHS, A, &N, B, &N, &info);

  } else if constexpr (std::is_same<Tw, float>()) {
    strtrs_(&UPLO, &TRANS, &DIAG, &N, &NRHS, A, &N, B, &N, &info);
  } else if constexpr (true) {
    throw std::logic_error("not implemented");
  }
  return info;
}

template <typename Tw> Ti Matrix<Tw>::SolvePos0(Matrix<Tw> &b, bool upper) {
  const char UPLO = upper ? 'U' : 'L';
  const Ti N = ColsCount;
  const Ti NRHS = static_cast<Ti>(b.ColsCount);
  Tw *A = Data;
  Tw *B = b.Data;
  Ti info = (Ti)0;

  if constexpr (std::is_same<Tw, double>()) {
    dposv_(&UPLO, &N, &NRHS, A, &N, B, &N, &info);
  } else if constexpr (std::is_same<Tw, float>()) {
    sposv_(&UPLO, &N, &NRHS, A, &N, B, &N, &info);
  } else if constexpr (true) {
    throw std::logic_error("not implemented");
  }

  return info;
}

template <typename Tw>
Ti Matrix<Tw>::SVD0(Tw *Data, const Ti M, const Ti N, Tw *WORK, Ti lwork,
                    Matrix<Tw> &U, Matrix<Tw> &S, Matrix<Tw> &VT,
                    const char jobU, const char jobVT) {

  Ti info = (Ti)0;
  auto LDA = M;
  auto LDU = M;
  auto LDVT = N;
  Tw *A = Data;
  Tw *resS = &S.Data[0];
  Tw *resU = jobU != 'N' ? &U.Data[0] : nullptr;
  Tw *resVT = jobVT != 'N' ? &VT.Data[0] : nullptr;

  if constexpr (std::is_same<Tw, double>()) {
    dgesvd_(&jobU, &jobVT, &M, &N, A, &LDA, resS, resU, &LDU, resVT, &LDVT,
            WORK, &lwork, &info);
  } else if constexpr (std::is_same<Tw, float>()) {
    sgesvd_(&jobU, &jobVT, &M, &N, A, &LDA, resS, resU, &LDU, resVT, &LDVT,
            WORK, &lwork, &info);
  } else if constexpr (true) {
    throw std::logic_error("not implemented (svd type).");
  }

  return info;
}
