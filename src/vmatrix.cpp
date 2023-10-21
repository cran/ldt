
/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the GPL 3.0.
 See accompanying file LICENSE
*/

#include "matrix.h"

using namespace ldt;

template <typename Tw> VMatrix<Tw>::VMatrix() {}

template <typename Tw>
VMatrix<Tw>::VMatrix(Ti m, Ti n) : Vec(m * n), Mat(m, n) {
  Mat.Data = &Vec[0];
}

template <typename Tw>
VMatrix<Tw>::VMatrix(const std::vector<Tw> &data, Ti m, Ti n) : Vec(data) {
  if (m == -1) {
    if (data.size() % n != 0) {
      throw LdtException(ErrorType::kLogic, "matrix",
                         "Size of vector must be divisible by n");
    }
    m = data.size() / n;
  }
  Mat = Matrix<Tw>(m, n);
  Mat.Data = &Vec[0];
  if ((Ti)Vec.size() != m * n)
    throw LdtException(ErrorType::kLogic, "matrix",
                       "Inconsistent arguments. Size of vector must be m*n");
}

template <typename Tw>
VMatrix<Tw>::VMatrix(std::initializer_list<Tw> initList, Ti m, Ti n)
    : Vec(initList) {
  if (m == -1) {
    if (initList.size() % n != 0) {
      throw LdtException(ErrorType::kLogic, "matrix",
                         "Size of initializer list must be divisible by n");
    }
    m = initList.size() / n;
  }
  Mat = Matrix<Tw>(m, n);
  Mat.Data = &Vec[0];
  if ((Ti)Vec.size() != m * n)
    throw LdtException(ErrorType::kLogic, "matrix",
                       "Inconsistent arguments. Size of vector must be m*n");
}

template class ldt::VMatrix<Ti>;
template class ldt::VMatrix<Tv>;
