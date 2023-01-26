/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the GPL 3.0.
 See accompanying file LICENSE
*/

#include "polynomial.h"
#include <algorithm>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

using namespace ldt;

// #pragma region Polynomial

template <class Tw> Polynomial<Tw>::Polynomial() {
  Coefficients = Matrix<Tw>();
}

template <class Tw> void Polynomial<Tw>::Data(Matrix<Tw> &a, bool trim) {

  // trim
  if (trim) {
    Ti j = a.length();
    for (; j > 0; j--)
      if (a.Data[j - 1] != 0)
        break;
    if (j == 0)
      throw std::logic_error("length of 'a' must be > 0.");
    Coefficients.Restructure0(j, 1);
  } else
    Coefficients.Restructure0(a.length(), (Ti)1);
  Coefficients.SetData(a.Data);
}

template <class Tw> void Polynomial<Tw>::Data(Tw value, Tw *coefs, Ti length) {
  Coefficients.SetData(value, coefs, length, 1);
}

template <class Tw> Ti Polynomial<Tw>::GetDegree() const {
  return Coefficients.length() - 1;
}

// #pragma endregion

// #pragma region Multiply

template <class Tw>
PolynomialMultiply<Tw>::PolynomialMultiply(Ti degree_a, Ti degree_b,
                                           Ti maxLength) {
  StorageSize = std::min(degree_a + degree_b + 1, maxLength);
  Result = Polynomial<Tw>();
}

template <class Tw>
void PolynomialMultiply<Tw>::Calculate(const Polynomial<Tw> &a,
                                       const Polynomial<Tw> &b, Tw *storage,
                                       Ti maxLength) {
  Ti a_n = a.GetDegree();
  Ti b_n = b.GetDegree();
  auto temp = PolynomialMultiply<Tw>(a_n, b_n, maxLength);
  if (temp.StorageSize > StorageSize)
    throw std::logic_error("Inconsistent arguments (in polynomial multiply)");
  Ti length = temp.StorageSize;

  Result.Data(0, storage, length);

  for (Ti i = 0; i <= a_n; i++) {
    for (Ti j = 0; j <= b_n; j++) {
      Ti ij = i + j;
      if (ij < length)
        storage[ij] =
            storage[ij] + a.Coefficients.Data[i] * b.Coefficients.Data[j];
    }
  }
}

// #pragma endregion

// #pragma region Power

template <class Tw>
PolynomialPower<Tw>::PolynomialPower(Ti power, Ti degree, Ti maxLength) {
  StorageSize = std::min(power * degree + 1, maxLength);
  auto pm = PolynomialMultiply<Tw>(degree, StorageSize - 1, maxLength);
  WorkSize = pm.StorageSize;
  Result = Polynomial<Tw>();
}

template <class Tw>
void PolynomialPower<Tw>::Calculate(const Polynomial<Tw> &a, Ti power,
                                    Tw *storage, Tw *work, Ti maxLength) {

  Ti a_n = a.GetDegree();
  auto temp = PolynomialPower<Tw>(power, a_n, maxLength);
  if (temp.StorageSize > StorageSize || temp.WorkSize > WorkSize)
    throw("Inconsistent arguments (in polynomial power)");
  Ti length = temp.StorageSize;
  Result.Data(0, storage, length);

  if (power == 0) {
    Result.Coefficients.Data[0] = (Tw)1; // just a guess
    return;
  }

  Result.Coefficients.SetSubVector0(0, a.Coefficients, 0,
                                    a.Coefficients.length());
  auto pm = PolynomialMultiply<Tw>(a_n, length - 1, maxLength);
  auto x = Matrix<Tw>(work, length, 1);
  for (Ti i = 1; i < power; i++) { // TODO: is there a direct formula?
    pm.Calculate(a, Result, work, maxLength);
    x.CopyTo00(Result.Coefficients);
  }
}

// #pragma endregion

template class ldt::Polynomial<Tv>;
template class ldt::Polynomial<Ti>;

template class ldt::PolynomialMultiply<Tv>;
template class ldt::PolynomialMultiply<Ti>;

template class ldt::PolynomialPower<Tv>;
template class ldt::PolynomialPower<Ti>;
