/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the GPL 3.0.
 See accompanying file LICENSE
*/

#include "polynomial.h"
#include <algorithm>
#include <iostream>
#include <math.h>
#include <sstream>
#include <string>
#include <vector>

using namespace ldt;

// #pragma region Polynomial

PolynomialM::PolynomialM() { Coefficients = std::vector<Matrix<Tv> *>(); }

PolynomialM::~PolynomialM() {
  if (isOwner) {
    for (auto i : Coefficients)
      delete i;
    Coefficients.clear();
  }
}

void PolynomialM::Data(std::vector<Matrix<Tv> *> &a, bool trim) {
  isOwner = false;
  Ti j = (Ti)a.size();
  // trim
  if (trim) {
    for (; j > 0; j--)
      if (a.at(j - 1)->EqualsValue(0.0, 0.0) == false) {
        break;
      }
  }
  if (j == 0)
    throw LdtException(ErrorType::kLogic, "mpoly", "length of 'a' must be > 0");
  j--;
  for (int i = 0; i < j + 1; i++)
    Coefficients.push_back(a.at(i));
}

Ti PolynomialM::Data(Ti degree, Ti m, Tv *data) {

  auto mm = m * m;
  isOwner = true; // delete the list
  for (Ti i = 0; i < degree + 1; i++)
    Coefficients.push_back(
        new Matrix<Tv>(&data[i * mm], m, m)); // TODO: avoid using new
  return (degree + 1) * mm;
}

Ti PolynomialM::GetDegree() const { return (Ti)Coefficients.size() - 1; }

Ti PolynomialM::GetSize() const { return Coefficients.at(0)->RowsCount; }

bool PolynomialM::IsMonic() const {
  return Matrix<Tv>::IsDiagonal(*Coefficients.at(GetDegree()));
}

// #pragma endregion

// #pragma region Multiply

PolynomialMMultiply::PolynomialMMultiply(Ti size, Ti degree1, Ti degree2,
                                         Ti maxLength) {
  StorageSize = std::min(degree1 + degree2 + 1, maxLength) * size * size;
  Result = PolynomialM();
}

void PolynomialMMultiply::Calculate(const PolynomialM &a, const PolynomialM &b,
                                    Tv *storage, Ti maxLength) {

  Ti size = a.GetSize();
  Ti degree1 = a.GetDegree();
  Ti degree2 = b.GetDegree();

  auto temp = PolynomialMMultiply(size, degree1, degree2, maxLength);
  if (temp.StorageSize > StorageSize)
    throw LdtException(ErrorType::kLogic, "mpoly",
                       "inconsistent arguments (in polynomialM multiply)");
  auto length = std::min(degree1 + degree2 + 1, maxLength);

  Result.Data(length - 1, size, storage);

  for (auto &m : Result.Coefficients)
    m->SetValue(0);

  for (Ti i = 0; i <= degree1; i++) {
    for (Ti j = 0; j <= degree2; j++) {
      Ti ij = i + j;
      if (ij < length)
        a.Coefficients.at(i)->Dot0(*b.Coefficients.at(j),
                                   *Result.Coefficients.at(ij), 1, 1);
    }
  }
}

void PolynomialMMultiply::Calculate(const PolynomialM &a,
                                    const Polynomial<Tv> &b, Tv *storage,
                                    Ti maxLength) {

  Ti size = a.GetSize();
  Ti degree1 = a.GetDegree();
  Ti degree2 = b.GetDegree();

  auto temp = PolynomialMMultiply(size, degree1, degree2, maxLength);
  if (temp.StorageSize > StorageSize)
    throw LdtException(ErrorType::kLogic, "mpoly",
                       "inconsistent arguments (in polynomialM multiply)");
  auto length = std::min(degree1 + degree2 + 1, maxLength);

  Result.Data(length - 1, size, storage);

  for (auto &m : Result.Coefficients)
    m->SetValue(0);

  for (Ti i = 0; i <= degree1; i++) {
    for (Ti j = 0; j <= degree2; j++) {
      Ti ij = i + j;
      if (ij < length)
        a.Coefficients.at(i)->Multiply0(b.Coefficients.Data[j],
                                        *Result.Coefficients.at(ij), 1);
    }
  }
}

// #pragma endregion

// #pragma region Inverse

PolynomialMInvert::PolynomialMInvert(Ti size, Ti degree, Ti maxLength) {
  StorageSize = maxLength * size * size;
  WorkSize = (degree + 1) * size * size;
  Result = PolynomialM();
}

void PolynomialMInvert::Calculate(const PolynomialM &a, Tv *storage, Tv *work,
                                  Ti maxLength) {

  Ti size = a.Coefficients.at(0)->RowsCount;
  Ti degree = a.GetDegree();
  auto temp = PolynomialMInvert(size, degree, maxLength);
  if (temp.StorageSize > StorageSize || temp.WorkSize > WorkSize)
    throw LdtException(ErrorType::kLogic, "mpoly",
                       "inconsistent arguments (in polynomialM invert)");

  Result.Data(maxLength - 1, size, storage);

  try {
    a.Coefficients.at(0)->Inv(*Result.Coefficients.at(0));
  } catch (...) {
    throw LdtException(ErrorType::kLogic, "mpoly", "'A0' is not invertible");
  }

  auto inv0 = Result.Coefficients.at(0);
  auto s2 = size * size;
  auto tempmats = std::vector<std::unique_ptr<Matrix<Tv>>>();
  Ti q = 0;
  for (Ti i = 1; i <= degree; i++) {
    auto m = std::make_unique<Matrix<Tv>>(&work[q], size, size);
    q += s2; // degree * size * size
    tempmats.push_back(std::move(m));
    inv0->Dot0(*a.Coefficients.at(i), *tempmats.back(), -1);
  }

  auto x = Matrix<Tv>(&work[q], size, size); // size * size
  Ti siz = (Ti)a.Coefficients.size();
  for (Ti s = 1; s < maxLength; s++) {
    Result.Coefficients.at(s)->SetValue(0.0);
    for (Ti j = 1; j < siz; j++) {
      if (s < j)
        break;
      Ti s_j = s - j;
      tempmats.at(j - 1).get()->Dot0(*Result.Coefficients.at(s_j), x);
      Result.Coefficients.at(s)->Add_in0(x);
    }
  }
  tempmats.clear();
}

// #pragma endregion
