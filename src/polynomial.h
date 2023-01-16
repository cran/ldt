#pragma once

#include "ldt_base.h"
#include "matrix.h"
#include <functional>
#include <iostream>
#include <iterator>
#include <vector>

namespace ldt {

/// @brief Represents a polynomial such as f(x)
/// =a_0+a_1x+a_2x^2+a_3x^3+...+a_nx^n,   a_n\ne0
/// @tparam Tw Type of data
template <class Tw = Tv> class LDT_EXPORT Polynomial {

public:
  /// @brief Gets the coefficients of this polynomial. It does not own the inner
  /// array.
  Matrix<Tw> Coefficients;

  /// @brief Initializes a new instance of the class. Use \ref Data to populate
  /// it
  Polynomial();

  /// @brief Set data
  /// @param coefficients The coefficients. Its inner array is shared with \ref
  /// Coefficients
  /// @param trim If true, leading zeros are omitted
  void Data(Matrix<Tw> &coefficients, bool trim = true);

  /// @brief Set data in \ref Coefficients directly
  /// @param value A default value for the coefficients
  /// @param coefficients pointer to the data array
  /// @param length length of \p coefficients
  void Data(Tw value, Tw *coefficients, Ti length);

  /// @brief Degree of the Polynomial. It is length of \ref Coefficients minus 1
  /// @return The degree
  Ti GetDegree() const;
};

/// @brief A helper class for multiplying two polynomials
/// @tparam Tw Type of data
template <class Tw = Tv> class LDT_EXPORT PolynomialMultiply {

public:
  /// @brief Initializes a new instance of the class
  /// @param degree_a Expected degree of the first polynomial
  /// @param degree_b Expected degree of the second polynomial
  /// @param maxLength Expected maximum length
  PolynomialMultiply(Ti degree_a, Ti degree_b, Ti maxLength = INT_MAX);

  /// @brief Gets the required size of the storage
  Ti StorageSize = 0;

  /// @brief After \ref Calculates, it is the result
  Polynomial<Tw> Result;

  /// @brief Calculates the result
  /// @param a First polynomial
  /// @param b Second polynomial
  /// @param storage An array of size \ref StorageSize for keeping the results
  /// @param maxLength Restricts the length of the output. It cannot be larger
  /// than this parameter in the constructor.
  void Calculate(const Polynomial<Tw> &a, const Polynomial<Tw> &b, Tw *storage,
                 Ti maxLength = INT_MAX);
};

/// @brief A helper class to calculate the power of a polynomial
/// @tparam Tw Type of data
template <class Tw = Tv> class LDT_EXPORT PolynomialPower {

public:
  /// @brief Initializes a new instance of the class
  /// @param power Expected value of the power
  /// @param degree Expected degree of the polynomial
  /// @param maxLength Expected maximum length
  PolynomialPower(Ti power, Ti degree, Ti maxLength = INT_MAX);

  /// @brief Gets the required size of the storage array
  Ti StorageSize = 0;

  /// @brief Gets the required size of the work array
  Ti WorkSize = 0;

  /// @brief After \ref Calculate, it contains the results
  Polynomial<Tw> Result;

  /// @brief Calculate the results
  /// @param a The polynomial
  /// @param power Value of the power
  /// @param storage An array of size \ref StorageSize for keeping the results
  /// @param work An array of size \ref WorkSize for calculating the results
  /// @param maxLength A restriction on the length of the results
  void Calculate(const Polynomial<Tw> &a, Ti power, Tw *storage, Tw *work,
                 Ti maxLength = INT_MAX);
};

/// @brief Represents a multivariate polynomial such as
/// f(x)=a_0+a_1x+a_2x^2+a_3x^3+...+a_nx^n,   a_n\ne0 where all a_i are m x m
class LDT_EXPORT PolynomialM {
private:
  bool isOwner = false;

public:
  /// @brief Coefficients of the polynomial
  std::vector<Matrix<Tv> *> Coefficients;

  /// @brief initializes a new instance of the class. Use \ref Data to populate
  /// it
  PolynomialM();
  ~PolynomialM();

  /// @brief Sets data given a list of matrixes (it just keep the references)
  /// @param a List of matrices
  /// @param trim Removes zero matrices from the end
  void Data(std::vector<Matrix<Tv> *> &a, bool trim = false);

  /// @brief Set data given an array
  /// @param degree Degree of the polynomial
  /// @param m Size of the polynomial (number of rows or columns in the
  /// coefficients)
  /// @param data An array with length equal to (\p degree+1) * \p m * \p m
  /// @return (\p degree+1) * \p m * \p m
  Ti Data(Ti degree, Ti m, Tv *data);

  /// @brief Gets the storage size for using in \ref Data
  /// @param degree Degree of the polynomial
  /// @param m Size of the polynomial (number of rows or columns in the
  /// coefficients)
  /// @return (\p degree+1) * \p m * \p m
  static Ti GetStorageSize(Ti degree, Ti m) { return (degree + 1) * m * m; }

  /// @brief Degree of the Polynomial. It is length of \ref Coefficients minus 1
  /// @return The degree
  Ti GetDegree() const;

  /// @brief Gets the size of the polynomial (number of rows or columns in the
  /// coefficients)
  /// @return The size
  Ti GetSize() const;

  /// @brief Determines if this polynomial is monic
  /// @return true if it the last coefficient is an identity matrix
  bool IsMonic() const;
};

/// @brief A helper class for multiplying two multivariate polynomials
class LDT_EXPORT PolynomialMMultiply {

public:
  /// @brief Initializes a new instance of the class
  /// @param size Expected size of the first (and second) polynomials
  /// @param degree1 Expected degree of the first polynomials
  /// @param degree2 Expected degree of the second polynomials
  /// @param maxLength Expected maximum length
  PolynomialMMultiply(Ti size, Ti degree1, Ti degree2, Ti maxLength = INT_MAX);

  /// @brief Gets the required size of the storage array
  Ti StorageSize = 0;

  /// @brief After \ref Calculate, it contains the results
  PolynomialM Result;

  /// @brief Calculate for two multivariate polynomials
  /// @param a First polynomial
  /// @param b Second polynomial
  /// @param storage An array of size \ref StorageSize to keep the results
  /// @param maxLength A restriction on the length of the results
  void Calculate(const PolynomialM &a, const PolynomialM &b, Tv *storage,
                 Ti maxLength = INT_MAX);

  /// @brief Calculate if the second polynomial is not multivariate
  /// @param a First polynomial
  /// @param b Second polynomial
  /// @param storage An array of size \ref StorageSize to keep the results
  /// @param maxLength A restriction on the length of the results
  void Calculate(const PolynomialM &a, const Polynomial<Tv> &b, Tv *storage,
                 Ti maxLength = INT_MAX);
};

/// @brief A helper class for finding the inverse of a multivariate polynomial
class LDT_EXPORT PolynomialMInvert {

public:
  /// @brief Initializes a new instance of the class
  /// @param size Expected size of the polynomial
  /// @param degree Expected degree of the polynomial
  /// @param maxLength Expected length of the inverse
  PolynomialMInvert(Ti size, Ti degree, Ti maxLength);

  /// @brief Gets the required size of the storage array
  Ti StorageSize = 0;

  /// @brief Gets the required size of the work array
  Ti WorkSize = 0;

  /// @brief After calculate, it contains the result
  PolynomialM Result;

  /// @brief Finds the inverse
  /// @param a The polynomial
  /// @param storage Storage array of size \ref StorageSize
  /// @param work Work array
  /// @param maxLength A restriction on the length of the results
  void Calculate(const PolynomialM &a, Tv *storage, Tv *work, Ti maxLength);
};

extern template class ldt::Polynomial<Tv>;
extern template class ldt::Polynomial<Ti>;

extern template class ldt::PolynomialMultiply<Tv>;
extern template class ldt::PolynomialMultiply<Ti>;

extern template class ldt::PolynomialPower<Tv>;
extern template class ldt::PolynomialPower<Ti>;

} // namespace ldt
