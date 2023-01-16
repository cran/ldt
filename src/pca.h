#pragma once

#include "ldt_base.h"
#include "matrix_utils.h"
#include <string>
#include <vector>

namespace ldt {

/// @brief A class for Principle Component Analysis
class LDT_EXPORT PcaAnalysis {
  bool mDoPcs = false;

public:
  /// @brief Gets the required storage size
  Ti StorageSize = 0;

  /// @brief Gets the required work size
  Ti WorkSize = 0;

  /// @brief Standardized data
  MatrixStandardized<Tv> DataS;

  /// @brief Directions. Dimension: k x k. If there is no zero variance column
  /// then 'k=cols'. Otherwise 'k < cols'.
  Matrix<Tv> Directions;

  /// @brief Standard deviation of the principle components. length:
  /// min(RowsCount,k). 'k' is number of columns in \ref Directions.
  Matrix<Tv> Stds;

  /// @brief Principle components. Dimension: 'rows x k' in which 'k' is number
  /// of columns in \ref Directions.
  Matrix<Tv> PCs;

  /// @brief Stds^2/sum(Stds^2)
  Matrix<Tv> Stds2Ratios;

  /// @brief 'fRows x k'. where 'fRows' is the number of forecasts observations
  /// given in the Calculate method. k is 'cols' but if zero variance is found,
  /// it might be less than cols.
  Matrix<Tv> Forecasts;

  /// @brief Initializes a new instance of the class
  PcaAnalysis(){};

  /// @brief Initializes a new instance of the class
  /// @param rows Maximum expected number of rows
  /// @param cols Maximum expected number of columns
  /// @param numForecast Maximum expected number of forecasts
  /// @param doPCs If true, principal components are predicted by using
  /// calculated Directions
  /// @param checkZeroVar If true and 'scale==true', it searches for zero
  /// variance columns and removes them
  /// @param center If true, it demeans the data
  /// @param scale If true it scales the data
  PcaAnalysis(Ti rows, Ti cols, Ti numForecast = 0, bool doPCs = true,
              bool checkZeroVar = true, bool center = true, bool scale = true);

  /// @brief Calculates the Directions, standard errors, principal components
  /// (if requested), etc.
  /// @param mat 'rows x cols' Matrix. 'rows' and 'cols' are defined in
  /// the constructor
  /// @param work Work array of size \ref WorkSize
  /// @param storage Storage array of size \ref StorageSize
  /// @param Xforecast Data for forecasting
  void Calculate(const Matrix<Tv> &mat, Tv *work, Tv *storage,
                 const Matrix<Tv> *Xforecast = nullptr);

  /// @brief It calculates and returns the number of principal component up to
  /// which (CutoffRate*100) percentage of total variance is explained.
  /// @param CutoffRate A value in (0,1) that determines the cutoff rate
  /// @return Number of columns
  Ti GetCutoffColumn(Tv CutoffRate) const;
};

/// @brief A helper class for PCA analysis options
class LDT_EXPORT PcaAnalysisOptions {

public:
  /// @brief Initializes a new instance of the class
  PcaAnalysisOptions(){};

  /// @brief Set it e.g. 1 if the first variable is intercept and you don't want
  /// a model without it
  Ti IgnoreFirstCount = 1;

  /// @brief Exact number of component to be used (ignored if 0)
  Ti ExactCount = 0;

  /// @brief Gets number of components based on cutoff rate (ignored if 0)
  Tv CutoffRate = 0;

  /// @brief Restricts cutoff rate result to this value
  Ti CutoffCountMax =
      100000; // dont use INT_MAX or its sum with a number will result errors;

  /// @brief Determines if PC analysis is requested.
  /// @return true if exact count of cutoff rate is not zero
  bool IsEnabled() { return ExactCount != 0 || CutoffRate != 0; };

  /// @brief Checks the validity of the values
  void CheckValidity();

  /// @brief Gets the final count of columns based on a estimated model
  /// @param model The estimated model
  /// @return Final count
  Ti GetFinalCount(const PcaAnalysis &model) const;

  /// @brief Ignores the first columns, Calculate PCs and updates/restructure
  /// 'data'.
  /// @param model An estimated model
  /// @param data Data
  /// @param work Work array
  /// @param storage Storage array
  /// @param Xforecast Data for forecast
  /// @param throwIfConstant
  void CalculateForModel(PcaAnalysis &model, Matrix<Tv> &data, Tv *work,
                         Tv *storage, Matrix<Tv> *Xforecast = nullptr,
                         bool throwIfConstant = true) const;
};
} // namespace ldt
