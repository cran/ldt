#pragma once

#include <algorithm>
#include <iostream>
#include <random>
#include <set>
#include <sstream>
#include <string>
#include <vector>

#include "functionx.h"
#include "ldt_base.h"

#include "distributions.h"
#include "matrix.h"
#include "optimization.h"
#include "pca.h"
#include "polynomial.h"
#include "scoring.h"
#include "searchers.h"

namespace ldt {

/// todo: Merge VarmaSizes and VarmaStorage with Varma class

struct LDT_EXPORT VarmaSizes {
  VarmaSizes(){};

  VarmaSizes(Ti obsCount, Ti eqsCount, Ti exoCount, Ti arP = 1, Ti arD = 0,
             Ti arQ = 1, Ti maP = 0, Ti maD = 0, Ti maQ = 0,
             Ti seasonsCount = 0, bool calculate = true);

  /// @brief (Expected) number of observations
  Ti ObsCount;

  /// @brief (Expected) number of equations
  Ti EqsCount;

  /// @brief (Expected) number of exogenous variables
  Ti ExoCount;

  /// @brief Parameters of the ARMA
  Ti ArP, ArD, ArQ, MaP, MaD, MaQ;

  /// @brief Number of seasons in the data
  Ti SeasonsCount;

  /// @brief Size of AR lags
  Ti ArLength = -1;

  /// @brief Size of MA lags
  Ti MaLength = -1;

  /// @brief Maximum AR lag. This is also the degree of AR polynomial
  Ti ArMax = -1;

  /// @brief Maximum MA lag. This is also the degree of MA polynomial
  Ti MaMax = -1;

  /// @brief Degree of Diff polynomial
  Ti DiffDegree = -1;

  /// @brief \ref ArMax + \ref DiffDegree
  Ti ArMax_d = -1;

  /// @brief If true, it has AR lags or exogenous data
  bool HasArExo = false;

  /// @brief If true, it has AR lags
  bool HasAr = false;

  /// @brief If true, it has MA lags
  bool HasMa = false;

  /// @brief If true, it has Diff lags
  bool HasDiff = false;

  /// @brief Determines where MA data starts. It is \ref ArLength * \ref
  /// EqsCount + \ref ExoCount
  Ti MaStart = -1;

  /// @brief Number of parameters in the system
  Ti NumParams = -1;

  /// @brief Number of parameters in each equation
  Ti NumParamsEq = -1;

  /// @brief Number of observations
  Ti T = -1;

  /// @brief Lags of AR
  std::vector<Ti> ArLags;

  /// @brief Lags of MA
  std::vector<Ti> MaLags;

  /// @brief Lags of Diff
  std::vector<Ti> DiffPoly;

  /// @brief Requested work size
  Ti WorkSizeI = 0;

  /// @brief Calculates the results
  void Calculate();

  /// @brief Calculates the results with a working array
  /// @param workI Work array of size \ref WorkSizeI
  void Calculate(Ti *workI);

  /// @brief Updates the results. Use it when observation, number of equations
  /// or number exogenous data changes
  void UpdateChanged();
};

/// @brief A class that stores estimation results
struct LDT_EXPORT VarmaStorage {

  /// @brief Initializes a new instance of the class
  VarmaStorage(){};

  /// @brief Initializes a new instance of the class
  /// @param keepDetails If true, details such as p-value matrix is calculated
  /// @param isRestricted If true, a restricted model is expected
  /// @param sizes Expected sizes
  /// @param calculateVarCoefs If true, variance of the coefficients is
  /// calculated
  /// @param optimOptions Optimization options
  VarmaStorage(bool keepDetails, bool isRestricted, const VarmaSizes &sizes,
               bool calculateVarCoefs,
               LimitedMemoryBfgsbOptions *optimOptions = nullptr);

  /// @brief Sets the storage
  /// @param storage Storage array of size \ref StorageSize
  /// @param sizes Expected sizes
  /// @param sampleEnd Determines the number of last observations excluded from
  /// estimation
  void SetStorage(Tv *storage, const VarmaSizes &sizes, Ti sampleEnd);

  /// @brief Gets the required storage size
  Ti StorageSize = 0;

  /// @brief Gets the required work size
  Ti WorkSize = 0;

  /// @brief Estimated coefficients (q x 1 or mf x 1), where q=R.ColsCount or
  /// m*f
  Matrix<Tv> gamma;

  /// @brief Endogenous data (m x T)
  Matrix<Tv> y;

  /// @brief Transposed explanatory data (k x T)
  Matrix<Tv> Xt;

  /// @brief Residuals
  Matrix<Tv> resid;

  /// @brief Variance of the regression
  Matrix<Tv> sigma2;

  /// @brief Variance of the coefficients. It is q x q, where q=R.ColsCount or
  /// m*f
  Matrix<Tv> gammavar;

  /// @brief Coefficients matrix: gamma * R
  Matrix<Tv> coef;

  /// @brief Condition number (norm1(XX)*norm1((XX)^-1)
  Tv cn = -1;

  /// @brief Optimization class
  LimitedMemoryBFGSB Optim;

  /// @brief Logarithm of the likelihood
  Tv LogLikelihood = -1;

  /// @brief AIC
  Tv Aic = -1;

  /// @brief SIC
  Tv Sic = -1;

  // Tv R2 = -1;

  /// @brief (details) Matrix of estimated standard errors
  Matrix<Tv> coefstd;

  /// @brief (details) Matrix of t statistics
  Matrix<Tv> coeft;

  /// @brief (details) Matrix of p-values
  Matrix<Tv> coefprob;

  /// @brief Gets the name of explanatory data (adds lags)
  /// @param sizes Sizes
  /// @param endoNames Endogenous names
  /// @param exoNames Exogenous names
  /// @param result A vector to store the results
  static void GetExpNames(const VarmaSizes &sizes,
                          std::vector<std::string> &endoNames,
                          std::vector<std::string> &exoNames,
                          std::vector<std::string> &result);

private:
  bool mKeepDetails;
};

/// @brief VARMA model estimation
class LDT_EXPORT Varma {
private:
  bool mIsRestricted = false;
  bool mDoDetails = false;
  bool mCalculateVarCoefs = false;

public:
  /// @brief Parameters and sizes. It gets updated in the \ref EstimateOls
  VarmaSizes Sizes;

  /// @brief Estimation results
  VarmaStorage Result;

  /// @brief Gets the last applied sample end
  Ti SampleEnd = 0;

  /// @brief Initializes a new instance of this class
  Varma(){};

  /// @brief Initializes a new instance of this class
  /// @param sizes Expected sizes
  /// @param isRestricted If true, a restricted model is expected
  /// @param doDetails If true, details such as p-value matrix is calculated
  /// @param calculateVarCoefs If true, variance of the coefficients is also
  /// calculated
  /// @param optimOptions Optimization options. If null, default values are
  /// used.
  Varma(const VarmaSizes &sizes, bool isRestricted = false,
        bool doDetails = false, bool calculateVarCoefs = true,
        LimitedMemoryBfgsbOptions *optimOptions = nullptr);

  static void Difference(std::vector<Ti> &polyDiff, const Matrix<Tv> &y,
                         Matrix<Tv> &storage);

  static void UnDiferences(std::vector<Ti> &polyDiff, Matrix<Tv> &storage);

  static std::tuple<Matrix<Tv>, Matrix<Tv>>
  Simulate(std::vector<Matrix<Tv> *> *ar, std::vector<Matrix<Tv> *> *ma,
           Matrix<Tv> *intercept = nullptr, Matrix<Tv> *exocoef = nullptr,
           Matrix<Tv> *sigma = nullptr, Ti n = 200, Ti skip = 100,
           unsigned int seed = 0, Matrix<Tv> *y0 = nullptr, Ti horizon = 10);

  void EstimateOls(const Matrix<Tv> &data, const Matrix<Tv> *exoData,
                   const Matrix<Tv> *R, const Matrix<Tv> *r, Tv *work,
                   Tv *storage, Ti sampleEnd, bool noestimation,
                   Matrix<Tv> *ma_resid, Tv maxCondNum = INFINITY);

  void EstimateMl(const Matrix<Tv> &data, const Matrix<Tv> *exoData, Tv *work,
                  Tv *storage, const Matrix<Tv> *R = nullptr,
                  const Matrix<Tv> *r = nullptr, Ti sampleEnd = 0,
                  bool useCurrentEstime = false, Tv stdMultipler = 2,
                  Tv maxCondNum = INFINITY /*, bool recurs_init*/);

  static std::string ModelToString(VarmaSizes sizes);
};

class LDT_EXPORT VarmaArma {
  const VarmaSizes *pSizes = nullptr;

  Ti mMaInfCount = 0;

public:
  Ti WorkSize = 0;

  Ti StorageSize = 0;

  PolynomialM Ar;

  PolynomialM Ma;

  PolynomialM MaInf;

  VarmaArma(const VarmaSizes &sizes, Ti maInfCount = 0);

  void Calculate(const Matrix<Tv> &Pi, Tv *storage, Tv *work);
};

class LDT_EXPORT VarmaForecast {
public:
  Ti WorkSize = 0;

  Ti StorageSize = 0;

  /// @brief Determines where forecasts start in \ref Forecast
  Ti StartIndex = 0;

  /// @brief Determines the number of observations and the first observation in
  /// forecast. Use it e.g. for setting the frequency.
  Ti StartDiff = 0;

  Ti mHorizon = 0;
  Ti mDoVariance = 0;
  bool mCoefUncertainty = false;

  Matrix<Tv> Forecast;

  Matrix<Tv> Variance;

  /// @brief Coefficient uncertainty (if enabled). It is not implemented yet
  Matrix<Tv> Variance2;

  VarmaForecast(){};

  VarmaForecast(const VarmaSizes &sizes, Ti horizon = 1, bool doVariance = true,
                bool coefUncertainty = false);

  /// @brief
  /// @param estimate
  /// @param exo
  /// @param undiff_y
  /// @param storage
  /// @param work
  /// @param horizon
  /// @param exoIsNew If true, then \p exo starts after the actual data
  /// used in estimation. Otherwise, it is the full matrix that contains the
  /// estimation data
  void Calculate(const Varma &estimate, const Matrix<Tv> *exo,
                 const Matrix<Tv> *undiff_y, Tv *storage, Tv *work,
                 Ti horizon = -1, bool exoIsNew = false);
};

enum class VarmaRestrictionType {
  kNone,
  kGeneral,
  kMaFinal,
};

class LDT_EXPORT VarmaRestriction {
public:
  bool IsRestricted = false;
  Ti StorageSize = 0;
  VarmaRestrictionType mType = VarmaRestrictionType::kNone;
  Ti mGeneralRestrictionCount = 0;

  /// @brief A pointer to the given VarmaSizes
  const VarmaSizes *pSizes = nullptr;

  Matrix<Tv> R;

  Matrix<Tv> r;

  VarmaRestriction(){};

  VarmaRestriction(const VarmaSizes &sizes, VarmaRestrictionType type,
                   Ti generalRestrictionCount = 0);

  void Calculate(Tv *storage, std::vector<Ti> *generalIndexes = nullptr);

  static Ti GetNumRestrictionInEq(Matrix<Tv> &R, Ti eqIndex, Ti eqCount);
};

class LDT_EXPORT VarmaExtended {
  bool mDoDetails = false;
  bool mCalcVariance = false;
  bool mHasPcaY = false;
  bool mHasPcaX = false;

public:
  bool mCheckNan = false;
  VarmaRestrictionType mRestriction;
  PcaAnalysisOptions *pPcaOptionsY = nullptr;
  PcaAnalysisOptions *pPcaOptionsX = nullptr;
  Ti mHorizon = 0;

  Ti WorkSize = 0;
  Ti StorageSize = 0;

  Matrix<Tv> Y;
  Matrix<Tv> X;

  DatasetTs<false> Data;

  /// @brief It might be different from the given 'sizes' in the constructor,
  /// due to PCA
  VarmaSizes Sizes;

  PcaAnalysis PcaY;
  PcaAnalysis PcaX;

  Varma Model;

  VarmaForecast Forecasts;

  // Matrix<Tv> ForecastError;

  VarmaExtended(){};

  VarmaExtended(const VarmaSizes &sizes, VarmaRestrictionType restriction,
                bool checkNan, bool doDetails, bool calcVariance, Ti fHorizon,
                PcaAnalysisOptions *pcaOptionsY,
                PcaAnalysisOptions *pcaOptionsX,
                LimitedMemoryBfgsbOptions *optimOptions);

  /// @brief
  /// @param data Data with variables in the columns
  /// @param storage
  /// @param work
  /// @param useCurrentEstime
  /// @param horizon
  /// @param sampleEnd
  /// @param maxCn
  /// @param stdMultiplier
  void Calculate(Matrix<Tv> &data, Tv *storage, Tv *work, bool useCurrentEstime,
                 Ti horizon, Ti sampleEnd, double maxCn = -1,
                 double stdMultiplier = 2);
};

/*

struct VarmaSimulationDetail {

        VarmaSimulationDetail();
        ~VarmaSimulationDetail();

        Matrix<Tv>* Actuals = nullptr;

        Matrix<Tv>* Forecasts = nullptr;

        Matrix<Tv>* ForecastsStd = nullptr;

        bool IsValid = false;

        Ti SampleEnds = 0;

};*/

class LDT_EXPORT VarmaSimulation {

  bool mDoForecastVar = true;

  void AddError(std::string state);

public:
  bool IsExtended = false;

  Ti WorkSize = 0;

  Ti StorageSize = 0;

  const VarmaSizes *pSizes = nullptr;

  Ti mCount = 0;

  std::map<std::string, Ti> Errors;

  const std::vector<Ti> *pHorizons = nullptr;

  const std::vector<ScoringType> *pMetrics = nullptr;

  Varma Model;
  VarmaForecast Forecast;
  VarmaExtended EModel;

  /// @brief at(i)->[j,h] => i-th metric, j-th variable, h-th horizon
  std::vector<Matrix<Tv>> Results;

  /// @brief Results aggregated by horizon [i,j] => i-th metric, j-th variable
  /// (aggregated over horizons). Note that it might be a weighted average due
  /// to different number of evaluations over different horizon. For RMSE, it
  /// is different furthermore, because of its nonlinearity nature.
  Matrix<Tv> ResultAggs;

  Ti ValidCounts = 0;

  bool KeepDetails = false;

  /// @brief The items are: sample end, metric index, horizon, target index,
  /// last value, actual value, forecast, forecast error, std
  std::vector<std::tuple<Ti, Ti, Ti, Ti, Tv, Tv, Tv, Tv, Tv>> Details;

  VarmaSimulation(){};

  VarmaSimulation(
      const VarmaSizes &sizes, Ti count, const std::vector<Ti> &horizons,
      const std::vector<ScoringType> &metrics,
      LimitedMemoryBfgsbOptions *optimOptions = nullptr,
      bool isExtended = false,
      VarmaRestrictionType restriction = VarmaRestrictionType::kMaFinal,
      bool checkNan = true, PcaAnalysisOptions *pcaOptionsY = nullptr,
      PcaAnalysisOptions *pcaOptionsX = nullptr);

  void
  Calculate(Tv *storage, Tv *work, Matrix<Tv> &data, bool &cancel,
            Matrix<Tv> *exo = nullptr, const Matrix<Tv> *R = nullptr,
            const Matrix<Tv> *r = nullptr, bool usePreviousEstim = true,
            Tv maxCondNum = 1e12, Tv stdMultipler = 2, bool coefUncer = false,
            Ti maxInvalidSim = INT32_MAX,
            const std::function<void(Tv &)> *transformForMetrics = nullptr);

  void
  CalculateE(Tv *storage, Tv *work, Matrix<Tv> &data, Tv maxCondNum = 1e12,
             Tv stdMultipler = 2, bool coefUncer = false,
             bool usePreviousEstim = false,
             const std::function<void(Tv &)> *transformForMetrics = nullptr);
};

class LDT_EXPORT VarmaSearcher : public Searcher {
  bool UsePreviousEstim = false;

  const Matrix<Tv> *pForLowerBounds = nullptr;
  const Matrix<Tv> *pForUpperBounds = nullptr;

  Tv StdMultiplier = 2.0;
  Ti mMaxHorizonCheck = 0;

  const std::vector<Ti> *pExoIndexes = nullptr;

  DatasetTs<true> Source;
  VarmaSizes Sizes;
  Varma DModel;
  VarmaForecast FModel;
  VarmaSimulation Model;

  std::unique_ptr<Tv[]> RestrictionData;
  VarmaRestriction Restriction;

  Matrix<Tv> Y;
  Matrix<Tv> X;
  Matrix<Ti> Params;
  Matrix<Ti> ExoIndexes;
  std::vector<Ti> Indexes;

  std::string EstimateOne(Tv *work, Ti *workI) override;

public:
  VarmaSearcher(SearchOptions &searchOptions, const SearchItems &searchItems,
                const SearchMetricOptions &metrics,
                const SearchModelChecks &checks, Ti sizeG,
                const std::vector<std::vector<Ti>> &groupIndexMap, Ti fixFirstG,
                DatasetTs<true> &source, const VarmaSizes sizes,
                const std::vector<Ti> &exoIndexes, Matrix<Tv> *forLowerBounds,
                Matrix<Tv> *forUpperBounds,
                LimitedMemoryBfgsbOptions *optimOptions, Tv stdMultiplier,
                bool usePreviousEstim, Ti maxHorizonCheck);
  ~VarmaSearcher();
};

class LDT_EXPORT VarmaModelset {
public:
  std::vector<std::vector<Ti>> ExoIndexes;

  ModelSet Modelset;

  std::vector<Searcher *> Searchers;

  Matrix<Tv> ForecastLowers;
  Matrix<Tv> ForecastUppers;

  VarmaModelset(){};

  VarmaModelset(SearchOptions &searchOptions, SearchItems &searchItems,
                SearchMetricOptions &metrics, SearchModelChecks &checks,
                const std::vector<Ti> &sizes,
                std::vector<std::vector<Ti>> &groupIndexMap,
                DatasetTs<true> &source, std::vector<Ti> varmaMaxParameters6,
                Ti seasonCount, const std::vector<std::vector<Ti>> &exoIndexes,
                bool usePreviousEstim, LimitedMemoryBfgsbOptions *optimOptions,
                Tv stdMultiplier, Ti maxHorizonCheck);

  ~VarmaModelset() {
    for (auto s : Searchers)
      delete s;
    delete[] ForecastLowers.Data;
    delete[] ForecastUppers.Data;
  };
};

} // namespace ldt
