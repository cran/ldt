#pragma once

#include <map>
#include <string>

#include "ldt_base.h"

#include "data_split.h"
#include "distributions.h"
#include "matrix.h"
#include "pca.h"
#include "searchers.h"

namespace ldt {

/// @brief A class for estimating a system of Seemingly Unrelated Regressions
class LDT_EXPORT Sur {

public:
  /// @brief Gets the required storage size
  Ti StorageSize = 0;

  /// @brief Gets the required work size
  Ti WorkSize = 0;

  /// @brief Gets whether the model is restricted or not
  bool mIsRestricted = false;

  /// @brief Gets whether the details are calculated or not
  bool mDoDetails = false;

  /// @brief Gets the number of iterations for searching for significant
  /// coefficients. If zero,
  Ti mSigSearchMaxIter = 0;

  /// @brief A pointer to the given endogenous data in the \ref Calculate
  const Matrix<Tv> *pY = nullptr;

  /// @brief A pointer to the given exogenous data in the \ref Calculate
  const Matrix<Tv> *pX = nullptr;

  /// @brief A pointer to the given restrictions in the \ref Calculate
  Matrix<Tv> *pR = nullptr;

  /// @brief A pointer to the given restrictions in the \ref Calculate
  Matrix<Tv> *pr = nullptr;

  /// @brief After \ref Calculate, it is the vector of the estimated parameters.
  /// For an unrestricted model (0:m) elements are for the first equation
  Matrix<Tv> gamma;

  /// @brief After \ref Calculate, it is the matrix of the estimated parameters.
  /// (i,j) element is for the j-th equation and i-th exogenous variable
  Matrix<Tv> beta;

  /// @brief After \ref Calculate, condition number in the estimation
  Tv condition_number = NAN;

  /// @brief After \ref Calculate, estimated variance of \ref gamma
  Matrix<Tv> gamma_var;

  /// @brief After \ref Calculate, matrix of projections. Variables are in the
  /// columns
  Matrix<Tv> yhat;

  /// @brief After \ref Calculate, matrix of residuals. Variables are in the
  /// columns
  Matrix<Tv> resid;

  /// @brief After \ref Calculate, estimated variance of the residuals
  Matrix<Tv> resid_var;

  /// @brief After \ref Calculate, logarithm of the likelihood
  Tv logLikelihood = NAN;

  /// @brief After \ref Calculate, coefficient of determination
  Tv r2 = NAN;

  /// @brief After \ref Calculate, F statistics
  Tv f = NAN;

  /// @brief After \ref Calculate, p-value of \ref f
  Tv f_prob = NAN;

  /// @brief After \ref Calculate, AIC
  Tv Aic = NAN;

  /// @brief After \ref Calculate, SIC
  Tv Sic = NAN;

  /// @brief After \ref Calculate, HQIC
  Tv Hqic = NAN;

  /// @brief After \ref Calculate and if details are requested, matrix of
  /// estimated standard error
  Matrix<Tv> e_beta_std;

  /// @brief After \ref Calculate and if details are requested, matrix of t
  /// statistics
  Matrix<Tv> e_beta_t;

  /// @brief After \ref Calculate and if details are requested, matrix of
  /// p-values
  Matrix<Tv> e_beta_prob;

  /// @brief After \ref Calculate, final number of significant searches
  Ti mSigSearchIter = 0;

  /// @brief Initializes an instance of this class
  Sur(){};

  /// @brief Initializes an instance of this class
  /// @param N Maximum expected number of observations
  /// @param m Maximum expected number of equations
  /// @param k Maximum expected number of exogenous variables
  /// @param is_restricted If true, a restricted model is expected
  /// @param do_details If true, details are calculated (e.g., p-values matrix)
  /// @param max_sig_search_iter If not 0, this determines the number of
  /// iterations for finding significant coefficients
  Sur(Ti N, Ti m, Ti k, bool is_restricted, bool do_details,
      Ti max_sig_search_iter = 0);

  /// @brief Calculates the results
  /// @param y Matrix of endogenous data with variables in the columns
  /// @param x Matrix of exogenous data with variables in the columns
  /// @param storage Storage array with size \ref StorageSize
  /// @param work Work array with size \ref WorkSize
  /// @param R Matrix of restrictions. It can be null for an unrestricted model.
  /// If significant search is requested, It should be a (km x km) matrix in
  /// which 'k' and 'm' are the number of exogenous and endogenous variables.
  /// @param sigSearchMaxProb A value for p-value that determines a coefficient
  /// is insignificant
  void Calculate(const Matrix<Tv> &y, const Matrix<Tv> &x, Tv *storage,
                 Tv *work, Matrix<Tv> *R = nullptr, Tv sigSearchMaxProb = 0);

private:
  void calculate_details(Ti N, Ti m, Tv *work, bool just_probs = false,
                         bool force_unrestricted = false);
  void estim_un(Ti N, Ti m, Tv *work, bool do_gamma_var = true);
  void estim_r(Ti N, Ti m, Tv *work);
  void estim_search(Ti N, Ti m, Tv *work, Tv sigSearchMaxProb);
};

/// @brief A class for projecting with an SUR model
class LDT_EXPORT SurProjection {
public:
  /// @brief see the constructor
  bool mIsRestricted = false;

  /// @brief see the constructor
  bool mDoVariance = false;

  /// @brief Gets the required storage size
  Ti StorageSize = 0;

  /// @brief Gets the required work size
  Ti WorkSize = 0;

  /// @brief After \ref Calculate, it is the projected values
  Matrix<Tv> Means;

  /// @brief After \ref Calculate and if requested, it is the estimated variance
  /// of the projected values
  Matrix<Tv> Variances;

  /// @brief Covariance matrix for the last row in x. Dimension: (m x m) in
  /// which 'm' is the number of equations
  Matrix<Tv> Covariance;

  /// @brief Initializes a new instance of this class
  SurProjection(){};

  /// @brief Initializes a new instance of this class
  /// @param n Maximum expected number of observations (i.e., rows in x (see
  /// \ref Calculate))
  /// @param m Maximum expected number of equations (or endogenous variables)
  /// @param k Maximum expected number of columns in x (or exogenous variables)
  /// @param isRestricted If true, a restricted model is expected
  /// @param doVariance If true, variance is also estimated.
  SurProjection(Ti n, Ti m, Ti k, bool isRestricted, bool doVariance);

  /// @brief Calculates the results for an estimated model
  /// @param model The estimated model
  /// @param x Matrix of exogenous data with variables in the columns.
  /// Dimension: (n x k)
  /// @param storage Storage array with size \ref StorageSize
  /// @param work Work array with size \ref WorkSize
  void Calculate(const Sur &model, const Matrix<Tv> &x, Tv *storage, Tv *work);
};

/// @brief An extended SUR class that deals with NAN, projections, etc.
class LDT_EXPORT SurExtended {
  bool mHasPcaY = false;
  bool mHasPcaX = false;

public:
  /// @brief Gets the required storage size
  Ti StorageSize = 0;

  /// @brief Gets the required work size
  Ti WorkSize = 0;

  /// @brief See the constructor
  bool mCheckNan = false;

  /// @brief See the constructor
  PcaAnalysisOptions *pPcaOptionsY = nullptr;

  /// @brief See the constructor
  PcaAnalysisOptions *pPcaOptionsX = nullptr;

  /// @brief A data set for dealing with NANs or PCAs
  Dataset<Tv> Data;

  /// @brief PCA for endogenous data (if requested)
  PcaAnalysis PcaY;

  /// @brief PCA for exogenous data (if requested)
  PcaAnalysis PcaX;

  /// @brief The inner model
  Sur Model;

  /// @brief Projection class (if requested)
  SurProjection Projections;

  /// @brief After calculate, it contains the used endogenous data
  Matrix<Tv> Y;

  /// @brief After calculate, it contains the used exogenous data
  Matrix<Tv> X;

  /// @brief Initializes a new instance of this class
  SurExtended(){};

  /// @brief Initializes a new instance of this class
  /// @param N Maximum expected number of observations
  /// @param m Maximum expected number of equations
  /// @param k Maximum expected number of exogenous variables
  /// @param isRestricted If true, a restricted model is expected
  /// @param checkNan If true, NAN data is checked
  /// @param doDetails If true, details are calculated
  /// @param numProjections Maximum expected number of projections
  /// @param maxSigSearchIter Maximum number of significant search iterations
  /// @param doProjVariance If true, variance of the projected values are
  /// estimated
  /// @param pcaOptionsY If not null, PCA analysis is performed for endogenous
  /// data
  /// @param pcaOptionsX If not null, PCA analysis is performed for exogenous
  /// data
  SurExtended(Ti N, Ti m, Ti k, bool isRestricted, bool checkNan,
              bool doDetails, Ti numProjections, Ti maxSigSearchIter,
              bool doProjVariance, PcaAnalysisOptions *pcaOptionsY,
              PcaAnalysisOptions *pcaOptionsX);

  /// @brief Calculates the results
  /// @param data Data with variables in the columns
  /// @param m Number of equations
  /// @param storage Storage array with size \ref StorageSize
  /// @param work Work array with size \ref WorkSize
  /// @param R Matrix of restrictions. It can be null for an unrestricted model.
  /// If significant search is requested, It should be a (km x km) matrix in
  /// which 'k' and 'm' are the number of exogenous and endogenous variables.
  /// @param sigSearchMaxProb A value for p-value that determines a coefficient
  /// is insignificant
  /// @param newX New exogenous data for projection
  /// @param checks Enables checking some values
  void Calculate(const Matrix<Tv> &data, Ti m, Tv *storage, Tv *work,
                 Matrix<Tv> *R, Tv sigSearchMaxProb, const Matrix<Tv> *newX,
                 const SearchModelChecks *checks = nullptr);
};

/// @brief A simulation class for SUR
class LDT_EXPORT SurSimulation {

  bool mDoForecastVar = true;

  void AddError(std::string state);

public:
  /// @brief Gets the required storage size
  Ti StorageSize = 0;

  /// @brief Gets the required work size
  Ti WorkSize = 0;

  /// @brief See the constructor
  Tv mTrainPerc = 0.5;

  /// @brief See the constructor
  Ti mTrainFixSize = 0;

  /// @brief In \ref Calculate, show current number of iteration
  Ti Iteration = 0;

  /// @brief Splits the data
  DataSplit Split;

  /// @brief inner array for \ref Split
  std::unique_ptr<Ti[]> Split_d;

  /// @brief Inner estimating model
  SurExtended Model;

  /// @brief Gets or sets whether error must be kept
  bool KeepErrors = true;

  /// @brief A pointer to the list of metrics given in the constructor
  const std::vector<ScoringType> *pMetricsOut = nullptr;

  /// @brief List of errors. See \ref KeepErrors
  std::map<std::string, Ti> Errors;

  /// @brief Simulation results. Measures are in the rows, variables are in the
  /// columns
  Matrix<Tv> Results;

  /// @brief Number of valid iterations
  Ti ValidIters = 0;

  /// @brief Number of valid out-of-sample projections
  Ti ValidCounts = 0;

  /// @brief Initializes a new instance of this class
  SurSimulation(){};

  /// @brief Initializes a new instance of this class
  /// @param N Maximum expected number of observations
  /// @param m Maximum expected number of equations
  /// @param k Maximum expected number of exogenous variables
  /// @param trainRatio Size of the training sample relative to the number of
  /// observations
  /// @param trainFixSize If \p trainRatio is 0, it determines a fixed size for
  /// the training sample
  /// @param metricsOut List of out-of-sample metrics
  /// @param isRestricted If true, a restricted model is expected
  /// @param maxSigSearchIter Maximum number of significant search iterations
  /// @param pcaOptionsY If not null, PCA analysis is performed for endogenous
  /// data
  /// @param pcaOptionsX If not null, PCA analysis is performed for exogenous
  /// data
  SurSimulation(Ti N, Ti m, Ti k, Tv trainRatio, Ti trainFixSize,
                const std::vector<ScoringType> &metricsOut, bool isRestricted,
                Ti maxSigSearchIter, PcaAnalysisOptions *pcaOptionsY,
                PcaAnalysisOptions *pcaOptionsX);

  /// @brief Calculates the results
  /// @param data Data with variables in the columns
  /// @param m Number of endogenous variables (first m columns of \p data)
  /// @param storage Storage array with size \ref StorageSize
  /// @param work Work array with size \ref WorkSize
  /// @param R Restrictions
  /// @param cancel If true, breaks the iteration loop
  /// @param maxIteration Maximum number of iterations
  /// @param seed A seed for RNG
  /// @param sigSearchMaxProb A value for p-value that determines a coefficient
  /// is insignificant
  /// @param maxCondNum Maximum valid condition number
  /// @param maxInvalidSim Maximum number of invalid simulations before it exit
  /// the iteration loop
  /// @param transformForMetrics A function to be used for transforming the data
  /// when calculating metrics such as RMSE or MAE
  void
  Calculate(Matrix<Tv> &data, Ti m, Tv *storage, Tv *work, Matrix<Tv> *R,
            bool &cancel, Ti maxIteration, unsigned int seed,
            Tv sigSearchMaxProb, Tv maxCondNum = INFINITY,
            Ti maxInvalidSim = INT32_MAX,
            const std::function<void(Tv &)> *transformForMetrics = nullptr);
};

/// @brief A searcher class for SUR
class LDT_EXPORT SurSearcher : public Searcher {
  Tv SigSearchMaxProb = 0.05;
  Ti SigSearchMaxIter = 0;

  /// @brief It might be different from \ref pMetrics->Seed
  unsigned int Seed;

  Matrix<Ti> EndoIndexes;
  const Matrix<Tv> *pSource = nullptr;

  Dataset<Tv> Data;
  SurExtended DModel;
  SurSimulation Model;

  Matrix<Tv> Y;
  Matrix<Tv> X;

  Matrix<Tv> R;
  std::unique_ptr<Tv[]> R_d;

  std::string EstimateOne(Tv *work, Ti *workI) override;

  std::vector<Ti> Indexes;
  std::vector<Ti> TargetsPositions;

public:
  /// @brief Initializes a new instance of this method
  /// @param searchOptions Passed to the base constructor
  /// @param searchItems Passed to the base constructor
  /// @param metrics Passed to the base constructor
  /// @param checks Passed to the base constructor
  /// @param sizeG Passed to the base constructor
  /// @param groupIndexMap Passed to the base constructor
  /// @param fixFirstG Passed to the base constructor
  /// @param source Data with variables in columns
  /// @param endoIndices Endogenous indices in this searcher
  /// @param sigSearchMaxIter Maximum iterations for significant search
  /// @param sigSearchMaxProb p-value for significant search
  /// @param seed A seed for RNG
  SurSearcher(SearchOptions &searchOptions, const SearchItems &searchItems,
              const SearchMetricOptions &metrics,
              const SearchModelChecks &checks, Ti sizeG,
              const std::vector<std::vector<Ti>> &groupIndexMap, Ti fixFirstG,
              Matrix<Tv> &source, std::vector<Ti> &endoIndices,
              Ti sigSearchMaxIter, Tv sigSearchMaxProb, unsigned int seed);
};

/// @brief A model set for SUR
class LDT_EXPORT SurModelset {
public:
  /// @brief The inner model set
  ModelSet Modelset;

  /// @brief List of searchers
  std::vector<Searcher *> Searchers;

  /// @brief Initializes a new instance of this class
  SurModelset(){};

  /// @brief Initializes a new instance of this class
  /// @param searchOptions Passed to the searcher
  /// @param searchItems Passed to the searcher
  /// @param metrics Passed to the searcher
  /// @param checks Passed to the searcher
  /// @param exoSizes Determines different sizes for exogenous variables
  /// @param groupIndexMap Groups for exogenous variables
  /// @param numFixXPartitions Number of first fixed partitions
  /// @param source Data
  /// @param endoIndices A list of potential endogenous indices
  /// @param sigSearchMaxIter Maximum iterations for significant search
  /// @param sigSearchMaxProb p-value for significant search
  SurModelset(SearchOptions &searchOptions, SearchItems &searchItems,
              SearchMetricOptions &metrics, SearchModelChecks &checks,
              const std::vector<Ti> &exoSizes,
              std::vector<std::vector<Ti>> &groupIndexMap, Ti numFixXPartitions,
              Matrix<Tv> &source, std::vector<std::vector<Ti>> &endoIndices,
              Ti sigSearchMaxIter, Tv sigSearchMaxProb);

  ~SurModelset() {
    for (auto s : Searchers)
      delete s;
  }
};

} // namespace ldt
