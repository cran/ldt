#pragma once

#include "helpers.h"
#include "ldt_base.h"

#include "distributions.h"
#include "matrix.h"
#include "optimization.h"
#include "pca.h"
#include "running.h"
#include "scoring.h"
#include "searchers.h"
#include "statistics.h"
#include <algorithm>
#include <iostream>
#include <numeric>
#include <random>
#include <set>
#include <sstream>
#include <string>
#include <vector>

namespace ldt {

/// @brief Distribution types in a discrete choice model
enum class DiscreteChoiceDistType {

  /// @brief Logit model
  kLogit = 0,

  /// @brief Probit model
  kProbit = 1
};

/// @brief Converts a value of \ref DiscreteChoiceDistType to a string
/// @param v The value
/// @return The string
inline const char *ToString(DiscreteChoiceDistType v) {
  switch (v) {
  case ldt::DiscreteChoiceDistType::kLogit:
    return "Logit";
  case ldt::DiscreteChoiceDistType::kProbit:
    return "Probit";
  default:
    return "[Unknown DiscreteChoiceDistType]";
  }
}

/// @brief Converts a string to a value of \ref DiscreteChoiceDistType
/// @param v The string
/// @return The value
inline DiscreteChoiceDistType FromString_DiscreteChoiceDistType(const char *v) {
  if (StartsWith("log", v))
    return DiscreteChoiceDistType::kLogit;
  else if (StartsWith("pro", v))
    return DiscreteChoiceDistType::kProbit;

  throw LdtException(
      ErrorType::kLogic, "discrete choice",
      format("invalid or not implemented link function (name={})", v));
}

/// @brief Model type in a discrete choice model
enum class DiscreteChoiceModelType {

  /// @brief Binary model
  kBinary,

  /// @brief Ordered model
  kOrdered

};

/// @brief Converts a value of \ref DiscreteChoiceModelType to a string
/// @param v The value
/// @return The string
inline const char *ToString(DiscreteChoiceModelType v) {
  switch (v) {
  case ldt::DiscreteChoiceModelType::kBinary:
    return "Binary";
  case ldt::DiscreteChoiceModelType::kOrdered:
    return "Ordered";
  default:
    return "[Unknown 'DiscreteChoiceModelType']";
  }
}

/// @brief Converts a string to a value of \ref DiscreteChoiceModelType
/// @param v The string
/// @return The value
inline DiscreteChoiceModelType
FromString_DiscreteChoiceModelType(const char *v) {
  if (StartsWith("bin", v))
    return DiscreteChoiceModelType::kBinary;
  else if (StartsWith("ord", v))
    return DiscreteChoiceModelType::kOrdered;

  throw LdtException(ErrorType::kLogic, "discrete choice",
                     format("invalid or not implemented model (name={})", v));
}

/// @brief A base class for discrete choice regression model
class LDT_EXPORT DiscreteChoiceBase {
protected:
  bool mDoDetails = false;

public:
  /// @brief Model type
  DiscreteChoiceModelType mModelType;

  /// @brief Distribution type
  DiscreteChoiceDistType mDistType;

  DiscreteChoiceBase(){};
  virtual ~DiscreteChoiceBase(){};

  /// @brief Gets the required storage size
  Ti StorageSize = 0;

  /// @brief Gets the required work size
  Ti WorkSize = 0;

  /// @brief Gets or sets the optimization options
  Newton Optim;

  // #pragma region Result

  /// @brief After \ref Calculate, number of observations used in estimation
  Ti NumObs = 0;

  /// @brief After \ref Calculate, estimated coefficients (k x 1). Set the start
  /// values after 'setStorage'. Before estimation, if the first element is NAN,
  /// it is overridden by the initial values
  Matrix<Tv> Beta;

  /// @brief After \ref Calculate, estimated variance of coefficients (k x k)
  Matrix<Tv> BetaVar;

  /// @brief Condition number which is 1-norm of variance beta multiplied by
  /// 1-norm of its inverse
  Tv condition_number = NAN;

  // @brief n x 1 (todo)
  // Matrix<Tv>* resid = nullptr;

  /// @brief After \ref Calculate, gets the number of cutoffs. With 'M' choices,
  /// there are 'M-1' cutoffs and 'M-2' estimated parameters
  Ti NumCutoff = 0;

  /// @brief After \ref Calculate, gets the number of choices. This is maximum
  /// value of y plus 1. For binary model it is 2.
  Ti NumChoices = 0;

  /// @brief  After \ref Calculate, gets the number of different labels. Length
  /// = \ref NumCutoff + 1 or = \ref NumChoices. Index indicates the label.
  Matrix<Tv> Counts;

  // @brief The index of constant in the variables. Is -1 if constant is
  // missing.
  // int constIndex = -2;

  /// @brief After \ref Calculate, log likelihood
  Tv LogL = NAN;

  /// @brief After \ref Calculate, AIC
  Tv Aic = NAN;

  /// @brief After \ref Calculate, SIC
  Tv Sic = NAN;

  // @brief If there is constant, it is the value of log-likelihood function
  // when all slopes are restricted to 0.0 If there is no constant, it is NAN
  // Tv logL_b0 = NAN;

  /// @brief k x 1 vector. sqrt of BetaVar diagonal (if details is requested)
  Matrix<Tv> BetaStd;

  /// @brief z statistics (if details is requested)
  Matrix<Tv> BetaZ;

  /// @brief p-values (if details is requested)
  Matrix<Tv> BetaProb;

  // #pragma endregion

  virtual void Calculate(const Matrix<Tv> &y, const Matrix<Tv> &x,
                         const Matrix<Tv> *w, Tv *storage, Tv *work,
                         Ti numChoices = -1, bool olsInitial = true) = 0;

  virtual void GetProbabilities(const Matrix<Tv> &x, Matrix<Tv> &result,
                                Tv *work) = 0;

  /// @brief Gets a pointer to the class by type
  /// @param modelType Model type
  /// @param distType Distribution type
  /// @param numObs Maximum expected number of observations
  /// @param numExo Maximum expected number of exogenous variables
  /// @param numChoices Number of choices. For binary model it is 2.
  /// @param doDetails If true, p-values are calculated
  /// @return A pointer to the initialized class
  static DiscreteChoiceBase *GetFromType(DiscreteChoiceModelType modelType,
                                         DiscreteChoiceDistType distType,
                                         Ti numObs, Ti numExo,
                                         Ti numChoices = 2,
                                         bool doDetails = true);

protected:
  virtual void EstimatePriorBinary(const Matrix<Tv> &y, const Matrix<Tv> &x,
                                   const Matrix<Tv> *w, Tv *work) = 0;
  virtual void EstimatePriorOrdered(const Matrix<Tv> &y, const Matrix<Tv> &x,
                                    const Matrix<Tv> *w, Tv *work) = 0;

  virtual void EstimateBinary(const Matrix<Tv> &y, const Matrix<Tv> &x,
                              const Matrix<Tv> *w, Tv *work,
                              bool olsInitial) = 0;
  virtual void EstimateOrdered(const Matrix<Tv> &y, const Matrix<Tv> &x,
                               const Matrix<Tv> *w, Tv *work,
                               bool olsInitial) = 0;
};

template <DiscreteChoiceModelType modelType = DiscreteChoiceModelType::kBinary,
          DiscreteChoiceDistType distType = DiscreteChoiceDistType::kLogit>
class LDT_EXPORT DiscreteChoice : public DiscreteChoiceBase {
public:
  DiscreteChoice(){};

  /// @brief Initializes a new instance of the class
  /// @param numObs Maximum expected number of observations
  /// @param numExo Maximum expected number of exogenous variables
  /// @param numChoices Number of choices. For binary model it is 2.
  /// @param doDetails If true, p-values are calculated
  DiscreteChoice(Ti numObs, Ti numExo, Ti numChoices = 2,
                 bool doDetails = true);

  /// @brief Starts estimation
  /// @param y Observations. labels must start from 0. E.g., in binary, there
  /// are just 0s and 1s.
  /// @param x Exogenous data with variables in the columns
  /// @param w Observations weights
  /// @param storage Storage array of size \ref StorageSize
  /// @param work Work array of size \ref WorkArray
  /// @param numChoices Number of choices. If -1, it will be calculated
  /// @param olsInitial If true, it calculates initial values with OLS
  /// estimation. Otherwise, uses current values in \ref Beta
  virtual void Calculate(const Matrix<Tv> &y, const Matrix<Tv> &x,
                         const Matrix<Tv> *w, Tv *storage, Tv *work,
                         Ti numChoices = -1, bool olsInitial = true) override;

  /// @brief Calculates the probabilities for a sample of exogenous data
  /// @param x Exogenous data (M x K)
  /// @param result A place to keep the results. Given (M x K) matrix of
  /// exogenous data, it is (M x C) where 'C' is the number of choices
  /// @param work Work array of size M + NumChoices - 2
  virtual void GetProbabilities(const Matrix<Tv> &x, Matrix<Tv> &result,
                                Tv *work) override;

protected:
  virtual void EstimatePriorBinary(const Matrix<Tv> &y, const Matrix<Tv> &x,
                                   const Matrix<Tv> *w, Tv *work) override;
  virtual void EstimatePriorOrdered(const Matrix<Tv> &y, const Matrix<Tv> &x,
                                    const Matrix<Tv> *w, Tv *work) override;

  virtual void EstimateBinary(const Matrix<Tv> &y, const Matrix<Tv> &x,
                              const Matrix<Tv> *w, Tv *work,
                              bool olsInitial) override;
  virtual void EstimateOrdered(const Matrix<Tv> &y, const Matrix<Tv> &x,
                               const Matrix<Tv> *w, Tv *work,
                               bool olsInitial) override;
};

extern template class ldt::DiscreteChoice<DiscreteChoiceModelType::kBinary,
                                          DiscreteChoiceDistType::kLogit>;
extern template class ldt::DiscreteChoice<DiscreteChoiceModelType::kBinary,
                                          DiscreteChoiceDistType::kProbit>;

extern template class ldt::DiscreteChoice<DiscreteChoiceModelType::kOrdered,
                                          DiscreteChoiceDistType::kLogit>;
extern template class ldt::DiscreteChoice<DiscreteChoiceModelType::kOrdered,
                                          DiscreteChoiceDistType::kProbit>;

/// @brief A extended version of \ref DiscreteChoice in which NAN, PCA,
/// Projection, ... are handled
class LDT_EXPORT DiscreteChoiceExtended {

  PcaAnalysisOptions *pPcaOptions = nullptr;
  bool mDoDetails = false;
  Ti mNumChoices = 0;
  bool mHasWeight = false;
  bool mCheckNan = false;
  bool mWeightedEval = false;
  DiscreteChoiceModelType mModelType = DiscreteChoiceModelType::kBinary;

public:
  /// @brief Gets the required storage size
  Ti StorageSize = 0;

  /// @brief Gets the required work size
  Ti WorkSize = 0;

  /// @brief After \ref Calculate, endogenous data
  Matrix<Tv> Y;

  /// @brief After \ref Calculate, exogenous data
  Matrix<Tv> X;

  /// @brief After \ref Calculate, weights
  Matrix<Tv> W;

  /// @brief Data for handling NANs. It is empty if \ref mCheckNan is false
  Dataset<Tv> Data;

  /// @brief A place to calculate the PCA for exogenous data. It is empty if PCA
  /// is not requested.
  PcaAnalysis Pca;

  /// @brief A pointer to the estimated model. The class is its owner.
  DiscreteChoiceBase *Model = nullptr;

  /// @brief A pointer to the given cost matrices. The class does not own it.
  std::vector<Matrix<Tv>> *pCostMatrices = nullptr;

  /// @brief After \ref Calculate, predicted probabilities
  Matrix<Tv> PredProbs;

  /// @brief After \ref Calculate, the projections
  Matrix<Tv> Projections;

  /// @brief After \ref Calculate, in-sample AUC (i.e., for \ref Projections)
  Tv Auc = NAN;

  /// @brief After \ref Calculate, average of in-sample cost ratios (i.e., for
  /// \ref Projections)
  Tv CostRatioAvg = NAN;

  /// @brief Brier score, the mean squared difference between predicted
  /// probabilities and actual outcomes.
  Tv BrierScore = NAN;

  /// @brief Initializes a new instance of this class
  /// @param modelType Type of model
  /// @param distType Type of distribution
  /// @param rows Maximum expected number of rows (for data in \ref Calculate)
  /// @param cols Maximum expected number of columns in data (for data in \ref
  /// Calculate)
  /// @param hasWeight If true, observations are weighted
  /// @param checkNan If true, NAN might exist and it is checked
  /// @param numChoices Number of unique labels
  /// @param doDetails If true, extra information is also calculated
  /// @param numForecast Maximum expected number of forecasts
  /// @param pcaOptions If not null, PCA for exogenous variables is used
  /// @param costMatrices If not null, average of cost ratio is calculated
  /// @param weightedEval If true, weights are used in evaluations
  DiscreteChoiceExtended(DiscreteChoiceModelType modelType,
                         DiscreteChoiceDistType distType, Ti rows, Ti cols,
                         bool hasWeight, bool checkNan, Ti numChoices,
                         bool doDetails, Ti numForecast,
                         PcaAnalysisOptions *pcaOptions,
                         std::vector<Matrix<Tv>> *costMatrices = nullptr,
                         bool weightedEval = true);

  ~DiscreteChoiceExtended();

  /// @brief Estimates the model and calculated other requested information
  /// @param data (N x M) matrix of data with variables in the columns
  /// @param storage Storage array of size \ref StorageSize
  /// @param work Work array of size \ref WorkSize
  /// @param olsInitial If true, OLS is used to initialize the coefficients.
  /// Otherwise, current values are used.
  /// @param xForecast If not null, exogenous data for forecasting
  /// @param aucOptions Options for calculating AUC
  void Calculate(const Matrix<Tv> &data, Tv *storage, Tv *work, bool olsInitial,
                 const Matrix<Tv> *xForecast, RocOptions &aucOptions);
};

// todo: add DiscreteChoiceFrequencyTable class

/// @brief A base class for discrete choice simulation
class LDT_EXPORT DiscreteChoiceSimBase {
public:
  DiscreteChoiceSimBase(){};
  virtual ~DiscreteChoiceSimBase(){};

  /// @brief Gets the required storage size
  Ti StorageSize = 0;

  /// @brief Gets the required work size (Tv)
  Ti WorkSize = 0;

  /// @brief Gets the required work size (Ti)
  Ti WorkSizeI = 0;

  /// @brief Optimization options
  Newton Optim;

  /// @brief Gets or sets the seed in shuffle function
  unsigned int Seed = 0;

  /// @brief Gets or sets the maximum number of simulations
  Ti SimulationMax = 100;

  /// @brief A pointer to the given class
  PcaAnalysisOptions *pPcaOptions = nullptr;

  // results

  /// @brief After \ref Calculate, gets the current number of simulations
  /// performed
  Ti SimulationCounter = 0;

  /// @brief After \ref Calculate, gets a table with frequency of each
  /// probability in each choice
  Matrix<Tv> FrequencyTable;

  /// @brief After \ref Calculate, number of valid simulations at the end. Each
  /// simulation contains N1 test observations
  Ti ValidSimulationCount = -1;

  /// @brief After \ref Calculate, test sample size
  Ti N1 = -1;

  /// @brief After \ref Calculate, values are in [0,1]. Smaller values means
  /// smaller expected cost
  Matrix<Tv> CostRatios;

  /// @brief After \ref Calculate, average value of AUC
  Tv Auc = NAN;

  /// @brief Brier score, the mean squared difference between predicted
  /// probabilities and actual outcomes.
  Tv BrierScore = NAN;

  /// @brief Get a simulation class from types. Arguments are passed to the
  /// class or the constructors
  static DiscreteChoiceSimBase *
  GetFromType(bool hasWeight, DiscreteChoiceModelType modelType,
              DiscreteChoiceDistType distType, Ti rows, Ti cols, Ti numChoices,
              Tv trainPercentage, Ti trainFixSize, Ti costMatrixCount,
              bool doBrier, bool doAuc, bool doFrequecyTable,
              PcaAnalysisOptions *pcaOptions, bool weightedEval);

  virtual void Calculate(const Matrix<Tv> &data,
                         const std::vector<Matrix<Tv>> *costMatrixes,
                         Tv *storage, Tv *work, Ti *workI, bool &cancel,
                         RocOptions &aucOptions, bool checkSizes = true,
                         std::set<const char *> *errors = nullptr,
                         Ti maxInvalidSim = INT32_MAX) = 0;
};

/// @brief A discrete choice simulation class derived from \ref
/// DiscreteChoiceSimBase
/// @tparam hasWeight If true, the observations are weighted
/// @tparam modelType Type of model
/// @tparam distType Type of distribution
template <bool hasWeight,
          DiscreteChoiceModelType modelType = DiscreteChoiceModelType::kBinary,
          DiscreteChoiceDistType distType = DiscreteChoiceDistType::kLogit>
class LDT_EXPORT DiscreteChoiceSim : public DiscreteChoiceSimBase {

  Tv mTrainRatio = 0;
  Ti mTrainFixSize = 0;
  bool mDoFrequecyTable = false;
  Ti mCostMatrixCount = 0;
  Ti mNumChoices = 0;
  bool mDoAuc = false;
  bool mWeightedEval = false;
  bool mDoBrier = false;

public:
  /// @brief Initializes a new instance of this class
  DiscreteChoiceSim(){};

  /// @brief Initializes a new instance of this class
  /// @param rows Maximum expected number of rows (for data in \ref Calculate)
  /// @param cols Maximum expected number of columns (for data in \ref
  /// Calculate)
  /// @param numChoices Number of choices (it cannot be less than 1)
  /// @param trainPercentage Maximum value for training sample size
  /// @param trainFixSize If \p trainPercentage, this value is used for a fixed
  /// training sample size
  /// @param costMatrixCount Maximum expected number of cost matrices
  /// @param doAuc If true, average of out-of-sample AUC (over all simulations)
  /// is calculated
  /// @param doFrequencyTable If true, a frequency table (average over all
  /// simulations) is calculated
  /// @param pcaOptions If true, PCA of exogenous data is calculated in the
  /// simulations
  /// @param weightedEval If true, weights are used in evaluations
  DiscreteChoiceSim(Ti rows, Ti cols, Ti numChoices, Tv trainPercentage,
                    Ti trainFixSize, Ti costMatrixCount, bool doBrier,
                    bool doAuc, bool doFrequencyTable = false,
                    PcaAnalysisOptions *pcaOptions = nullptr,
                    bool weightedEval = false);

  /// @brief Calculates the results
  /// @param data Data with variables in the columns. First column is
  /// endogenous, if weighted, second column is weight, next column in the
  /// intercept
  /// @param costMatrixes List of cost matrices for calculating the average of
  /// cost ratios
  /// @param storage Storage array of size \ref StorageSize
  /// @param work Work array of size \ref WorkSize (real)
  /// @param workI Work array of size \ref WorkSize  (integer)
  /// @param cancel A reference to be used for cancelling the operations
  /// @brief aucOptions Options for calculating AUC
  /// @param checkSizes If true, length of work and storage arrays are checked.
  /// @param errors If not null, on exit it contains the list of errors occurred
  /// @param maxInvalidSim A maximum value for the number of invalid simulations
  /// before breaking the loop
  virtual void Calculate(const Matrix<Tv> &data,
                         const std::vector<Matrix<Tv>> *costMatrixes,
                         Tv *storage, Tv *work, Ti *workI, bool &cancel,
                         RocOptions &aucOptions, bool checkSizes = true,
                         std::set<const char *> *errors = nullptr,
                         Ti maxInvalidSim = INT32_MAX) override;
};

extern template class ldt::DiscreteChoiceSim<
    true, DiscreteChoiceModelType::kBinary, DiscreteChoiceDistType::kLogit>;
extern template class ldt::DiscreteChoiceSim<
    true, DiscreteChoiceModelType::kBinary, DiscreteChoiceDistType::kProbit>;

extern template class ldt::DiscreteChoiceSim<
    true, DiscreteChoiceModelType::kOrdered, DiscreteChoiceDistType::kLogit>;
extern template class ldt::DiscreteChoiceSim<
    true, DiscreteChoiceModelType::kOrdered, DiscreteChoiceDistType::kProbit>;

extern template class ldt::DiscreteChoiceSim<
    false, DiscreteChoiceModelType::kBinary, DiscreteChoiceDistType::kLogit>;
extern template class ldt::DiscreteChoiceSim<
    false, DiscreteChoiceModelType::kBinary, DiscreteChoiceDistType::kProbit>;

extern template class ldt::DiscreteChoiceSim<
    false, DiscreteChoiceModelType::kOrdered, DiscreteChoiceDistType::kLogit>;
extern template class ldt::DiscreteChoiceSim<
    false, DiscreteChoiceModelType::kOrdered, DiscreteChoiceDistType::kProbit>;

/// @brief A searcher for discrete choice
/// @tparam hasWeight If true, the observations are weighted
/// @tparam modelType Type of model
/// @tparam distType Type of distribution
template <bool hasWeight,
          DiscreteChoiceModelType modelType = DiscreteChoiceModelType::kBinary,
          DiscreteChoiceDistType distType = DiscreteChoiceDistType::kLogit>
class LDT_EXPORT DiscreteChoiceSearcher : public Searcher {

  const std::vector<Matrix<Tv>> *pCostMatrixes = nullptr;
  const Matrix<Tv> *pSource = nullptr;

  Matrix<Tv> Y;
  Matrix<Tv> X;
  Matrix<Tv> W;
  Dataset<Tv> Data;
  DiscreteChoiceSim<hasWeight, modelType, distType> Model;
  DiscreteChoice<modelType, distType> DModel;
  Ti mNumChoices = 0;

  std::string EstimateOne(Tv *work, Ti *workI) override;

  std::vector<Ti> Indexes;
  Matrix<Ti> ExoIndexes;
  Matrix<Tv> Weights;
  std::unique_ptr<FrequencyCostBase> CostIn;
  Matrix<Tv> Probs;
  std::unique_ptr<RocBase> AucIn;
  RocOptions *pAucOptions;

public:
  /// @brief Initializes a new instance of the class
  /// @param searchOptions It is passed to the base class
  /// @param searchItems It is passed to the base class
  /// @param metrics It is passed to the base class
  /// @param checks It is passed to the base class
  /// @param SizeG It is passed to the base class
  /// @param groupIndexMap It is passed to the base class
  /// @param fixFirstG It is passed to the base class
  /// @param source Data with variables in the columns
  /// @param numChoices Number of unique labels
  /// @param costMatrixes List of cost matrices
  /// @param seed A seed for the simulations. It can be negative for replicating
  /// the results
  /// @param newtonOptions Optimization options
  /// @param aucOptions Options for calculating AUC
  DiscreteChoiceSearcher(SearchOptions &searchOptions,
                         const SearchItems &searchItems,
                         const SearchMetricOptions &metrics,
                         const SearchModelChecks &checks, Ti SizeG,
                         const std::vector<std::vector<Ti>> &groupIndexMap,
                         Ti fixFirstG, const Matrix<Tv> &source, Ti numChoices,
                         const std::vector<Matrix<Tv>> &costMatrixes,
                         unsigned int seed, Newton &newtonOptions,
                         RocOptions &aucOptions);
};

/// @brief A base class for a model set for discrete choice
class LDT_EXPORT DiscreteChoiceModelsetBase {
public:
  /// @brief Number of choices
  Ti mNumChoices = 0;

  /// @brief A pointer to the given \ref SearchItems
  /// @remark It is not const, because we should be able to set Cancel Request
  SearchItems *pItems = nullptr;

  /// @brief A pointer to the given data
  const Matrix<Tv> *pSource = nullptr;

  /// @brief A pointer to the given cost matrices
  std::vector<Matrix<Tv>> *pCostMatrixes = nullptr;

  /// @brief Inner model set
  ModelSet Modelset;

  /// @brief Pointer to the given groups
  std::vector<std::vector<Ti>> *pGroupIndexMap = nullptr;

  /// @brief List of searchers. This is passed to the \ref Modelset and it will
  /// be the owner
  std::vector<Searcher *> Searchers;

  DiscreteChoiceModelsetBase(){};

  virtual ~DiscreteChoiceModelsetBase(){};

  /// @brief Initializes the templated class from the types
  static DiscreteChoiceModelsetBase *
  GetFromTypes(bool isBinary, bool hasWeight, SearchOptions &searchOptions,
               SearchItems &searchItems, SearchMetricOptions &metrics,
               SearchModelChecks &checks, const std::vector<Ti> &sizes,
               const Matrix<Tv> &source, std::vector<Matrix<Tv>> &costMatrixes,
               std::vector<std::vector<Ti>> &groupIndexMaps, bool addLogit,
               bool addProbit, Newton &newtonOptions, RocOptions &aucOptions);

  /// @brief It checks inputs and calls 'ModelSet.Start(...)'
  /// @param work Work array with size given in the base class (real)
  /// @param workI Work array with size given in the base class (integer)
  void Start(Tv *work, Ti *workI);
};

/// @brief Templated model set for discrete choice, derived from \ref
/// DiscreteChoiceModelsetBase
/// @tparam hasWight If true, observations have weight
/// @tparam modelType Type of model
template <bool hasWight, DiscreteChoiceModelType modelType>
class LDT_EXPORT DiscreteChoiceModelset : public DiscreteChoiceModelsetBase {

public:
  /// @brief
  /// @param searchOptions It is passed to the base class
  /// @param searchItems It is passed to the base class
  /// @param metrics It is passed to the base class
  /// @param checks It is passed to the base class
  /// @param sizes Determines the number of exogenous data in different
  /// searchers
  /// @param source Source of data
  /// @param costMatrixes List of cost matrices
  /// @param groupIndexMaps A group for the exogenous data. It is passed to the
  /// base class
  /// @param newtonOptions Optimization options
  /// @param aucOptions Options for calculating AUC
  /// @param addLogit If true, logit models are included in the model set
  /// @param addProbit If true, probit models are included in the model set
  DiscreteChoiceModelset(SearchOptions &searchOptions, SearchItems &searchItems,
                         SearchMetricOptions &metrics,
                         SearchModelChecks &checks,
                         const std::vector<Ti> &sizes, const Matrix<Tv> &source,
                         std::vector<Matrix<Tv>> &costMatrixes,
                         std::vector<std::vector<Ti>> &groupIndexMaps,
                         Newton &newtonOptions, RocOptions &aucOptions,
                         bool addLogit = true, bool addProbit = false);

  virtual ~DiscreteChoiceModelset();
};

extern template class ldt::DiscreteChoiceModelset<
    true, DiscreteChoiceModelType::kBinary>;

extern template class ldt::DiscreteChoiceModelset<
    true, DiscreteChoiceModelType::kOrdered>;

extern template class ldt::DiscreteChoiceModelset<
    false, DiscreteChoiceModelType::kBinary>;

extern template class ldt::DiscreteChoiceModelset<
    false, DiscreteChoiceModelType::kOrdered>;

extern template class ldt::DiscreteChoiceSearcher<
    true, DiscreteChoiceModelType::kBinary, DiscreteChoiceDistType::kLogit>;
extern template class ldt::DiscreteChoiceSearcher<
    true, DiscreteChoiceModelType::kBinary, DiscreteChoiceDistType::kProbit>;

extern template class ldt::DiscreteChoiceSearcher<
    true, DiscreteChoiceModelType::kOrdered, DiscreteChoiceDistType::kLogit>;
extern template class ldt::DiscreteChoiceSearcher<
    true, DiscreteChoiceModelType::kOrdered, DiscreteChoiceDistType::kProbit>;

extern template class ldt::DiscreteChoiceSearcher<
    false, DiscreteChoiceModelType::kBinary, DiscreteChoiceDistType::kLogit>;
extern template class ldt::DiscreteChoiceSearcher<
    false, DiscreteChoiceModelType::kBinary, DiscreteChoiceDistType::kProbit>;

extern template class ldt::DiscreteChoiceSearcher<
    false, DiscreteChoiceModelType::kOrdered, DiscreteChoiceDistType::kLogit>;
extern template class ldt::DiscreteChoiceSearcher<
    false, DiscreteChoiceModelType::kOrdered, DiscreteChoiceDistType::kProbit>;

} // namespace ldt
