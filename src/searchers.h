#pragma once

#include "ldt_base.h"
#include "matrix.h"
#include "running.h"
#include "scoring.h"
#include <map>
#include <set>
#include <string>

namespace ldt {

/*

A general process starts by taking a 'SearchItems' from the user,
  Using it and creating a list of searchers.
  Using it and creating a 'ModelSet',
   populating the model-set with the searchers
   'Start' the ModelSet
*/

/// @brief Keeps estimation result
struct EstimationKeep {

  /// @brief Estimated mean (if any)
  Tv Mean = NAN;

  /// @brief Estimated variance
  Tv Variance = NAN;

  /// @brief Metric in the estimation
  Tv Metric = NAN;

  /// @brief Weight in the estimation
  Tv Weight = NAN;

  /// @brief Dependent indices. Class owns the inner array
  std::vector<Ti> Endogenous;

  /// @brief Exogenous indices. Class owns the inner array
  std::vector<Ti> Exogenouses;

  /// @brief Any extra information transformed into integers
  std::vector<Ti> Extra;

  /// @brief Initializes a new instance of this class
  /// @param metric Metric in this estimation
  /// @param weight Weight in this estimation
  /// @param exogenous Exogenous indices.
  /// @param extra Any extra information transformed into integers
  /// @param endogenous Dependent indices
  /// @param mean Estimated mean
  /// @param variance Estimated variance
  EstimationKeep(Tv metric, Tv weight, const std::vector<Ti> &exogenous,
                 const std::vector<Ti> &extra = std::vector<Ti>(),
                 const std::vector<Ti> &endogenous = std::vector<Ti>(),
                 Tv mean = NAN, Tv variance = NAN);
};

struct EstimationKeepComp {
  bool positiveOriented;
  EstimationKeepComp(bool posOri = true) : positiveOriented(posOri) {}
  bool operator()(const std::shared_ptr<EstimationKeep> &lhs,
                  const std::shared_ptr<EstimationKeep> &rhs) const {
    return positiveOriented ? lhs->Metric > rhs->Metric
                            : lhs->Metric < rhs->Metric;
  }
};

/// @brief Search options
struct LDT_EXPORT SearchData {

  /// @brief holding the data
  Matrix<Tv> Data;

  /// @brief holding the out of sample exogenous data
  Matrix<Tv> NewX;

  /// @brief number of endogenous variables
  Ti NumEndo;

  /// @brief number of exogenous variables
  Ti NumExo;

  /// @brief number of observations in the data
  Ti ObsCount;

  /// @brief number of new observations
  Ti NewObsCount;

  /// @brief Box-Cox transformation parameters
  std::vector<Tv> Lambdas;

  /// @brief indicating whether the data includes an intercept
  bool HasIntercept;

  /// @brief indicating whether the data includes weights
  bool HasWeight;
};

/// @brief parameters for 2-level nested loop
struct SearchCombinations {

  /// @brief the sizes.
  std::vector<Ti> Sizes;

  /// @brief the partitions.
  std::vector<std::vector<Ti>> Partitions;

  /// @brief number of fixed partitions.
  Ti NumFixPartitions;

  /// @brief number of fixed items in each partition
  Ti NumFixItems;

  /// @brief List of inner groups.
  std::vector<std::vector<Ti>> InnerGroups;

  /// @brief number of targets
  Ti NumTargets;
};

/// @brief Search options
struct LDT_EXPORT SearchOptions {

  /// @brief If true, search uses a parallel loop
  bool Parallel = false;

  /// @brief Set it to be true in order to signal stop
  bool RequestCancel = false;

  /// @brief determines the interval between reporting the result.
  /// To be used by R
  Ti ReportInterval;
};

/// @brief Search measuring options
struct LDT_EXPORT SearchMetricOptions {

  /// @brief List of in-sample metrics
  std::vector<GoodnessOfFitType> MetricsIn;

  /// @brief List of out-of-sample metrics
  std::vector<ScoringType> MetricsOut;

  /// @brief determines the orientation of the metrics
  std::vector<bool> EvalIsPosOrientation;

  /// @brief A fixed size for the training sample
  Ti TrainFixSize = 0;

  /// @brief If \ref TrainFixSize is zero, this ratio determines the size of the
  /// training sample
  Tv TrainRatio = 0;

  /// @brief A fixed size for the number of simulations in out-of-sample
  /// simulations
  Ti SimFixSize = 0;

  /// @brief In some cases such as VARMA, number of simulations can be a
  /// percentage of observations. Use zero to use 'SimFixSize'.
  // Tv SimRatio;

  /// @brief A searcher might have its own seed (e.g., as a linear function of
  /// this seed).
  /// @remark For development: Don't use unsigned Ti, in order to allow negative
  /// values for specific purposes
  Ti Seed;

  /// @brief Evaluation horizons for out-of-sample simulations of time-series
  /// data
  std::vector<Ti> Horizons;

  /// @brief Determines the type of evaluation for discrete models. If true,
  /// weights are used in AUC or FrequencyCost calculations
  bool WeightedEval = false;

  /// @brief Updates the indices, etc.
  /// @param isOutOfSampleRandom If true, the training sample is randomly
  /// generated
  /// @param isTimeSeries If true, the data is time-series such as VARMA
  void Update(bool isOutOfSampleRandom, bool isTimeSeries);

  /// @brief After \ref Update, determines the type of the measuring
  bool mIsTimeSeries = false, mIsOutOfSampleRandom = false;

  std::map<GoodnessOfFitType, Ti> MetricInIndices;

  std::map<ScoringType, Ti> MetricOutIndices;

  /// @brief minimum value for metrics to be used in AIC weight
  /// formula: exp(-0.5*(metric-minMetric)). Size of vectors must equal the
  /// number of target variables. Number of rows is equal to the number of
  /// metrics. Each column belongs to a target variable.
  std::map<GoodnessOfFitType, std::vector<Tv>> MinMetricIn;

  /// @brief Similar to \ref MinMetricIn
  std::map<ScoringType, std::vector<Tv>> MinMetricOut;
};

/// @brief Different options for checking models in the search
struct LDT_EXPORT SearchModelChecks {

  /// @brief If true, model must be estimated without error, given the whole
  /// data. If there is an in-sample metric or other options such as MinR2,
  /// this is checked automatically.
  bool Estimation = false;

  /// @brief Minimum value for the number of observations. Use zero to disable
  /// this check
  Ti MinObsCount = 0;

  /// @brief Minimum value for the degrees of freedom (equation-wise). Use zero
  /// to disable this check
  Ti MinDof = 0;

  /// @brief Minimum value for the number of valid out-of-sample simulations.
  /// Use zero to disable this check
  Ti MinOutSim = 0;

  /// @brief R2 of the model (if applicable) must be larger than this. Set -Inf
  /// to disable this check
  Tv MinR2 = -INFINITY;

  /// @brief AIC of the model must be smaller than this value. Set +Inf to
  /// disable this check
  Tv MaxAic = INFINITY;

  /// @brief SIC of the model must be smaller than this value. Set +Inf to
  /// disable this check
  Tv MaxSic = INFINITY;

  /// @brief Condition number of the estimation (e.g. in (X'X)^-1) must be
  /// smaller than this value. Set +Inf to disable this check
  Tv MaxConditionNumber = INFINITY;

  /// @brief Model must provide valid predictions given all sample. E.g., in
  /// case of VARMA, if there is exogenous data, out-of-sample exogenous data
  /// must not be missing.
  bool Prediction = false;

  /// @brief The predictions of the model must lie within a bound. The bound is
  /// the value multiplied by the average growth rate. Use zero to disable this
  /// check.
  Tv PredictionBoundMultiplier = 0;

  /// @brief Updates the options such as \ref Estimation or \ref Prediction
  /// based on other options
  /// @param metrics Current metric options
  void Update(const SearchMetricOptions &metrics);

  /// @brief After \ref Update, determines the type of checks
  bool mCheckCN = false, mCheckCN_all = false, mCheckPredBound = false;
};

/// @brief Items to be kept during a search
/// @remark 'Length...' fields are generally set internally based on data
struct LDT_EXPORT SearchItems {

  /// @brief Length of the first dimension which is the evaluation metrics.
  /// (might be overridden internally, given the data)
  Ti LengthEvals = 0;

  /// @brief Length of the second dimension which is the target variables.
  /// (might be overridden internally, given the data)
  Ti LengthTargets = 0;

  /// @brief If true, model evaluation data is kept
  bool KeepModelEvaluations = true;

  /// @brief If true, a vector of inclusion weights are calculated. The length
  /// of the vector is \ref LengthEndogenous + \ref LengthExogenous
  bool KeepInclusionWeights = false;

  /// @brief Length of type 1
  Ti Length1 = 0;

  /// @brief Length of type 2
  Ti Length2 = 0;

  /// @brief Number of the dependent variable (might be overridden internally,
  /// given the data)
  Ti LengthEndogenous = 0;

  /// @brief Number of the exogenous variables (might be overridden internally,
  /// given the data)
  Ti LengthExogenous = 0;

  /// @brief If larger than zero, it keeps the data regarding the first K count
  /// models
  Ti KeepBestCount = 0;

  /// @brief If true, it keeps all data of all models (might raise memory issues
  /// in large projects)
  bool KeepAll = false;

  /// @brief If true, it keeps data regarding of mixture distribution
  bool KeepMixture = false;

  /// @brief If not empty, it keeps the CDF at the given values
  std::vector<Tv> CdfsAt;

  /// @brief If larger than 0, extreme bounds are calculated and saved given
  /// this multiplier
  Tv ExtremeBoundsMultiplier = 0;

  /// @brief Update the fields given the current state of the project
  /// @param metrics Current metric options
  /// @param targetCount Number of target variables
  /// @param DepenCount Number of dependent variables
  /// @param exoCount Number of exogenous variables
  void Update(const SearchMetricOptions metrics, Ti targetCount, Ti DepenCount,
              Ti exoCount);
};

/// @brief Keeps information about the current state of the project
struct LDT_EXPORT SearcherModelingInfo {

  /// @brief Initializes a new instance of the class
  SearcherModelingInfo() { FailsCount = std::map<std::string, Ti>(); };

  /// @brief Expected number of members
  Ti ExpectedCount = 0;

  /// @brief Number of estimated members
  Ti SearchedCount = 0;

  /// @brief Failure messages and their counts
  std::map<std::string, Ti> FailsCount;
};

/// @brief A searcher summary.
/// @remark Each summary belongs to a cell in a cube. First dimension is the
/// evaluation method, second is the target, third is e.g., forecast horizon
class LDT_EXPORT SearcherSummary {

public:
  /// @brief Index to the first dimension which is the evaluation method
  Ti Index1 = 0;

  /// @brief Index to the second dimension which is the target index
  Ti Index2 = 0;

  /// @brief Index to the third dimension which is (e.g.) the forecast horizon,
  /// index of a coefficient, etc.
  Ti Index3 = 0;

  /// @brief If requested in the options, it contains the information about the
  /// first K best models
  std::multiset<std::shared_ptr<EstimationKeep>, EstimationKeepComp> Bests;

  /// @brief If requested in the options, it contains the information about all
  /// models
  std::vector<std::shared_ptr<EstimationKeep>> All;

  /// @brief If requested in the options, it is the CDFs at the given points
  std::vector<RunningMoments<1, true, true, Tv>> Cdfs;

  /// @brief If requested in the options, it is the first 4 moments of the
  /// Mixture4 distribution
  RunningMoments<4, true, true, Tv> Mixture4;

  /// @brief If requested in the options, it is an array of size 2 [min,max],
  /// indicating the extreme bounds
  std::vector<Tv> ExtremeBounds;

  /// @brief If requested in the options, it has the information about the
  /// inclusion weights of dependent and exogenous variables
  std::vector<RunningMoments<1, true, false, Tv>> InclusionsInfo;

  /// @brief Pointer to the given \ref SearchItems in the constructor
  const SearchItems *pItems = nullptr;

  const SearchData *pData = nullptr;

  /// @brief Initializes a new instance of this class
  SearcherSummary(){};

  /// @brief Initializes a new instance of this class
  /// @param Index1 Index to the first dimension which is the evaluation method
  /// @param Index2 Index to the second dimension which is the target index
  /// @param Index3 Index to the third dimension which is (e.g.) the forecast
  /// horizon, index of a coefficient, etc.
  /// @param options \ref SearchItems in the current state of the project.
  /// @param isPositiveOriented determines the comparison in \ref Bests
  SearcherSummary(Ti Index1, Ti Index2, Ti Index3, const SearchItems *options,
                  bool isPositiveOriented, const SearchData *data);

  /// @brief Inserts a new estimation to the storage
  /// @param estimation Estimation information
  /// @param isModel If true, it is a push for model information. Otherwise, it
  /// is a estimated coefficient, or a forecast, etc.
  void Push(std::shared_ptr<EstimationKeep> &estimation, bool isModel);
};

/// @brief A searcher class with 3 types of summaries: Model, Type1, Type2.
class LDT_EXPORT Searcher {
private:
  bool mIsFinished = false;

  void AddState(std::string state);

  bool MoveNext(Ti &c, Ti &T, Ti &free);

  void UpdateCurrent();

  /// @brief Indices of selected partitions with respect to the partitions in
  /// \ref pCombinations
  VMatrix<Ti> PartitionIndices;

  /// @brief Indices of items in the selected partitions given by \ref
  /// PartitionIndices
  VMatrix<Ti> ItemIndices;

  /// @brief A storage for the size of different partitions
  std::vector<Ti> partitionSizes;

protected:
  /// @brief An integer for adjusting the value of \ref CurrentIndices
  bool CheckForEmpty;

  /// @brief Number of selected partitions
  Ti NumPartitions = 0;

  /// @brief The main indices to be used in the \ref EstimateOne function.
  /// Use \ref indicesOffset in the constructor to adjust the positions, e.g.,
  /// for exogenous columns indices. Its length is \ref NumPartitions.
  /// It has zero-based indexation which means in an unrestricted framework,
  /// it starts from 0,1,2,... Restrictions are imposed by partitioning or
  /// fixating partitions or items.
  VMatrix<Ti> CurrentIndices;

  /// @brief Override this method, use \ref CurrentIndices, do what ever is
  /// required, use \ref Push0, \ref Push1, and/or \ref Push2 to save the
  /// result. Return empty string if everything is OK. Return a specific error
  /// string if something is wrong
  /// @param work Work array (Tv) of size \ref WorkSize
  /// @param workI Work array (Ti) of size \ref WorkSizeI
  /// @return Empty or an error message
  virtual std::string EstimateOne(Tv *work, Ti *workI) = 0;

  /// @brief Pushes model information to \ref Summaries0
  /// @param estimation The information
  /// @param evalIndex Index of the evaluation metric
  /// @param targetIndex Index of the target variable
  /// @param overrideIncExo If not null, it overrides the exogenous inclusion
  /// indices
  void Push0(std::shared_ptr<EstimationKeep> &estimation, Ti evalIndex,
             Ti targetIndex);

  /// @brief Pushes information to \ref Summaries1
  /// @param estimation The information
  /// @param evalIndex Index of the evaluation metric
  /// @param targetIndex Index of the target variable
  /// @param thirdIndex Index of the third data (e.g., horizon or index of the
  /// coefficient)
  void Push1(std::shared_ptr<EstimationKeep> &estimation, Ti evalIndex,
             Ti targetIndex, Ti thirdIndex);

  /// @brief Similar to \ref Push1 but for Summaries2
  /// @param estimation
  /// @param evalIndex
  /// @param targetIndex
  /// @param thirdIndex
  void Push2(std::shared_ptr<EstimationKeep> &estimation, Ti evalIndex,
             Ti targetIndex, Ti thirdIndex);

public:
  /// @brief A pointer to the provided \ref SearchData
  const SearchData *pData = nullptr;

  /// @brief A pointer to the provided \ref SearchCombinations
  const SearchCombinations *pCombinations = nullptr;

  /// @brief A pointer to the provided \ref SearchItems
  const SearchItems *pItems = nullptr;

  /// @brief A pointer to the provided \ref SearchOptions. Not 'const' so that
  /// we can set 'RequestCancel' in the searcher
  SearchOptions *pOptions; // don't use 'const'

  /// @brief A pointer to the provided \ref SearchModelChecks
  const SearchModelChecks *pChecks = nullptr;

  /// @brief A pointer to the provided \ref SearchMetricOptions
  const SearchMetricOptions *pMetrics = nullptr;

  /// @brief Current number of estimated members
  Ti Counter = 0;

  /// @brief Current errors
  std::map<std::string, Ti> States;

  /// @brief A list for keeping model information and inclusion weights
  std::vector<std::vector<SearcherSummary>> Summaries0;

  /// @brief A list for keeping first type of information (see options.Length1)
  std::vector<std::vector<std::vector<SearcherSummary>>> Summaries1;

  /// @brief A list for keeping first type of information (see options.Length2)
  std::vector<std::vector<std::vector<SearcherSummary>>> Summaries2;

  /// @brief A unique id for the current search
  Ti mId = 0;

  /// @brief Required size of the worker (Tv)
  Ti WorkSize = 0;

  /// @brief Required size of the worker (Ti)
  Ti WorkSizeI = 0;

  /// @brief Initializes a new instance of the class
  Searcher(){};

  /// @brief Initializes a new instance of the class
  /// @brief data Search data in the project
  /// @brief combinations Combinations in the project
  /// @param options Search options in the project
  /// @param items Search items in the project
  /// @param metrics Metrics in the project
  /// @param checks Model checks in the project
  /// @param numPartitions The size of an integer array that can define this
  /// subset of models. E.g. if it is 3, then you should build models with
  ///      Group=[1,2,3], [1,2,4], [1,2,5], ..., [1,3,4], [1,3,5], ...,
  ///      [2,3,4], [2,3,5], ...
  /// contains these indices
  /// @param checkForEmpty Use true if inner indices are exogenous and you might
  /// estimate a model with no target due to an update in current indices
  Searcher(const SearchData &data, const SearchCombinations &combinations,
           SearchOptions &options, const SearchItems &items,
           const SearchMetricOptions &metrics, const SearchModelChecks &checks,
           Ti numPartitions, bool checkForEmpty);

  /// @brief
  /// @remark in order to avoid the following warning: Deleting object of
  /// abstract class type ‘ldt::Searcher’ which has non-virtual destructor will
  /// cause undefined behavior
  virtual ~Searcher() {}

  /// @brief Call this function before using 'Start' in an async function
  void CheckStart();

  /// @brief Starts the search
  /// @param work Work array (Tv) of size \ref WorkSize
  /// @param workI Work array (Ti) of size \ref WorkSizeI
  /// @return
  std::string Start(Tv *work, Ti *workI);

  /// @brief Calculates the size of this searcher
  /// @param effective If true, it tries to consider the size of the models and
  /// generates a weighted count
  /// @return Size of this searcher
  Ti GetCount(bool effective = false) const;
};

/// @brief A helper class for testing
class LDT_EXPORT SearcherTest : public Searcher {

  using Searcher::Searcher;

  std::string EstimateOne(Tv *work, Ti *workI) override;
};

/// @brief A searcher class for multivariate regression analysis, with
/// endogenous and exogenous indices
class LDT_EXPORT SearcherReg : public Searcher {

  Ti mExtraLength;

protected:
  /// @brief 'isInnerExogenous' argument passed by the constructor. It
  /// determines what is \ref InnerIndices refers to.
  bool IsInnerExogenous;

  /// @brief Combined endogenous and exogenous indices (It includes weight
  /// column too). Use it to remove NAN (extract complete observations)
  std::vector<Ti> ColIndices;

  /// @brief The indices of endogenous or exogenous columns, depending on the
  /// 'isInnerExogenous' argument of the constructor. Note that the other set of
  /// indices is defined by \ref CurrentIndices
  std::vector<Ti> InnerIndices;

  /// @brief A mapping between the index of dependent variable and the targets
  std::vector<Ti> TargetsPositions;

  std::string EstimateOne(Tv *work, Ti *workI) override;

  /// @brief Override this function instead of \ref EstimateOne. It manages
  /// endogenous and exogenous indices.
  /// @param work Similar to \ref EstimateOne
  /// @param workI Similar to \ref EstimateOne
  /// @param metrics A matrix of size ('number of measures' x 'number of
  /// targets') to be updated by after estimation of forecast.
  /// @param type1Mean A matrix to be updated after estimation or forecast. Its
  /// size is ('number of items in length 1' x 'number of forecasts').
  /// @param type1Var Similar to \par type1Mean but for variances.
  /// @param extra A matrix of length \ref mExtraLength to be updated by extra
  /// information.
  /// @return Similar to \ref EstimateOne
  virtual std::string EstimateOneReg(Tv *work, Ti *workI, VMatrix<Tv> &metrics,
                                     VMatrix<Tv> &type1Mean,
                                     VMatrix<Tv> &type1Var,
                                     VMatrix<Ti> &extra) = 0;

public:
  /// @brief Initialize an instance
  /// @param data
  /// @param combinations
  /// @param options
  /// @param items
  /// @param metrics
  /// @param checks
  /// @param numPartitions
  /// @param isInnerExogenous
  /// @param innerIndices
  /// @param extraLength Length of extra information to be kept
  SearcherReg(const SearchData &data, const SearchCombinations &combinations,
              SearchOptions &options, const SearchItems &items,
              const SearchMetricOptions &metrics,
              const SearchModelChecks &checks, const Ti &numPartitions,
              const bool &isInnerExogenous, const std::vector<Ti> &innerIndices,
              const Ti extraLength);
};

/// @brief A set of \ref Searcher
class LDT_EXPORT ModelSet {

public:
  const SearchData *pData = nullptr;

  const SearchCombinations *pCombinations = nullptr;

  /// @brief A pointer to the provided \ref SearchItems
  const SearchItems *pItems = nullptr;

  /// @brief A pointer to the provided \ref SearchOptions
  const SearchOptions *pOptions; // don't use 'const' so that we can set
                                 // 'RequestCancel' in the searcher

  /// @brief A pointer to the provided \ref SearchModelChecks
  const SearchModelChecks *pChecks = nullptr;

  /// @brief A pointer to the provided \ref SearchMetricOptions
  const SearchMetricOptions *pMetrics = nullptr;

  /// @brief A pointer to the given list in the constructor. This
  /// class becomes the owner and deletes them
  /// @remark Don't use 'const' for shuffle
  std::vector<Searcher *> *pSearchers = nullptr;

  /// @brief If true, if shuffles the items in \ref pSearchers randomly for a
  /// better estimation of remaining time
  bool ShuffleSearchers = true;

  /// @brief Required size of the work array (Tv)
  Ti WorkSize = 0;

  /// @brief Required size of the work array (Ti)
  Ti WorkSizeI = 0;

  /// @brief Initializes a new instance of this class
  ModelSet(){};

  /// @brief Initializes a new instance of this class
  /// @param searchers List of searchers
  /// @param options Search options in the project
  /// @param items Search items in the project
  /// @param metrics Metrics in the project
  /// @param checks Model checks in the project
  ModelSet(std::vector<Searcher *> &searchers, const SearchData &data,
           const SearchCombinations &combinations, const SearchOptions &options,
           const SearchItems &items, const SearchMetricOptions &metrics,
           const SearchModelChecks &checks);

  /// @brief Starts the search in all the searchers
  /// @param work Work array of size \ref WorkSize
  /// @param workI Work array of size \ref WorkSizeI
  void Start(Tv *work, Ti *workI);

  /// @brief Gets the expected number of models
  /// @return expected number of models
  Ti GetExpectedNumberOfModels() const;

  /// @brief Gets the number of the estimated of models
  /// @return Number of the estimated of models
  Ti GetNumberOfEstimatedModels() const;

  /// @brief Combines information in all the searchers
  /// @param result On exit, contains combined information
  /// @param list0 On input, an empty list. On exit, it is filled with All
  /// the searchers of \ref Summaries0
  /// @param list1 On input, an empty list. On exit, it is filled with All
  /// the searchers of \ref Summaries1
  /// @param list2 On input, an empty list. On exit, it is filled with All
  /// the searchers of \ref Summaries2
  void CombineInfo(SearcherModelingInfo &result,
                   std::vector<SearcherSummary *> &list0,
                   std::vector<SearcherSummary *> &list1,
                   std::vector<SearcherSummary *> &list2) const;

  /// @brief Combines all estimations for a specific item
  /// @param index1 Metric index of the item
  /// @param index2 Target index of the item
  /// @param index3 Third index of the item
  /// @param summaries The related list filled in \ref CombineInfo
  /// @param result A place to save the result
  void CombineAll(const Ti &index1, const Ti &index2, const Ti &index3,
                  const std::vector<SearcherSummary *> &summaries,
                  std::vector<std::shared_ptr<EstimationKeep>> &result) const;

  /// @brief Combines best estimations for a specific item
  /// @param index1 Metric index of the item
  /// @param index2 Target index of the item
  /// @param index3 Third index of the item
  /// @param summaries The related list filled in \ref CombineInfo
  /// @param result A place to save the result
  void CombineBests(const Ti &index1, const Ti &index2, const Ti &index3,
                    const std::vector<SearcherSummary *> &summaries,
                    std::vector<std::shared_ptr<EstimationKeep>> &result) const;

  /// @brief Combines inclusion weights for a specific item
  /// @param index1 Metric index of the item
  /// @param index2 Target index of the item
  /// @param index3 Third index of the item
  /// @param summaries The related list filled in \ref CombineInfo
  /// @param result A place to save the result
  void
  CombineInclusionWeights(const Ti &index1, const Ti &index2, const Ti &index3,
                          const std::vector<SearcherSummary *> &summaries,
                          RunningMoments<1, true, false, Tv> &result) const;

  /// @brief Combines CDF at a specific point for a specific item
  /// @param index1 Metric index of the item
  /// @param index2 Target index of the item
  /// @param index3 Third index of the item
  /// @param cdfIndex the specific point for CDF
  /// @param summaries The related list filled in \ref CombineInfo
  /// @param result A place to save the result
  void CombineCdfAt(const Ti &index1, const Ti &index2, const Ti &index3,
                    const Ti &cdfIndex,
                    const std::vector<SearcherSummary *> &summaries,
                    RunningMoments<1, true, true, Tv> &result) const;

  /// @brief Combines extreme bounds for a specific item
  /// @param index1 Metric index of the item
  /// @param index2 Target index of the item
  /// @param index3 Third index of the item
  /// @param summaries The related list filled in \ref CombineInfo
  /// @param result A place to save the result
  void CombineExtremeBounds(const Ti &index1, const Ti &index2,
                            const Ti &index3,
                            const std::vector<SearcherSummary *> &summaries,
                            Tv &min, Tv &max) const;

  /// @brief Combines mixture data for a specific item
  /// @param index1 Metric index of the item
  /// @param index2 Target index of the item
  /// @param index3 Third index of the item
  /// @param summaries The related list filled in \ref CombineInfo
  /// @param result A place to save the result
  void CombineMixture(const Ti &index1, const Ti &index2, const Ti &index3,
                      const std::vector<SearcherSummary *> &summaries,
                      RunningMoments<4, true, true, Tv> &result) const;
};

} // namespace ldt
