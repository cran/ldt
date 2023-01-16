#pragma once

#include "ldt_base.h"

#include "helpers.h"
#include "optimization.h"
#include <cmath>
#include <random>

namespace ldt {

enum class DistributionType {

  /// @brief None
  kNone = 0,

  /// @brief Normal distribution. params: mean, standard deviation
  kNormal = 'n',

  /// @brief Log-normal distribution. params: mu, sigma
  kLogNormal = 'l',

  /// @brief Continuous uniform distribution. params: lower, upper
  kUniformCon = 'u',

  /// @brief Chi-squared distribution. params: degrees of freedom >0
  kChi2 = 'c',

  /// @brief Student's T distribution. params: degrees of freedom > 0
  kT = 't',

  /// @brief Exponential distribution. params: rate >= 0
  kExponential = 'e',

  /// @brief Fisher distribution. params: freedom1>0, freedom2>0
  kF = 'f',

  /// @brief Gamma distribution. params: shape>0, scale>0
  kGamma = 'g',

  /// @brief Beta distribution. params: shape1>0, shape2>0
  kBeta = 'b',

  /// @brief Generalized Lambda distribution, FKML (Freimer, Mudholkar, Kollia,
  /// Lin)
  /// parametrization params: location, scale, shape1, shape2 using: Au-Yeung,
  /// S. W. M. (2003) Finding Probability Distributions From Moments, Master
  /// thesis and van Staden, P. J. (2013) Modelling ... PHD thesis
  kGldFkml = 'k',

  /// @brief Discrete uniform distribution. params: lower, upper
  kUniformDis = 'o',

  /// @brief Bernoulli distribution. params: p\in[0,1]  success probability
  kBernoulli = 'i',

  /// @brief Poisson distribution. params: rate (or mean)>0
  kPoisson = 's',

  /// @brief Geometric distribution. params: p\in[0,1]  success probability (x
  /// is failures)
  kGeometric = 'r',

  /// @brief Binomial distribution. params: p\in[0,1]  (success probability),
  /// n\in[0, ) number of trials
  kBinomial = 'a',
};

/// @brief Types of distribution properties
enum class DistributionProperty {

  /// @brief Mean
  kMean = 'm',

  /// @brief Variance
  kVariance = 'v',

  /// @brief Standard error
  kStandardError = 's',

  /// @brief Skewness
  kSkewness = 'w',

  /// @brief Kurtosis
  kKurtosis = 'k',

  /// @brief Minimum
  kMinimum = 'n',

  /// @brief Maximum
  kMaximum = 'x',

  /// @brief Median
  kMedian = 'a',

  /// @brief Mode
  kMode = 'o'
};

/// @brief A base class for distribution
class LDT_EXPORT DistributionBase {

public:
  /// @brief Initializes the base class
  DistributionBase(){};
  virtual ~DistributionBase(){};

  /// @brief A factory to create a distribution given a type
  /// @param type The type
  /// @param d1 First parameter
  /// @param d2 Second parameter
  /// @param d3 Third parameter
  /// @param d4 Fourth parameter
  /// @return The distribution class
  static std::unique_ptr<DistributionBase>
  GetDistributionFromType(DistributionType type, Tv d1, Tv d2, Tv d3, Tv d4);

  /// @brief Gets first parameter
  /// @return First parameter
  virtual Tv GetParam1() = 0;

  /// @brief Gets second parameter
  /// @return Second parameter
  virtual Tv GetParam2() = 0;

  /// @brief Gets third parameter
  /// @return Third parameter
  virtual Tv GetParam3() = 0;

  /// @brief Gets fourth parameter
  /// @return Fourth parameter
  virtual Tv GetParam4() = 0;

  /// @brief Gets the value of a property
  /// @param propType The property
  /// @return Value of the property
  Tv GetProperty(DistributionProperty propType);

  virtual Tv GetMinimum() = 0;

  virtual Tv GetMaximum() = 0;

  virtual Tv GetMean() = 0;

  virtual Tv GetVariance() = 0;

  virtual Tv GetSandardDeviation() = 0;

  virtual Tv GetSkewness() = 0;

  virtual Tv GetKurtosis() = 0;

  virtual Tv GetMedian() = 0;

  virtual Tv GetMode() = 0;

  virtual Ti GetPmfSupportIncrement() = 0;

  virtual Ti GetPmfSupportSize(Tv min, Tv max) = 0;

  virtual void GetPmfSupport(Tv *x, Tv *Value, bool log, Ti length,
                             bool for_continuous_plot = false,
                             Tv min = NAN) = 0;

  virtual Tv GetPdfOrPmf(Tv x) = 0;
  virtual Tv GetPdfOrPmfLog(Tv x) = 0;
  virtual Tv GetCdf(Tv x) = 0;
  virtual Tv GetQuantile(Tv p) = 0;

  virtual void GetSample(Tv *storage, Ti length, unsigned int seed = 0) = 0;

  virtual Tv GetSample1(std::mt19937 &eng) = 0;

  virtual Tv GetDensityQuantile(Tv p) = 0;

  virtual Tv IsQuantile() = 0;

  virtual bool IsDiscrete() = 0;
};

/// @brief A univariate distribution
/// @tparam type Type of the distribution
template <DistributionType type = DistributionType::kNormal>
class LDT_EXPORT Distribution : public DistributionBase {
private:
  Tv mParam1, mParam2 = 0, mParam3 = 0, mParam4 = 0;

public:
  /// @brief Initializes a new instance of this class
  /// @param param1 First parameter
  /// @param param2 Second parameter (if any)
  /// @param param3 Third parameter (if any)
  /// @param param4 Fourth parameter (if any)
  Distribution(Tv param1 = 0, Tv param2 = 1, Tv param3 = 0, Tv param4 = 0);

  ~Distribution() override = default;

  /// @brief Gets first parameter
  /// @return First parameter
  virtual Tv GetParam1() override { return mParam1; }

  /// @brief Gets second parameter
  /// @return Second parameter
  virtual Tv GetParam2() override { return mParam2; }

  /// @brief Gets third parameter
  /// @return Third parameter
  virtual Tv GetParam3() override { return mParam3; }

  /// @brief Gets fourth parameter
  /// @return Fourth parameter
  virtual Tv GetParam4() override { return mParam4; }

  /// @brief Gets the minimum
  /// @return Minimum
  virtual Tv GetMinimum() override;

  /// @brief Gets the maximum
  /// @return Maximum
  virtual Tv GetMaximum() override;

  /// @brief Gets the mean
  /// @return Mean
  virtual Tv GetMean() override;

  /// @brief Gets the variance
  /// @return Variance
  virtual Tv GetVariance() override;

  /// @brief Gets the standard deviation
  /// @return Standard deviation
  virtual Tv GetSandardDeviation() override;

  /// @brief Gets the skewness
  /// @return Skewness
  virtual Tv GetSkewness() override;

  /// @brief Gets the kurtosis
  /// @return Kurtosis
  virtual Tv GetKurtosis() override;

  /// @brief Gets the median
  /// @return Median
  virtual Tv GetMedian() override;

  /// @brief Gets the mode
  /// @return Mode
  virtual Tv GetMode() override;

  virtual Ti GetPmfSupportIncrement() override;

  /// @brief Support size when you are dealing with discrete random variables
  /// and PMF function Note that the Distance between min and max will be cast
  /// to 'Ti'. e.g. min cannot be very small or max cannot be very large
  /// @param min Use it to override the minimum of support, esp. when they
  /// are unbounded. It cannot be NAN
  /// @param max Similar to \p min
  /// @return
  virtual Ti GetPmfSupportSize(Tv min, Tv max) override;

  /// @brief calculates x and PMF values for non-zero values (supports). Note
  /// that for some distributions this is the set of natural numbers (e.g.)
  /// there for uncountable. use 'pmf_support_size' and it it returns !=0, use
  /// this function.
  /// @param x
  /// @param Value Value of the function
  /// @param log
  /// @param length take it from 'pdf_pmf_support_size'
  /// @param for_continuous_plot it true, multiply length by 3
  /// @param min use it to override the minimum of support
  virtual void GetPmfSupport(Tv *x, Tv *Value, bool log, Ti length,
                             bool for_continuous_plot = false,
                             Tv min = NAN) override;

  /// @brief Gets PDF or PMF at a point
  /// @param x The point
  /// @return PDF or PMF
  virtual Tv GetPdfOrPmf(Tv x) override;

  /// @brief Gets logarithm of PDF or PMF at a point
  /// @param x The point
  /// @return Logarithm of PDF or PMF
  virtual Tv GetPdfOrPmfLog(Tv x) override;

  /// @brief Gets CDF at a point
  /// @param x The point
  /// @return CDF
  virtual Tv GetCdf(Tv x) override;

  /// @brief Gets the quantile at a point
  /// @param x The point
  /// @return Quantile
  virtual Tv GetQuantile(Tv p) override;

  /// @brief Gets sample from distribution
  /// @param storage A place for storing the result
  /// @param length Length of the storage array
  /// @param seed A seed for random generator
  virtual void GetSample(Tv *storage, Ti length,
                         unsigned int seed = 0) override;

  /// @brief Gets a sample
  /// @param eng Random generator
  /// @return A sample
  virtual Tv GetSample1(std::mt19937 &eng) override;

  /// @brief Gets the density quantile at a point
  /// @param p The point
  /// @return Density quantile
  virtual Tv GetDensityQuantile(Tv p) override;

  /// @brief Determines the type of this distribution
  /// @return true if it is a quantile distribution
  virtual Tv IsQuantile() override;

  /// @brief Determines the type of this distribution
  /// @return true if it is a discrete distribution
  virtual bool IsDiscrete() override;
};

/// @brief Generalized Lambda Distribution
class LDT_EXPORT DistributionGld {
  Tv mParam1 = 0, mParam2 = 0, mParam3 = 0, mParam4 = 0;

public:
  /// @brief Initializes a new instance of this class
  /// @param d1 First parameter
  /// @param d2 Second parameter
  /// @param d3 Third parameter
  /// @param d4 Fourth parameter
  DistributionGld(Tv d1, Tv d2, Tv d3, Tv d4);

  /// @brief Gets quantile at a specific point
  /// @param p The point
  /// @param d1 First parameter
  /// @param d2 Second parameter
  /// @param d3 Third parameter
  /// @param d4 Fourth parameter
  /// @return quantile
  static Tv GetQuantile(Tv p, Tv d1, Tv d2, Tv d3, Tv d4);

  /// @brief gets density quantile at a specific point
  /// @param p The point
  /// @param d1 First parameter
  /// @param d2 Second parameter
  /// @param d3 Third parameter
  /// @param d4 Fourth parameter
  /// @return density quantile
  static Tv GetDensityQuantile(Tv p, Tv d1, Tv d2, Tv d3, Tv d4);

  /// @brief Gets M_k
  /// @param k k
  /// @param L3 Third parameter
  /// @param L4 Fourth parameter
  /// @return M_k
  static Tv GetMk(int k, Tv L3, Tv L4);

  /// @brief Gets Gld region
  /// @param L3 Third parameter
  /// @param L4 Fourth parameter
  /// @return Region
  static int GetGldFklmRegion(Tv L3, Tv L4);

  /// @brief Get all M_k(s)
  /// @param L3 Third parameter
  /// @param L4 Fourth parameter
  /// @param M1 M_1
  /// @param M2 M_2
  /// @param M3 M_3
  /// @param M4 M_4
  static void GetMs(Tv L3, Tv L4, Tv &M1, Tv &M2, Tv &M3, Tv &M4);

  static std::tuple<Tv, Tv, Tv, Tv>
  GetFromMoments(Tv mean, Tv variance, Tv skewness, Tv ex_kurtosis, int type,
                 NelderMead &optim, Tv startL3 = 0.0, Tv startL4 = 0.0);
};

/// @brief Types of mixture distribution
enum class DistributionMixtureType {
  /// @brief It contains just continuous distributions
  kContinuous,

  /// @brief It contains just discrete distributions
  kDiscrete,

  /// @brief It contains both continuous and discrete distributions
  kBoth
};

/// @brief A mixture of distributions
/// todo: this class needs a restructure (e.g., separate storage sizes, or split
/// to helper classes)
class LDT_EXPORT DistributionMixture {

public:
  std::vector<Tv> *pWeights = nullptr;
  std::vector<DistributionBase *> *pDistributions = nullptr;
  DistributionMixtureType pType = DistributionMixtureType::kBoth;

  DistributionMixture(std::vector<Tv> &weights,
                      std::vector<DistributionBase *> &dists);

  Tv GetMinimum();
  Tv GetMaximum();

  void GetMoments(Tv &mean, Tv &variance);

  void GetMoments(Tv &mean, Tv &variance, Tv &skewness, Tv &kurtosis);

  /// @brief moments when all distributions are normal
  /// @param mean
  /// @param variance
  /// @param skewness
  /// @param kurtosis
  void GetMomentsNormal(Tv &mean, Tv &variance, Tv &skewness, Tv &kurtosis);

  /// @brief It is available for all possible combinations of variables
  /// @param x
  /// @return
  Tv GetCdf(Tv x);

  Ti GetPmfSupportSize(Tv &min, Tv &max);

  void GetPmfSupport(Tv *x, Tv *Value, bool log, Ti length,
                     bool for_continuous_plot = false, Tv min = NAN,
                     Tv max = NAN);

  /// @brief The distributions must be whether discrete or continuous.
  /// They must be wether integrable (has pdf) or discrete (pmf != 0)
  /// @param x
  /// @return
  Tv GetPdfOrPmf(Tv x);

  Tv GetPdfOrPmfLog(Tv x);

  void GetSample(Tv *storage, Ti length, unsigned int seed);
};

/// @brief A histogram
/// todo: this class needs a restructure (e.g., separate storage sizes, or split
/// to helper classes)
class LDT_EXPORT Histogram {
private:
public:
  /// @brief Initializes a new instance of the class
  Histogram(){};

  /// @brief
  /// @param forsize true: sorts data, if bincount is negative or max,min are
  /// NAN, it computes them. finally it returns the required size for axis. the
  /// size is equal to bincount+1. required size for count is axis.size + 1 e.g.
  /// 2 means
  ///        axis:  -1,1
  ///        count: (-inf,-1),[-1,1],[1,+inf)
  ///        note that the last one is [,] others are [,)
  /// @param data
  /// @param storageAxis length: n (which is bincount+1)
  /// @param storageCount length: n + 1 (n is size of storageAxis, which
  /// means it is bincount+2)
  /// @param bincount
  /// @param min
  /// @param max
  /// @param iqrMultiply
  /// @param checkNAN
  /// @param step if not NAN, 'bincount' is calculated from it. Otherwise, it
  /// overrides calculation of bincount
  /// @return
  static Ti Compute(bool forsize, Matrix<Tv> *data, Matrix<Tv> *storageAxis,
                    Matrix<Ti> *storageCount, Ti &bincount, double &min,
                    double &max, double iqrMultiply, bool checkNAN,
                    double step = NAN);

  /// @brief compute given a 'variable' axis
  /// @param data
  /// @param storageAxis length: n
  /// @param storageCount length: n + 1
  /// @param checkNAN
  /// @return
  static Ti ComputeV(Matrix<Tv> *data, Matrix<Tv> *storageAxis,
                     Matrix<Ti> *storageCount, bool checkNAN);
};

/// @brief A multivariate normal distribution
/// todo: this class needs a restructure (e.g., separate storage sizes, or split
/// to helper classes)
class LDT_EXPORT NormalM {
private:
  Ti pM = 0;
  bool pIsZeroVariance = false;

  bool pIsConstantDiagVariance = false;
  Tv pConstantVariance = 0;

  bool pSampleInRows = true;

  bool pDeleteMean = false;
  bool pDeleteVariance = false;

public:
  /// @brief Initializes a new instance of this class
  /// @param m Dimension of this function
  /// @param mean Mean of the distribution
  /// @param variance Variance of the distribution
  /// @param sampling_length
  /// @param samples_in_rows
  /// @param mean_is_const
  /// @param mean_const
  /// @param variance_is_const
  /// @param variance_const
  /// @param covariance_is_const
  /// @param covariance_const
  NormalM(Ti m, Matrix<Tv> *mean = {}, Matrix<Tv> *variance = {},
          Ti sampling_length = 0, bool samples_in_rows = true,
          bool mean_is_const = false, Tv mean_const = 0,
          bool variance_is_const = false, Tv variance_const = 1,
          bool covariance_is_const = false, Tv covariance_const = 0);

  ~NormalM();

  Ti StorageSize = 0;
  Ti WorkSize = 0;

  Matrix<Tv> *pMean = nullptr;
  Matrix<Tv> *pVariance = nullptr;
  Matrix<Tv> *pSample = nullptr;

  /// @brief Generates samples from this distribution using Cholesky
  /// decomposition
  /// @param storage Storage size
  /// @param WORK An array of size: 2m+m^2 where m
  /// is length of X
  /// @param seed A seed for random generator
  void GetSample(Tv *storage, Tv *WORK, unsigned int seed) const;

  /// @brief Calculates the density of normal distribution with m variables
  /// @param x An m x n Matrix where n is the number of observations
  /// @param storage Length = n
  /// @param WORK An array of size: n+m^2 where m is length of X and n is
  /// rows of storage
  /// @param log If true, logarithm of density is returned
  /// @return 0 for success. Otherwise, error
  Ti GetDensity(Matrix<Tv> *x, Matrix<Tv> *storage, Tv *WORK,
                bool log = true) const;

  /// todo: fix return value for GetDensity and throw exception
};

extern template class ldt::Distribution<DistributionType::kBeta>;
extern template class ldt::Distribution<DistributionType::kChi2>;
extern template class ldt::Distribution<DistributionType::kExponential>;
extern template class ldt::Distribution<DistributionType::kF>;
extern template class ldt::Distribution<DistributionType::kGamma>;
extern template class ldt::Distribution<DistributionType::kGldFkml>;
extern template class ldt::Distribution<DistributionType::kLogNormal>;
extern template class ldt::Distribution<DistributionType::kNormal>;
extern template class ldt::Distribution<DistributionType::kT>;
extern template class ldt::Distribution<DistributionType::kUniformCon>;

extern template class ldt::Distribution<DistributionType::kBernoulli>;

extern template class ldt::Distribution<DistributionType::kUniformDis>;
extern template class ldt::Distribution<DistributionType::kBinomial>;
extern template class ldt::Distribution<DistributionType::kPoisson>;
extern template class ldt::Distribution<DistributionType::kGeometric>;

} // namespace ldt
