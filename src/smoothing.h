#pragma once

#include "functionx.h"
#include "ldt_base.h"
#include "matrix.h"
#include "optimization.h"
#include <numeric>
#include <vector>

#include "distributions.h"
#include "running.h"
#include "statistics.h"

/// todo: the classes of this file need restructuring.

namespace ldt {

/// @brief time-series filtering
class LDT_EXPORT TimeSeriesFilters {

public:
  const Matrix<Tv> *_source;
  Matrix<Tv> *Data;
  bool _delete = false;
  bool _byrow;
  IndexRange *_ranges;

  TimeSeriesFilters(const Matrix<Tv> *source, bool byrow = false,
                    bool checkNAN = true);
  ~TimeSeriesFilters() {
    if (_delete) {
      delete[] Data->Data;
      delete Data;
    }
    delete _ranges;
  }

  void calculate_hp_size(Ti &workSize, Ti &storageRow, Ti &storageCol);

  /// @brief
  /// hordrick
  /// @param lambda
  /// @param storage_filt
  /// @param WORK
  void calculate_hp(Tv lambda, Matrix<Tv> *storage_filt, Tv *WORK);

  void calculate_bk_size(Ti &storageRow, Ti &storageCol);

  /// @brief baxter-king filter
  /// @param minf
  /// @param maxf
  /// @param k
  /// @param storage_cycle
  void calculate_bk(Tv minf, Tv maxf, Ti k, Matrix<Tv> *storage_cycle);
};

enum class ExpSmoothingType {
  ann,
  aan,
  amn,
  ana,
  aaa,
  ama,
  anm,
  aam,
  amm,
  aa_dn,
  am_dn,
  aa_da,
  am_da,
  aa_dm,
  am_dm,

  mnn,
  man,
  mmn,
  mna,
  maa,
  mma,
  mnm,
  mam,
  mmm,
  ma_dn,
  mm_dn,
  ma_da,
  mm_da,
  ma_dm,
  mm_dm,
};

struct ExpSmoothingResult {

  unsigned short seasonsCount = 0;

  ExpSmoothingType type = ExpSmoothingType::ann;

  Tv alpha = NAN;
  Tv beta = NAN;
  Tv phi = NAN;
  Tv gamma = NAN;

  Tv init_Level = NAN;
  Tv init_Trend = NAN;
  Matrix<Tv> *init_seasons = nullptr;

  Tv last_Level = NAN;
  Tv last_Trend = NAN;
  Matrix<Tv> *last_seasons;

  Matrix<Tv> *resid = nullptr;
  Matrix<Tv> *levels = nullptr;
  Matrix<Tv> *trends = nullptr;
  std::vector<Matrix<Tv> *> *seasons = nullptr;

  Tv sigma2 = NAN;
  Ti T = 0;

  /// @brief no constant term
  Tv logL_con = NAN;
  Tv logL = NAN;
  Tv aic = NAN;
  Tv sic = NAN;
  Tv r2 = NAN;

  void setStorage(Tv *S, unsigned short seasoncount, Ti length, bool keepAll);

  ~ExpSmoothingResult();
};

struct expsmoothing_outsample_res_details {
  Ti sampleEnd;
  bool isValid;
  Matrix<Tv> *act_for_sd;
  Matrix<Tv> *metrics;
};

struct expsmoothing_outsample_res {
  bool hasDetails;
  Matrix<Tv> *metrics;
  std::vector<expsmoothing_outsample_res_details *> *details;
  Ti validCount;

  Ti WORKsize;

  Ti getStorageSize();
  void setStorage(Tv *storage);

  ~expsmoothing_outsample_res();
};

class LDT_EXPORT expsmoothing_base {

public:
  virtual ~expsmoothing_base(){};

  static expsmoothing_base *get_model(ExpSmoothingType type,
                                      unsigned short seasonsCount);

  static expsmoothing_base *get_model(bool additiveError, bool3 additiveTrend,
                                      bool3 additiveSeason,
                                      unsigned short seasonCount = 0,
                                      bool damped = false);

  virtual unsigned short get_seasonCount() = 0;
  virtual ExpSmoothingType get_type() = 0;
  virtual bool get_additiveError() = 0;
  virtual bool3 get_additiveTrend() = 0;
  virtual bool3 get_additiveSeason() = 0;
  virtual bool get_damped() = 0;
  virtual int get_class() = 0;
  virtual unsigned short get_m() = 0;
  virtual unsigned short get_k() = 0;

  virtual void estimate_size(Matrix<Tv> *data, LimitedMemoryBFGSB *optim,
                             ldt::Derivative *derv, Ti &workSize,
                             Ti &storageSize, bool keepAll,
                             Ti initialUpdateLength = 0) = 0;

  virtual void estimate(Matrix<Tv> *data, ExpSmoothingResult *res, Tv *WORK,
                        LimitedMemoryBFGSB *optim, ldt::Derivative *derv,
                        ExpSmoothingResult *initials, Ti &initialUpdateLength,
                        bool keepAll = false) = 0;

  virtual void forecast(Ti horizon, ExpSmoothingResult *estim, Tv *WORK,
                        bool &simulatedVar, Matrix<Tv> *forecast,
                        Matrix<Tv> *varianceF, bool forceSimV = false,
                        Ti simFixSize = 500, unsigned int seed = 0) = 0;

  virtual void calculatemeasure(bool forsize, std::vector<Ti> *metrics,
                                std::vector<Ti> *horizons, Ti outOfSampleCount,
                                Tv outOfSamplePercentage, bool keepDetails,
                                expsmoothing_outsample_res *result,
                                Matrix<Tv> *data, ExpSmoothingResult *initials,
                                Ti initializeLength, Tv *WORK,
                                LimitedMemoryBFGSB *optim, Derivative *derv,
                                bool usePreviousEstim, bool &simulateForecast,
                                bool forceSimFor, Ti simFixSize,
                                unsigned int seed) = 0;

  static void interval(Tv &lower, Tv &upper, Tv forecast, Tv variance,
                       Tv confidence = 0.95);
};

template <ExpSmoothingType type = ExpSmoothingType::ann>
class expsmoothing : public expsmoothing_base {

public:
  bool _additive_error = true;
  bool3 _additive_trend = Null;
  bool3 _additive_season = Null;
  bool _damped = false;
  unsigned short _seasonCount = 0;

  ExpSmoothingType _type = ExpSmoothingType::aaa;

  int _class = -1;

  /// @brief Number of states: Level + (Growth) + (Seasons Count - 1 (minus one
  /// is because the last season is restricted.))
  unsigned short _m = 0;

  /// @brief Number of parameters: alpha + (Growth: beta + Damped: phi) +
  /// Seasonal: gamma
  unsigned short _k = 0;

  expsmoothing(unsigned short seasonCount = 0);

  ~expsmoothing() override = default;

  virtual unsigned short get_seasonCount() override { return _seasonCount; };
  virtual ExpSmoothingType get_type() override { return _type; };
  virtual bool get_additiveError() override { return _additive_error; };
  virtual bool3 get_additiveTrend() override { return _additive_trend; };
  virtual bool3 get_additiveSeason() override { return _additive_season; };
  virtual bool get_damped() override { return _damped; };
  virtual int get_class() override { return _class; };
  virtual unsigned short get_m() override { return _m; };
  virtual unsigned short get_k() override { return _k; };

  int check_getexpclass();

  /// @brief
  /// @param pars starting point. alpha, beta, gamma, phi, init_level,
  /// init_trend, init_seasons, sigma2 will be used
  /// @param storage
  /// @param n number of draws
  /// @param burnin
  /// @param shock if null, a normal distribution will be used
  static void simulate(ExpSmoothingResultTv *pars, Tv *storage, Ti n, Ti burnin,
                       std::function<Tv(Ti)> shock = NULL);

  virtual void estimate_size(Matrix<Tv> *data, LimitedMemoryBFGSBTv *optim,
                             ldt::DerivativeTv *derv, Ti &workSize,
                             Ti &storageSize, bool keepAll,
                             Ti initialUpdateLength = 0) override;

  virtual void estimate(Matrix<Tv> *data, ExpSmoothingResultTv *res, Tv *WORK,
                        LimitedMemoryBFGSBTv *optim, ldt::DerivativeTv *derv,
                        ExpSmoothingResultTv *initials, Ti &initialUpdateLength,
                        bool keepAll = false) override;

  virtual void forecast(Ti horizon, ExpSmoothingResultTv *estim, Tv *WORK,
                        bool &simulatedVar, Matrix<Tv> *forecast,
                        Matrix<Tv> *varianceF, bool forceSimV = false,
                        Ti simFixSize = 500, unsigned int seed = 0) override;

  virtual void
  calculatemeasure(bool forsize, std::vector<Ti> *metrics,
                   std::vector<Ti> *horizons, Ti outOfSampleCount,
                   Tv outOfSamplePercentage, bool keepDetails,
                   expsmoothing_outsample_res *result, Matrix<Tv> *data,
                   ExpSmoothingResultTv *initials, Ti initializeLength,
                   Tv *WORK, LimitedMemoryBFGSBTv *optim, DerivativeTv *derv,
                   bool usePreviousEstim, bool &simulateForecast,
                   bool forceSimFor, Ti simFixSize, unsigned int seed) override;
};

extern template class ldt::expsmoothing<ExpSmoothingType::aaa>;
extern template class ldt::expsmoothing<ExpSmoothingType::aam>;
extern template class ldt::expsmoothing<ExpSmoothingType::aan>;
extern template class ldt::expsmoothing<ExpSmoothingType::aa_da>;
extern template class ldt::expsmoothing<ExpSmoothingType::aa_dm>;
extern template class ldt::expsmoothing<ExpSmoothingType::aa_dn>;
extern template class ldt::expsmoothing<ExpSmoothingType::ama>;
extern template class ldt::expsmoothing<ExpSmoothingType::amm>;
extern template class ldt::expsmoothing<ExpSmoothingType::amn>;
extern template class ldt::expsmoothing<ExpSmoothingType::am_da>;
extern template class ldt::expsmoothing<ExpSmoothingType::am_dm>;
extern template class ldt::expsmoothing<ExpSmoothingType::am_dn>;
extern template class ldt::expsmoothing<ExpSmoothingType::ana>;
extern template class ldt::expsmoothing<ExpSmoothingType::anm>;
extern template class ldt::expsmoothing<ExpSmoothingType::ann>;
extern template class ldt::expsmoothing<ExpSmoothingType::maa>;
extern template class ldt::expsmoothing<ExpSmoothingType::mam>;
extern template class ldt::expsmoothing<ExpSmoothingType::man>;
extern template class ldt::expsmoothing<ExpSmoothingType::ma_da>;
extern template class ldt::expsmoothing<ExpSmoothingType::ma_dm>;
extern template class ldt::expsmoothing<ExpSmoothingType::ma_dn>;
extern template class ldt::expsmoothing<ExpSmoothingType::mma>;
extern template class ldt::expsmoothing<ExpSmoothingType::mmm>;
extern template class ldt::expsmoothing<ExpSmoothingType::mmn>;
extern template class ldt::expsmoothing<ExpSmoothingType::mm_da>;
extern template class ldt::expsmoothing<ExpSmoothingType::mm_dm>;
extern template class ldt::expsmoothing<ExpSmoothingType::mm_dn>;
extern template class ldt::expsmoothing<ExpSmoothingType::mna>;
extern template class ldt::expsmoothing<ExpSmoothingType::mnm>;
extern template class ldt::expsmoothing<ExpSmoothingType::mnn>;

} // namespace ldt
