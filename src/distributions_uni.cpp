/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the LGPL 3.0.
 See accompanying file LICENSE
*/

#include "distributions.h"

using namespace ldt;

// #pragma region Initialize

std::unique_ptr<DistributionBase>
DistributionBase::GetDistributionFromType(DistributionType type, Tv d1, Tv d2,
                                          Tv d3, Tv d4) {
  std::unique_ptr<DistributionBase> d;

  switch (type) {
  case ldt::DistributionType::kUniformCon:
    d = std::unique_ptr<DistributionBase>(
        new Distribution<DistributionType::kUniformCon>(d1, d2, d3, d4));
    break;
  case ldt::DistributionType::kNormal:
    d = std::unique_ptr<DistributionBase>(
        new Distribution<DistributionType::kNormal>(d1, d2, d3, d4));
    break;
  case ldt::DistributionType::kChi2:
    d = std::unique_ptr<DistributionBase>(
        new Distribution<DistributionType::kChi2>(d1, d2, d3, d4));
    break;
  case ldt::DistributionType::kT:
    d = std::unique_ptr<DistributionBase>(
        new Distribution<DistributionType::kT>(d1, d2, d3, d4));
    break;
  case ldt::DistributionType::kExponential:
    d = std::unique_ptr<DistributionBase>(
        new Distribution<DistributionType::kExponential>(d1, d2, d3, d4));
    break;
  case ldt::DistributionType::kLogNormal:
    d = std::unique_ptr<DistributionBase>(
        new Distribution<DistributionType::kLogNormal>(d1, d2, d3, d4));
    break;
  case ldt::DistributionType::kF:
    d = std::unique_ptr<DistributionBase>(
        new Distribution<DistributionType::kF>(d1, d2, d3, d4));
    break;
  case ldt::DistributionType::kGamma:
    d = std::unique_ptr<DistributionBase>(
        new Distribution<DistributionType::kGamma>(d1, d2, d3, d4));
    break;
  case ldt::DistributionType::kBeta:
    d = std::unique_ptr<DistributionBase>(
        new Distribution<DistributionType::kBeta>(d1, d2, d3, d4));
    break;
  case ldt::DistributionType::kGldFkml:
    d = std::unique_ptr<DistributionBase>(
        new Distribution<DistributionType::kGldFkml>(d1, d2, d3, d4));
    break;
  case ldt::DistributionType::kUniformDis:
    d = std::unique_ptr<DistributionBase>(
        new Distribution<DistributionType::kUniformDis>(d1, d2, d3, d4));
    break;
  case ldt::DistributionType::kBernoulli:
    d = std::unique_ptr<DistributionBase>(
        new Distribution<DistributionType::kBernoulli>(d1, d2, d3, d4));
    break;
  case ldt::DistributionType::kPoisson:
    d = std::unique_ptr<DistributionBase>(
        new Distribution<DistributionType::kPoisson>(d1, d2, d3, d4));
    break;
  case ldt::DistributionType::kGeometric:
    d = std::unique_ptr<DistributionBase>(
        new Distribution<DistributionType::kGeometric>(d1, d2, d3, d4));
    break;
  case ldt::DistributionType::kBinomial:
    d = std::unique_ptr<DistributionBase>(
        new Distribution<DistributionType::kBinomial>(d1, d2, d3, d4));
    break;
  default:
    throw std::logic_error("not implemented (distribution type).");
  }
  return d;
}

template <DistributionType type>
Distribution<type>::Distribution(Tv param1, Tv param2, Tv param3, Tv param4) {
  if constexpr (type == DistributionType::kNormal) {
    if (param2 < 0)
      throw std::logic_error("negative standard-deviation");
  } else if constexpr (type == DistributionType::kLogNormal) {
    if (param2 < 0)
      throw std::logic_error("zero/negative standard-deviation");
  } else if constexpr (type == DistributionType::kT ||
                       type == DistributionType::kChi2) {
    if (param1 <= 0)
      throw std::logic_error("zero/negative degrees of freedom");
  } else if constexpr (type == DistributionType::kUniformCon) {
    if (param1 > param2)
      throw std::logic_error("larger lower bound");
  } else if constexpr (type == DistributionType::kExponential) {
    if (param1 <= 0)
      throw std::logic_error(
          "zero or negative parameter (rate, shape, scale, etc.)");
  } else if constexpr (type == DistributionType::kF) {
    if (param1 <= 0 || param2 <= 0)
      throw std::logic_error("zero/negative degrees of freedom");
  } else if constexpr (type == DistributionType::kGamma) {
    if (param1 <= 0 || param2 <= 0)
      throw std::logic_error(
          "zero or negative parameter (rate, shape, scale, etc.)");
  } else if constexpr (type == DistributionType::kBeta) {
    if (param1 <= 0 || param2 <= 0)
      throw std::logic_error(
          "zero or negative parameter (rate, shape, scale, etc.)");
  } else if constexpr (type == DistributionType::kGldFkml) {
    if (param2 <= 0)
      throw std::logic_error(
          "zero or negative parameter (rate, shape, scale, etc.)");
    // if (param3<=-1 || param4<=-1)
    //	throw std::logic_error("shape1 && shape2 must be larger than -1.");
    //  Staden, p. 96: r-th order moment only exist if both parameters are
    //  larger than >-1/r . So, this restriction means that the distribution has
    //  a mean, both other moments might not be available

  } else if constexpr (type == DistributionType::kUniformDis) {
    if (param1 > param2)
      throw std::logic_error("larger lower bound");
  } else if constexpr (type == DistributionType::kPoisson) {
    if (param1 < 0)
      throw std::logic_error(
          "zero or negative parameter (rate, shape, scale, etc.)");
  } else if constexpr (type == DistributionType::kGeometric ||
                       type == DistributionType::kBernoulli) {
    if (param1 < 0 || param1 > 1)
      throw std::logic_error("Invalid probability (outside zero-one interval)");
  } else if constexpr (type == DistributionType::kBinomial) {
    if (param1 < 0.0 || param1 > 1.0)
      throw std::logic_error("Invalid probability (outside zero-one interval)");
    if (param2 < 0) // param2 is the number of trials in kBinomial
      throw std::logic_error("negative parameter (number of trials, etc.)");
  } else if constexpr (true) {
    throw std::logic_error("not implemented (distribution type).");
    ;
  }

  mParam1 = param1;
  mParam2 = param2;
  mParam3 = param3;
  mParam4 = param4;
}

// #pragma endregion

// #pragma region Properties

Tv DistributionBase::GetProperty(DistributionProperty propType) {

  switch (propType) {
  case ldt::DistributionProperty::kMean:
    return GetMean();
  case ldt::DistributionProperty::kVariance:
    return GetVariance();
  case ldt::DistributionProperty::kStandardError:
    return GetSandardDeviation();
  case ldt::DistributionProperty::kSkewness:
    return GetSkewness();
  case ldt::DistributionProperty::kKurtosis:
    return GetKurtosis();
  case ldt::DistributionProperty::kMinimum:
    return GetMinimum();
  case ldt::DistributionProperty::kMaximum:
    return GetMinimum();
  case ldt::DistributionProperty::kMedian:
    return GetMedian();
  case ldt::DistributionProperty::kMode:
    return GetMode();
  default:
    throw std::logic_error("not implemented (distribution type).");
  }
}

Tv Sign(Tv a) {
  if (a > 0)
    return (Tv)1;
  else if (a < 0)
    return (Tv)-1;
  else
    return (Tv)0;
}

template <DistributionType type> Tv Distribution<type>::GetMinimum() {
  if constexpr (type == DistributionType::kNormal ||
                type == DistributionType::kT) {
    if constexpr (std::numeric_limits<Tv>::has_infinity) {
      return -std::numeric_limits<Tv>::infinity();
    } else if constexpr (true) {
      throw std::logic_error(
          "type requires infinity but it is missing"); // kT must have infinity
    }
  } else if constexpr (type == DistributionType::kLogNormal) {
    return 0;
  } else if constexpr (type == DistributionType::kChi2) {
    return 0;
  } else if constexpr (type == DistributionType::kUniformCon) {
    return mParam1;
  } else if constexpr (type == DistributionType::kExponential) {
    return 0;
  } else if constexpr (type == DistributionType::kF) {
    return 0;
  } else if constexpr (type == DistributionType::kGamma) {
    return 0;
  } else if constexpr (type == DistributionType::kBeta) {
    return 0;
  } else if constexpr (type == DistributionType::kGldFkml) {
    auto r = DistributionGld::GetGldFklmRegion(mParam3, mParam4);
    if (r == (Tv)1 || r == (Tv)4) {
      if constexpr (std::numeric_limits<Tv>::has_infinity) {
        return -std::numeric_limits<Tv>::infinity();
      } else if constexpr (true) {
        throw std::logic_error(
            "type requires infinity but it is missing"); // kT must have
                                                         // infinity
      }
    } else {
      return (mParam1 - 1 / (mParam2 * mParam3));
    }
  } else if constexpr (type == DistributionType::kUniformDis) {
    return mParam1;
  } else if constexpr (type == DistributionType::kPoisson) {
    return 0;
  } else if constexpr (type == DistributionType::kGeometric) {
    return 0;
  } else if constexpr (type == DistributionType::kBernoulli) {
    return 0;
  } else if constexpr (type == DistributionType::kBinomial) {
    return 0;
  } else if constexpr (true) {
    throw std::logic_error("not implemented (distribution type).");
    ;
  }
}

template <DistributionType type> Tv Distribution<type>::GetMaximum() {

  if constexpr (type == DistributionType::kNormal ||
                type == DistributionType::kChi2 ||
                type == DistributionType::kT ||
                type == DistributionType::kExponential ||
                type == DistributionType::kLogNormal ||
                type == DistributionType::kF ||
                type == DistributionType::kGamma) {
    if constexpr (std::numeric_limits<Tv>::has_infinity) {
      return std::numeric_limits<Tv>::infinity();
    } else if constexpr (true) {
      throw std::logic_error(
          "type requires infinity but it is missing"); // kT must have infinity
    }
  } else if constexpr (type == DistributionType::kUniformCon) {
    return mParam2;
  } else if constexpr (type == DistributionType::kBeta) {
    return (Tv)1;
  } else if constexpr (type == DistributionType::kGldFkml) {
    auto r = DistributionGld::GetGldFklmRegion(mParam3, mParam4);

    if (r == (Tv)2 || r == (Tv)4) {
      if constexpr (std::numeric_limits<Tv>::has_infinity) {
        return std::numeric_limits<Tv>::infinity();
      } else if constexpr (true) {
        throw std::logic_error(
            "type requires infinity but it is missing"); // kT must have
                                                         // infinity
      }
    } else
      return mParam1 + (Tv)1 / (mParam2 * mParam4);
  } else if constexpr (type == DistributionType::kUniformDis) {
    return mParam2;
  } else if constexpr (type == DistributionType::kGeometric ||
                       type == DistributionType::kPoisson) {
    if constexpr (std::numeric_limits<Tv>::has_infinity) {
      return std::numeric_limits<Tv>::infinity();
    } else if constexpr (true) {
      throw std::logic_error(
          "type requires infinity but it is missing"); // kT must have infinity
    }
  } else if constexpr (type == DistributionType::kBernoulli) {
    return (Tv)1;
  } else if constexpr (type == DistributionType::kBinomial) {
    return mParam2;
  } else if constexpr (true) {
    throw std::logic_error("not implemented (distribution type).");
    ;
  }
}

template <DistributionType type> Tv Distribution<type>::GetMean() {

  if constexpr (type == DistributionType::kNormal) {
    return mParam1;
  } else if constexpr (type == DistributionType::kLogNormal) {
    return std::exp(mParam1 + (mParam2 * mParam2 / (Tv)2));
  } else if constexpr (type == DistributionType::kT) {
    if (mParam1 > (Tv)1)
      return (Tv)0;
    else {
      if constexpr (std::numeric_limits<Tv>::has_quiet_NaN) {
        return std::numeric_limits<Tv>::quiet_NaN();
      } else if constexpr (true) {
        throw std::logic_error("type requires NaN but it is missing");
      }
    }
  } else if constexpr (type == DistributionType::kChi2) {
    return mParam1;
  } else if constexpr (type == DistributionType::kUniformCon) {
    return (mParam1 + mParam2) / (Tv)2;
  } else if constexpr (type == DistributionType::kExponential) {
    return (Tv)1 / mParam1;
  } else if constexpr (type == DistributionType::kF) {
    if (mParam2 > (Tv)2)
      return mParam2 / (mParam2 - (Tv)2);
    if constexpr (std::numeric_limits<Tv>::has_quiet_NaN) {
      return std::numeric_limits<Tv>::quiet_NaN();
    } else if constexpr (true) {
      throw std::logic_error("type requires NaN but it is missing");
    }
  } else if constexpr (type == DistributionType::kGamma) {
    return mParam1 * mParam2;
  } else if constexpr (type == DistributionType::kBeta) {
    return mParam1 / (mParam1 + mParam2);
  } else if constexpr (type == DistributionType::kGldFkml) {
    if (mParam3 <= (Tv)-1 || mParam4 <= (Tv)-1) {
      if constexpr (std::numeric_limits<Tv>::has_quiet_NaN) {
        return std::numeric_limits<Tv>::quiet_NaN();
      } else if constexpr (true) {
        throw std::logic_error("type requires NaN but it is missing");
      }
    }
    // van staden, p. 96
    if (mParam3 != (Tv)0 && mParam4 != (Tv)0 && mParam3 != mParam4)
      return mParam1 +
             (1 / mParam2) * (DistributionGld::GetMk(1, mParam3, mParam4) -
                              1 / mParam3 + 1 / mParam4);
    return mParam1 + DistributionGld::GetMk(1, mParam3, mParam4) / mParam2;
  } else if constexpr (type == DistributionType::kUniformDis) {
    return (mParam1 + mParam2) / (Tv)2;
  } else if constexpr (type == DistributionType::kPoisson) {
    return mParam1;
  } else if constexpr (type == DistributionType::kGeometric) {
    return (1 - mParam1) / mParam1;
  } else if constexpr (type == DistributionType::kBernoulli) {
    return mParam1;
  } else if constexpr (type == DistributionType::kBinomial) {
    return mParam1 * mParam2;
  } else if constexpr (true) {
    throw std::logic_error("not implemented (distribution type).");
    ;
  }
}

template <DistributionType type> Tv Distribution<type>::GetVariance() {

  if constexpr (type == DistributionType::kNormal) {
    return mParam2 * mParam2;
  } else if constexpr (type == DistributionType::kLogNormal) {
    auto s2 = mParam2 * mParam2;
    return (std::exp(s2) - 1) * std::exp(mParam1 + mParam1 + s2);
  } else if constexpr (type == DistributionType::kT) {
    if (mParam1 > 2)
      return mParam1 / (mParam1 - 2);
    if (mParam1 > 1) {
      if constexpr (std::numeric_limits<Tv>::has_infinity) {
        return std::numeric_limits<Tv>::infinity();
      } else if constexpr (true) {
        throw std::logic_error(
            "type requires infinity but it is missing"); // kT must have
                                                         // infinity
      }
    }

    if constexpr (std::numeric_limits<Tv>::has_quiet_NaN) {
      return std::numeric_limits<Tv>::quiet_NaN();
    } else if constexpr (true) {
      throw std::logic_error("type requires NaN but it is missing");
    }
  } else if constexpr (type == DistributionType::kChi2) {
    return (Tv)2 * mParam1;
  } else if constexpr (type == DistributionType::kUniformCon) {
    auto n = (mParam2 - mParam1);
    return (n * n) / 12;
  } else if constexpr (type == DistributionType::kExponential) {
    return (Tv)1 / (mParam1 * mParam1);
  } else if constexpr (type == DistributionType::kF) {
    if (mParam2 > (Tv)4)
      return (2 * mParam2 * mParam2 * (mParam1 + mParam2 - 2)) /
             (mParam1 * (mParam2 - 2) * (mParam2 - 2) * (mParam2 - 4));
    if constexpr (std::numeric_limits<Tv>::has_quiet_NaN) {
      return std::numeric_limits<Tv>::quiet_NaN();
    } else if constexpr (true) {
      throw std::logic_error("type requires NaN but it is missing");
    }
  } else if constexpr (type == DistributionType::kGamma) {
    return mParam1 * mParam2 * mParam2;
  } else if constexpr (type == DistributionType::kBeta) {
    return (mParam1 * mParam2) / ((mParam1 + mParam2) * (mParam1 + mParam2) *
                                  (mParam1 + mParam2 + 1));
  } else if constexpr (type == DistributionType::kGldFkml) {
    if (mParam3 <= (Tv)-0.5 || mParam4 <= (Tv)-0.5) {
      if constexpr (std::numeric_limits<Tv>::has_quiet_NaN) {
        return std::numeric_limits<Tv>::quiet_NaN();
      } else if constexpr (true) {
        throw std::logic_error("type requires NaN but it is missing");
      }
    }
    // van staden, p. 96
    auto m1 = DistributionGld::GetMk(1, mParam3, mParam4);
    return (DistributionGld::GetMk(2, mParam3, mParam4) - m1 * m1) /
           (mParam2 * mParam2);
  } else if constexpr (type == DistributionType::kUniformDis) {
    auto n = mParam2 - mParam1 + 1;
    return (n * n - 1) / (Tv)12;
  } else if constexpr (type == DistributionType::kPoisson) {
    return mParam1;
  } else if constexpr (type == DistributionType::kGeometric) {
    return (1 - mParam1) / (mParam1 * mParam1);
  } else if constexpr (type == DistributionType::kBernoulli) {
    return mParam1 * (1 - mParam1);
  } else if constexpr (type == DistributionType::kBinomial) {
    return mParam1 * (1 - mParam1) * mParam2;
  } else if constexpr (true) {
    throw std::logic_error("not implemented (distribution type).");
    ;
  }
}

template <DistributionType type> Tv Distribution<type>::GetSandardDeviation() {
  if constexpr (type == DistributionType::kNormal) {
    return mParam2;
  } else if constexpr (type == DistributionType::kLogNormal) {
    return (Tv)1 / mParam1;
  } else if constexpr (true) {
    return (Tv)std::sqrt(GetVariance());
  }
}

template <DistributionType type> Tv Distribution<type>::GetSkewness() {
  if constexpr (type == DistributionType::kNormal) {
    return (Tv)0;
  } else if constexpr (type == DistributionType::kLogNormal) {
    auto es2 = std::exp(mParam2 * mParam2);
    return (es2 + 2) * std::sqrt(es2 - 1);
  } else if constexpr (type == DistributionType::kT) {
    if (mParam1 > (Tv)3) {
      return (Tv)0;
    } else {
      if constexpr (std::numeric_limits<Tv>::has_quiet_NaN) {
        return std::numeric_limits<Tv>::quiet_NaN();
      } else if constexpr (true) {
        throw std::logic_error("type requires NaN but it is missing");
      }
    }
  } else if constexpr (type == DistributionType::kChi2) {
    return std::sqrt(8 / mParam1);
  } else if constexpr (type == DistributionType::kUniformCon) {
    return (Tv)0;
  } else if constexpr (type == DistributionType::kExponential) {
    return (Tv)2;
  } else if constexpr (type == DistributionType::kF) {
    if (mParam2 > 6)
      return (((2 * mParam1) + mParam2 - 2) * std::sqrt(8 * (mParam2 - 4))) /
             ((mParam2 - 6) * std::sqrt(mParam1 * (mParam1 + mParam2 - 2)));
    ;
    if constexpr (std::numeric_limits<Tv>::has_quiet_NaN) {
      return std::numeric_limits<Tv>::quiet_NaN();
    } else if constexpr (true) {
      throw std::logic_error("type requires NaN but it is missing");
    }
  } else if constexpr (type == DistributionType::kGamma) {
    return (Tv)2 / std::sqrt(mParam1);
  } else if constexpr (type == DistributionType::kBeta) {
    return (Tv)2 * (mParam2 - mParam1) * std::sqrt(mParam1 + mParam2 + 1) /
           ((mParam1 + mParam2 + 2) * std::sqrt(mParam1 * mParam2));
  } else if constexpr (type == DistributionType::kGldFkml) {
    if (mParam3 <= (Tv)-1 / 3 || mParam4 <= (Tv)-1 / 3) {
      if constexpr (std::numeric_limits<Tv>::has_quiet_NaN) {
        return std::numeric_limits<Tv>::quiet_NaN();
      } else if constexpr (true) {
        throw std::logic_error("type requires NaN but it is missing");
      }
    }
    // van staden, p. 96
    auto m1 = DistributionGld::GetMk(1, mParam3, mParam4);
    auto m2 = DistributionGld::GetMk(2, mParam3, mParam4);
    return (DistributionGld::GetMk(3, mParam3, mParam4) - 3 * m1 * m2 +
            2 * std::pow(m1, (Tv)3)) /
           std::pow(std::sqrt(m2 - m1 * m1), (Tv)3);

  } else if constexpr (type == DistributionType::kUniformDis) {
    return (Tv)0;
  } else if constexpr (type == DistributionType::kPoisson) {
    return (Tv)1 / std::sqrt(mParam1);
  } else if constexpr (type == DistributionType::kGeometric) {
    return (2 - mParam1) / std::sqrt(1 - mParam1);
  } else if constexpr (type == DistributionType::kBernoulli) {
    return ((Tv)1 - (2 * mParam1)) / std::sqrt(mParam1 * (1 - mParam1));
  } else if constexpr (type == DistributionType::kBinomial) {
    return ((Tv)1 - (2 * mParam1)) /
           std::sqrt(mParam2 * mParam1 * (1 - mParam1));
  } else if constexpr (true) {
    throw std::logic_error("not implemented (distribution type).");
    ;
  }
}

template <DistributionType type> Tv Distribution<type>::GetKurtosis() {

  if constexpr (type == DistributionType::kNormal) {
    return (Tv)0;
  } else if constexpr (type == DistributionType::kLogNormal) {
    auto s2 = mParam2 * mParam2;
    return std::exp(4 * s2) + 2 * std::exp(3 * s2) + 3 * std::exp(2 * s2) - 6;
  } else if constexpr (type == DistributionType::kT) {
    if (mParam1 > (Tv)4) {
      return (Tv)6 / (mParam1 - 4);
    } else {
      if (mParam1 > (Tv)2) {
        if constexpr (std::numeric_limits<Tv>::has_infinity) {
          return std::numeric_limits<Tv>::infinity();
        } else if constexpr (true) {
          throw std::logic_error(
              "type requires infinity but it is missing"); // kT must have
                                                           // infinity
        }
      } else {
        if constexpr (std::numeric_limits<Tv>::has_quiet_NaN) {
          return std::numeric_limits<Tv>::quiet_NaN();
        } else if constexpr (true) {
          throw std::logic_error("type requires NaN but it is missing");
        }
      }
    }
  } else if constexpr (type == DistributionType::kChi2) {
    return 12 / mParam1;
  } else if constexpr (type == DistributionType::kUniformCon) {
    return (Tv)-1.2;
  } else if constexpr (type == DistributionType::kExponential) {
    return (Tv)6;
  } else if constexpr (type == DistributionType::kF) {
    if (mParam2 > (Tv)8)
      return 12 *
             (mParam1 * (5 * mParam2 - 22) * (mParam1 + mParam2 - 2) +
              (mParam2 - 4) * std::pow(mParam2 - 2, (Tv)2)) /
             (mParam1 * (mParam2 - 6) * (mParam2 - 8) *
              (mParam1 + mParam2 - 2));

    if constexpr (std::numeric_limits<Tv>::has_quiet_NaN) {
      return std::numeric_limits<Tv>::quiet_NaN();
    } else if constexpr (true) {
      throw std::logic_error("type requires NaN but it is missing");
    }
  } else if constexpr (type == DistributionType::kGamma) {
    return (Tv)6 / mParam1;
  } else if constexpr (type == DistributionType::kBeta) {
    auto d = mParam1 * mParam2 * (mParam1 + mParam2 + 2);
    return 6 *
           (std::pow(mParam1 - mParam2, 2) * (mParam1 + mParam2 + (Tv)1) - d) /
           (d * (mParam1 + mParam2 + (Tv)3));
  } else if constexpr (type == DistributionType::kGldFkml) {
    if (mParam3 <= (Tv)-0.25 || mParam4 <= (Tv)-0.25) {
      if constexpr (std::numeric_limits<Tv>::has_quiet_NaN) {
        return std::numeric_limits<Tv>::quiet_NaN();
      } else if constexpr (true) {
        throw std::logic_error("type requires NaN but it is missing");
      }
    } // my guess, van Staden restriction: 0.25
    // van staden, p. 96
    auto m1 = DistributionGld::GetMk(1, mParam3, mParam4);
    auto m2 = DistributionGld::GetMk(2, mParam3, mParam4);
    auto m3 = DistributionGld::GetMk(3, mParam3, mParam4);
    auto v = (m2 - m1 * m1);
    auto alpha4 =
        (DistributionGld::GetMk(4, mParam3, mParam4) - (Tv)4 * m1 * m3 +
         (Tv)6 * m1 * m1 * m2 - (Tv)3 * std::pow(m1, (Tv)4)) /
        std::pow(v, (Tv)2);
    return alpha4 - (Tv)3; // this method is for excess kurrtosis
  } else if constexpr (type == DistributionType::kUniformDis) {
    auto n = mParam2 - mParam1 + 1;
    auto nn = n * n;
    return (Tv)-1.2 * (nn + 1) / (nn - 1);
  } else if constexpr (type == DistributionType::kPoisson) {
    return (Tv)1 / mParam1;
  } else if constexpr (type == DistributionType::kGeometric) {
    return (Tv)6 + (mParam1 * mParam1) / (1 - mParam1);
  } else if constexpr (type == DistributionType::kBernoulli) {
    auto pq = mParam1 * (1 - mParam1);
    return (1 - 6 * pq) / pq;
  } else if constexpr (type == DistributionType::kBinomial) {
    auto pq = mParam1 * (1 - mParam1);
    return (1 - 6 * pq) / (mParam2 * pq);
  } else if constexpr (true) {
    throw std::logic_error("not implemented (distribution type).");
    ;
  }
}

template <DistributionType type> Tv Distribution<type>::GetMedian() {
  if constexpr (type == DistributionType::kNormal) {
    return mParam1;
  } else if constexpr (type == DistributionType::kLogNormal) {
    return std::exp(mParam1);
  } else if constexpr (type == DistributionType::kT) {
    return (Tv)0;
  } else if constexpr (type == DistributionType::kChi2) {
    return mParam1 * std::pow(1 - (Tv)2 / (9 * mParam1), (Tv)3);
  } else if constexpr (type == DistributionType::kUniformCon) {
    return (mParam1 + mParam2) / 2;
  } else if constexpr (type == DistributionType::kExponential) {
    return std::log(2) / mParam1;
  } else if constexpr (type == DistributionType::kF) {
    if constexpr (std::numeric_limits<Tv>::has_quiet_NaN) {
      return std::numeric_limits<Tv>::quiet_NaN();
    } else if constexpr (true) {
      throw std::logic_error("type requires NaN but it is missing");
    }
  } else if constexpr (type == DistributionType::kGamma) {
    if constexpr (std::numeric_limits<Tv>::has_quiet_NaN) {
      return std::numeric_limits<Tv>::quiet_NaN();
    } else if constexpr (true) {
      throw std::logic_error("type requires NaN but it is missing");
    }
  } else if constexpr (type == DistributionType::kBeta) {
    if constexpr (std::numeric_limits<Tv>::has_quiet_NaN) {
      return std::numeric_limits<Tv>::quiet_NaN();
    } else if constexpr (true) {
      throw std::logic_error("type requires NaN but it is missing");
    }
  } else if constexpr (type == DistributionType::kUniformDis) {
    return (mParam1 + mParam2) / (Tv)2;
  } else if constexpr (type == DistributionType::kPoisson) {
    return std::floor(mParam1 + (Tv)1 / 3 -
                      ((Tv)0.02 / mParam1)); // see wikipedia
  } else if constexpr (type == DistributionType::kGeometric) {
    if (mParam1 == 0) {
      if constexpr (std::numeric_limits<Tv>::has_infinity) {
        return std::numeric_limits<Tv>::infinity();
      } else if constexpr (true) {
        throw std::logic_error(
            "type requires infinity but it is missing"); // kT must have
                                                         // infinity
      }
    } else if (mParam1 == (Tv)1) {
      return 1;
    } else {
      return std::ceil(-c_ln2 / std::log(1 - mParam1)) - (Tv)1;
    }
  } else if constexpr (type == DistributionType::kBernoulli) {
    return mParam1 < (Tv)0.5   ? 0
           : mParam1 > (Tv)0.5 ? (Tv)1
                               : (Tv)0.5; // not unique. see wikipedia
  } else if constexpr (type == DistributionType::kBinomial) {
    return std::floor(mParam1 * mParam2);
  } else if constexpr (true) {
    if constexpr (std::numeric_limits<Tv>::has_quiet_NaN) {
      return std::numeric_limits<Tv>::quiet_NaN();
    } else if constexpr (true) {
      throw std::logic_error("type requires NaN but it is missing");
    }
  }
}

template <DistributionType type> Tv Distribution<type>::GetMode() {
  if constexpr (type == DistributionType::kNormal) {
    return mParam1;
  } else if constexpr (type == DistributionType::kLogNormal) {
    return std::exp(mParam1 - (mParam2 * mParam2));
  } else if constexpr (type == DistributionType::kT) {
    return 0;
  } else if constexpr (type == DistributionType::kChi2) {
    return std::max((Tv)0, mParam1 - (Tv)2);
  } else if constexpr (type == DistributionType::kUniformCon) {
    return GetMedian(); // any value in (lower,upper)
  } else if constexpr (type == DistributionType::kExponential) {
    return 0;
  } else if constexpr (type == DistributionType::kF) {
    if (mParam1 > 2)
      return ((mParam1 - 2) * mParam2) / ((mParam2 + 2) * mParam1);
    if constexpr (std::numeric_limits<Tv>::has_quiet_NaN) {
      return std::numeric_limits<Tv>::quiet_NaN();
    } else if constexpr (true) {
      throw std::logic_error("type requires NaN but it is missing");
    }
  } else if constexpr (type == DistributionType::kGamma) {
    if (mParam1 >= (Tv)1) {
      return (mParam1 - 1) * mParam2;
    } else {
      if constexpr (std::numeric_limits<Tv>::has_quiet_NaN) {
        return std::numeric_limits<Tv>::quiet_NaN();
      } else if constexpr (true) {
        throw std::logic_error("type requires NaN but it is missing");
      }
    }
  } else if constexpr (type == DistributionType::kBeta) {
    if (mParam1 == (Tv)1 && mParam2 == (Tv)1)
      return 0.5; // any value in (0,1))
    if (mParam1 <= (Tv)1 && mParam2 > (Tv)0)
      return 0;
    if (mParam1 > (Tv)1 && mParam2 <= (Tv)0)
      return (Tv)1;
    if (mParam1 < 1 && mParam2 < 0)
      return (Tv)1; // or 0
    return (mParam1 - 1) / (mParam1 + mParam2 - (Tv)2);
  } else if constexpr (type == DistributionType::kUniformDis) {
    if constexpr (std::numeric_limits<Tv>::has_quiet_NaN) {
      return std::numeric_limits<Tv>::quiet_NaN();
    } else if constexpr (true) {
      throw std::logic_error("type requires NaN but it is missing");
    }
  } else if constexpr (type == DistributionType::kPoisson) {
    return std::floor(mParam1); // has another mode too
  } else if constexpr (type == DistributionType::kGeometric) {
    return (Tv)0;
  } else if constexpr (type == DistributionType::kBernoulli) {
    return mParam1 < (Tv)0.5 ? 0
           : mParam1 > 0.5   ? (Tv)1
                             : (Tv)0; // for 0.5, 1 is also mode
  } else if constexpr (type == DistributionType::kBinomial) {
    return std::floor((mParam2 + 1) * mParam1);
  } else if constexpr (true) {
    if constexpr (std::numeric_limits<Tv>::has_quiet_NaN) {
      return std::numeric_limits<Tv>::quiet_NaN();
    } else if constexpr (true) {
      throw std::logic_error("type requires NaN but it is missing");
    }
  }
}

template <DistributionType type>
Ti Distribution<type>::GetPmfSupportIncrement() {

  if constexpr (type == DistributionType::kUniformDis ||
                type == DistributionType::kPoisson ||
                type == DistributionType::kGeometric ||
                type == DistributionType::kBernoulli ||
                type == DistributionType::kBinomial) {
    return 1;
  } else if constexpr (true) {
    throw std::logic_error("not implemented (distribution type).");
    ;
  }
}

template <DistributionType type>
Ti Distribution<type>::GetPmfSupportSize(Tv min, Tv max) {

  if (std::isnan(min) || std::isnan(max))
    throw std::logic_error("Data is 'NAN' or contains 'NaN'");

  if constexpr (type == DistributionType::kUniformDis) {
    max = std::fmin(mParam2, max);
    min = std::fmax(mParam1, min);
    return static_cast<Ti>(max - min) + 1;
  } else if constexpr (type == DistributionType::kPoisson) {
    return static_cast<Ti>(max - min) + 1;
  } else if constexpr (type == DistributionType::kGeometric) {
    return static_cast<Ti>(max - min) + 1;
  } else if constexpr (type == DistributionType::kBernoulli) {
    // never consider min or max.
    return 2;
  } else if constexpr (type == DistributionType::kBinomial) {
    max = std::min(mParam2, max);
    min = std::max((Tv)0, min);
    return static_cast<Ti>(max - min) + 1;
  } else if constexpr (true) {
    throw std::logic_error("not implemented (distribution type).");
    ;
  }
}

template <DistributionType type>
void Distribution<type>::GetPmfSupport(Tv *x, Tv *Value, bool log, Ti length,
                                       bool for_continuous_plot, Tv min) {
  if (length <= 0)
    throw std::logic_error("invalid length for support of distribution.");

  if constexpr (type == DistributionType::kUniformDis ||
                type == DistributionType::kPoisson ||
                type == DistributionType::kGeometric ||
                type == DistributionType::kBernoulli ||
                type ==
                    DistributionType::kBinomial) { // their
                                                   // 'pmf_support_increment's
                                                   // are all 1
    if (for_continuous_plot) {
      Tv xx;
      length /= 3;
      Ti j = 0;
      for (Ti i = 0; i < length; i++) {
        xx = min + i;
        x[j] = xx;
        x[j + 1] = xx;
        x[j + 2] = xx;
        Value[j] = (Tv)0;
        Value[j + 1] = log ? GetPdfOrPmfLog(xx) : GetPdfOrPmf(xx);
        Value[j + 2] = (Tv)0;
        j += 3;
      }
    } else {
      Tv xx = min;
      for (Ti i = 0; i < length; i++) {
        xx = min + i;
        x[i] = xx;
        Value[i] = log ? GetPdfOrPmfLog(xx) : GetPdfOrPmf(xx);
      }
    }
  } else if constexpr (true) {
    throw std::logic_error("invalid operation");
  }
}

template <DistributionType type> Tv Distribution<type>::GetPdfOrPmf(Tv x) {
  if (x < GetMinimum() ||
      x > GetMaximum()) // impossible event -> zero probability event
    return (Tv)0;

  if constexpr (type == DistributionType::kNormal) {
    auto n = (x - mParam1) / mParam2;
    return std::exp(-0.5 * n * n) / (c_sqrt_2Pi * mParam2);
  } else if constexpr (type == DistributionType::kLogNormal) {
    auto n = (std::log(x) - mParam1) / mParam2;
    return std::exp(-0.5 * n * n) / (x * c_sqrt_2Pi * mParam2);
  } else if constexpr (type == DistributionType::kT) {
    auto g = (mParam1 + 1) / (Tv)2;
    return std::tgamma(g) * std::pow(1 + x * x / mParam1, -g) /
           (std::tgamma(mParam1 / 2) * std::sqrt(mParam1 * c_pi));
  } else if constexpr (type == DistributionType::kChi2) {
    if (std::isinf(mParam1))
      return (Tv)0;
    auto dof2 = mParam1 / (Tv)2;
    return std::exp(-x / 2) * std::pow(x, dof2 - 1) /
           (std::pow(2, dof2) * std::tgamma(dof2));
  } else if constexpr (type == DistributionType::kUniformCon) {
    auto n = (mParam2 - mParam1);
    return (Tv)1 / n;
  } else if constexpr (type == DistributionType::kExponential) {
    return mParam1 * std::exp(-mParam1 * x);
  } else if constexpr (type == DistributionType::kF) {
    return std::sqrt(std::pow(mParam1 * x, mParam1) *
                     std::pow(mParam2, mParam2) /
                     std::pow((mParam1 * x) + mParam2, mParam1 + mParam2)) /
           (x * Math_Beta(mParam1 / 2, mParam2 / 2));
  } else if constexpr (type == DistributionType::kGamma) {
    return std::pow(mParam2, -mParam1) * std::pow(x, mParam1 - 1) *
           std::exp(-x / mParam2) / std::tgamma(mParam1);
  } else if constexpr (type == DistributionType::kBeta) {
    auto d = std::tgamma(mParam1 + mParam2) /
             (std::tgamma(mParam1) * std::tgamma(mParam2));
    return d * std::pow(x, mParam1 - 1) * std::pow(1 - x, mParam2 - 1);
  } else if constexpr (type == DistributionType::kGldFkml) {
    throw std::logic_error("not implemented (distribution type).");
    ;
  } else if constexpr (type == DistributionType::kUniformDis) {
    return x >= mParam1 && x <= mParam2 ? 1 / (mParam2 - mParam1 + 1) : 0;
  } else if constexpr (type == DistributionType::kPoisson) {
    return std::exp(-mParam1 + (x * std::log(mParam1)) -
                    std::lgamma(x + 1)); // or exp(pdf_ln)
  } else if constexpr (type == DistributionType::kGeometric) {
    return std::pow(1 - mParam1, x) * mParam1;
  } else if constexpr (type == DistributionType::kBernoulli) {
    return x == (Tv)0 ? 1 - mParam1 : x == 1 ? mParam1 : 0;
  } else if constexpr (type == DistributionType::kBinomial) {
    if (x > mParam2)
      return 0;
    // TODO: exp of a logarithm version of binomial_coefficient ?!
    return Math_BinomialCoefficient<Tv>((unsigned int)mParam2,
                                        (unsigned int)x) *
           std::pow(mParam1, x) * std::pow(1 - mParam1, mParam2 - x);
  } else if constexpr (true) {
    throw std::logic_error("not implemented (distribution type).");
    ;
  }
}

template <DistributionType type> Tv Distribution<type>::GetPdfOrPmfLog(Tv x) {
  if (x < GetMinimum() ||
      x > GetMaximum()) // impossible event -> zero probability event
  {
    if constexpr (std::numeric_limits<Tv>::has_infinity) {
      return -std::numeric_limits<Tv>::infinity();
    } else if constexpr (true) {
      throw std::logic_error(
          "type requires infinity but it is missing"); // kT must have infinity
    }
  }

  if constexpr (type == DistributionType::kNormal) {
    auto n = (x - mParam1) / mParam2;
    return ((Tv)-0.5 * n * n) - std::log(mParam2) - c_ln_sqrt2Pi;
  } else if constexpr (type == DistributionType::kLogNormal) {
    auto a = (std::log(x) - mParam1) / mParam2;
    return (Tv)-0.5 * a * a - std::log(x * mParam2) - c_ln_sqrt2Pi;
  } else if constexpr (type == DistributionType::kT) {
    auto g = (mParam1 + (Tv)1) / (Tv)2;
    return std::lgamma(g) - g * std::log(1 + (x * x / mParam1)) -
           std::lgamma(mParam1 / (Tv)2) - (Tv)0.5 * std::log(mParam1 * c_pi);
  } else if constexpr (type == DistributionType::kChi2) {
    auto dof2 = mParam1 / 2;
    return -x / (Tv)2 + (dof2 - 1) * std::log(x) - dof2 * std::log(2) -
           std::lgamma(dof2);
  } else if constexpr (type == DistributionType::kExponential) {
    if (x == 0) {
      if constexpr (std::numeric_limits<Tv>::has_infinity) {
        return -std::numeric_limits<Tv>::infinity();
      } else if constexpr (true) {
        throw std::logic_error(
            "type requires infinity but it is missing"); // kT must have
                                                         // infinity
      }
    }
    return std::log(mParam1) - (mParam1 * x);
  } else if constexpr (type == DistributionType::kGamma) {
    return (-mParam1 * std::log(mParam2)) + ((mParam1 - 1) * std::log(x)) -
           (x / mParam2) - std::lgamma(mParam1);
  } else if constexpr (type == DistributionType::kBeta) {
    auto d = std::lgamma(mParam1 + mParam2) - std::lgamma(mParam1) -
             std::lgamma(mParam2);
    return d + (mParam1 - (Tv)1) * std::log(x) +
           (mParam2 - 1) * std::log(1 - x);
  } else if constexpr (type == DistributionType::kUniformDis) {
    if (x >= mParam1 && x <= mParam2)
      return -std::log(mParam2 - mParam1 + (Tv)1);
    else {
      if constexpr (std::numeric_limits<Tv>::has_infinity) {
        return -std::numeric_limits<Tv>::infinity();
      } else if constexpr (true) {
        throw std::logic_error(
            "type requires infinity but it is missing"); // kT must have
                                                         // infinity
      }
    }
  } else if constexpr (type == DistributionType::kPoisson) {
    return -mParam1 + (x * std::log(mParam1)) - std::lgamma(x + (Tv)1);
  } else if constexpr (type == DistributionType::kGeometric) {
    return x * std::log(1 - mParam1) + std::log(mParam1);
  } else if constexpr (type == DistributionType::kBernoulli) {
    if (x == 0) {
      return std::log(1 - mParam1);
    } else if (x == 1) {
      return std::log(mParam1);
    } else {
      if constexpr (std::numeric_limits<Tv>::has_infinity) {
        return -std::numeric_limits<Tv>::infinity();
      } else if constexpr (true) {
        throw std::logic_error(
            "type requires infinity but it is missing"); // kT must have
                                                         // infinity
      }
    }
  } else if constexpr (type == DistributionType::kBinomial) {
    if (x > mParam2) {
      if constexpr (std::numeric_limits<Tv>::has_quiet_NaN) {
        return std::numeric_limits<Tv>::quiet_NaN();
      } else if constexpr (true) {
        throw std::logic_error("type requires NaN but it is missing");
      }
    } // TODO: a logarithm version of binomial_coefficient ?!
    return std::log(Math_BinomialCoefficient<Tv>((unsigned int)mParam2,
                                                 (unsigned int)x)) +
           (x * std::log(mParam1)) + ((mParam2 - x) * std::log(1 - mParam1));
  } else if constexpr (true) {
    return std::log(GetPdfOrPmf(x));
  }
}

template <DistributionType type> Tv Distribution<type>::GetCdf(Tv x) {

  if (x < GetMinimum()) // impossible event -> zero probability event
    return (Tv)0;
  if (x > GetMaximum())
    return (Tv)1;
  if (std::isinf(x) && x > (Tv)0)
    return (Tv)1;
  if (std::isinf(x) && x < (Tv)0)
    return (Tv)0;

  if constexpr (type == DistributionType::kNormal) {
    return 0.5 * erfc((mParam1 - x) / (mParam2 * c_sqrt2));
  } else if constexpr (type == DistributionType::kLogNormal) {
    return 0.5 * erfc((mParam1 - std::log(x)) / (mParam2 * c_sqrt2));
  } else if constexpr (type == DistributionType::kT) {
    auto b =
        (Tv)0.5 * Math_iBeta(mParam1 / 2, (Tv)0.5, mParam1 / (x * x + mParam1));
    return x <= (Tv)0 ? b : (Tv)1 - b;
  } else if constexpr (type == DistributionType::kChi2) {
    return Math_GammaP(mParam1 / 2, x / (Tv)2);
  } else if constexpr (type == DistributionType::kUniformCon) {
    auto n = (mParam2 - mParam1);
    return (x - mParam1) / n;
  } else if constexpr (type == DistributionType::kExponential) {
    return (Tv)1 - std::exp(-mParam1 * x);
  } else if constexpr (type == DistributionType::kF) {
    return Math_iBeta(mParam1 / 2, mParam2 / (Tv)2,
                      mParam1 * x / (mParam1 * x + mParam2));
  } else if constexpr (type == DistributionType::kGamma) {
    return Math_GammaP(mParam1, x / mParam2);
  } else if constexpr (type == DistributionType::kBeta) {
    return Math_iBeta(mParam1, mParam2, x);
  } else if constexpr (type == DistributionType::kUniformDis) {
    return std::min((Tv)1,
                    (std::floor(x) - mParam1 + 1) / (mParam2 - mParam1 + 1));
  } else if constexpr (type == DistributionType::kPoisson) {
    return (Tv)1 - Math_GammaP(x + 1, mParam1);
  } else if constexpr (type == DistributionType::kGeometric) {
    return (Tv)1 - std::pow(1 - mParam1, x + 1);
  } else if constexpr (type == DistributionType::kBernoulli) {
    return x == (Tv)0 ? (Tv)0 : x == 1 ? 1 : 1 - mParam1;
  } else if constexpr (type == DistributionType::kBinomial) {
    // using kF
    auto d1 = (Tv)2 * (mParam2 - x);
    auto d2 = (Tv)2 * (x + 1);
    auto xx = ((1 - mParam1) / mParam1) * ((x + 1) / (mParam2 - x));
    return Math_iBeta(d1 / (Tv)2, d2 / (Tv)2,
                      d1 * xx / (d1 * xx + d2)); // todo: simplify
  } else if constexpr (true) {
    throw std::logic_error("not implemented (distribution type).");
    ;
  }
}

template <DistributionType type> Tv Distribution<type>::GetQuantile(Tv p) {
  if (p <= (Tv)0)
    return GetMinimum();
  if (p >= (Tv)1)
    return GetMaximum();

  if constexpr (type == DistributionType::kNormal) {
    return mParam1 + mParam2 * c_sqrt2 * Math_ErfInv(2 * p - 1);
  } else if constexpr (type == DistributionType::kLogNormal) {
    return std::exp(mParam1 + mParam2 * c_sqrt2 * Math_ErfInv(2 * p - 1));
  } else if constexpr (type == DistributionType::kT) {
    if (p == (Tv)0.5)
      return 0;
    if (std::isinf(mParam1))
      return mParam1 +
             (mParam2 * c_sqrt2 * Math_ErfInv((Tv)2 * p - (Tv)1)); // kNormal

    if (mParam1 == (Tv)1)
      return std::tan(c_pi * (p - (Tv)0.5));
    else if (mParam1 == 2) {
      Tv a = (Tv)4 * p * ((Tv)1 - p);
      return ((Tv)2 * p - (Tv)1) * std::sqrt((Tv)2 / a);
    } else if (mParam1 == (Tv)4) {
      Tv a = (Tv)4 * p * (1 - p);
      Tv q = std::cos((Tv)1 / (Tv)3 * std::acos(std::sqrt(a))) / std::sqrt(a);
      return Sign(p - (Tv)0.5) * (Tv)2 * std::sqrt(q - 1);
    } else {
      auto x = Math_iBetaInv(mParam1 / (Tv)2, (Tv)0.5,
                             p < (Tv)0.5 ? (Tv)2 * p : (Tv)2 * ((Tv)1 - p));
      x = std::sqrt(mParam1 * ((Tv)1 - x) / x);
      return p < (Tv)0.5 ? -x : x;
    }
  } else if constexpr (type == DistributionType::kChi2) {
    return Math_GammaPInv(mParam1 / 2, p) * 2;
  } else if constexpr (type == DistributionType::kUniformCon) {
    return mParam1 + p * (mParam2 - mParam1);
  } else if constexpr (type == DistributionType::kExponential) {
    return -std::log(1 - p) / mParam1;
  } else if constexpr (type == DistributionType::kF) {
    auto x = Math_iBetaInv(mParam1 / 2, mParam2 / (Tv)2, p);
    return mParam2 * x / (mParam1 * (1 - x));
  } else if constexpr (type == DistributionType::kGamma) {
    return Math_GammaPInv(mParam1, p) * mParam2;
  } else if constexpr (type == DistributionType::kBeta) {
    return Math_iBetaInv(mParam1, mParam2, p);
  } else if constexpr (type == DistributionType::kGldFkml) {
    return DistributionGld::GetQuantile(p, mParam1, mParam2, mParam3, mParam4);
  } else if constexpr (type == DistributionType::kUniformDis) {
    Tv d = mParam1 + p * (mParam2 - mParam1);
    return std::floor(d);
  } else if constexpr (type == DistributionType::kPoisson ||
                       type == DistributionType::kGeometric ||
                       type == DistributionType::kBernoulli ||
                       type == DistributionType::kBinomial) {

    //
    auto tryN = 100;
    auto m = GetMean();
    auto sd = GetVariance();
    auto norm = Distribution<DistributionType::kNormal>(m, sd);
    auto q = std::min(GetMaximum(),
                      std::max(GetMinimum(), std::floor(norm.GetQuantile(p))));

    auto c0 = GetCdf(q);
    if (c0 > p) {
      // decrease value of q until cdf is smaller
      for (int i = 0; i < tryN; i++) {
        q--;
        c0 = GetCdf(q);
        if (c0 < p)
          return (q + 1);
      }
    } else { // cdf < p
      // increase value of q until cdf is larger than p
      for (int i = 0; i < tryN; i++) {
        q++;
        c0 = GetCdf(q);
        if (c0 >= p)
          return q;
      }
    }

    // return NaN
    if constexpr (std::numeric_limits<Tv>::has_quiet_NaN) {
      return std::numeric_limits<Tv>::quiet_NaN();
    } else if constexpr (true) {
      throw std::logic_error("type requires NaN but it is missing");
    }

  } else if constexpr (true) {
    if constexpr (std::numeric_limits<Tv>::has_quiet_NaN) {
      return std::numeric_limits<Tv>::quiet_NaN();
    } else if constexpr (true) {
      throw std::logic_error("type requires NaN but it is missing");
    }
  }
}

template <DistributionType type>
Tv Distribution<type>::GetDensityQuantile(Tv p) {

  if constexpr (type == DistributionType::kGldFkml) {
    return DistributionGld::GetDensityQuantile(p, mParam1, mParam2, mParam3,
                                               mParam4);
  } else if constexpr (true) {
    if constexpr (std::numeric_limits<Tv>::has_quiet_NaN) {
      return std::numeric_limits<Tv>::quiet_NaN();
    } else {
      throw std::logic_error("type requires NaN but it is missing");
    }
  }
}

template <DistributionType type> Tv Distribution<type>::IsQuantile() {

  if constexpr (type == DistributionType::kGldFkml) {
    return true;
  } else if constexpr (true) {
    return false;
  }
}

template <DistributionType type> bool Distribution<type>::IsDiscrete() {

  if constexpr (type == DistributionType::kUniformDis ||
                type == DistributionType::kPoisson ||
                type == DistributionType::kGeometric ||
                type == DistributionType::kBernoulli ||
                type == DistributionType::kBinomial) {
    return true;

  } else if constexpr (true) {
    return false;
  }
}

// #pragma endregion

// #pragma region Sampling

// #pragma warning(push)
// #pragma warning(disable : 4244)

template <DistributionType type>
void Distribution<type>::GetSample(Tv *storage, Ti length, unsigned int seed) {

  std::mt19937 eng;
  if (seed != (Tv)0)
    eng = std::mt19937(seed);
  else {
    std::random_device rdev{};
    eng = std::mt19937(rdev());
  }

  Ti i;

  // if constexpr (std::is_floating_point() == false)??

  if constexpr (type == DistributionType::kUniformDis) {

    // template is integer, parameters are integer too
    std::uniform_int_distribution<int> Dst(static_cast<int>(mParam1),
                                           static_cast<int>(mParam2));
    for (i = 0; i < length; i++)
      storage[i] = static_cast<Tv>(Dst(eng));

  } else if constexpr (type == DistributionType::kPoisson) {

    // template is integer, parameter is floating point
    std::poisson_distribution<int> Dst(mParam1);
    for (i = 0; i < length; i++)
      storage[i] = static_cast<Tv>(Dst(eng));

  } else if constexpr (type == DistributionType::kGeometric) {

    // template is integer, parameter is floating point
    std::geometric_distribution<int> Dst(mParam1);
    for (i = 0; i < length; i++)
      storage[i] = static_cast<Tv>(Dst(eng));
  } else if constexpr (type == DistributionType::kBernoulli) {

    // parameter is floating point
    std::bernoulli_distribution Dst(mParam1);
    for (i = 0; i < length; i++)
      storage[i] = static_cast<Tv>(Dst(eng));
  } else if constexpr (type == DistributionType::kBinomial) {

    // template is integer, parameter is floating point
    std::binomial_distribution<int> Dst(static_cast<Tv>(mParam2), mParam1);
    for (i = 0; i < length; i++)
      storage[i] = static_cast<Tv>(Dst(eng));
  }

  else if constexpr (type == DistributionType::kNormal) {
    std::normal_distribution<Tv> Dst(mParam1, mParam2);
    for (i = 0; i < length; i++)
      storage[i] = Dst(eng);
  } else if constexpr (type == DistributionType::kLogNormal) {
    std::lognormal_distribution<Tv> Dst(mParam1, mParam2);
    for (i = 0; i < length; i++)
      storage[i] = Dst(eng);
  } else if constexpr (type == DistributionType::kT) {
    std::student_t_distribution<Tv> Dst(mParam1);
    for (i = 0; i < length; i++)
      storage[i] = Dst(eng);
  } else if constexpr (type == DistributionType::kChi2) {
    std::chi_squared_distribution<Tv> Dst(mParam1);
    for (i = 0; i < length; i++)
      storage[i] = Dst(eng);
  } else if constexpr (type == DistributionType::kUniformCon) {
    std::uniform_real_distribution<Tv> Dst(mParam1, mParam2);
    for (i = 0; i < length; i++)
      storage[i] = Dst(eng);
  } else if constexpr (type == DistributionType::kExponential) {
    std::exponential_distribution<Tv> Dst(mParam1);
    for (i = 0; i < length; i++)
      storage[i] = Dst(eng);
  } else if constexpr (type == DistributionType::kF) {
    std::fisher_f_distribution<Tv> Dst(mParam1);
    for (i = 0; i < length; i++)
      storage[i] = Dst(eng);
  } else if constexpr (type == DistributionType::kGamma) {
    std::gamma_distribution<Tv> Dst(mParam1, mParam2);
    for (i = 0; i < length; i++)
      storage[i] = Dst(eng);
  } else if constexpr (type == DistributionType::kBeta) {
    throw std::logic_error("not implemented (Beta)");
    // std::_Beta_distribution<Tv> Dst(mParam1, mParam2);
    // for (i = 0; i < length; i++)
    //	storage[i] = Dst(eng);
  } else if constexpr (type == DistributionType::kGldFkml) {
    std::uniform_real_distribution<Tv> Dst(0.0, 1.0); // generate p
    for (i = 0; i < length; i++)
      storage[i] = DistributionGld::GetQuantile(Dst(eng), mParam1, mParam2,
                                                mParam3, mParam4);
  } else if constexpr (true) {
    throw std::logic_error("not implemented (distribution type).");
    ;
  }
}

template <DistributionType type>
Tv Distribution<type>::GetSample1(std::mt19937 &eng) {

  if constexpr (type == DistributionType::kUniformDis) {

    // template is integer, parameters are integer too
    std::uniform_int_distribution<int> Dst(static_cast<int>(mParam1),
                                           static_cast<int>(mParam2));
    return static_cast<Tv>(Dst(eng));
  } else if constexpr (type == DistributionType::kPoisson) {

    // template is integer, parameter is floating point
    std::poisson_distribution<int> Dst(mParam1);
    return static_cast<Tv>(Dst(eng));

  } else if constexpr (type == DistributionType::kGeometric) {
    // template is integer, parameter is floating point
    std::geometric_distribution<int> Dst(mParam1);
    return static_cast<Tv>(Dst(eng));
  } else if constexpr (type == DistributionType::kBernoulli) {
    // parameter is floating point
    std::bernoulli_distribution Dst(mParam1);
    return static_cast<Tv>(Dst(eng));
  } else if constexpr (type == DistributionType::kBinomial) {
    // template is integer, parameter is floating point
    std::binomial_distribution<int> Dst(static_cast<Tv>(mParam2), mParam1);
    return static_cast<Tv>(Dst(eng));
  }

  else if constexpr (type == DistributionType::kNormal) {
    std::normal_distribution<Tv> Dst(mParam1, mParam2);
    return Dst(eng);
  } else if constexpr (type == DistributionType::kLogNormal) {
    std::lognormal_distribution<Tv> Dst(mParam1, mParam2);
    return Dst(eng);
  } else if constexpr (type == DistributionType::kT) {
    std::student_t_distribution<Tv> Dst(mParam1);
    return Dst(eng);
  } else if constexpr (type == DistributionType::kChi2) {
    std::chi_squared_distribution<Tv> Dst(mParam1);
    return Dst(eng);
  } else if constexpr (type == DistributionType::kUniformCon) {
    std::uniform_real_distribution<Tv> Dst(mParam1, mParam2);
    return Dst(eng);
  } else if constexpr (type == DistributionType::kExponential) {
    std::exponential_distribution<Tv> Dst(mParam1);
    return Dst(eng);
  } else if constexpr (type == DistributionType::kF) {
    std::fisher_f_distribution<Tv> Dst(mParam1);
    return Dst(eng);
  } else if constexpr (type == DistributionType::kGamma) {
    std::gamma_distribution<Tv> Dst(mParam1, mParam2);
    return Dst(eng);
  } else if constexpr (type == DistributionType::kBeta) {
    throw std::logic_error("not implemented (Beta)");
    // std::Beta_distribution Dst(mParam1, mParam2);
    // return Dst(eng);
  } else if constexpr (type == DistributionType::kGldFkml) {
    std::uniform_real_distribution<Tv> Dst(0, 1); // generate p
    return Dst(eng);
  } else if constexpr (true) {
    throw std::logic_error("not implemented (distribution type).");
    ;
  }
}

// #pragma warning(pop)

// #pragma endregion

template class ldt::Distribution<DistributionType::kBeta>;
template class ldt::Distribution<DistributionType::kChi2>;
template class ldt::Distribution<DistributionType::kExponential>;
template class ldt::Distribution<DistributionType::kF>;
template class ldt::Distribution<DistributionType::kGamma>;
template class ldt::Distribution<DistributionType::kGldFkml>;
template class ldt::Distribution<DistributionType::kLogNormal>;
template class ldt::Distribution<DistributionType::kNormal>;
template class ldt::Distribution<DistributionType::kT>;
template class ldt::Distribution<DistributionType::kUniformCon>;

template class ldt::Distribution<DistributionType::kBernoulli>;

template class ldt::Distribution<DistributionType::kUniformDis>;
template class ldt::Distribution<DistributionType::kBinomial>;
template class ldt::Distribution<DistributionType::kPoisson>;
template class ldt::Distribution<DistributionType::kGeometric>;
