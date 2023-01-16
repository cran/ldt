/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the LGPL 3.0.
 See accompanying file LICENSE
*/

#include "matrix.h"
#include "statistics.h"

using namespace ldt;

Descriptive::Descriptive(const Matrix<Tv> *array) { pArray = array; }

double Descriptive::Minimum() {
  auto data = *pArray;
  Ti L = data.length();
  if (L == 0)
    return NAN;
  double min = INFINITY;
  double d;
  Ti i;
  for (i = 0; i < L; i++) {
    d = data.Data[i];
    if (d < min)
      min = d;
  }
  return min;
}

double Descriptive::MinimumSorted() {
  auto data = *pArray;
  if (data.length() == 0)
    return NAN;
  return data.Data[0];
}

double Descriptive::Maximum() {
  auto data = *pArray;
  Ti L = data.length();
  if (L == 0)
    return NAN;
  double max = -INFINITY;
  double d;
  Ti i;
  for (i = 0; i < L; i++) {
    d = data.Data[i];
    if (d > max)
      max = d;
  }
  return max;
}

double Descriptive::MaximumSorted() {
  auto data = *pArray;
  Ti L = data.length();
  if (L == 0)
    return NAN;
  return data.Data[L - 1];
}

double Descriptive::Sum() { return pArray->Sum(); }

std::tuple<double, double> Descriptive::MeanVariance(bool sample) {
  auto data = *pArray;
  Ti L = data.length();
  if (L == 0)
    return std::tuple<double, double>(NAN, NAN);
  else if (L == 1) {
    double d;
    double mean = 0.0;
    for (Ti i = 0; i < L; i++) {
      d = data.Data[i];
      mean += (d - mean) / (i + 1);
    }
    return std::tuple<double, double>(mean, NAN);
  } else {
    double d;
    double diff;
    double mean = 0.0;
    double s;
    double m2 = 0.0;
    for (Ti i = 0; i < L; i++) {
      d = data.Data[i];
      diff = d - mean;
      s = diff / (i + 1);
      mean += s;
      m2 += diff * s * i;
    }
    return std::tuple<double, double>(mean, sample ? m2 / L : m2 / (L - 1));
  }
}

std::tuple<double, double, double, double>
Descriptive::MeanVarianceKurtosisSkewness(bool sample) {
  auto data = *pArray;
  Ti L = data.length();
  if (L == 0)
    return std::tuple<double, double, double, double>(NAN, NAN, NAN, NAN);

  double d;
  Ti i;
  double d2;
  Ti count = 0;
  double sum = 0.0;
  double meanSquared = 0.0;

  double mean = 0.0;
  double m2 = 0.0;
  double m3 = 0.0;
  double m4 = 0.0;
  double diff;
  double s;
  double s2;
  double t;

  for (i = 0; i < L; i++) {
    d = data.Data[i];
    count++;
    d2 = std::pow(d, 2);
    sum += d;
    meanSquared += (d2 - meanSquared) / count;
    diff = d - mean;
    s = diff / count;
    s2 = s * s;
    t = diff * s * (count - 1);

    mean += s;
    m4 += t * s2 * (count * count - 3 * count + 3) + 6 * s2 * m2 - 4 * s * m3;
    m3 += t * s * (count - 2) - 3 * s * m2;
    m2 += t; // after being used by others
  }

  double variance = NAN;
  double skewness = NAN;
  double kurtosis = NAN;

  if (count > 1) {
    variance = sample ? m2 / (count - 1) : m2 / count;
    if (count > 2) {
      skewness = sample ? (count * m3 * std::sqrt(m2 / (count - 1)) /
                           (m2 * m2 * (count - 2))) *
                              (count - 1)
                        : std::sqrt(count) * m3 / std::pow(m2, 1.5);
      if (count > 3) {
        kurtosis = sample ? ((double)count * count - 1) /
                                ((count - 2) * (count - 3)) *
                                (count * m4 / (m2 * m2) - 3 + 6.0 / (count + 1))
                          : count * m4 / (m2 * m2) - 3.0;
      }
    }
  }

  return std::tuple<double, double, double, double>(mean, variance, skewness,
                                                    kurtosis);
}

void Descriptive::FilterAr(const Matrix<Tv> *coefs, Matrix<Tv> *storage) {
  Ti k = coefs->length();
  Ti T = pArray->length();
  if (storage->length() < T)
    throw std::logic_error(
        "invalid storage length"); // invalid length for storage
  if (k > T)
    throw std::logic_error("invalid length"); // invalid length

  for (Ti i = 0; i < coefs->length(); i++)
    storage->Data[i] = pArray->Data[i];
  // set data
  double r;
  for (Ti i = k; i < T; i++) {
    r = pArray->Data[i];
    for (int j = 0; j < k; j++)
      r += coefs->Data[j] * storage->Data[i - j - 1];
    storage->Data[i] = r;
  }
}

void Descriptive::FilterMa(const Matrix<Tv> &coefs, bool centered,
                           Matrix<Tv> &storage) {
  Ti k = static_cast<Ti>(coefs.length());
  Ti T = static_cast<Ti>(pArray->length());
  if (static_cast<Ti>(storage.length()) < T)
    throw std::logic_error("invalid storage length");

  storage.SetValue(NAN);

  double r;
  Ti o = centered ? static_cast<Ti>(std::floor(k / 2.0)) : 0;
  // set data
  for (Ti i = -o + (k - 1); i < T - o; i++) {
    r = 0.0;
    for (Ti j = std::max((Ti)0, o + i - T); j < std::min(k, i + o + 1); j++)
      r += coefs.Data[j] * pArray->Data[i + o - j];
    storage.Data[i] = r;
  }
}

void Descriptive::SeasonalDecompositionMa(Matrix<Tv> &storage_trend,
                                          Matrix<Tv> &storage_seasonal,
                                          Matrix<Tv> &storage_resid,
                                          Matrix<Tv> &storage_means,
                                          Matrix<Tv> *trendcoefs,
                                          Ti seasonCount, bool ismultiplicative,
                                          bool do_resid, bool trend_centred) {

  double m;
  Ti T, f, j;

  bool del = false;
  if (!trendcoefs) {
    throw std::logic_error("not implemented"); // fix new

    del = true;
    double sc = static_cast<double>(seasonCount);
    if (seasonCount % 2 == 0) {
      trendcoefs =
          new Matrix<Tv>(1.0, new double[seasonCount + 1], seasonCount + 1, 1);
      trendcoefs->Divide_in(sc);
      trendcoefs->Data[0] /= 2.0;
      trendcoefs->Data[seasonCount] /= 2.0;
    } else {
      trendcoefs = new Matrix<Tv>(1.0, new double[seasonCount], seasonCount, 1);
      trendcoefs->Divide_in(sc);
    }
  }

  FilterMa(*trendcoefs, trend_centred, storage_trend);

  if (ismultiplicative)
    pArray->Divide0(storage_trend, storage_seasonal);
  else
    pArray->Subtract0(storage_trend, storage_seasonal);

  T = pArray->length();
  auto count = new Ti[seasonCount];
  for (j = 0; j < seasonCount; j++) {
    storage_means.Data[j] = 0.0;
    count[j] = 0;
  }
  // Ti start = 0; //the results will be the same. So there is no need to define
  // this as an argument.
  f = 0; // start - 1;
  for (j = 0; j < T; j++) {
    if (f >= seasonCount)
      f = 0;
    if (std::isnan(storage_seasonal.Data[j])) {
      f++;
      continue;
    }
    storage_means.Data[f] += storage_seasonal.Data[j];
    count[f] = count[f] + 1;

    f++;
  }

  for (j = 0; j < seasonCount; j++)
    storage_means.Data[j] /= count[j];

  // demean
  m = storage_means.Sum() / seasonCount;
  if (ismultiplicative)
    storage_means.Divide_in(m);
  else
    storage_means.Subtract_in(m);

  f = 0;
  for (j = 0; j < T; j++) {
    storage_seasonal.Data[j] = storage_means.Data[f];
    f++;
    if (f >= seasonCount)
      f = 0;
  }

  // residuals
  if (do_resid) {
    if (ismultiplicative) {
      pArray->Divide0(storage_trend, storage_resid);
      storage_resid.Divide0(storage_seasonal, storage_resid);
    } else {
      pArray->Subtract0(storage_trend, storage_resid);
      storage_resid.Subtract0(storage_seasonal, storage_resid);
    }
  }

  if (del) {
    delete[] trendcoefs->Data;
    delete trendcoefs;
  }
  if (seasonCount > 0)
    delete[] count;
}

void Descriptive::RegressionTrend(double *storage2) {
  double a00 = static_cast<double>(pArray->length());
  double a01 = a00 * (a00 + 1) / 2.0;
  double a11 = a01 * (2 * a00 + 1) / 3.0;
  double data[4]{a00, a01, a01, a11};
  auto mat = Matrix<Tv>(data, 2, 2);
  int info = mat.Inv2x2();
  if (info != 0)
    throw exp_mat_sin;

  auto b0 = pArray->Sum();
  double b1 = 0.0;
  for (Ti i = 1; i <= pArray->length(); i++)
    b1 += i * pArray->Data[i - 1];

  double bdata[2]{b0, b1};
  auto bmat = Matrix<Tv>(bdata, 2, 1);
  auto resmat = Matrix<Tv>(storage2, 2, 1);
  mat.Dot(bmat, resmat);
}

double Descriptive::SumOfSquares(bool central) {
  if (central == false) {
    double sum2 = 0.0;
    for (Ti i = 0; i < pArray->length(); i++)
      sum2 += pArray->Data[i] * pArray->Data[i];
    return sum2;
  } else {

    double d;
    Ti i;
    Ti count = 0;
    double mean = 0.0;
    double m2 = 0.0;
    double diff;
    double s;
    double t;

    for (i = 0; i < pArray->length(); i++) {
      d = pArray->Data[i];
      count++;
      diff = d - mean;
      s = diff / count;
      t = diff * s * (count - 1);
      mean += s;
      m2 += t;
    }
    return m2;
  }
}

double Descriptive::QuantileSorted(double tau) {
  Ti count = pArray->length();
  if (count == 1 || tau <= 0.0)
    return pArray->Data[0];
  else if (tau >= 1.0)
    return pArray->Data[count - 1];
  else {
    double e3 = 1.0 / 3.0;
    double h;
    Ti hf;
    h = (count + e3) * tau + e3;
    hf = static_cast<Ti>(std::floor(h));
    return hf < 1 ? pArray->Data[0]
           : hf >= count
               ? pArray->Get(count - 1)
               : pArray->Data[hf - 1] +
                     (h - hf) * (pArray->Data[hf] - pArray->Data[hf - 1]);
  }
}
