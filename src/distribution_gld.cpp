/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the GPL 3.0.
 See accompanying file LICENSE
*/

#include "distributions.h"

using namespace ldt;

DistributionGld::DistributionGld(Tv d1, Tv d2, Tv d3, Tv d4) {
  if (d2 <= 0)
    throw std::logic_error("scale parameter must be positive.");

  mParam1 = d1;
  mParam2 = d2;
  mParam3 = d3;
  mParam4 = d4;
}

Tv DistributionGld::GetQuantile(Tv p, Tv d1, Tv d2, Tv d3, Tv d4) {
  // van Staden p. 94, eq. 3.5
  if (d3 == 0) {
    if (d4 == 0)
      return d1 + (1.0 / d2) * std::log(p / (1.0 - p));
    if (std::isinf(d4))
      return d1 + (1.0 / d2) * std::log(p);
    return d1 + (1.0 / d2) * (std::log(p) - (std::pow(1.0 - p, d4) - 1.0) / d4);
  }
  if (d4 == 0) {
    // d3==0 is resolved
    if (std::isinf(d3))
      return d1 - (1.0 / d2) * std::log(1.0 - p);
    return d1 + (1.0 / d2) * ((std::pow(p, d3) - 1.0) / d3 - std::log(1.0 - p));
  }
  if (std::isinf(d3)) {
    // d4==0 is resolved
    return d1 - (1.0 / d2) * ((std::pow(1 - p, d4) - 1) / d4);
  }
  if (std::isinf(d4)) {
    return d1 + (1.0 / d2) * ((std::pow(p, d3) - 1) / d3);
  }
  // van Staden, p. 89
  return d1 + (1.0 / d2) * ((std::pow(p, d3) - 1.0) / d3 -
                            (std::pow(1 - p, d4) - 1.0) / d4);
}

Tv DistributionGld::GetDensityQuantile(Tv p, Tv d1, Tv d2, Tv d3, Tv d4) {
  // reciprocal of derivatives
  if (d3 == 0) {
    if (d4 == 0)
      return d2 * p - d2 * p * p;
    if (std::isinf(d4))
      return d2 * p;
    return d2 / (std::pow(1 - p, d4 - 1) + ((Tv)1 / p));
  }
  if (d4 == 0) {
    // d3==0 is resolved
    if (std::isinf(d3))
      return d2 * (1 - p);
    return d2 / (std::pow(p, d3 - 1) + (Tv)1 / ((Tv)1 - p));
  }
  if (std::isinf(d3)) {
    // d4==0 is resolved
    return d2 / (std::pow(1 - p, d4 - (Tv)1));
  }
  if (std::isinf(d4)) {
    return d2 / std::pow(p, d3 - 1);
  }

  return d2 / (std::pow(p, d3 - 1) + std::pow(1 - p, d4 - 1));
}

// #pragma region tools

Tv vk_M2_zero(Tv L) {
  return (2.0 * (2.0 * std::pow(L, 3.0) + std::pow(L, 2.0) - L - 1.0)) /
             (L * (L + 1.0) * (2 * L + 1)) +
         (2.0 * (Math_ConstantEuler<Tv>() + Math_DiGamma(L + 2.0))) /
             (L * (L + 1.0));
}

Tv vk_M3_zero(Tv L, Tv sign) {
  constexpr Tv C = Math_ConstantEuler<Tv>();
  Tv L2 = L * L;
  Tv L2_1 = L2 * (L + 1);
  Tv diG2 = Math_DiGamma(L + 2.0);
  return (3.0 * sign *
          (12.0 * std::pow(L, 5.0) + 10.0 * L2 * L2 - 4.0 * std::pow(L, 3.0) -
           L2 + 4.0 * L + 1)) /
             (L2_1 * (2 * L + 1) * (3 * L + 1)) -
         (6.0 * sign * (C + diG2)) / L2_1 +
         (3.0 * sign * (C + Math_DiGamma(2.0 * L + 2.0))) / (L2 * (2 * L + 1)) +
         (3.0 * sign *
          (c_pi_pow_2 / 6.0 + std::pow(C + diG2, 2.0) -
           Math_PolyGamma(1, L + 2))) /
             (L * (L + 1));
}

Tv vk_M4_zero(Tv L) {
  constexpr Tv C = Math_ConstantEuler<Tv>();
  Tv L2 = L * L;
  Tv L3 = L2 * L;
  Tv pi2_6 = c_pi_pow_2 / 6.0;
  Tv C_dG_L_2 = C + Math_DiGamma(L + 2.0);
  Tv C_dG_2L_2 = C + Math_DiGamma(2.0 * L + 2.0);
  Tv psi_G_L_2 = Math_PolyGamma(1, L + 2);

  return (4.0 * (144.0 * std::pow(L, 7.0) + 156.0 * std::pow(L, 6.0) -
                 18.0 * std::pow(L, 5.0) - 24 * std::pow(L2, 2.0) + 7.0 * L3 -
                 11 * L2 - 7 * L - 1)) /
             (L3 * (L + 1) * (2 * L + 1) * (3 * L + 1) * (4 * L + 1)) +
         (12.0 * (C_dG_L_2)) / (L3 * (L + 1.0)) -
         (12.0 * (C_dG_2L_2)) / (L3 * (2 * L + 1)) +
         (4.0 * (C + Math_DiGamma(3 * L + 2.0))) / (L3 * (3 * L + 1)) -
         (12.0 * (pi2_6 + std::pow(C_dG_L_2, 2.0) - psi_G_L_2)) /
             (L2 * (L + 1)) +
         (6.0 *
          (pi2_6 + std::pow(C_dG_2L_2, 2.0) - Math_PolyGamma(1, 2 * L + 2))) /
             (L2 * (2 * L + 1)) +
         (4.0 *
          (3 * (pi2_6 - psi_G_L_2) * (C_dG_L_2) + std::pow(C_dG_L_2, 3.0) +
           2.0 * c_zeta3 + Math_PolyGamma(2, L + 2.0))) /
             (L * (L + 1));
}

/// @brief for gld_FKML,
/// Au-Yeung, page: 12
/// staden, page 95
/// @param k
/// @param L3
/// @param L4
/// @return
Tv DistributionGld::GetMk(int k, Tv L3, Tv L4) {

  switch (k) {
  case 1:
    if (L3 == L4)
      return 0.0;
    if (L3 == 0 && L4 != 0)
      return -L4 / (L4 + 1);
    if (L3 != 0 && L4 == 0)
      return L3 / (L3 + 1);
    return 1.0 / (L3 * (L3 + 1)) - 1.0 / (L4 * (L4 + 1.0));

  case 2:
    if (L3 == 0 && L4 == 0)
      return c_pi_pow_2 / 3.0;
    if (L3 == 0 && L4 != 0)
      return vk_M2_zero(L4);
    if (L3 != 0 && L4 == 0)
      return vk_M2_zero(L3);
    return 1.0 / (std::pow(L3, 2.0) * (2.0 * L3 + 1.0)) +
           1.0 / (std::pow(L4, 2.0) * (2.0 * L4 + 1.0)) -
           2.0 / (L3 * L4) * Math_Beta(L3 + 1.0, L4 + 1.0);

  case 3:
    if (L3 == L4)
      return 0.0;
    if (L3 == 0 && L4 != 0)
      return vk_M3_zero(L4, -1);
    if (L3 != 0 && L4 == 0)
      return vk_M3_zero(L3, 1);
    return 1.0 / (std::pow(L3, 3.0) * (3.0 * L3 + 1.0)) -
           1.0 / (std::pow(L4, 3.0) * (3.0 * L4 + 1.0)) -
           3.0 / (std::pow(L3, 2.0) * L4) * Math_Beta(2.0 * L3 + 1, L4 + 1.0) +
           3.0 / (L3 * std::pow(L4, 2.0)) * Math_Beta(L3 + 1, 2.0 * L4 + 1.0);

  case 4:
    if (L3 == 0 && L4 == 0)
      return 7.0 * c_pi_pow_4 / 15.0;
    if (L3 != 0 && L4 == 0)
      return vk_M4_zero(L3);
    if (L3 == 0 && L4 != 0)
      return vk_M4_zero(L4);
    return 1.0 / (std::pow(L3, 4.0) * (4.0 * L3 + 1.0)) +
           1.0 / (std::pow(L4, 4.0) * (4.0 * L4 + 1.0)) +
           6.0 / (std::pow(L3 * L4, 2.0)) *
               Math_Beta(2.0 * L3 + 1, 2.0 * L4 + 1.0) -
           4.0 / (std::pow(L3, 3.0) * L4) * Math_Beta(3.0 * L3 + 1, L4 + 1.0) -
           4.0 / (L3 * std::pow(L4, 3.0)) * Math_Beta(L3 + 1, 3.0 * L4 + 1.0);
  }
  throw std::logic_error("not implemented");
}

void DistributionGld::GetMs(Tv H3, Tv H4, Tv &M1, Tv &M2, Tv &M3, Tv &M4) {

  if (H3 == 0 && H4 == 0) {
    M1 = 0.0;
    M2 = c_pi_pow_2 / 3.0;
    M3 = 0.0;
    M4 = 7.0 * c_pi_pow_4 / 15.0;
  } else if (H3 == 0 && H4 != 0) {
    M1 = -H4 / (H4 + 1);

    Tv H4_p_4 = std::pow(H4, 3.0);
    Tv H4_p_5 = std::pow(H4, 5.0);

    Tv sign = -1;
    constexpr Tv C = Math_ConstantEuler<Tv>();
    Tv L2 = H4 * H4;
    Tv L3 = L2 * H4;
    Tv L2_1 = L2 * (H4 + 1);
    Tv pi2_6 = c_pi_pow_2 / 6.0;
    Tv diG2 = Math_DiGamma(H4 + 2.0);
    Tv C_dG_L_2 = C + Math_DiGamma(H4 + 2.0);
    Tv C_dG_2L_2 = C + Math_DiGamma(2.0 * H4 + 2.0);
    Tv psi_G_L_2 = Math_PolyGamma(1, H4 + 2);
    M2 = (2.0 * (2.0 * H4_p_4 + std::pow(H4, 2.0) - H4 - 1.0)) /
             (H4 * (H4 + 1.0) * (2 * H4 + 1)) +
         (2.0 * C_dG_L_2) / (H4 * (H4 + 1.0));
    M3 = (3.0 * sign *
          (12.0 * H4_p_5 + 10.0 * L2 * L2 - 4.0 * H4_p_4 - L2 + 4.0 * H4 + 1)) /
             (L2_1 * (2 * H4 + 1) * (3 * H4 + 1)) -
         (6.0 * sign * (C + diG2)) / L2_1 +
         (3.0 * sign * C_dG_2L_2) / (L2 * (2 * H4 + 1)) +
         (3.0 * sign *
          (c_pi_pow_2 / 6.0 + std::pow(C + diG2, 2.0) - psi_G_L_2)) /
             (H4 * (H4 + 1));
    M4 =
        (4.0 * (144.0 * std::pow(H4, 7.0) + 156.0 * std::pow(H4, 6.0) -
                18.0 * H4_p_5 - 24 * std::pow(L2, 2.0) + 7.0 * L3 - 11 * L2 -
                7 * H4 - 1)) /
            (L3 * (H4 + 1) * (2 * H4 + 1) * (3 * H4 + 1) * (4 * H4 + 1)) +
        (12.0 * (C_dG_L_2)) / (L3 * (H4 + 1.0)) -
        (12.0 * (C_dG_2L_2)) / (L3 * (2 * H4 + 1)) +
        (4.0 * (C + Math_DiGamma(3 * H4 + 2.0))) / (L3 * (3 * H4 + 1)) -
        (12.0 * (pi2_6 + std::pow(C_dG_L_2, 2.0) - psi_G_L_2)) /
            (L2 * (H4 + 1)) +
        (6.0 *
         (pi2_6 + std::pow(C_dG_2L_2, 2.0) - Math_PolyGamma(1, 2 * H4 + 2))) /
            (L2 * (2 * H4 + 1)) +
        (4.0 * (3 * (pi2_6 - psi_G_L_2) * (C_dG_L_2) + std::pow(C_dG_L_2, 3.0) +
                2.0 * c_zeta3 + Math_PolyGamma(2, H4 + 2.0))) /
            (H4 * (H4 + 1));

  } else if (H3 != 0 && H4 == 0) {
    M1 = H3 / (H3 + 1);

    Tv H3_p_4 = std::pow(H3, 3.0);
    Tv H3_p_5 = std::pow(H3, 5.0);

    Tv sign = 1;
    constexpr Tv C = Math_ConstantEuler<Tv>();
    Tv L2 = H3 * H3;
    Tv L3 = L2 * H3;
    Tv L2_1 = L2 * (H3 + 1);
    Tv pi2_6 = c_pi_pow_2 / 6.0;
    Tv diG2 = Math_DiGamma(H3 + 2.0);
    Tv C_dG_L_2 = C + Math_DiGamma(H3 + 2.0);
    Tv C_dG_2L_2 = C + Math_DiGamma(2.0 * H3 + 2.0);
    Tv psi_G_L_2 = Math_PolyGamma(1, H3 + 2);
    M2 = (2.0 * (2.0 * H3_p_4 + std::pow(H3, 2.0) - H3 - 1.0)) /
             (H3 * (H3 + 1.0) * (2 * H3 + 1)) +
         (2.0 * C_dG_L_2) / (H3 * (H3 + 1.0));
    M3 = (3.0 * sign *
          (12.0 * H3_p_5 + 10.0 * L2 * L2 - 4.0 * H3_p_4 - L2 + 4.0 * H3 + 1)) /
             (L2_1 * (2 * H3 + 1) * (3 * H3 + 1)) -
         (6.0 * sign * (C + diG2)) / L2_1 +
         (3.0 * sign * C_dG_2L_2) / (L2 * (2 * H3 + 1)) +
         (3.0 * sign *
          (c_pi_pow_2 / 6.0 + std::pow(C + diG2, 2.0) - psi_G_L_2)) /
             (H3 * (H3 + 1));
    M4 =
        (4.0 * (144.0 * std::pow(H3, 7.0) + 156.0 * std::pow(H3, 6.0) -
                18.0 * H3_p_5 - 24 * std::pow(L2, 2.0) + 7.0 * L3 - 11 * L2 -
                7 * H3 - 1)) /
            (L3 * (H3 + 1) * (2 * H3 + 1) * (3 * H3 + 1) * (4 * H3 + 1)) +
        (12.0 * (C_dG_L_2)) / (L3 * (H3 + 1.0)) -
        (12.0 * (C_dG_2L_2)) / (L3 * (2 * H3 + 1)) +
        (4.0 * (C + Math_DiGamma(3 * H3 + 2.0))) / (L3 * (3 * H3 + 1)) -
        (12.0 * (pi2_6 + std::pow(C_dG_L_2, 2.0) - psi_G_L_2)) /
            (L2 * (H3 + 1)) +
        (6.0 *
         (pi2_6 + std::pow(C_dG_2L_2, 2.0) - Math_PolyGamma(1, 2 * H3 + 2))) /
            (L2 * (2 * H3 + 1)) +
        (4.0 * (3 * (pi2_6 - psi_G_L_2) * (C_dG_L_2) + std::pow(C_dG_L_2, 3.0) +
                2.0 * c_zeta3 + Math_PolyGamma(2, H3 + 2.0))) /
            (H3 * (H3 + 1));
  } else {
    Tv L3_p_2 = std::pow(H3, 2.0);
    Tv L3_p_3 = std::pow(H3, 3.0);
    Tv L4_p_2 = std::pow(H4, 2.0);
    Tv L4_p_3 = std::pow(H4, 3.0);

    M1 = 1.0 / (H3 * (H3 + 1)) - 1.0 / (H4 * (H4 + 1.0));
    M2 = 1.0 / (L3_p_2 * (2.0 * H3 + 1.0)) + 1.0 / (L4_p_2 * (2.0 * H4 + 1.0)) -
         2.0 / (H3 * H4) * Math_Beta(H3 + 1.0, H4 + 1.0);
    M3 = 1.0 / (L3_p_3 * (3.0 * H3 + 1.0)) - 1.0 / (L4_p_3 * (3.0 * H4 + 1.0)) -
         3.0 / (L3_p_2 * H4) * Math_Beta(2.0 * H3 + 1, H4 + 1.0) +
         3.0 / (H3 * L4_p_2) * Math_Beta(H3 + 1, 2.0 * H4 + 1.0);
    M4 = 1.0 / (std::pow(H3, 4.0) * (4.0 * H3 + 1.0)) +
         1.0 / (std::pow(H4, 4.0) * (4.0 * H4 + 1.0)) +
         6.0 / (std::pow(H3 * H4, 2.0)) *
             Math_Beta(2.0 * H3 + 1, 2.0 * H4 + 1.0) -
         4.0 / (L3_p_3 * H4) * Math_Beta(3.0 * H3 + 1, H4 + 1.0) -
         4.0 / (H3 * L4_p_3) * Math_Beta(H3 + 1, 3.0 * H4 + 1.0);
  }
}

/// @brief van Staden, page 93, table 3.3
/// @param L3
/// @param L4
/// @return
int DistributionGld::GetGldFklmRegion(Tv L3, Tv L4) {
  if (L3 <= 0) {
    if (L4 > 0)
      return 1;
    else
      return 4;
  } else { // L3>0
    if (L4 > 0)
      return 3;
    else
      return 2;
  }
}

// #pragma endregion

std::tuple<Tv, Tv, Tv, Tv>
DistributionGld::GetFromMoments(Tv mean, Tv variance, Tv skewness,
                                Tv ex_kurtosis, int type, NelderMead &optim,
                                Tv startL3, Tv startL4) {

  // type 0: general, no restriction
  // type 1: symmetric 'type 0', L3 = L4
  // type 2: unimodal continuous tail,   L3<1    &  L4<1
  // type 3: symmetric 'type 2',  L3<1    &  L4<1 & L3==L4
  // type 4: unimodal continuous tail finite slope, L3<=0.5 &  L4<=5
  // type 5: symmetric 'type 4', L3<=0.5 &  L4<=5 & L3==L4
  // type 6: unimodal truncated density curves,    L3>=2   &  L4>=2
  // type 7: symmetric 'type 6', L3>=2   &  L4>=2  & L3==L4
  // type 8: S shaped, L3>2    &  1<L4<2 ||  1<L3<2 & L4>2
  // type 9: U shaped, 1<L3<=2 &  1<L4<=2
  // type 10: symmetric 'type 9', 1<L3<=2 &  1<L4<=2  & L4==L4
  // type 11: monotone L3>1  &  L4<=1

  Tv work[4] = {-0.25 + DBL_EPSILON, -0.25 + DBL_EPSILON, INFINITY, INFINITY};

  auto lower = Matrix<Tv>(work, 2, 1);
  auto upper = Matrix<Tv>(&work[2], 2, 1);

  std::function<Tv(const Matrix<Tv> &)> objective;

  Tv m1, m2, m3, m4;

  bool isSymetric =
      type == 1 || type == 3 || type == 5 || type == 7 || type == 10;

  if (isSymetric) {
    objective = [skewness, ex_kurtosis, &m1, &m2, &m3,
                 &m4](const Matrix<Tv> &x) -> Tv {
      GetMs(x.Data[0], x.Data[0], m1, m2, m3, m4);
      auto v = m2 - m1 * m1;
      Tv skew = (m3 - 3 * m1 * m2 + 2.0 * std::pow(m1, 3.0)) / std::pow(v, 1.5);
      auto exc_kurt = -3.0 + (m4 - 4.0 * m1 * m3 + 6.0 * m1 * m1 * m2 -
                              3.0 * std::pow(m1, 4.0)) /
                                 std::pow(v, 2.0);
      return std::pow(ex_kurtosis - exc_kurt, 2.0) +
             std::pow(skew - skewness, 2.0);
    };

    lower.Restructure0(1, 1);
    upper.Restructure0(1, 1);
    switch (type) {
    case 3:
      upper.Data[0] = 1 - DBL_EPSILON;
      break;
    case 5:
      upper.Data[0] = 0.5 - DBL_EPSILON;
      break;
    case 7:
      lower.Data[0] = 2 + DBL_EPSILON;
      break;
    case 10:
      lower.Data[0] = 1 + DBL_EPSILON;
      upper.Data[0] = 2 - DBL_EPSILON;
      break;
    }

  } else {

    objective = [skewness, ex_kurtosis, &m1, &m2, &m3,
                 &m4](const Matrix<Tv> &x) -> Tv {
      GetMs(x.Data[0], x.Data[1], m1, m2, m3, m4);
      auto v = m2 - m1 * m1;
      Tv skew = (m3 - 3 * m1 * m2 + 2.0 * std::pow(m1, 3.0)) / std::pow(v, 1.5);
      auto exc_kurt = -3.0 + (m4 - 4.0 * m1 * m3 + 6.0 * m1 * m1 * m2 -
                              3.0 * std::pow(m1, 4.0)) /
                                 std::pow(v, 2.0);
      return std::pow(ex_kurtosis - exc_kurt, 2.0) +
             std::pow(skew - skewness, 2.0);
    };

    switch (type) {
    case 2:
      upper.Data[0] = 1 - DBL_EPSILON;
      upper.Data[1] = 1 - DBL_EPSILON;
      break;
    case 4:
      upper.Data[0] = 0.5 - DBL_EPSILON;
      upper.Data[1] = 0.5 - DBL_EPSILON;
      break;
    case 6:
      lower.Data[0] = 2 + DBL_EPSILON;
      lower.Data[1] = 2 + DBL_EPSILON;
      break;
    case 9:
      lower.Data[0] = 1 + DBL_EPSILON;
      lower.Data[1] = 1 + DBL_EPSILON;
      upper.Data[0] = 2 - DBL_EPSILON;
      upper.Data[1] = 2 - DBL_EPSILON;
      break;
    case 11:
      lower.Data[0] = 1 + DBL_EPSILON;
      upper.Data[1] = 1 - DBL_EPSILON;
      break;
    case 8:
      // type 8: S shaped, L3>2    &  1<L4<2 ||  1<L3<2 & L4>2
      // combine the restriction and the objective

      objective = [skewness, ex_kurtosis, &m1, &m2, &m3,
                   &m4](const Matrix<Tv> &x) -> Tv {
        Tv penalty = 0.0;
        if (x.Data[0] > 2 + DBL_EPSILON && x.Data[1] > 1 + DBL_EPSILON &&
            x.Data[1] < 2 - DBL_EPSILON) {
          // ok
        } else if (x.Data[1] > 2 + DBL_EPSILON && x.Data[0] > 1 + DBL_EPSILON &&
                   x.Data[0] < 2 - DBL_EPSILON) {
          // ok
        } else // what is a good penalty?!
          penalty +=
              std::pow(1.5 - x.Data[0], 2) + std::pow(1.5 - x.Data[1], 2);

        GetMs(x.Data[0], x.Data[1], m1, m2, m3, m4);
        auto v = m2 - m1 * m1;
        Tv skew =
            (m3 - 3 * m1 * m2 + 2.0 * std::pow(m1, 3.0)) / std::pow(v, 1.5);
        auto exc_kurt = -3.0 + (m4 - 4.0 * m1 * m3 + 6.0 * m1 * m1 * m2 -
                                3.0 * std::pow(m1, 4.0)) /
                                   std::pow(v, 2.0);
        return penalty * std::pow(ex_kurtosis - exc_kurt, 2.0) +
               std::pow(skew - skewness, 2.0);
      };
      break;
    }
  }

  Tv da[2] = {startL3, startL4};
  auto x0 = Matrix<Tv>(da, isSymetric ? 1 : 2);
  auto W = std::unique_ptr<Tv[]>(new Tv[optim.WorkSize]);
  auto S = std::unique_ptr<Tv[]>(new Tv[optim.StorageSize]);
  optim.Minimize(objective, x0, W.get(), S.get(), &lower, &upper);

  Tv L3 = optim.Result.Data[0];
  Tv L4 = optim.Result.Data[isSymetric ? 0 : 1];

  m1 = GetMk(1, L3, L4);
  m2 = GetMk(2, L3, L4);
  Tv L2 = std::sqrt((m2 - m1 * m1) / variance);
  Tv L1;
  if (L3 == 0.0 || L4 == 0.0 || L3 == L4)
    L1 = mean - (1.0 / L2) * m1;
  else
    L1 = mean - (1.0 / L2) * (m1 - 1 / L3 + 1 / L4);

  return std::tuple<Tv, Tv, Tv, Tv>(L1, L2, L3, L4);
}
