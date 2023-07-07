#include "optimization.h"

using namespace ldt;

NelderMead::NelderMead(Ti n) {
  WorkSize = (n + 1) * (n + 1) * 4 * n;
  StorageSize = n;
}

Tv PenaltyFunction(const Matrix<Tv> &x, const Matrix<Tv> *lower,
                   const Matrix<Tv> *upper) {
  Tv penalty = 0.0;
  if (lower && upper) {
    for (int i = 0; i < x.length(); i++) {
      if (x.Data[i] < lower->Data[i])
        penalty += std::pow(lower->Data[i] - x.Data[i], 2);
      else if (x.Data[i] > upper->Data[i])
        penalty += std::pow(x.Data[i] - upper->Data[i], 2);
    }
  } else if (lower) {
    for (int i = 0; i < x.length(); i++) {
      if (x.Data[i] < lower->Data[i])
        penalty += std::pow(lower->Data[i] - x.Data[i], 2);
    }
  } else if (upper) {
    for (int i = 0; i < x.length(); i++) {
      if (x.Data[i] > upper->Data[i])
        penalty += std::pow(x.Data[i] - upper->Data[i], 2);
    }
  }
  return penalty;
}

void NelderMead::Minimize(
    const std::function<Tv(const Matrix<Tv> &)> &objective,
    const Matrix<Tv> &start, Tv *work, Tv *storage, const Matrix<Tv> *lower,
    const Matrix<Tv> *upper, Tv penaltyMultiplier) {
  Ti n = start.length();

  // Check if starting point is within bounds and adjust if necessary
  for (int i = 0; i < n; i++) {
    if (std::isnan(start.Data[i]))
      throw std::logic_error("A starting value is NAN.");
    if (lower && upper && lower->Data[i] > upper->Data[i])
      throw std::logic_error(
          "Lower bound must be equal or smaller than the upper bound.");
    if (lower && start.Data[i] < lower->Data[i]) {
      start.Data[i] = lower->Data[i];
    }
    if (upper && start.Data[i] > upper->Data[i]) {
      start.Data[i] = upper->Data[i];
    }
  }

  std::function<Tv(const Matrix<Tv> &)> f = [&](const Matrix<Tv> &x) -> Tv {
    try {
      return objective(x) +
             penaltyMultiplier * PenaltyFunction(x, lower, upper);
    } catch (...) {
      return INFINITY;
    }
  };

  std::vector<Matrix<Tv>> simplex(n + 1);
  Ti q = 0;
  for (Ti i = 0; i <= n; ++i) {
    simplex[i] = Matrix<Tv>(&work[q], n, 1);
    q += n;
  }
  auto values = Matrix<Tv>(&work[q], n + 1, 1);
  q += n + 1;

  for (Ti i = 0; i <= n; i++) {
    start.CopyTo00(simplex[i]);
    if (i > 0)
      simplex[i].Data[i - 1] += 1.0;
    values.Data[i] = f(simplex[i]);
  }

  // Initialize centroid, reflected, expanded, and contracted arrays
  auto centroid = Matrix<Tv>(&work[q], n, 1);
  q += n;
  auto reflected = Matrix<Tv>(&work[q], n, 1);
  q += n;
  auto expanded = Matrix<Tv>(&work[q], n, 1);
  q += n;
  auto contracted = Matrix<Tv>(&work[q], n, 1);
  q += n;

  Ti worst_index = -1, best_index = -1;
  Tv temp = 0.0;
  Iter = 0;
  while (Iter < MaxIteration) {
    Array<Tv>::MaxIndex<false>(values.Data, n + 1, temp, worst_index);
    Array<Tv>::MinIndex<false>(values.Data, n + 1, temp, best_index);

    Diff = std::abs(values.Data[worst_index] - values.Data[best_index]);
    if (Diff < Tolerance)
      break;

    // centroid = average of all points except worst point
    centroid.SetValue(0.0);
    for (Ti i = 0; i <= n; i++) {
      if (i == worst_index)
        continue;
      for (Ti j = 0; j < n; j++)
        centroid.Data[j] += simplex[i].Data[j];
    }
    for (Ti i = 0; i < n; i++)
      centroid.Data[i] /= n;

    // reflected = centroid + reflection * (centroid - simplex[worst_index])
    for (Ti i = 0; i < n; i++)
      reflected.Data[i] =
          centroid.Data[i] +
          ParamReflection * (centroid.Data[i] - simplex[worst_index].Data[i]);

    auto reflected_value = f(reflected);
    if (reflected_value < values.Data[best_index]) {

      // reflected point is better that best point
      // expand and move in that direction

      // expanded = centroid + expansion * (reflected - centroid
      for (Ti i = 0; i < n; i++)
        expanded.Data[i] =
            centroid.Data[i] +
            ParamExpansion * (reflected.Data[i] - centroid.Data[i]);

      auto expanded_value = f(expanded);
      if (expanded_value < reflected_value) {

        // Expanded point is better than reflected point

        expanded.CopyTo00(simplex[worst_index]);
        values.Data[worst_index] = expanded_value;
      } else {

        // reflected point is better

        reflected.CopyTo00(simplex[worst_index]);
        values.Data[worst_index] = reflected_value;
      }
    } else {

      // Reflected point is not better than best point
      // check and see if it is better that some other point

      bool is_accepted = false;
      for (Ti i = 0; i <= n; i++) {
        if (i == worst_index)
          continue;
        if (reflected_value < values.Data[i]) {

          // accept the reflected point

          reflected.CopyTo00(simplex[worst_index]);
          values.Data[worst_index] = reflected_value;
          is_accepted = true;
          break;
        }
      }

      if (!is_accepted) {

        // Reflected point is not accepted, try contraction
        // Move the worst point towards the centroid

        // contracted = centroid + contraction * (simplex[worst_index] -
        // centroid)
        for (Ti i = 0; i < n; i++)
          contracted.Data[i] =
              centroid.Data[i] +
              ParamContraction *
                  (simplex[worst_index].Data[i] - centroid.Data[i]);

        auto contracted_value = f(contracted);

        if (contracted_value < values.Data[worst_index]) {

          // Contracted point is better than worst point

          contracted.CopyTo00(simplex[worst_index]);
          values.Data[worst_index] = contracted_value;

        } else {

          // Contracted point is not better than worst point, perform shrink
          // moves all points towards the best point

          for (Ti i = 0; i <= n; i++) {
            if (i == best_index)
              continue;
            for (Ti j = 0; j < n; j++)
              simplex[i].Data[j] = simplex[best_index].Data[j] +
                                   ParamShrink * (simplex[i].Data[j] -
                                                  simplex[best_index].Data[j]);
            values.Data[i] = f(simplex[i]);
          }
        }
      }
    }
    ++Iter;
  }

  Result.SetData(storage, n, 1);
  Array<Tv>::MinIndex<false>(values.Data, n + 1, temp, best_index);
  simplex[best_index].CopyTo00(Result);
  Min = f(Result);
}

std::tuple<Tv, Ti>
NelderMead::Minimize1(const std::function<Tv(const Tv &)> &objective, Tv x0,
                      Tv step, int max_iter, Tv tol, Tv x_min, Tv x_max) {

  // set constraints:

  std::function<Tv(const Tv &)> f;
  if (std::isnan(x_min) && std::isnan(x_max))
    f = objective;
  else if (std::isnan(x_min)) {
    if (x0 > x_max)
      x0 = x_max;
    f = [&](Tv x) {
      Tv P = 0.0;
      if (x > x_max) {
        P = 1e5 * std::pow(x, 2);
      }
      return objective(x) + P;
    };
  } else if (std::isnan(x_max)) {
    if (x0 < x_min)
      x0 = x_min;
    f = [&](Tv x) {
      Tv P = 0.0;
      if (x < x_min) {
        P = 1e5 * std::pow(x, 2);
      }
      return objective(x) + P;
    };
  } else {
    if (x0 < x_min)
      x0 = x_min;
    if (x0 > x_max)
      x0 = x_max;
    f = [&](Tv x) {
      Tv P = 0.0;
      if (x > x_max || x < x_min) {
        P = 1e5 * std::pow(x, 2);
      }
      return objective(x) + P;
    };
  }

  // optimization:

  Tv x1 = x0 + step;
  Tv x2 = x0 - step;
  int iter = 0;
  while (iter < max_iter) {
    iter++;
    Tv f1 = f(x1);
    Tv f2 = f(x2);
    Tv f0 = f(x0);
    if (f1 <= f2 && f1 <= f0) {
      x0 = x1;
      x2 = x1 - step;
      x1 = x0 + step;
    } else if (f2 <= f1 && f2 <= f0) {
      x0 = x2;
      x1 = x0 + step;
      x2 = x0 - step;
    } else {
      if (abs(x0 - x1) < tol && abs(x0 - x2) < tol) {
        break;
      }
      step /= 2.0;
      x1 = x0 + step;
      x2 = x0 - step;
    }
  }
  return std::tuple<Tv, Ti>(x0, iter);
}
