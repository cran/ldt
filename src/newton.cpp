/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the GPL 3.0.
 See accompanying file LICENSE
*/

#include "optimization.h"

using namespace ldt;

Newton::Newton(Ti k) {
  mK = k;
  WorkSize = 3 * k;
  StorageSize = k + k * k;
}

static Tv norm2(Matrix<Tv> &m) {
  Tv sum = 0.0;
  for (Ti i = 0; i < m.length(); i++)
    sum += std::pow(m.Data[i], 2.0);
  return std::sqrt(sum);
}

/// @brief A Bisection Method for the Weak Wolfe Conditions
/// @param d Search direction
/// @param x0 On success, it is updated to the new value. i.e., x0+td
/// where t is the largest valid value
/// @param vf0 Current value. On success, it is updated to the new value at the
/// new x0
/// @param vg0 Current gradient. On success, it is updated to the new value at
/// the new x0
/// @param t step length. Use 1.0 for (quasi-)Newton. (see p. 59, Nocedal &
/// Wright, 2006)
/// @param c1 in [0,1]. suggested value is 1e-4 (p. 33, Nocedal & Wright, 2006)
/// @param c2 in [c1,1]. suggested value is 0.9 for Newton or quasi-Newton
/// method or 0.1 when p is from nonlinear conjugate gradient method
static void
wolfe_weak(const std::function<Tv(const Matrix<Tv> &)> &F,
           const std::function<void(const Matrix<Tv> &, Matrix<Tv> &)> &G,
           const Matrix<Tv> &d, Matrix<Tv> &x0, Tv &vf0, Matrix<Tv> &vg0,
           Ti &Iteration, Tv *work, Tv &t, Tv c1 = 1e-4, Tv c2 = 0.9,
           Ti maxIterations = 100) {

  auto n = x0.length();
  Matrix<Tv> wo1 = Matrix<Tv>(work, n, 1);
  Matrix<Tv> vg = Matrix<Tv>(&work[n], n, 1);

  Tv alpha = 0.0, beta = INFINITY, g_d0, g_d;
  g_d0 = d.VectorDotVector0(vg0);
  Tv FunctionValue = NAN; // updated value
  for (Iteration = 1; Iteration < maxIterations; Iteration++) {
    d.Multiply(t, wo1);
    wo1.Add_in0(x0); // //x+α*p
    FunctionValue = F(wo1);

    if (std::isnan(FunctionValue) || FunctionValue > vf0 + c1 * t * g_d0) {
      // Armijo sufficient decrease condition does not hold.
      beta = t;
      t = 0.5 * (alpha + beta);
    } else {
      // sufficient condition holds
      G(wo1, vg); // gradient at the new point
      g_d = d.VectorDotVector0(vg);

      if (std::isnan(g_d) || g_d < c2 * g_d0) {
        // curvature does not hold
        alpha = t;
        t = std::isinf(beta) ? 2 * alpha : 0.5 * (alpha + beta);
      } else // Wolfe condition holds
      {
        wo1.CopyTo00(x0);
        vf0 = FunctionValue;
        vg.CopyTo00(vg0);
        break;
      }
    }
  }

  // Lemma 1.2:
  //    The procedure terminates finitely at a value of t for which the weak
  //    Wolfe conditions are satisfied. or The procedure does not terminate
  //    finitely, the parameter β is never set to a finite
  //        value, the parameter α becomes positive on the first iteration and
  //        is doubled in magnitude at every iteration thereafter, and f(x + td)
  //        ↓ −∞

  if (Iteration == maxIterations && std::isinf(beta))
    throw LdtException(ErrorType::kLogic, "newton",
                       "line search failed. f(x+td)->-inf");
}

void Newton::minimize(
    const std::function<Tv(const Matrix<Tv> &)> &function,
    const std::function<void(const Matrix<Tv> &, Matrix<Tv> &)> &gradient,
    const std::function<void(const Matrix<Tv> &, Matrix<Tv> &)> &hessian,
    Matrix<Tv> &x0, Tv *work) {

  Ti n = x0.length();

  Tv steplength;
  Ti pos = 0;
  Ti info = 0;
  bool checkFtol = TolFunction > 0;
  bool checkGtol = TolGradient > 0;

  auto d = Matrix<Tv>(&work[pos], n, 1);
  pos += n; // search direction
  Tv *Wline = &work[pos];
  pos += 2 * n;

  FunctionValue = function(x0);
  gradient(x0, Gradient);

  Tv pre_vf = FunctionValue;
  Iteration = 0;
  iter_line = 0;

  Ti iline = 0;
  while (true) {
    if (Iteration == IterationMax)
      break;

    if (checkGtol) {
      gnorm = norm2(Gradient);
      if (std::isnan(gnorm) || std::isinf(gnorm))
        throw LdtException(ErrorType::kLogic, "newton",
                           "NAN or INF in gradient/value of function");
      if (gnorm < TolGradient)
        break;
    }

    hessian(x0, Hessian); // calculate Hessian
    Gradient.CopyTo00(d);
    d.Multiply_in(-1.0);
    info = Hessian.SolvePos0(d, false);
    if (info != 0)
      throw LdtException(ErrorType::kLogic, "newton",
                         "solving for direction failed");

    if (UseLineSearch) {
      steplength = 1.0;
      wolfe_weak(function, gradient, d, x0, FunctionValue, Gradient, iline,
                 Wline, steplength, 1e-4, 0.9, 100);
      iter_line += iline;
    } else {
      x0.Add_in0(d);
      FunctionValue = function(x0);
    }

    if (checkFtol) {
      vf_diff = std::abs(pre_vf - FunctionValue);
      if (vf_diff < TolFunction)
        break;
    }
    pre_vf = FunctionValue;

    Iteration++;
  }
}

void Newton::Minimize(
    const std::function<Tv(const Matrix<Tv> &)> &function,
    const std::function<void(const Matrix<Tv> &, Matrix<Tv> &)> &gradient,
    const std::function<void(const Matrix<Tv> &, Matrix<Tv> &)> &hessian,
    Matrix<Tv> &x0, Tv *storage, Tv *work) {
  X = &x0;
  auto n = x0.length();
  if (n > mK)
    throw LdtException(ErrorType::kLogic, "newton", "inconsistent arguments");

  Gradient.SetData(storage, n, 1);
  Hessian.SetData(&storage[n], n, n);

  minimize(function, gradient, hessian, x0, work);
}

void Newton::Minimize2(
    const std::function<Tv(const Matrix<Tv> &)> &function,
    const std::function<void(const Matrix<Tv> &, Matrix<Tv> &)> &gradient,
    const std::function<void(const Matrix<Tv> &, Matrix<Tv> &)> &hessian,
    Matrix<Tv> &x0, Tv *storageG, Tv *storageH, Tv *work) {
  X = &x0;
  auto n = x0.length();
  if (n > mK)
    throw LdtException(ErrorType::kLogic, "newton", "inconsistent arguments");

  Gradient.SetData(storageG, n, 1);
  Hessian.SetData(storageH, n, n);

  minimize(function, gradient, hessian, x0, work);
}
