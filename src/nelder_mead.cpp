/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the LGPL 3.0.
 See accompanying file LICENSE
*/

#include "optimization.h"

using namespace ldt;

/*
The original code:
       Copyright (c) 1997 Michael F. Hutt
       Released under the MIT License
*/

NelderMead::NelderMead(Ti n) { WorkSize = (n + 1) * (n + 1) * 4 * n; }

// #pragma region functions

/* create the initial simplex */

void initialize_simplex(std::vector<Matrix<Tv>> &v, Matrix<Tv> &start, Tv scale,
                        Ti n) {
  Tv pn, qn; /* values used to create initial simplex */
  Ti i, j;

  pn = scale * (std::sqrt(n + 1) - 1 + n) / (n * std::sqrt(2));
  qn = scale * (std::sqrt(n + 1) - 1) / (n * std::sqrt(2));

  for (i = 0; i < n; i++) {
    v.at(0).Data[i] = start.Data[i];
  }

  for (i = 1; i <= n; i++) {
    for (j = 0; j < n; j++) {
      if (i - 1 == j) {
        v.at(i).Data[j] = pn + start.Data[j];
      } else {
        v.at(i).Data[j] = qn + start.Data[j];
      }
    }
  }
}

/* print out the initial values */

void print_initial_simplex(Tv **v, Tv *f, Ti n) {
  Ti i, j;

  // printf("Initial Values\n");
  for (j = 0; j <= n; j++) {
    for (i = 0; i < n; i++) {
      // printf("%f, ", v[j][i]);
    }
    // printf("value %f\n", f[j]);
  }
}

/* print out the value at each iteration */

void print_iteration(Tv **v, Tv *f, Ti n, Ti itr) {
  Ti i, j;

  // printf("Iteration %zd\n", itr);
  for (j = 0; j <= n; j++) {
    for (i = 0; i < n; i++) {
      // printf("%f %f\n", v[j][i], f[j]);
    }
  }
}

/* find the index of the largest value */

Ti vg_index(Matrix<Tv> &f, Ti vg, Ti n) {
  Ti j;
  for (j = 0; j <= n; j++) {
    if (f.Data[j] > f.Data[vg]) {
      vg = j;
    }
  }
  return vg;
}

/* find the index of the smallest value */

Ti vs_index(Matrix<Tv> &f, Ti vs, Ti n) {
  Ti j;
  for (j = 0; j <= n; j++) {
    if (f.Data[j] < f.Data[vs]) {
      vs = j;
    }
  }
  return vs;
}

/* find the index of the second largest value */

Ti vh_index(Matrix<Tv> &f, Ti vh, Ti vg, Ti n) {
  Ti j;
  for (j = 0; j <= n; j++) {
    if (f.Data[j] > f.Data[vh] && f.Data[j] < f.Data[vg]) {
      vh = j;
    }
  }
  return vh;
}

/* calculate the centroid */

void centroid(Matrix<Tv> &vm, std::vector<Matrix<Tv>> &v, Ti n, Ti vg) {
  Ti j, m;
  Tv cent;
  for (j = 0; j <= n - 1; j++) {
    cent = 0;
    for (m = 0; m <= n; m++) {
      if (m != vg) {
        cent += v.at(m).Data[j];
      }
    }
    vm.Data[j] = cent / n;
  }
}

// #pragma endregion

Tv NelderMead::Minimize(const std::function<Tv(const Matrix<Tv> &)> &objective,
                        Matrix<Tv> &start, Tv *work,
                        const std::function<void(Matrix<Tv> &)> *constrain) {

  Ti n = start.length();
  Ti vs; /* vertex with smallest value */
  Ti vh; /* vertex with next smallest value */
  Ti vg; /* vertex with largest value */
  Ti i, j, row;
  Ti k; /* track the number of function evaluations */
  Tv min;
  Tv fsum, favg;

  std::function<void(Matrix<Tv> &)> constrain0 = 0;
  if (constrain)
    constrain0 = *constrain;

  std::vector<Matrix<Tv>> v; /* holds vertices of simplex */
  Matrix<Tv> f;              /* value of function at each vertex */
  Tv fr;                     /* value of function at reflection point */
  Tv fe;                     /* value of function at expansion point */
  Tv fc;                     /* value of function at contraction point */
  Matrix<Tv> vr;             /* reflection - coordinates */
  Matrix<Tv> ve;             /* expansion - coordinates */
  Matrix<Tv> vc;             /* contraction - coordinates */
  Matrix<Tv> vm;             /* centroid - coordinates */

  /* allocate the rows of the arrays */
  Ti w = 0;
  v = std::vector<Matrix<Tv>>(n + 1);
  f = Matrix<Tv>(&work[w], n + 1);
  w += n + 1;
  vr = Matrix<Tv>(&work[w], n);
  w += n;
  ve = Matrix<Tv>(&work[w], n);
  w += n;
  vc = Matrix<Tv>(&work[w], n);
  w += n;
  vm = Matrix<Tv>(&work[w], n);
  w += n;

  /* allocate the columns of the arrays */
  for (i = 0; i <= n; i++) {
    v[i] = Matrix<Tv>(&work[w], n);
    w += n;
  }

  /* create the initial simplex */
  initialize_simplex(v, start, Scale, n);

  /* impose constraints */
  if (constrain) {
    for (j = 0; j <= n; j++) {
      constrain0(v[j]);
    }
  }

  /* find the initial function values */
  for (j = 0; j <= n; j++) {
    f.Data[j] = objective(v[j]);
  }
  k = n + 1;

  /* print out the initial values */
  // print_initial_simplex(v, f, n);

  /* begin the main loop of the minimization */
  for (Iter = 1; Iter <= MaxIterations; Iter++) {
    /* find the index of the largest value */
    vg = vg_index(f, (Ti)0, n);

    /* find the index of the smallest value */
    vs = vs_index(f, (Ti)0, n);

    /* find the index of the second largest value */
    vh = vh_index(f, vs, vg, n);

    /* calculate the centroid */
    centroid(vm, v, n, vg);

    /* reflect vg to new vertex vr */
    for (j = 0; j <= n - 1; j++) {
      /*vr[j] = (1+ALPHA)*vm[j] - ALPHA*v[vg][j];*/
      vr.Data[j] = vm.Data[j] + Alpha * (vm.Data[j] - v[vg].Data[j]);
    }
    if (constrain) {
      constrain0(vr);
    }
    fr = objective(vr);
    k++;

    if (fr < f.Data[vh] && fr >= f.Data[vs]) {
      for (j = 0; j <= n - 1; j++) {
        v[vg].Data[j] = vr.Data[j];
      }
      f.Data[vg] = fr;
    }

    /* investigate a step further in this direction */
    if (fr < f.Data[vs]) {
      for (j = 0; j <= n - 1; j++) {
        /*ve[j] = GAMMA*vr[j] + (1-GAMMA)*vm[j];*/
        ve.Data[j] = vm.Data[j] + Gamma * (vr.Data[j] - vm.Data[j]);
      }
      if (constrain) {
        constrain0(ve);
      }
      fe = objective(ve);
      k++;

      /*
     by making fe < fr as opposed to fe < f[vs],
     Rosenbrocks function takes 63 iterations as opposed
     to 64 when using Tv variables.
      */

      if (fe < fr) {
        for (j = 0; j <= n - 1; j++) {
          v[vg].Data[j] = ve.Data[j];
        }
        f.Data[vg] = fe;
      } else {
        for (j = 0; j <= n - 1; j++) {
          v[vg].Data[j] = vr.Data[j];
        }
        f.Data[vg] = fr;
      }
    }

    /* check to see if a contraction is necessary */
    if (fr >= f.Data[vh]) {
      if (fr < f.Data[vg] && fr >= f.Data[vh]) {
        /* perform outside contraction */
        for (j = 0; j <= n - 1; j++) {
          /*vc[j] = BETA*v[vg][j] + (1-BETA)*vm[j];*/
          vc.Data[j] = vm.Data[j] + Beta * (vr.Data[j] - vm.Data[j]);
        }
        if (constrain != NULL) {
          constrain0(vc);
        }
        fc = objective(vc);
        k++;
      } else {
        /* perform inside contraction */
        for (j = 0; j <= n - 1; j++) {
          /*vc[j] = BETA*v[vg][j] + (1-BETA)*vm[j];*/
          vc.Data[j] = vm.Data[j] - Beta * (vm.Data[j] - v[vg].Data[j]);
        }
        if (constrain) {
          constrain0(vc);
        }
        fc = objective(vc);
        k++;
      }

      if (fc < f.Data[vg]) {
        for (j = 0; j <= n - 1; j++) {
          v[vg].Data[j] = vc.Data[j];
        }
        f.Data[vg] = fc;
      }

      else {
        /*
           at this point the contraction is not successful,
           we must halve the Distance from vs to all the
           vertices of the simplex and then continue.
           1997-10-31 - modified to account for ALL vertices.
        */

        for (row = 0; row <= n; row++) {
          if (row != vs) {
            for (j = 0; j <= n - 1; j++) {
              v[row].Data[j] =
                  v[vs].Data[j] + (v[row].Data[j] - v[vs].Data[j]) / (Tv)2;
            }
          }
        }

        /* re-evaluate all the vertices */
        for (j = 0; j <= n; j++) {
          f.Data[j] = objective(v[j]);
        }

        /* find the index of the largest value */
        vg = vg_index(f, (Ti)0, n);

        /* find the index of the smallest value */
        vs = vs_index(f, (Ti)0, n);

        /* find the index of the second largest value */
        vh = vh_index(f, vs, vg, n);

        if (constrain) {
          constrain0(v[vg]);
        }
        f.Data[vg] = objective(v[vg]);
        k++;
        if (constrain != NULL) {
          constrain0(v[vh]);
        }
        f.Data[vh] = objective(v[vh]);
        k++;
      }
    }

    /* print out the value at each iteration */
    /*print_iteration(v,f,n,itr);*/

    /* test for convergence */
    fsum = 0;
    for (j = 0; j <= n; j++) {
      fsum += f.Data[j];
    }
    favg = fsum / (n + 1);
    Std = 0;
    for (j = 0; j <= n; j++) {
      Std += pow((f.Data[j] - favg), (Tv)2) / (n);
    }
    Std = std::sqrt(Std);
    if (Std < Epsilon)
      break;
  }
  /* end main loop of the minimization */

  /* find the index of the smallest value */
  vs = vs_index(f, (Ti)0, n);

  /*printf("The minimum was found at\n"); */
  for (j = 0; j < n; j++) {
    /*printf("%e\n",v[vs][j]);*/
    start.Data[j] = v[vs].Data[j];
  }
  min = objective(v[vs]);
  k++;
  // printf("%d Function Evaluations\n", k);
  // printf("%d Iterations through program\n", itr);

  Min = min;
  Iter--;
  return min;
}
