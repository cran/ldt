#pragma once

#include "blas.h"

#ifdef __cplusplus
extern "C" {
#endif

float slange_(char const *, int const *, int const *, float const *,
              int const *, float *);
double dlange_(char const *, int const *, int const *, double const *,
               int const *, double *);

void spotrf2_(char const *, int const *, float *, int const *, int *);
void dpotrf2_(char const *, int const *, double *, int const *, int *);

void dtrtrs_(char const *, char const *, char const *, int const *, int const *,
             double const *, int const *, double *, int const *, int *);

void strtrs_(char const *, char const *, char const *, int const *, int const *,
             float const *, int const *, float *, int const *, int *);

void sposv_(char const *, int const *, int const *, float *, int const *,
            float *, int const *, int *);
void dposv_(char const *, int const *, int const *, double *, int const *,
            double *, int const *, int *);

void sgesvd_(char const *jobu, char const *jobvt, int const *, int const *,
             float *, int const *, float *, float *, int const *, float *,
             int const *, float *, int const *, int *);
void dgesvd_(char const *jobu, char const *jobvt, int const *, int const *,
             double *, int const *, double *, double *, int const *, double *,
             int const *, double *, int const *, int *);

void sgetrf_(int const *, int const *, float *, int const *, int *, int *);
void dgetrf_(int const *, int const *, double *, int const *, int *, int *);

void sgetri_(int const *, float *, int const *, int const *, float *,
             int const *, int *);
void dgetri_(int const *, double *, int const *, int const *, double *,
             int const *, int *);

void sgetrf_(int const *, int const *, float *, int const *, int *, int *);
void dgetrf_(int const *, int const *, double *, int const *, int *, int *);

void sgeqrf_(int const *, int const *, float *, int const *, float *, float *,
             int const *, int *);
void dgeqrf_(int const *, int const *, double *, int const *, double *,
             double *, int const *, int *);

#ifdef __cplusplus
}
#endif
