#pragma once

#ifdef __cplusplus
extern "C" {
#endif

int scopy_(int *, float *, int *, float *, int *);
int dcopy_(int *, double *, int *, double *, int *);

float sdot_(int *, float *, int *, float *, int *);
double ddot_(int *, double *, int *, double *, int *);

int sgemv_(const char *, const int *, const int *, const float *, const float *,
           const int *, const float *, const int *, const float *, float *,
           const int *);
int dgemv_(const char *, const int *, const int *, const double *,
           const double *, const int *, const double *, const int *,
           const double *, double *, const int *);

int sgemm_(const char *, const char *, const int *, const int *, const int *,
           const float *, const float *, const int *, const float *,
           const int *, const float *, float *, const int *);
int dgemm_(const char *, const char *, const int *, const int *, const int *,
           const double *, const double *, const int *, const double *,
           const int *, const double *, double *, const int *);

int ssyrk_(const char *, const char *, const int *, const int *, const float *,
           const float *, const int *, const float *, float *, const int *);
int dsyrk_(const char *, const char *, const int *, const int *, const double *,
           const double *, const int *, const double *, double *, const int *);

int ssymm_(const char *, const char *, const int *, const int *, const float *,
           const float *, const int *, const float *, const int *,
           const float *, float *, const int *);
int dsymm_(const char *, const char *, const int *, const int *, const double *,
           const double *, const int *, const double *, const int *,
           const double *, double *, const int *);

#ifdef __cplusplus
}
#endif
