#ifndef LDT_BASE
#define LDT_BASE

#ifdef USE_FLOAT
#define Tv float
#else
#define Tv double
#endif
#ifdef USE_INTEGER64
#define Ti long long
#else
#define Ti int
#endif

#ifdef __cplusplus

#include <complex>
#include <cstring>
#include <memory>
#include <stdexcept>
#include <string>

// #define lapack_complex_float std::complex<float>
// #define lapack_complex_double std::complex<double>

#endif

typedef long int logical;
#define TRUE_ (1)
#define FALSE_ (0)

enum bool3 { False = -1, Null = 0, True = 1 };

#if defined _WIN32 || defined __CYGWIN__
#ifdef __GNUC__
#define LDT_EXPORT __attribute__((dllexport))
#else
#define LDT_EXPORT __declspec(dllexport)
#endif
#else
#if __GNUC__ >= 4
#define LDT_EXPORT __attribute__((visibility("default")))
#else
#define LDT_EXPORT
#error Unknown dynamic link export semantics.
#endif
#endif

#endif // LDT_BASE
