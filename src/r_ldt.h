// [[Rcpp::depends(BH)]]
// [[Rcpp::plugins(cpp17)]]

#pragma once

#include "helpers.h"
#include "matrix.h"
#include "pca.h"
#include "searchers.h"
#include <Rcpp.h>
#include <chrono>
#include <cstring>
#include <exception>
#include <future>
#include <iostream>
#include <sstream>
#include <string>
#include <thread>

#ifdef _OPENMP
#include <omp.h>
// [[Rcpp::plugins(openmp)]]
#endif

// #include <Rinternals.h>

using namespace Rcpp;

std::unique_ptr<double[]>
CombineEndoExo(bool printMsg, ldt::Matrix<double> &result,
               std::vector<std::string> &colNames, ldt::Matrix<double> &my,
               ldt::Matrix<double> &mx, ldt::Matrix<double> &mw,
               ldt::Matrix<double> &mnewX, SEXP &y, SEXP &x, SEXP &w,
               SEXP &newX, bool removeNan, bool addIntercept, int minExpectedY,
               int minExpectedX, int minExpectedW, int minExpectedNewX = 0,
               bool appendNewX = false);

void GetSizes(bool printMsg, std::vector<int> &result, SEXP &sizes,
              int variableCount, bool isX);

void GetPartitions(bool printMsg, std::vector<std::vector<int>> &result,
                   SEXP &partitions, int variableCount, int adjustPos = 0,
                   bool isX = true);

void GetGroups(bool printMsg, std::vector<std::vector<int>> &result,
               SEXP &groups, int variableCount, int adjustPos, bool isX);

void UpdateOptions(bool printMsg, List &searchItems, List &metricOptions,
                   List &modelCheckItems, ldt::SearchMetricOptions &res_metric,
                   ldt::SearchItems &res_items,
                   ldt::SearchModelChecks &res_checks,
                   std::vector<std::string> &metricsNames, int length1,
                   int exoCount, int numTargets, int numDependents,
                   bool isTimeSeries = false, bool type1NeedsModelEstim = true,
                   const char *length1Informtion = "Coefficients",
                   bool isDc = false);

NumericMatrix insert_intercept(NumericMatrix a);

NumericMatrix cbind_matrix(NumericMatrix a, NumericMatrix b);

NumericMatrix cbind_vectormatrix(NumericVector a, NumericMatrix b,
                                 std::string vectorName);

NumericMatrix as_matrix(ldt::Matrix<double> &mat,
                        std::vector<std::string> *rowNames = nullptr,
                        std::vector<std::string> *colNames = nullptr);

NumericVector as_vector(ldt::Matrix<double> &vec,
                        std::vector<std::string> *names = nullptr);

IntegerMatrix as_imatrix(ldt::Matrix<int> &mat,
                         std::vector<std::string> *rowNames = nullptr,
                         std::vector<std::string> *colNames = nullptr);

IntegerVector as_ivector(ldt::Matrix<int> &vec,
                         std::vector<std::string> *names = nullptr);

std::vector<std::string> GetDefaultColNames(std::string pre, int length);

void UpdateRocOptions(bool printMsg, List &rocOptionsR,
                      ldt::RocOptions &options, const char *startMsg);

void UpdatePcaOptions(bool printMsg, List pcaOptionsR, bool hasPca,
                      ldt::PcaAnalysisOptions &options, const char *startMsg);

void UpdateLbfgsOptions(bool printMsg, List &lbfgsOptions,
                        ldt::LimitedMemoryBfgsbOptions &options);

void UpdateNewtonOptions(bool printMsg, List &newtonR, ldt::Newton &newton);

void UpdateSearchItems(bool printMsg, List &searchItems,
                       ldt::SearchItems &items, Ti length1, Ti length2,
                       const char *length1Informtion,
                       const char *length2Informtion, bool type1NeedsModelEstim,
                       bool type2NeedsModelEstim);

void UpdateSearchOptions(List &searchOptions, ldt::SearchOptions &options,
                         int &reportInterval, bool &printMsg);

void UpdateModelCheckItems(bool printMsg, List &checkOptions,
                           ldt::SearchModelChecks &checks,
                           const ldt::SearchMetricOptions &metrics,
                           const ldt::SearchItems &items);

void UpdatemetricOptions(bool printMsg, List &metricOptions,
                         ldt::SearchMetricOptions &metrics,
                         std::vector<std::string> &metricNames,
                         bool isTimeSeries, bool isDc);

void ReportProgress(bool pringMsg, int reportInterval, ldt::ModelSet &model,
                    bool &estimating, ldt::SearchOptions &options,
                    int allCount);

List GetModelSetResults(ldt::ModelSet &model, ldt::SearchItems &searchItems,
                        std::vector<std::string> &metricNames, int length1,
                        const char *extra1Label,
                        std::vector<std::string> *extra1Names,
                        int exoIndexesPlus,
                        std::vector<std::string> &length1Names,
                        std::vector<std::string> &inclusionNames,
                        const char *length1Label,
                        const char *length1_itemlabel);
