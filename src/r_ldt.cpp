#include "r_ldt.h"

using namespace Rcpp;
using namespace ldt;

std::unique_ptr<double[]>
CombineEndoExo(bool printMsg, ldt::Matrix<double> &result,
               std::vector<std::string> &colNames, ldt::Matrix<double> &my,
               ldt::Matrix<double> &mx, ldt::Matrix<double> &mw,
               ldt::Matrix<double> &mnewX, SEXP &y, SEXP &x, SEXP &w,
               SEXP &newX, bool removeNan, bool addIntercept, int minExpectedY,
               int minExpectedX, int minExpectedW, int minExpectedNewX,
               bool appendNewX) {
  if (minExpectedY == 0 && minExpectedX == 0)
    throw std::logic_error("Combining two null matrices is not implemented.");

  double *y_ = nullptr;
  double *x_ = nullptr;
  double *w_ = nullptr;
  double *newX_ = nullptr;
  int yCols = 0, xCols = 0, yRows = 0, xRows = 0, wRows = 0, newxRows = 0,
      newxCols = 0;

  if (minExpectedY > 0 && y == R_NilValue)
    throw std::logic_error("Invalid 'y'. It is empty.");
  if (minExpectedX > 0 && x == R_NilValue)
    throw std::logic_error("Invalid 'x'. It is empty.");
  if (minExpectedW > 0 && y == R_NilValue)
    throw std::logic_error("Invalid 'w'. It is empty.");

  NumericMatrix y0 = internal::convert_using_rfunction(y, "as.matrix");

  if (y0.nrow() == 0 && y0.ncol() == 0)
    throw std::logic_error("Invalid data: 'y' is empty.");
  y_ = &y0[0];
  yCols = y0.ncol();
  yRows = y0.nrow();
  // names
  SEXP ynames = colnames(y0);

  if (ynames != R_NilValue) {
    StringVector ynames0(ynames);
    for (int i = 0; i < ynames0.length(); i++)
      colNames.push_back(as<std::string>(ynames0[i]));
  } else
    for (int i = 0; i < yCols; i++)
      colNames.push_back(std::string("Y") + std::to_string(i + 1));

  if (w != R_NilValue) {
    NumericVector w0 = internal::convert_using_rfunction(w, "as.numeric");
    w_ = &w0[0];
    wRows = w0.length();
    colNames.push_back("Weight");
  }

  if (addIntercept) {
    colNames.push_back("Intercept");
    if (x == R_NilValue)
      x = NumericMatrix(yRows, 0);
  }

  if (x != R_NilValue) {
    NumericMatrix x0 = internal::convert_using_rfunction(x, "as.matrix");
    if (x0.nrow() == 0 && x0.ncol() == 0)
      throw std::logic_error("Invalid data: 'x' is empty.");
    x_ = &x0[0];
    xCols = x0.ncol();
    xRows = x0.nrow();
    // names
    if (xCols > 0) {
      SEXP xnames = colnames(x0);
      if (xnames != R_NilValue) {
        StringVector xnames0(xnames);
        for (int i = 0; i < xnames0.length(); i++)
          colNames.push_back(as<std::string>(xnames0[i]));
      } else
        for (int i = 0; i < xCols; i++)
          colNames.push_back(std::string("X") + std::to_string(i + 1));
    }

    // handle new data if x exists
    if (newX != R_NilValue) {

      NumericMatrix newX0 =
          internal::convert_using_rfunction(newX, "as.matrix");

      newX_ = &newX0[0];
      newxCols = newX0.ncol();
      newxRows = newX0.nrow();

      if (newxCols != xCols + (int)addIntercept) {

        Rcout << "Number of columns in:\n    - newX = " << newxCols
              << "\n    - x = " << (xCols + (int)addIntercept) << "\n";
        throw std::logic_error("Invalid number of columns in 'newX'. It must "
                               "be equal to the number of columns in 'x'.");
      }
    }

    if (newxRows < minExpectedNewX) {
      Rcout << "Required number of new exogenous data = " << minExpectedNewX;
      throw std::logic_error("There is not enough new exogenous data.");
    }
  }

  if (y != R_NilValue && x != R_NilValue) {
    if (yRows != xRows)
      throw std::logic_error(
          "Invalid data. Different number of observations in 'x' and 'y'.");
  }
  if (y != R_NilValue && w != R_NilValue) {
    if (yRows != wRows)
      throw std::logic_error(
          "Invalid data. Different number of observations in 'w' and 'y'.");
  }

  mx.SetData(x_, xRows, xCols);
  my.SetData(y_, yRows, yCols);
  if (w_)
    mw.SetData(w_, wRows, 1);
  mnewX.SetData(newX_, newxRows, newxCols);

  if (printMsg) {
    Rprintf("Data:\n");
    Rprintf("    - Number of 'Observations' = %i\n", my.RowsCount);
    Rprintf("    - Number of 'Endogenous' = %i\n", my.ColsCount);
    Rprintf("    - Number of 'Weight' = %i\n", mw.ColsCount);
    Rprintf("    - Number of 'Exogenous' = %i\n", mx.ColsCount);
    Rprintf("    - Adds 'Intercept' = %s\n", addIntercept ? "true" : "false");
  }

  int mat_rows = my.RowsCount + (appendNewX ? mnewX.RowsCount : 0);
  int mat_cols = mx.ColsCount + my.ColsCount + mw.ColsCount + (int)addIntercept;
  auto mat_data = std::unique_ptr<double[]>(new double[mat_rows * mat_cols]);
  result.SetData(NAN, mat_data.get(), mat_rows, mat_cols);
  int j = 0;
  if (y_) {
    result.SetSub(0, j, my, 0, 0, my.RowsCount, my.ColsCount);
    j += my.ColsCount;
  }

  // Rcout<< "\n\nResult after y:" << result.ToString('\t','\n',2) << "\n\n";

  if (w_) {
    result.SetSub(0, j, mw, 0, 0, mw.RowsCount, mw.ColsCount);
    j += mw.ColsCount;
  }
  if (addIntercept) {
    result.SetColumn(j, 1);
    j++;
  }

  // Rcout<< "\n\nResult after intercept:" << result.ToString('\t','\n',2) <<
  // "\n\n";

  if (x_) {
    result.SetSub(0, j, mx, 0, 0, mx.RowsCount, mx.ColsCount);

    // Rcout<< "\n\nResult after x:" << result.ToString('\t','\n',2) << "\n\n";

    if (appendNewX) {
      result.SetSub(mx.RowsCount,
                    addIntercept ? j - 1 : j, // newX has intercept
                    mnewX, 0, 0, mnewX.RowsCount, mnewX.ColsCount);
    }
  }

  if (removeNan) {
    auto dataset =
        ldt::Dataset<double>(result.RowsCount, result.ColsCount, true, false);
    auto mat_data0 = std::unique_ptr<double[]>(new double[dataset.StorageSize]);
    dataset.Calculate(result, nullptr, mat_data0.get());

    if (dataset.Result.RowsCount != result.RowsCount) {
      result.SetData(dataset.Result.Data, dataset.Result.RowsCount,
                     dataset.Result.ColsCount);
      if (printMsg)
        Rprintf("    - Number of 'Observations' (NAN Removed) = %i\n",
                dataset.Result.RowsCount);
      mat_data = std::move(mat_data0);
    }
  }

  if (printMsg) { // TODO: print a summary
    // Rcout<< "\n---\n" << result.ToString_R_Matrix(2) << "\n---\n";
  }

  return mat_data;
}

void GetSizes(bool printMsg, std::vector<int> &result, SEXP &sizes,
              int variableCount, bool isX) {

  if (sizes == R_NilValue)
    result.push_back(1);
  else {
    IntegerVector sizes_ =
        internal::convert_using_rfunction(sizes, "as.integer");
    for (int i = 0; i < sizes_.length(); i++)
      result.push_back((sizes_[i]));
  }
  if (result.size() == 0 ||
      *std::min_element(result.begin(), result.end()) < 1 ||
      *std::max_element(result.begin(), result.end()) > variableCount)
    throw std::logic_error(
        "Invalid sizes array. It cannot be empty and elements must larger than "
        "1 and less than the number of variables.");

  if (printMsg) {
    if (isX)
      Rprintf("Exogenous Sizes=%s\n", VectorToCsv<int>(result, ',').c_str());
    else
      Rprintf("Endogenous Sizes=%s\n", VectorToCsv<int>(result, ',').c_str());
  }
}

void GetPartitions(bool printMsg, std::vector<std::vector<int>> &result,
                   SEXP &partitions, int variableCount, int adjustPos,
                   bool isX) {

  if (partitions != R_NilValue) {
    List partitions0 =
        (List)internal::convert_using_rfunction(partitions, "as.list");
    for (int i = 0; i < partitions0.length(); i++) {
      IntegerVector par_i = (IntegerVector)internal::convert_using_rfunction(
          partitions0[i], "as.integer");
      auto ar = std::vector<int>();
      for (int j = 0; j < par_i.length(); j++) {
        ar.push_back(par_i[j] + adjustPos -
                     1); // adjust elements for zero-based indexing and their
                         // position in bind data
      }
      result.push_back(ar);
    }
  } else {
    for (int i = adjustPos; i < adjustPos + variableCount; i++)
      result.push_back(std::vector<int>({i}));
  }

  for (auto &p : result) {
    for (auto &pi : p) {
      if (pi > adjustPos + variableCount) {
        Rcout << "Position Adjustment =" << adjustPos
              << "\nNumber of Variables = " << variableCount
              << "\nIndex of Element = " << pi << "\n";
        throw std::logic_error(
            "Invalid element in a partition. Elements cannot be larger than "
            "the number of variables.");
      }
    }
  }

  if (printMsg) {
    if (isX)
      Rprintf("Number of Exogenous Partitons=%i\n", (int)result.size());
    else
      Rprintf("Number of Endogenous Partitons=%i\n", (int)result.size());
    for (int i = 0; i < (int)result.size(); i++) {
      if (i < 10)
        Rprintf(" %i. Partition:%s\n", i,
                VectorToCsv<int>(result.at(i), ',').c_str());
      else if (i == 10)
        Rprintf("     . . .\n");
    }
  }
}

void GetGroups(bool printMsg, std::vector<std::vector<int>> &result,
               SEXP &groups, int variableCount, int adjustPos, bool isX) {

  if (groups != R_NilValue) {
    List groups0 = (List)internal::convert_using_rfunction(groups, "as.list");
    for (int i = 0; i < groups0.length(); i++) {
      IntegerVector gro_i = (IntegerVector)internal::convert_using_rfunction(
          groups0[i], "as.integer");
      auto ar = std::vector<int>();
      for (int j = 0; j < gro_i.length(); j++) {
        ar.push_back(gro_i[j] + adjustPos -
                     1); // adjust elements for zero-based indexing and their
                         // position in bind data
      }
      result.push_back(ar);
    }
  } else if (variableCount >
             0) { // just one element (TODO: combination as an option)
    auto os = std::vector<int>(variableCount);
    std::iota(os.begin(), os.end(), adjustPos);
    result.push_back(os);
  }

  if (printMsg) {
    if (isX)
      Rprintf("Number of Exogenous Groups=%i\n", (int)result.size());
    else
      Rprintf("Number of Endogenous Groups=%i\n", (int)result.size());
    for (int i = 0; i < (int)result.size(); i++) {
      if (i < 10)
        Rprintf(" %i. Group:%s\n", i,
                VectorToCsv<int>(result.at(i), ',')
                    .c_str()); // TODO: print the names, indices are misleading
      else if (i == 10)
        Rprintf("     . . .\n");
    }
  }

  for (auto &a : result) {
    for (auto &b : a)
      if (b > variableCount + adjustPos || b + adjustPos < 0) {
        Rcout << "---------------\n";
        Rcout << "Position Adjustment = " << adjustPos << "\n";
        Rcout << "Element of a Groups = " << b << "\n";
        throw std::logic_error("Invalid variable group. An element is negative "
                               "or larger than the number of variables.");
      }
  }
}

void UpdateOptions(bool printMsg, List &searchItems, List &measureOptions,
                   List &modelCheckItems, SearchMeasureOptions &res_measure,
                   SearchItems &res_items, SearchModelChecks &res_checks,
                   std::vector<std::string> &measuresNames, int length1,
                   int exoCount, int numTargets, int numDependents,
                   bool isTimeSeries, bool type1NeedsModelEstim,
                   const char *length1Informtion, bool isDc) {

  CheckMeasureOptions(measureOptions);
  if (as<int>(measureOptions["simFixSize"]) == 0)
    measureOptions["typesOut"] = List();
  auto molist = as<List>(measureOptions["typesOut"]);
  if (molist.length() == 0) {
    measureOptions["simFixSize"] = 0;
    measureOptions["trainFixSize"] = 0;
    measureOptions["trainRatio"] = 0;
  }

  CheckSearchItems(searchItems);
  CheckModelCheckItems(modelCheckItems);

  UpdateMeasureOptions(printMsg, measureOptions, res_measure, measuresNames,
                       isTimeSeries, isDc);

  UpdateSearchItems(printMsg, searchItems, res_items, length1, 0,
                    length1Informtion, nullptr, type1NeedsModelEstim, false);

  UpdateModelCheckItems(printMsg, modelCheckItems, res_checks, res_measure,
                        res_items);

  res_items.LengthTargets = numTargets; // Modelset will use it
  if (printMsg)
    Rprintf("Number of Targets=%i\n", numTargets);
  res_items.LengthDependents = numDependents;
  res_items.LengthExogenouses = exoCount;
}

std::vector<std::string> GetDefaultColNames(std::string pre, int length) {
  std::vector<std::string> nms;
  for (int i = 1; i <= length; i++)
    nms.push_back(pre + std::to_string(i));
  return nms;
}

NumericMatrix as_matrix(ldt::Matrix<double> &mat,
                        std::vector<std::string> *rowNames,
                        std::vector<std::string> *colNames) {
  NumericMatrix res = NumericMatrix(mat.RowsCount, mat.ColsCount, mat.Data);
  if (rowNames) {
    if ((int)rowNames->size() != mat.RowsCount) {
      Rcout << "---------------\n";
      Rcout << "Number of Rows: " << mat.RowsCount << "\n";
      Rcout << "Row Names:" << VectorToCsv(*rowNames) << "\n";
      throw std::logic_error(std::string("Invalid number of rows/row_names."));
    }
    rownames(res) = wrap(*rowNames);
  }
  if (colNames) {
    if ((int)colNames->size() != mat.ColsCount) {
      Rcout << "---------------\n";
      Rcout << "Number of Columns: " << mat.ColsCount << "\n";
      Rcout << "Column Names:" << VectorToCsv(*colNames) << "\n";
      throw std::logic_error(
          std::string("Invalid number of columns/col_names."));
    }
    colnames(res) = wrap(*colNames);
  }
  return res;
}

NumericVector as_vector(ldt::Matrix<double> &vec,
                        std::vector<std::string> *names) {
  auto res = NumericVector(vec.Data, vec.Data + vec.length());
  if (names) {
    if ((int)names->size() != vec.length()) {
      Rcout << "names:" << VectorToCsv(*names);
      throw std::logic_error(std::string("Invalid number of elements/names."));
    }
    res.names() = wrap(*names);
  }
  return res;
}

IntegerMatrix as_imatrix(ldt::Matrix<int> &mat,
                         std::vector<std::string> *rowNames,
                         std::vector<std::string> *colNames) {
  auto res = IntegerMatrix(mat.RowsCount, mat.ColsCount, mat.Data);
  if (rowNames) {
    if ((int)rowNames->size() != mat.RowsCount) {
      Rcout << "Row names:" << VectorToCsv(*rowNames);
      throw std::logic_error(std::string("Invalid number of rows/row_names."));
    }
    rownames(res) = wrap(*rowNames);
  }
  if (colNames) {
    if ((int)colNames->size() != mat.ColsCount) {
      Rcout << "Column names:" << VectorToCsv(*colNames);
      throw std::logic_error(
          std::string("Invalid number of columns/col_names."));
    }
    colnames(res) = wrap(*colNames);
  }
  return res;
}

IntegerVector as_ivector(ldt::Matrix<int> &vec,
                         std::vector<std::string> *names) {
  auto res = IntegerVector(vec.Data, vec.Data + vec.length());
  if (names) {
    if ((int)names->size() != vec.length()) {
      Rcout << "names:" << VectorToCsv(*names);
      throw std::logic_error(std::string("Invalid number of elements/names."));
    }
    res.names() = wrap(*names);
  }
  return res;
}

NumericMatrix cbind_matrix(NumericMatrix a, NumericMatrix b) {
  int n1 = a.ncol();
  int n2 = b.ncol();
  NumericMatrix result = no_init_matrix(a.nrow(), n1 + n2);
  StringVector nms(n1 + n2);
  StringVector anms = colnames(a);
  StringVector bnms = colnames(b);
  for (int i = 0; i < n1 + n2; i++) {
    if (i < n1) {
      result(_, i) = a(_, i);
      nms[i] = anms[i];
    } else {
      result(_, i) = b(_, i - n1);
      nms[i] = bnms[i - n1];
    }
  }

  colnames(result) = nms;
  return result;
}

NumericMatrix insert_intercept(SEXP mat) {
  StringVector nms;
  NumericMatrix result;

  NumericMatrix a = internal::convert_using_rfunction(mat, "as.matrix");
  int n1 = a.ncol();
  result = no_init_matrix(a.nrow(), n1 + 1);
  nms = StringVector(n1 + 1);
  StringVector anms = colnames(a);
  for (int i = 0; i < 1 + n1; i++) {
    if (i < 1) {
      result(_, i) = rep(1, a.nrow());
      nms[i] = "Intercept";
    } else {
      result(_, i) = a(_, i - 1);
      if (anms.length() > 0)
        nms[i] = anms[i - 1];
    }
  }

  colnames(result) = nms;
  return result;
}

NumericMatrix cbind_vectormatrix(NumericVector a, NumericMatrix b,
                                 std::string vectorName) {

  int n2 = b.ncol();
  NumericMatrix result = no_init_matrix(a.length(), 1 + n2);
  StringVector nms(1 + n2);
  StringVector bnms = colnames(b);
  for (int i = 0; i < 1 + n2; i++) {
    if (i < 1) {
      result(_, i) = a;
      nms[i] = vectorName;
    } else {
      result(_, i) = b(_, i - 1);
      nms[i] = bnms[i - 1];
    }
  }

  colnames(result) = nms;
  return result;
}

void ReportProgress(bool printMsg, int reportInterval, ModelSet &model,
                    bool &estimating, SearchOptions &options) {
  auto start = std::chrono::system_clock::now();
  if (printMsg)
    Rprintf("Calculations Started ...\n");
  int c = 0;
  auto alli = model.GetExpectedNumberOfModels();
  if (printMsg)
    Rprintf("Expected Number of Models = %i\n", alli);
  double all = (double)alli;
  int i = 0;
  while (estimating) {

    std::this_thread::sleep_for(std::chrono::seconds(1));
    // boost::this_thread::sleep_for(boost::chrono::milliseconds(1000));

    try {
      Rcpp::checkUserInterrupt();
    } catch (...) {
      options.RequestCancel = true;
      throw;
    }

    i++;
    if (reportInterval == 0 || i <= reportInterval)
      continue;
    i = 0;

    auto now = std::chrono::system_clock::now();
    double prePecentage = -1;

    c = model.GetNumberOfEstimatedModels();
    double percentage = std::round((c / all) * 10000) / 100;
    if (percentage != prePecentage) {
      std::chrono::duration<double> elapsed_mins = (now - start) / 60.0;
      double remains_mins = (all - c) * elapsed_mins.count() / c;
      if (printMsg)
        Rprintf("    Searched=%i, All=%i  (%.2f%%, %.1f minutes remains)\n", c,
                alli, percentage > 100 || percentage < 0 ? NAN : percentage,
                remains_mins < 0 ? NAN : remains_mins);
      prePecentage = percentage;
    }
  }

  if (options.RequestCancel)
    throw std::logic_error("Calculations is canceled.");
  else if (printMsg)
    Rprintf("Calculations Ended.\n");
}

static void add_CoefInfo(List &L, std::vector<EstimationKeep *> &list,
                         const char *extra1Label, int exoIndexesPlus,
                         std::vector<std::string> *extra1Names,
                         const char *label, bool addCoefs,
                         std::vector<std::string> &nms, int startIndex = 0) {

  int j = -1;
  for (auto &b : list) {
    j++;
    nms.push_back(std::string(label) + std::to_string(j + 1));

    IntegerVector deps;
    IntegerVector exos;
    if (b->Dependents.Data) {
      deps = as_ivector(b->Dependents);
      deps = deps + 1;
    }
    if (b->Exogenouses.Data) {
      exos = as_ivector(b->Exogenouses);
      exos = exos + exoIndexesPlus;
    }

    List L_j = List::create(
        _["weight"] = b->Weight,
        _["depIndices"] = b->Dependents.Data ? (SEXP)deps : R_NilValue,
        _["exoIndices"] = b->Exogenouses.Data ? (SEXP)exos : R_NilValue,
        _["mean"] = addCoefs ? wrap(b->Mean) : R_NilValue,
        _["var"] = addCoefs ? wrap(b->Variance) : R_NilValue,
        _[extra1Label] =
            b->Extra.Data ? (SEXP)as_ivector(b->Extra) : R_NilValue);

    if (L_j[5] != R_NilValue)
      ((IntegerVector)L_j[5]).names() = wrap(*extra1Names);

    L[j + startIndex] = L_j;
  }
  L.names() = wrap(nms);
}

static void add_Lengthi(List L, int eIndex, int tIndex, ModelSet &model,
                        std::vector<SearcherSummary *> &list,
                        SearchItems &searchItems, const char *extra1Label,
                        int length1, std::vector<std::string> &length1Names,
                        std::vector<std::string> *extra1Names,
                        int exoIndexesPlus,
                        const char *length1_itemlabel = "item") {

  if (searchItems.KeepBestCount > 0) {
    List L_0 = List(length1);
    std::vector<std::string> L_0_names;
    for (auto i = 0; i < length1; i++) {

      auto L_0_i = List(searchItems.KeepBestCount + 1);
      std::vector<std::string> L_0_i_names;
      L_0_i_names.push_back(std::string("name"));

      L_0_i[0] = wrap(length1Names.at(i));

      auto bests = std::vector<EstimationKeep *>();
      model.CombineBests(eIndex, tIndex, i, list, bests);

      if (bests.size() != 0) {
        add_CoefInfo(L_0_i, bests, extra1Label, exoIndexesPlus, extra1Names,
                     "best", true, L_0_i_names, 1);
        L_0[i] = L_0_i;
      } else {
        L_0[i] = R_NilValue;
      }
      L_0_names.push_back(std::string(length1_itemlabel) +
                          std::to_string(i + 1));
    }

    L_0.names() = L_0_names;
    L[0] = L_0;
  } else
    L[0] = R_NilValue;

  // All ? do we really need all

  if (searchItems.CdfsAt.size() > 0) {

    List L_1 = List(searchItems.CdfsAt.size());
    std::vector<std::string> L_1_names;

    for (int k = 0; k < (int)searchItems.CdfsAt.size(); k++) {
      auto cdf = RunningWeightedMean();
      auto mat_d = std::unique_ptr<double[]>(new double[length1 * 3]);
      auto mat = ldt::Matrix<double>(mat_d.get(), length1, 3);
      auto colnames = std::vector<std::string>({"Mean", "Count", "SumWeights"});
      for (auto i = 0; i < length1; i++) {
        model.CombineCdfAt(eIndex, tIndex, i, k, list, cdf);
        mat.Set(i, 0, cdf.GetMean());
        mat.Set(i, 1, (double)cdf.GetCount());
        mat.Set(i, 2, cdf.GetSumOfWeights());
      }
      L_1[k] = as_matrix(mat, &length1Names, &colnames);
      L_1_names.push_back(std::string("cdf") + std::to_string(k + 1));
    }
    L_1.names() = L_1_names;
    L[1] = L_1;
  } else
    L[1] = R_NilValue;

  if (searchItems.ExtremeBoundsMultiplier > 0) {
    double min = 0, max = 0;
    auto mat_d = std::unique_ptr<double[]>(new double[length1 * 2]);
    auto mat = ldt::Matrix<double>(mat_d.get(), length1, 2);
    auto colnames = std::vector<std::string>({"Lower", "Upper"});
    for (auto i = 0; i < length1; i++) {
      model.CombineExtremeBounds(eIndex, tIndex, i, list, min, max);
      mat.Set(i, 0, min);
      mat.Set(i, 1, max);
    }

    L[2] = as_matrix(mat, &length1Names, &colnames);
  } else
    L[2] = R_NilValue;

  if (searchItems.KeepMixture) {
    auto mixture = RunningWeighted4();
    auto mat_d = std::unique_ptr<double[]>(new double[length1 * 6]);
    auto mat = ldt::Matrix<double>(mat_d.get(), length1, 6);
    auto colnames = std::vector<std::string>(
        {"Mean", "Variance", "Skewness", "Kurtosis", "Count", "SumWeights"});
    for (auto i = 0; i < length1; i++) {
      model.CombineMixture(eIndex, tIndex, i, list, mixture);
      mat.Set(i, 0, mixture.GetMean());
      mat.Set(i, 1, (double)mixture.GetVariancePopulation());
      mat.Set(i, 2, (double)mixture.GetSkewnessPopulation());
      mat.Set(i, 3, (double)mixture.GetKurtosisPopulation());
      mat.Set(i, 4, (double)mixture.GetCount());
      mat.Set(i, 5, (double)mixture.Sum());
    }
    L[3] = as_matrix(mat, &length1Names, &colnames);
  } else
    L[3] = R_NilValue;
}

List GetModelSetResults(ModelSet &model, SearchItems &searchItems,
                        std::vector<std::string> &measureNames, int length1,
                        const char *extra1Label,
                        std::vector<std::string> *extra1Names,
                        int exoIndexesPlus,
                        std::vector<std::string> &length1Names,
                        std::vector<std::string> &inclusionNames,
                        const char *length1Label,
                        const char *length1_itemlabel) {
  // output structure:

  std::vector<std::string> namesL;
  namesL.push_back("counts");
  for (auto eIndex = 0; eIndex < searchItems.LengthEvals; eIndex++)
    namesL.push_back(measureNames.at(eIndex));
  namesL.push_back("info");

  List L = List(1 + searchItems.LengthEvals + 1);
  L.names() = namesL;

  // general information:
  auto result = SearcherModelingInfo();
  auto list0 = std::vector<SearcherSummary *>();
  auto list1 = std::vector<SearcherSummary *>();
  auto list2 = std::vector<SearcherSummary *>();
  model.CombineInfo(result, list0, list1, list2);
  int fcount = 0;
  List failDetails = List(result.FailsCount.size());
  int i = -1;
  for (const auto &[k, v] : result.FailsCount) {
    i++;
    fcount += v;
    failDetails[i] = List::create(_["message"] = wrap(k), _["count"] = wrap(v));
  }
  L[0] = List::create(_["expectedCount"] = wrap(result.ExpectedCount),
                      _["searchedCount"] = wrap(result.SearchedCount),
                      _["failedCount"] = wrap(fcount),
                      _["failedDetails"] = wrap(failDetails));

  for (auto eIndex = 0; eIndex < searchItems.LengthEvals; eIndex++) {

    List L_i = List(searchItems.LengthTargets);
    std::vector<std::string> L_i_names;
    L[1 + eIndex] = L_i;

    for (auto tIndex = 0; tIndex < searchItems.LengthTargets; tIndex++) {
      L_i_names.push_back(std::string("target") + std::to_string(tIndex + 1));

      List L_i_t = List(3); // when item2 implemented -> 4
      L_i_t.names() = std::vector<std::string>({"name", "model", length1Label});
      L_i[tIndex] = L_i_t;

      L_i_t[0] = wrap(inclusionNames.at(
          tIndex)); // expecting target name in inclusion names

      if (searchItems.KeepModelEvaluations) {

        List L_i_t_m = List(3);
        L_i_t_m.names() =
            std::vector<std::string>({"bests", "all", "inclusion"});
        L_i_t[1] = L_i_t_m;

        if (searchItems.KeepBestCount > 0) {
          auto bests = std::vector<EstimationKeep *>();
          model.CombineBests(eIndex, tIndex, 0, list0, bests);
          List L_i_t_m_0 = List(bests.size());
          std::vector<std::string> nms;
          add_CoefInfo(L_i_t_m_0, bests, extra1Label, exoIndexesPlus,
                       extra1Names, "best", false, nms, 0);
          L_i_t_m[0] = L_i_t_m_0;
        } else
          L_i_t_m[0] = R_NilValue;

        if (searchItems.KeepAll) {
          auto all = std::vector<EstimationKeep *>();
          model.CombineAll(eIndex, tIndex, 0, list0, all);
          List L_i_t_m_1 = List(all.size());
          std::vector<std::string> nms;
          add_CoefInfo(L_i_t_m_1, all, extra1Label, exoIndexesPlus, extra1Names,
                       "model", false, nms, 0);
          L_i_t_m[1] = L_i_t_m_1;
        } else
          L_i_t_m[1] = R_NilValue;

        if (searchItems.KeepInclusionWeights) {
          auto covars =
              searchItems.LengthDependents + searchItems.LengthExogenouses;
          auto incweights = RunningWeightedMean();
          auto mat_d = std::unique_ptr<double[]>(new double[covars * 2]);
          auto mat = ldt::Matrix<double>(mat_d.get(), covars, 2);
          auto colnames = std::vector<std::string>({"Mean", "Count"});
          for (auto i = 0; i < covars; i++) {
            model.CombineInclusionWeights(eIndex, tIndex, i, list0, incweights);
            mat.Set(i, 0, incweights.GetMean());
            mat.Set(i, 1, (double)incweights.GetCount());
          }
          L_i_t_m[2] = as_matrix(mat, &inclusionNames, &colnames);

        } else
          L_i_t_m[2] = R_NilValue;
      } else
        L_i_t[1] = R_NilValue; // model is null

      if (searchItems.Length1 > 0) {
        List L_i_t_1 = List(4);
        L_i_t_1.names() = std::vector<std::string>(
            {"bests", "cdfs", "extremeBounds", "mixture"});
        L_i_t[2] = L_i_t_1;

        add_Lengthi(L_i_t_1, eIndex, tIndex, model, list1, searchItems,
                    extra1Label, length1, length1Names, extra1Names,
                    exoIndexesPlus, length1_itemlabel);
        L_i_t[2] = L_i_t_1;
      } else
        L_i_t[2] = R_NilValue;

      if (searchItems.Length2 > 0) {
        throw std::logic_error("not implemented: length2>0");
      }
    }
    L_i.names() = L_i_names;
  }

  return L;
}
