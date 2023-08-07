
#include "clustering.h"
#include "r_ldt.h"

using namespace Rcpp;
using namespace ldt;

// [[Rcpp::export(.GetDistance)]]
NumericVector GetDistance(NumericMatrix data,
                          std::string distance = "correlation",
                          std::string correlation = "pearson",
                          bool checkNan = true) {

  boost::algorithm::to_lower(distance);
  boost::algorithm::to_lower(correlation);

  auto x = ldt::Matrix<double>(&data[0], data.nrow(), data.ncol());

  DistanceMethod distance0 = FromString_DistanceMethod(distance.c_str());
  CorrelationMethod correlation0 =
      FromString_CorrelationMethod(correlation.c_str());

  // group
  auto dista = DistanceBase::GetFromType(checkNan, distance0, correlation0,
                                         x.RowsCount, x.ColsCount);
  auto work = std::unique_ptr<double[]>(new double[dista->WorkSize]);
  auto storage = std::unique_ptr<double[]>(new double[dista->StorageSize]);
  dista.get()->Calculate(x, storage.get(), work.get());

  return NumericVector(dista.get()->Result.Data,
                       dista.get()->Result.Data +
                           dista.get()->Result.length_array());
}

// [[Rcpp::export(.ClusterH)]]
List ClusterH(NumericVector distances, std::string linkage) {

  double n = (1 + sqrt(1 + 8 * distances.length())) / 2;
  int numVariables = std::floor(n);
  if (std::abs(n - numVariables) > 1e-16 || numVariables <= 1)
    throw LdtException(ErrorType::kLogic, "R-clustering",
                       "distance vector should be the lower "
                       "triangle of a symmetric matrix");

  boost::algorithm::to_lower(linkage);
  HClusterLinkage linkage0 = FromString_HClusterLinkage(linkage.c_str());

  auto cluster = HClusterBase::GetFromType(linkage0, numVariables);
  auto mdistances = MatrixSym<false>(&distances[0], numVariables);
  cluster.get()->Calculate(mdistances);

  // lets send similar output to R
  auto heightsData = std::unique_ptr<Tv[]>(new Tv[numVariables - 1]);
  auto heights = ldt::Matrix<double>(heightsData.get(), numVariables - 1);
  auto mergeData = std::unique_ptr<int[]>(new int[2 * (numVariables - 1)]);
  auto merge = ldt::Matrix<int>(mergeData.get(), numVariables - 1, 2);
  auto order = std::vector<int>();
  cluster->MergeR(merge, heights, order);

  List L =
      List::create(_["merge"] = as_imatrix(merge),
                   _["height"] = as_vector(heights), _["order"] = wrap(order));

  return L;
}

// [[Rcpp::export(.ClusterHGroup)]]
List ClusterHGroup(NumericMatrix data, int nGroups = 2, double threshold = 0,
                   std::string distance = "correlation",
                   std::string linkage = "single",
                   std::string correlation = "pearson") {

  if (threshold < 0)
    throw LdtException(ErrorType::kLogic, "R-clustering",
                       "threshold cannot be negative");

  boost::algorithm::to_lower(distance);
  boost::algorithm::to_lower(linkage);
  boost::algorithm::to_lower(correlation);

  HClusterLinkage linkage0 = FromString_HClusterLinkage(linkage.c_str());
  DistanceMethod distance0 = FromString_DistanceMethod(distance.c_str());
  CorrelationMethod correlation0 =
      FromString_CorrelationMethod(correlation.c_str());

  auto x = ldt::Matrix<double>(&data[0], data.nrow(), data.ncol());

  // group
  auto group = GroupDataBase::GetFromType(linkage0, distance0, correlation0,
                                          x.RowsCount, x.ColsCount);
  auto work = std::unique_ptr<Tv[]>(new Tv[group->WorkSize]);

  group.get()->Calculate(x, work.get(), nGroups, threshold);

  if (group.get()->NaNDistanceFound)
    Rf_warning(
        "NA distance found and converted to zero. If you are using a "
        "correlation based distance, make sure variables are not constant");

  auto groups = std::vector<IntegerVector>();

  int min = INT_MAX;
  int max = INT_MIN;
  for (auto &g : group.get()->Groups) {
    for (int i = 0; i < (int)g->size(); i++)
      g->at(i) += 1; // R indexation
    groups.push_back(wrap(*g));

    int si = (int)g->size();
    if (min > si)
      min = (int)g->size();
    if (max < si)
      max = (int)g->size();
  }

  std::vector<int> vremoved;
  for (auto i : group.get()->Removed)
    vremoved.push_back(i + 1); // R indexation

  List L =
      List::create(_["groups"] = wrap(groups), _["removed"] = wrap(vremoved));

  return L;
}
