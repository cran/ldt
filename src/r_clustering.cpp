
#include "clustering.h"
#include "r_ldt.h"

using namespace Rcpp;
using namespace ldt;

// clang-format off

//' Gets Distances Between Variables
//'
//' @param data (numeric matrix) Data with variables in the columns.
//' @param distance (string) Determines how distances are calculated. It can be \code{correlation}, \code{absCorrelation}, \code{euclidean}, \code{manhattan}, \code{maximum}.
//' @param correlation (string) If \code{distance} is correlation, it determines the type of the correlation. It can be \code{pearson}, \code{spearman}.
//' @param checkNan (bool) If false, \code{NAN}s are not omitted.
//'
//' @return A symmetric matrix (lower triangle as a vector).
//'
//' @export
// [[Rcpp::export]]
NumericVector GetDistance(NumericMatrix data,
                          std::string distance = "correlation",
                          std::string correlation = "pearson",
                          bool checkNan = true)
// clang-format on
{

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

// clang-format off

//' Hierarchical Clustering
//'
//'
//' @param distances (numeric vector) Determines the distances. This must be the lower triangle of a (symmetric) distance matrix (without the diagonal).
//' @param numVariables (int) Determines the number of variables. This should hold: '2 * length(\code{distances}) = \code{numVariables}(\code{numVariables} - 1)'.
//' @param linkage (string) Determines how Distances are calculated in a left-right node merge. It can be \code{single}, \code{complete}, \code{uAverage}, \code{wAverage}, \code{ward}.
//'
//' @return A list:
//' \item{merge}{(integer matrix)}
//' \item{height}{(numeric vector)}
//' \item{order}{(integer vector)}
//'
//' @export
// [[Rcpp::export]]
List ClusterH(NumericVector distances, int numVariables,
              std::string linkage = "single")
// clang-format on
{

  if (numVariables * (numVariables - 1) != 2 * distances.length())
    throw std::logic_error(
        "Invalid number of variables. '2 * distances.length() != numVariables "
        "* (numVariables - 1)'.");

  if (numVariables <= 1)
    throw std::logic_error(
        "Invalid number of variables. It must be larger than 1.");
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

// clang-format off

//' Groups Variables with Hierarchical Clustering
//'
//' @details The results might be different from R's 'cutree' function. I don't know how 'cutree' works, but here I iterate over the nodes and whenever a split occurs, I add a group until the required number of groups is reached.
//'
//' @param data (numeric matrix) Data with variables in the columns.
//' @param nGroups (int) Number of groups
//' @param threshold (double) A threshold for omitting variables. If distance between two variables in a group is less than this value, the second one will be omitted. Note that a change in the order of the columns might change the results.
//' @param distance (string)  Determines how distances are calculated. It can be \code{correlation}, \code{absCorrelation}, \code{euclidean}, \code{manhattan}, \code{maximum}.
//' @param linkage (string) Determines how Distances are calculated in a left-right node merge. It can be \code{single}, \code{complete}, \code{uAverage}, \code{wAverage}, \code{ward}.
//' @param correlation (string) If \code{distance} is correlation, it determines the type of the correlation. It can be \code{pearson}, \code{spearman}.
//'
//' @return A list:
//' \item{groups}{(List of integer vectors) indexes of variables in each group.}
//' \item{removed}{(integer vector) indexes of removed variables.}
//'
//' @export
// [[Rcpp::export]]
List ClusterHGroup(NumericMatrix data, int nGroups = 2, double threshold = 0,
                   std::string distance = "correlation",
                   std::string linkage = "single",
                   std::string correlation = "pearson")
// clang-format on
{

  if (threshold < 0)
    throw std::logic_error("Invalid threshold. It cannot be negative.");

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
        "correlation based distance, make sure variables are not constant.");

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
