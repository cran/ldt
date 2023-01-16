#include "r_ldt.h"

using namespace Rcpp;
using namespace ldt;

// clang-format off

//' Creates a Variable
//'
//' @param data Data of the variable
//' @param name Name of the variable
//' @param startFrequency Frequency of the first data-point. It is an \code{ldtf} object. See \code{F_?} functions.
//' @param fields Named list of any other fields
//'
//' @return An object of class \code{ldtv}.
//' @export
//' @examples
//' v1 = ldt::Variable(c(1,2,3,2,3,4,5),"V1",F_Monthly(2022,12),
//'      list(c("key1","value1"), c("key2", "value2")))
// [[Rcpp::export]]
List Variable(std::vector<double> data, std::string name, SEXP startFrequency,
              List fields)
// clang-format on
{
  List V =
      List::create(_["data"] = data, _["name"] = name,
                   _["startFrequency"] = startFrequency, _["fields"] = fields);
  V.attr("class") = std::vector<std::string>({"ldtv", "list"});
  return V;
}

std::unique_ptr<ldt::Variable<double>> GetVariableFromSEXP(Rcpp::List w) {

  auto v = new ldt::Variable<double>();
  auto uv = std::unique_ptr<ldt::Variable<double>>(v);

  v->Name = as<std::string>(w["name"]);

  try {
    std::vector<std::string> listItems;
    std::vector<boost::gregorian::date> listItemsDate;
    v->StartFrequency =
        GetFreqFromSEXP(w["startFrequency"], listItems, listItemsDate);
  } catch (...) {
    throw std::logic_error("Invalid 'startFrequency'.");
  }

  try {
    v->Data = as<std::vector<double>>(w["data"]);
  } catch (...) {
    throw std::logic_error(
        "Invalid 'data'. It should be a vector of numerics.");
  }

  try {
    List F = w["fields"];
    for (int j = 0; j < F.length(); j++) {
      CharacterVector Fj = as<CharacterVector>(F[j]);
      if (Fj.length() < 2)
        throw std::logic_error("Expected a 'key' and a 'value'.");
      v->Fields.insert({as<std::string>(Fj[0]), as<std::string>(Fj[1])});
    }
  } catch (...) {
    throw std::logic_error(
        "Invalid fields. It should be a 'List' of 'CharacterVector's. The "
        "vector must have 'Key-Value' pairs.");
  }

  return uv;
}

// clang-format off

//' Converts a Variable to String
//'
//' @param w The variable
//'
//' @return String representation of the variable in compact form
//' @export
// [[Rcpp::export]]
std::string VariableToString(List w)
// clang-format on
{
  auto uv = GetVariableFromSEXP(w);
  return uv.get()->ToString();
}

// clang-format off

//' Binds a List of Variables
//'
//' @param varList A list of variables ((i.e., \code{ldtv} objects)) with similar frequency class
//'
//' @return A matrix with variables in the columns and frequencies as the row names.
//' @export
//' @examples
//' v1 = ldt::Variable(c(1,2,3,2,3,4,5),"V1",F_Monthly(2022,12), list())
//' v2 = ldt::Variable(c(10,20,30,20,30,40,50),"V2",F_Monthly(2022,8), list())
//' vs = ldt::BindVariables(list(v1,v2))
// [[Rcpp::export]]
NumericMatrix BindVariables(SEXP varList)
// clang-format on
{
  auto vars = as<List>(varList);
  int n = vars.length();
  if (n == 0)
    throw std::logic_error("Empty list!");

  std::vector<std::unique_ptr<ldt::Variable<double>>> list0;
  std::vector<ldt::Variable<double> *> list;

  try {
    for (int i = 0; i < n; i++) {

      List w = as<List>(vars[i]);
      auto uv = GetVariableFromSEXP(w);
      auto v = uv.get();
      list.push_back(v);
      list0.push_back(std::move(uv));
    }
  } catch (...) {
    throw std::logic_error(
        "Creating a variable failed. Make sure all items of the list are "
        "'ldtv' objects.");
  }

  auto vs = Variables<double>(list);

  auto res = NumericMatrix(vs.NumObs, vs.Names.size(), vs.Data.begin());

  colnames(res) = wrap(vs.Names);

  auto f0 = vs.StartFrequency.get()->Clone();
  auto f = f0.get();
  std::vector<std::string> rns;
  for (int i = 0; i < vs.NumObs; i++) {
    rns.push_back(f->ToString());
    f->Next(1);
  }
  rownames(res) = wrap(rns);

  return res;
}
