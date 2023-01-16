
#include "variable.h"
#include <random>

using namespace ldt;

template <typename Tw>
Variables<Tw>::Variables(const std::vector<Variable<Tw> *> vars) {

  if (vars.size() == 0)
    throw std::logic_error("Variables: No variable is available.");

  auto minStarts = vars.at(0)->StartFrequency.get();
  auto temp = vars.at(0)->GetEndFrequency(); // we should define temp and then
                                             // .get() in a separate variable
  auto maxEnds = temp.get();
  std::unique_ptr<Frequency> temp1;

  for (auto const &v : vars) {
    try {
      if (minStarts->IsNewerThan(*v->StartFrequency.get()))
        minStarts = v->StartFrequency.get();
      temp1 = v->GetEndFrequency();
      auto e = temp1.get();
      if (maxEnds->IsOlderThan(*e))
        maxEnds = e;
    } catch (...) {
      Rethrow("Mixed frequency is not supported in 'Variables'.");
    }
    Names.push_back(v->Name);
  }
  StartFrequency = minStarts->Clone();
  NumObs = maxEnds->Minus(*minStarts);
  if (NumObs == 0)
    throw std::logic_error("Variables: No observation is available.");

  Data.resize(NumObs * vars.size());
  Ti i = 0;
  std::unique_ptr<Frequency> temp2;
  for (auto const &v : vars) {
    auto start = v->StartFrequency.get()->Minus(*minStarts);
    temp2 = v->GetEndFrequency();
    auto e = temp2.get();
    auto end = maxEnds->Minus(*e);
    for (Ti j = 0; j < start; j++)
      Data.at(i++) = NAN;
    for (Ti j = 0; j < (Ti)v->Data.size(); j++)
      Data.at(i++) = v->Data.at(j);
    for (Ti j = 0; j < end; j++)
      Data.at(i++) = NAN;
  }
}

template class ldt::Variables<Tv>;
// template class ldt::Variable < Ti>;
