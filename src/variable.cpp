
#include "variable.h"

using namespace ldt;

template <typename Tw> Variable<Tw>::Variable() {}

template <typename Tw>
std::unique_ptr<Frequency> Variable<Tw>::GetEndFrequency() const {
  auto end = StartFrequency.get()->Clone();
  auto temp = end.get();
  temp->Next(Data.size());
  return end;
}

template <typename Tw>
bool Variable<Tw>::IsEqualTo(Variable<Tw> &other, const Tv &epsilon) const {
  if (Name != other.Name)
    return false;
  if (Data.size() != other.Data.size())
    return false;
  for (auto i = 0; i < (Ti)Data.size(); i++)
    if (std::abs(Data.at(i) - other.Data.at(i)) > epsilon)
      return false;
  if (StartFrequency.get()->IsEqualTo(*other.StartFrequency.get()) == false)
    return false;
  if (Fields != other.Fields)
    return false;
  return true;
}

template <typename Tw>
void Variable<Tw>::Examples(std::vector<std::unique_ptr<Variable<Tw>>> &values,
                            std::vector<std::unique_ptr<Frequency>> &f_values,
                            std::vector<std::string> &listItemsString,
                            std::vector<boost::gregorian::date> &listItemsDate,
                            std::mt19937 &eng) {

  // if constexpr (std::is_floating_point<Tw>()) {
  std::normal_distribution<Tw> D(0, 1);
  /*} else if constexpr (true) {
    std::uniform_int_distribution<Ti> D((Tw)0, (Tw)1);
  }*/

  Frequency::Examples(f_values, listItemsString, listItemsDate);
  FrequencyClass fc = FrequencyClass::kCrossSection;
  Ti i = 0;
  for (auto &a : f_values) {
    i++;
    Ti length = 20;

    auto v = new Variable<Tw>();
    values.push_back(std::unique_ptr<Variable<Tw>>(v));

    v->Name = std::string("V") + std::to_string(i);

    auto b = a.get();
    if (fc == FrequencyClass::kListString) {
      auto d = dynamic_cast<FrequencyList<std::string> &>(*b);
      d.pItems = &listItemsString;
      length = listItemsString.size();
    } else if (fc == FrequencyClass::kListDate) {
      auto d = dynamic_cast<FrequencyList<boost::gregorian::date> &>(*b);
      d.pItems = &listItemsDate;
      length = listItemsDate.size();
    }

    v->Data = std::vector<Tw>(length);
    for (Ti j = 0; j < length; j++)
      v->Data.at(j) = D(eng);

    v->StartFrequency = std::move(a);

    v->Fields.insert(
        {std::string("Key1"), std::string("V") + std::to_string(i)});
    if (i % 2 == 0)
      v->Fields.insert(
          {std::string("Key2"), std::string("W") + std::to_string(i)});
  }
}

template <typename Tw> std::string Variable<Tw>::ToString() const {

  std::ostringstream ss;
  ss << Name;
  ss << '\t';

  auto sf = StartFrequency.get();
  ss << (sf == nullptr ? std::string("NA") : sf->ToClassString(true));
  ss << '\t';

  ss << (sf == nullptr ? std::string("NA") : sf->ToString());
  ss << '\t';

  ss << std::fixed << std::setprecision(16);
  Ti c = Data.size();
  Ti i = 0;
  for (auto const &d : Data) {
    i++;
    ss << d;
    if (i < c)
      ss << ";";
  }
  ss << '\t';

  c = Fields.size();
  i = 0;
  for (auto const &x : Fields) {
    i++;
    ss << x.first;
    ss << ';'; // assuming that 'key' does not contain it
    ss << x.second;
    if (i < c)
      ss << '\t';
  }

  return ss.str();
}

template <typename Tw>
void Variable<Tw>::Parse(const std::string &str, Variable<Tw> &result,
                         std::vector<std::string> &listItemsString,
                         std::vector<boost::gregorian::date> &listItemsDate) {
  try {

    auto parts = std::vector<std::string>();
    Split(str, "\t", parts);
    if (parts.size() < 5)
      throw std::logic_error("At least 4 tab-separated items is expected.");

    result.Name = parts.at(0);

    // frequency
    FrequencyClass fClass;
    auto frequency = Frequency::Parse(parts.at(2), parts.at(1), fClass);

    if (fClass == FrequencyClass::kListString)
      frequency = FrequencyList<std::string>::ParseList(
          parts.at(2), parts.at(1), fClass, listItemsString);
    else if (fClass == FrequencyClass::kListDate)
      frequency = FrequencyList<boost::gregorian::date>::ParseList(
          parts.at(2), parts.at(1), fClass, listItemsDate);
    result.StartFrequency = std::move(frequency);

    // data
    result.Data.clear();
    std::vector<std::string> parts_d;
    Split(parts.at(3), std::string(";"), parts_d);
    result.Data.reserve(parts_d.size());
    for (auto const &d : parts_d) {
      if constexpr (std::is_same<Tw, double>()) {
        result.Data.push_back(std::stod(d));
      } else if constexpr (std::is_same<Tw, float>()) {
        result.Data.push_back(std::stof(d));
      } else if constexpr (std::is_same<Tw, int>()) {
        result.Data.push_back(std::stoi(d));
      } else if constexpr (std::is_same<Tw, long long>()) {
        result.Data.push_back(std::stoll(d));
      } else if constexpr (true) {
        throw std::logic_error(
            "Conversion of the variable's data-type is not implemented.");
      }
    }

    // Fields
    result.Fields.clear();
    for (Ti i = 4; i < (Ti)parts.size(); i++) {
      auto j = parts.at(i).find(std::string(";"));
      if (j < 0)
        throw std::logic_error("Invalid field: Key-Value separator is missing");
      result.Fields.insert(
          {parts.at(i).substr(0, j), parts.at(i).substr(j + 1)});
    }

  } catch (...) {
    Rethrow("Invalid format in parsing 'Variable'.");
  }
}

template class ldt::Variable<Tv>;
// template class ldt::Variable < Ti>;
