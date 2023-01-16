
#include "variable.h"

using namespace ldt;

template <typename Tw> std::string Variable<Tw>::ToString() const {

  std::stringstream ss;
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
    ss << boost::lexical_cast<std::string>(d);
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
