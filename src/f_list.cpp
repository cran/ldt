/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the GPL 3.0.
 See accompanying file LICENSE
*/

#include "frequency.h"

using namespace ldt;

template <typename T> FrequencyList<T>::~FrequencyList() {}

template <typename T>
FrequencyList<T>::FrequencyList(T value, std::vector<T> *items) {
  if constexpr (std::is_same<T, std::string>())
    this->mClass = FrequencyClass::kListString;
  else if constexpr (std::is_same<T, boost::gregorian::date>())
    this->mClass = FrequencyClass::kListDate;
  else if constexpr (true)
    throw std::logic_error("Error in initializing a list frequency: only "
                           "'string' and 'date' is implemented.");
  mValue = value;
  pItems = items;
}

template <typename T> Ti FrequencyList<T>::GetIndex() {
  return IndexOf(*pItems, mValue);
}

template <typename T>
std::unique_ptr<Frequency> FrequencyList<T>::Clone() const {
  return std::unique_ptr<FrequencyList<T>>(new FrequencyList<T>(*this));
}

template <typename T> void FrequencyList<T>::Next(Ti steps) {
  Ti i = GetIndex();
  Ti j = i + steps;
  if (j < (int)pItems->size() && j >= 0)
    mValue = pItems->at(j);
  else
    throw std::logic_error("FrequenyList:Next:Invalid number of steps.");
}

template <typename T> Ti FrequencyList<T>::CompareTo(Frequency const &other) {
  CheckClassEquality(*this, other);
  auto second = dynamic_cast<FrequencyList<T> const &>(other);
  auto i1 = GetIndex();
  auto i2 = second.GetIndex();
  if (i1 > i2)
    return 1;
  else if (i1 < i2)
    return -1;
  return 0;
}

template <typename T> Ti FrequencyList<T>::Minus(Frequency const &other) {
  CheckClassEquality(*this, other);
  auto second = dynamic_cast<FrequencyList<T> const &>(other);
  return GetIndex() - second.GetIndex();
}

template <typename T>
void FrequencyList<T>::Parse0(const std::string &str,
                              const std::string &classStr,
                              const FrequencyClass &fClass,
                              FrequencyList<T> &result, std::vector<T> *items) {
  try {

    if constexpr (std::is_same<T, std::string>()) {
      result.mClass = FrequencyClass::kListString;
      result.mValue = str;
      if (items)
        result.pItems = items;
      if (items && classStr.length() > 2)
        SplitMultiple(classStr.substr(3), std::string(";"), *items);
    } else if constexpr (std::is_same<T, boost::gregorian::date>()) {
      result.mClass = FrequencyClass::kListDate;
      result.mValue = boost::gregorian::date_from_iso_string(str);

      if (items && classStr.length() > 2) {
        auto parts = std::vector<std::string>();
        SplitMultiple(classStr.substr(3), std::string(";"), parts);
        for (const auto &a : parts)
          items->push_back(boost::gregorian::date_from_iso_string(a));
      }
    }
  } catch (...) {
    Rethrow("Parsing list frequency failed. Invalid format.");
  }
}

template <typename T> std::string FrequencyList<T>::ToString() const {
  if constexpr (std::is_same<T, std::string>())
    return mValue;
  else if constexpr (std::is_same<T, boost::gregorian::date>())
    return boost::gregorian::to_iso_string(mValue);
}

template <typename T>
std::string FrequencyList<T>::ToClassString(bool details) const {
  if constexpr (std::is_same<T, std::string>()) {
    if (details) {
      if (!pItems)
        throw std::logic_error(
            "FrequencyList:ToClassString:Inner list is null");
      std::function<std::string(std::string)> fun =
          [](std::string d) -> std::string { return d; };
      return std::string("Ls:") + Join(*pItems, std::string(";"), fun);
    } else
      return std::string("Ls");
  } else if constexpr (std::is_same<T, boost::gregorian::date>()) {
    if (details) {
      if (!pItems)
        throw std::logic_error(
            "FrequencyList:ToClassString:Inner list is null");
      std::function<std::string(boost::gregorian::date)> fun =
          [](boost::gregorian::date d) -> std::string {
        return boost::gregorian::to_iso_string(d);
      };
      return std::string("Ld:") + Join(*pItems, std::string(";"), fun);
    } else
      return std::string("Ld");
  }
}

template <typename T>
std::unique_ptr<FrequencyList<T>>
FrequencyList<T>::ParseList(const std::string &str, const std::string &classStr,
                            FrequencyClass &fClass, std::vector<T> &items) {
  fClass = GetClass(classStr);

  if constexpr (std::is_same<T, std::string>()) {
    auto f = new FrequencyList<std::string>("", nullptr);
    FrequencyList<std::string>::Parse0(str, classStr, fClass, *f, &items);
    f->pItems = &items;
    return std::unique_ptr<FrequencyList<std::string>>(f);
  } else if constexpr (std::is_same<T, boost::gregorian::date>()) {
    auto f = new FrequencyList<boost::gregorian::date>(boost::gregorian::date(),
                                                       nullptr);
    FrequencyList<boost::gregorian::date>::Parse0(str, classStr, fClass, *f,
                                                  &items);
    f->pItems = &items;
    return std::unique_ptr<FrequencyList<boost::gregorian::date>>(f);
  } else if constexpr (true) {
    throw std::logic_error(
        "not implemented or invalid frequency class in 'ParseList'");
  }
}

template class ldt::FrequencyList<boost::gregorian::date>;
template class ldt::FrequencyList<std::string>;
