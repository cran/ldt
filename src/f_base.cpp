/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the LGPL 3.0.
 See accompanying file LICENSE
*/

#include "frequency.h"

using namespace ldt;

void Frequency::CheckClassEquality(Frequency const &first,
                                   Frequency const &second) {
  if (first.mClass != second.mClass ||
      AreEqual_i(first.ToClassString().c_str(),
                 second.ToClassString().c_str()) == false)
    throw std::logic_error(
        std::string("Class of the two frequencies are not the same: ") +
        first.ToClassString() + std::string(" != ") + second.ToClassString());
}

bool Frequency::IsEqualTo(Frequency const &other) {
  auto i = CompareTo(other);
  return i == 0;
}

bool Frequency::IsNewerThan(Frequency const &other) {
  auto i = CompareTo(other);
  return i > 0;
}

bool Frequency::IsEqualOrNewerThan(Frequency const &other) {
  auto i = CompareTo(other);
  return i >= 0;
}

bool Frequency::IsOlderThan(Frequency const &other) {
  auto i = CompareTo(other);
  return i < 0;
}

bool Frequency::IsEqualOrOlderThan(Frequency const &other) {
  auto i = CompareTo(other);
  return i <= 0;
}

FrequencyClass Frequency::GetClass(const std::string &classStr) {
  if (AreEqual_i(classStr.c_str(), "cs")) // cs
    return FrequencyClass::kCrossSection;

  // formats that start with more characters must come first

  // Day-Based
  else if (StartsWith("ho", classStr.c_str())) // ho...
    return FrequencyClass::kHourly;
  else if (StartsWith("mi", classStr.c_str())) // mi...
    return FrequencyClass::kMinutely;
  else if (StartsWith("se", classStr.c_str())) // se...
    return FrequencyClass::kSecondly;
  else if (StartsWith("da", classStr.c_str())) // da...
    return FrequencyClass::kXTimesADay;

  // Year-Based
  else if (AreEqual_i(classStr.c_str(), "y")) // y
    return FrequencyClass::kYearly;
  else if (StartsWith("z", classStr.c_str())) // z...
    return FrequencyClass::kMultiYear;
  else if (AreEqual_i(classStr.c_str(), "q")) // q
    return FrequencyClass::kQuarterly;
  else if (AreEqual_i(classStr.c_str(), "m")) // m
    return FrequencyClass::kMonthly;
  else if (StartsWith("y", classStr.c_str())) // y...
    return FrequencyClass::kXTimesAYear;
  else if (StartsWith("x", classStr.c_str())) // x...
    return FrequencyClass::kXTimesZYears;

  // Week-Based
  else if (AreEqual_i(classStr.c_str(), "w")) // w
    return FrequencyClass::kWeekly;
  else if (StartsWith("w", classStr.c_str())) // w...
    return FrequencyClass::kMultiWeekly;
  else if (AreEqual_i(classStr.c_str(), "d")) // d
    return FrequencyClass::kDaily;
  else if (StartsWith("d", classStr.c_str())) // d...
    return FrequencyClass::kMultiDaily;
  else if (StartsWith("i", classStr.c_str())) // i...
    return FrequencyClass::kDailyInWeek;

  // List
  else if (StartsWith("Ls", classStr.c_str())) // Ls...
    return FrequencyClass::kListString;
  else if (StartsWith("Ld", classStr.c_str())) // Ld...
    return FrequencyClass::kListDate;

  throw std::logic_error("not implemented or invalid class string");
}

std::unique_ptr<Frequency> Frequency::Parse(const std::string &str,
                                            const std::string &classStr,
                                            FrequencyClass &fClass) {
  fClass = GetClass(classStr);
  switch (fClass) {
  case FrequencyClass::kCrossSection: {
    auto f = new FrequencyCrossSection(0);
    FrequencyCrossSection::Parse0(str, *f);
    return std::unique_ptr<Frequency>(f);
  }

  case FrequencyClass::kYearly:
  case FrequencyClass::kMonthly:
  case FrequencyClass::kQuarterly:
  case FrequencyClass::kXTimesAYear:
  case FrequencyClass::kMultiYear:
  case FrequencyClass::kXTimesZYears: {
    auto f = new FrequencyYearBased();
    FrequencyYearBased::Parse0(str, classStr, fClass, *f);
    return std::unique_ptr<Frequency>(f);
  }

  case FrequencyClass::kWeekly:
  case FrequencyClass::kDaily:
  case FrequencyClass::kDailyInWeek:
  case FrequencyClass::kMultiWeekly:
  case FrequencyClass::kMultiDaily: {
    auto f = new FrequencyWeekBased();
    FrequencyWeekBased::Parse0(str, classStr, fClass, *f);
    return std::unique_ptr<Frequency>(f);
  }

  case FrequencyClass::kListString: {
    auto f = new FrequencyList<std::string>("", nullptr);
    FrequencyList<std::string>::Parse0(str, classStr, fClass, *f, nullptr);
    return std::unique_ptr<Frequency>(f);
  }
  case FrequencyClass::kListDate: {
    auto f = new FrequencyList<boost::gregorian::date>(boost::gregorian::date(),
                                                       nullptr);
    FrequencyList<boost::gregorian::date>::Parse0(str, classStr, fClass, *f,
                                                  nullptr);
    return std::unique_ptr<Frequency>(f);
  }

  case FrequencyClass::kHourly:
  case FrequencyClass::kMinutely:
  case FrequencyClass::kSecondly:
  case FrequencyClass::kXTimesADay: {
    auto f = new FrequencyDayBased();
    FrequencyDayBased::Parse0(str, classStr, fClass, *f);
    return std::unique_ptr<Frequency>(f);
  }

  default:
    throw std::logic_error("not implemented frequency class in 'Parse'");
  }
}

void Frequency::Examples(std::vector<std::unique_ptr<Frequency>> &values,
                         std::vector<std::string> &listItemsString,
                         std::vector<boost::gregorian::date> &listItemsDate) {

  values.push_back(std::unique_ptr<Frequency>(new FrequencyCrossSection(10)));

  values.push_back(FrequencyYearBased::Yearly(2000));
  values.push_back(FrequencyYearBased::Quarterly(2000, 2));
  values.push_back(FrequencyYearBased::Monthly(2000, 4));
  values.push_back(FrequencyYearBased::MultiYearly(2000, 3));
  values.push_back(FrequencyYearBased::XTimesAYear(2000, 3, 3));
  values.push_back(FrequencyYearBased::XTimesZYear(2000, 3, 3, 2));

  values.push_back(
      FrequencyWeekBased::Weekly(boost::gregorian::date(2000, 3, 15)));
  values.push_back(
      FrequencyWeekBased::MultiWeekly(boost::gregorian::date(2000, 3, 15), 2));
  values.push_back(
      FrequencyWeekBased::Daily(boost::gregorian::date(2000, 3, 15)));
  values.push_back(
      FrequencyWeekBased::MultiDaily(boost::gregorian::date(2000, 3, 15), 3));
  values.push_back(FrequencyWeekBased::DailyInWeek(
      boost::gregorian::date(2000, 3, 15),
      DayOfWeekRange(DayOfWeek::kMon, DayOfWeek::kFri), true));

  auto f5 = *FrequencyWeekBased::DailyInWeek(
                 boost::gregorian::date(2000, 3, 15),
                 DayOfWeekRange(DayOfWeek::kMon, DayOfWeek::kFri), true)
                 .get();

  values.push_back(FrequencyDayBased::Hourly(f5, 3));
  values.push_back(FrequencyDayBased::Minutely(f5, 67));
  values.push_back(FrequencyDayBased::Secondly(f5, 167));
  values.push_back(FrequencyDayBased::XTimesADay(f5, 8, 2));

  listItemsString.insert(
      std::end(listItemsString),
      {"A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M"});

  values.push_back(std::unique_ptr<Frequency>(new FrequencyList<std::string>(
      "G", &listItemsString))); // add an item at the middle or 'Next' might
                                // fail in some tests

  listItemsDate.insert(std::end(listItemsDate),
                       {
                           boost::gregorian::date(2000, 10, 1),
                           boost::gregorian::date(2001, 2, 12),
                           boost::gregorian::date(2002, 3, 2),
                           boost::gregorian::date(2003, 5, 3),
                           boost::gregorian::date(2004, 5, 24),
                           boost::gregorian::date(2005, 5, 25),
                           boost::gregorian::date(2006, 5, 4),
                           boost::gregorian::date(2007, 6, 5),
                           boost::gregorian::date(2008, 6, 15),
                           boost::gregorian::date(2009, 7, 9),
                           boost::gregorian::date(2010, 8, 19),
                       });

  values.push_back(
      std::unique_ptr<Frequency>(new FrequencyList<boost::gregorian::date>(
          boost::gregorian::date(2005, 5, 25), &listItemsDate)));
}

//#pragma region DayOfWeekRange

DayOfWeekRange::DayOfWeekRange(DayOfWeek start, DayOfWeek end) {
  mStart = start;
  mEnd = end;
}

Ti DayOfWeekRange::GetLength() const {
  Ti start = (Ti)mStart;
  Ti end = (Ti)mEnd;
  if (end > start)
    return (Ti)end - start + 1;
  else
    return 7 - (start - end) + 1;
}

Ti DayOfWeekRange::Distance(DayOfWeek start, DayOfWeek end, bool forward) {
  Ti s = (Ti)start;
  Ti e = (Ti)end;
  if (forward) {
    if (s > e)
      return 7 - s + e;
    else
      return e - s;
  } else {
    if (s >= e)
      return s - e;
    else
      return 7 - s + e;
  }
}

bool DayOfWeekRange::IsInRange(DayOfWeek value) const {
  if (value == mStart || value == mEnd || GetLength() == 7)
    return true;
  while (true) {
    value = DayOfWeekRange::NextDayOfWeek(value);
    if (value == mStart)
      return false;
    if (value == mEnd)
      return true;
  }
}

bool DayOfWeekRange::IsOutsideRange(DayOfWeek value, bool forward,
                                    Ti &step) const {
  step = 0;
  if (value == mStart || value == mEnd || GetLength() == 7)
    return false;
  if (forward) {
    while (true) {
      value = DayOfWeekRange::NextDayOfWeek(value);
      step++;
      if (value == mStart)
        return true;
      if (value == mEnd)
        return false;
    }
  } else {
    while (true) {
      value = DayOfWeekRange::PreviousDayOfWeek(value);
      step--;
      if (value == mEnd)
        return true;
      if (value == mStart)
        return false;
    }
  }
}

DayOfWeek DayOfWeekRange::NextDayOfWeek(DayOfWeek value) {
  Ti c = (Ti)value;
  if (c == 6)
    return (DayOfWeek)0;
  else
    return (DayOfWeek)(c + 1);
}

DayOfWeek DayOfWeekRange::PreviousDayOfWeek(DayOfWeek value) {
  Ti c = (Ti)value;
  if (c == 0)
    return (DayOfWeek)6;
  else
    return (DayOfWeek)(c - 1);
}

std::string DayOfWeekRange::ToString() const {
  auto a = ldt::ToString((DayOfWeek)mStart, true);
  auto b = ldt::ToString((DayOfWeek)mEnd, true);
  return std::string(a) + std::string("-") + std::string(b);
}

DayOfWeekRange DayOfWeekRange::Parse(std::string str) {
  try {
    auto parts = std::vector<std::string>();
    SplitMultiple(str, std::string("-:"), parts);
    auto s = FromString_DayOfWeek(parts.at(0).c_str());
    auto e = FromString_DayOfWeek(parts.at(1).c_str());
    return DayOfWeekRange(s, e);
  } catch (...) {
    throw std::logic_error("Invalid day of week range.");
  }
}

//#pragma endregion
