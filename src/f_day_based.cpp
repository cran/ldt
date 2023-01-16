/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the LGPL 3.0.
 See accompanying file LICENSE
*/

#include "frequency.h"

using namespace ldt;

FrequencyDayBased::FrequencyDayBased(FrequencyWeekBased &day, Ti partitionCount,
                                     Ti position) {
  mDay = day;
  mPartitionCount = partitionCount;
  mPosition = position;

  if (mPartitionCount <= 0)
    throw std::logic_error(
        "Invalid argument: Number of partitions must be positive.");
  if (mPosition <= 0)
    throw std::logic_error(
        "Invalid argument: Current position must be positive.");
  if (mPosition > mPartitionCount)
    throw std::logic_error(
        "Invalid argument: Current position must be equal or less than the "
        "number of partitions.");

  if (partitionCount == 24)
    this->mClass = FrequencyClass::kHourly;
  else if (partitionCount == 1440)
    this->mClass = FrequencyClass::kMinutely;
  else if (partitionCount == 86400)
    this->mClass = FrequencyClass::kSecondly;
  else
    this->mClass = FrequencyClass::kXTimesADay;
}

std::unique_ptr<FrequencyDayBased>
FrequencyDayBased::XTimesADay(FrequencyWeekBased &day, Ti X, Ti position) {
  return std::unique_ptr<FrequencyDayBased>(
      new FrequencyDayBased(day, X, position));
}

std::unique_ptr<FrequencyDayBased>
FrequencyDayBased::Hourly(FrequencyWeekBased &day, Ti hour) {
  return std::unique_ptr<FrequencyDayBased>(
      new FrequencyDayBased(day, 24, hour));
}

std::unique_ptr<FrequencyDayBased>
FrequencyDayBased::Minutely(FrequencyWeekBased &day, Ti minute) {
  return std::unique_ptr<FrequencyDayBased>(
      new FrequencyDayBased(day, 1440, minute));
}

std::unique_ptr<FrequencyDayBased>
FrequencyDayBased::Secondly(FrequencyWeekBased &day, Ti second) {
  return std::unique_ptr<FrequencyDayBased>(
      new FrequencyDayBased(day, 86400, second));
}

std::unique_ptr<Frequency> FrequencyDayBased::Clone() const {
  return std::unique_ptr<FrequencyDayBased>(new FrequencyDayBased(*this));
}

void FrequencyDayBased::Next(Ti steps) {
  Ti absCount = std::abs(steps);
  Ti days = (Ti)absCount / mPartitionCount;
  Ti rem = absCount % mPartitionCount;

  if (steps > 0) {
    if (mPosition + rem > mPartitionCount) {
      mDay.Next(days + 1);
      mPosition += rem - mPartitionCount;
    } else {
      mDay.Next(days);
      mPosition += rem;
    }
  } else {
    if (mPosition - rem <= 0) {
      mDay.Next(-days - 1);
      mPosition -= rem - mPartitionCount;
    } else {
      mDay.Next(-days);
      mPosition -= rem;
    }
  }
}

Ti FrequencyDayBased::CompareTo(Frequency const &other) {
  CheckClassEquality(*this, other);
  auto second = dynamic_cast<FrequencyDayBased const &>(other);

  auto com = mDay.CompareTo(second.mDay);
  if (com != 0)
    return com;

  if (mPosition < second.mPosition)
    return -1;
  else if (mPosition > second.mPosition)
    return 1;
  else
    return 0;
}

Ti FrequencyDayBased::Minus(Frequency const &other) {
  CheckClassEquality(*this, other);
  auto second = dynamic_cast<FrequencyDayBased const &>(other);

  if (IsNewerThan(second)) {
    Ti ddiff = mDay.Minus(second.mDay);
    Ti a = mPartitionCount;
    Ti b1 = second.mPosition;
    Ti b2 = mPosition;
    Ti res = a * (ddiff - 1) + b2 + (a - b1);
    return res;
  } else {
    Ti ddiff = second.mDay.Minus(mDay);
    Ti a = second.mPartitionCount;
    Ti b1 = mPosition;
    Ti b2 = second.mPosition;
    Ti res = a * (ddiff - 1) + b2 + (a - b1);
    return -res;
  }
}

void FrequencyDayBased::Parse0(const std::string &str,
                               const std::string &classStr,
                               const FrequencyClass &fClass,
                               FrequencyDayBased &result) {
  result.mClass = fClass;
  try {
    auto parts1 = std::vector<std::string>();
    SplitMultiple(str, std::string(":"), parts1);
    result.mPosition = std::stoi(parts1.at(1), nullptr, 10);

    auto parts2 = std::vector<std::string>();
    SplitMultiple(classStr, std::string("|"), parts2);

    FrequencyClass fc = Frequency::GetClass(parts2.at(1));
    FrequencyWeekBased::Parse0(parts1.at(0), parts2.at(1), fc, result.mDay);

    if (fClass == FrequencyClass::kHourly)
      result.mPartitionCount = 24;
    else if (fClass == FrequencyClass::kMinutely)
      result.mPartitionCount = 1440;
    else if (fClass == FrequencyClass::kSecondly)
      result.mPartitionCount = 86400;
    else if (fClass == FrequencyClass::kXTimesADay)
      result.mPartitionCount = std::stoi(parts2.at(0).substr(2), nullptr, 10);
    else
      throw std::logic_error("Invalid class for a day-based frequency");
  } catch (...) {
    Rethrow("Parsing day-based frequency failed. Invalid format.");
  }
}

std::string FrequencyDayBased::ToString() const {
  switch (this->mClass) {
  case FrequencyClass::kHourly:
  case FrequencyClass::kMinutely:
  case FrequencyClass::kSecondly:
    return mDay.ToString() + std::string(":") + std::to_string(mPosition);
  case FrequencyClass::kXTimesADay:
    return mDay.ToString() + std::string(":") + std::to_string(mPosition);
  default:
    throw std::logic_error("invalid class type");
  }
}

std::string FrequencyDayBased::ToClassString(bool details) const {
  switch (this->mClass) {
  case FrequencyClass::kHourly:
    return std::string("ho|") + mDay.ToClassString();
  case FrequencyClass::kMinutely:
    return std::string("mi|") + mDay.ToClassString();
  case FrequencyClass::kSecondly:
    return std::string("se|") + mDay.ToClassString();
  case FrequencyClass::kXTimesADay: {
    return std::string("da") + std::to_string(mPartitionCount) +
           std::string("|") + mDay.ToClassString();
  }
  default:
    throw std::logic_error("invalid class type");
  }
}
