/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the LGPL 3.0.
 See accompanying file LICENSE
*/

#include "frequency.h"

using namespace ldt;

FrequencyYearBased::FrequencyYearBased(Ti year, Ti partitionCount, Ti position,
                                       Ti yearMulti) {
  mYear = year;
  mYearMulti = yearMulti;
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

  if (yearMulti == 1) {
    if (partitionCount == 1)
      this->mClass = FrequencyClass::kYearly;
    else if (partitionCount == 4)
      this->mClass = FrequencyClass::kQuarterly;
    else if (partitionCount == 12)
      this->mClass = FrequencyClass::kMonthly;
    else
      this->mClass = FrequencyClass::kXTimesAYear;
  } else {
    if (partitionCount == 1)
      this->mClass = FrequencyClass::kMultiYear;
    else
      this->mClass = FrequencyClass::kXTimesZYears;
  }
}

std::unique_ptr<FrequencyYearBased>
FrequencyYearBased::XTimesZYear(Ti year, Ti X, Ti position, Ti Z) {
  return std::unique_ptr<FrequencyYearBased>(
      new FrequencyYearBased(year, X, position, Z));
}

std::unique_ptr<FrequencyYearBased>
FrequencyYearBased::XTimesAYear(Ti year, Ti X, Ti position) {
  return std::unique_ptr<FrequencyYearBased>(
      new FrequencyYearBased(year, X, position, 1));
}

std::unique_ptr<FrequencyYearBased> FrequencyYearBased::Yearly(Ti year) {
  return std::unique_ptr<FrequencyYearBased>(
      new FrequencyYearBased(year, 1, 1, 1));
}

std::unique_ptr<FrequencyYearBased>
FrequencyYearBased::MultiYearly(Ti year, Ti yearMulti) {
  return std::unique_ptr<FrequencyYearBased>(
      new FrequencyYearBased(year, 1, 1, yearMulti));
}

std::unique_ptr<FrequencyYearBased> FrequencyYearBased::Quarterly(Ti year,
                                                                  Ti quarter) {
  return std::unique_ptr<FrequencyYearBased>(
      new FrequencyYearBased(year, 4, quarter, 1));
}

std::unique_ptr<FrequencyYearBased> FrequencyYearBased::Monthly(Ti year,
                                                                Ti month) {
  return std::unique_ptr<FrequencyYearBased>(
      new FrequencyYearBased(year, 12, month, 1));
}

std::unique_ptr<Frequency> FrequencyYearBased::Clone() const {
  return std::unique_ptr<FrequencyYearBased>(new FrequencyYearBased(*this));
}

void FrequencyYearBased::Next(Ti steps) {
  Ti absCount = std::abs(steps);
  Ti years = (Ti)absCount / mPartitionCount;
  Ti rem = absCount % mPartitionCount;

  if (steps > 0) {
    if (mPosition + rem > mPartitionCount) {
      mYear += (years + 1) * mYearMulti;
      mPosition += rem - mPartitionCount;
    } else {
      mYear += years * mYearMulti;
      mPosition += rem;
    }
  } else {
    if (mPosition - rem <= 0) {
      mYear -= (years + 1) * mYearMulti;
      mPosition -= rem - mPartitionCount;
    } else {
      mYear -= (years)*mYearMulti;
      mPosition -= rem;
    }
  }
}

Ti FrequencyYearBased::CompareTo(Frequency const &other) {
  CheckClassEquality(*this, other);
  auto second = dynamic_cast<FrequencyYearBased const &>(other);
  if (mYear < second.mYear)
    return -1;
  else if (mYear > second.mYear)
    return 1;
  else { // mYear == second.mYear
    if (mPosition < second.mPosition)
      return -1;
    else if (mPosition > second.mPosition)
      return 1;
    else
      return 0;
  }
}

Ti FrequencyYearBased::Minus(Frequency const &other) {
  CheckClassEquality(*this, other);
  auto second = dynamic_cast<FrequencyYearBased const &>(other);
  if (IsNewerThan(second)) {
    Ti a = mPartitionCount;
    Ti b1 = second.mPosition;
    Ti b2 = mPosition;
    Ti res = a * ((mYear - second.mYear) / mYearMulti - 1) + b2 + (a - b1);
    return res;
  } else {
    Ti a = second.mPartitionCount;
    Ti b1 = mPosition;
    Ti b2 = second.mPosition;
    Ti res = a * ((second.mYear - mYear) / mYearMulti - 1) + b2 + (a - b1);
    return -res;
  }
}

void FrequencyYearBased::Parse0(const std::string &str,
                                const std::string &classStr,
                                const FrequencyClass &fClass,
                                FrequencyYearBased &result) {
  result.mClass = fClass;
  try {
    auto parts = std::vector<std::string>();
    SplitMultiple(str, std::string("QqMm:"), parts);
    result.mYear = std::stoi(parts.at(0), nullptr, 10);
    result.mYearMulti = 1;

    if (fClass == FrequencyClass::kYearly) {
      result.mPosition = 1;
      result.mPartitionCount = 1;
    } else if (fClass == FrequencyClass::kQuarterly) {
      result.mPosition = std::stoi(parts.at(1), nullptr, 10);
      result.mPartitionCount = 4;
    } else if (fClass == FrequencyClass::kMonthly) {
      result.mPosition = std::stoi(parts.at(1), nullptr, 10);
      result.mPartitionCount = 12;
    } else {
      auto parts0 = std::vector<std::string>();
      SplitMultiple(classStr.substr(1, classStr.length() - 1), std::string("z"),
                    parts0);
      if (fClass == FrequencyClass::kXTimesAYear) {
        result.mPosition = std::stoi(parts.at(1), nullptr, 10);
        result.mPartitionCount = std::stoi(parts0.at(0), nullptr, 10);
      } else if (fClass == FrequencyClass::kMultiYear) {
        result.mPosition = 1;
        result.mPartitionCount = 1;
        result.mYearMulti = std::stoi(parts0.at(0), nullptr, 10);
      } else if (fClass == FrequencyClass::kXTimesZYears) {
        result.mPosition = std::stoi(parts.at(1), nullptr, 10);
        result.mPartitionCount =
            std::stoi(parts0.at(0), nullptr, 10); // partition comes first
        result.mYearMulti = std::stoi(parts0.at(1), nullptr, 10);
      } else
        throw std::logic_error("Invalid class for a year-based frequency");
    }
  } catch (...) {
    Rethrow("Parsing year-based frequency failed. Invalid format.");
  }
}

std::string FrequencyYearBased::ToString() const {
  switch (this->mClass) {
  case FrequencyClass::kYearly:
  case FrequencyClass::kMultiYear:
    return std::to_string(mYear);
  case FrequencyClass::kQuarterly:
    return std::to_string(mYear) + std::string("Q") + std::to_string(mPosition);
  case FrequencyClass::kMonthly:
    return std::to_string(mYear) + std::string("M") + std::to_string(mPosition);
  case FrequencyClass::kXTimesAYear:
  case FrequencyClass::kXTimesZYears:
    return std::to_string(mYear) + std::string(":") + std::to_string(mPosition);
  default:
    throw std::logic_error("invalid class type");
  }
}

std::string FrequencyYearBased::ToClassString(bool details) const {
  switch (this->mClass) {
  case FrequencyClass::kYearly:
    return std::string("y");
  case FrequencyClass::kMultiYear:
    return std::string("z") + std::to_string(mYearMulti);
  case FrequencyClass::kQuarterly:
    return std::string("q");
  case FrequencyClass::kMonthly:
    return std::string("m");
  case FrequencyClass::kXTimesAYear:
    return std::string("y") + std::to_string(mPartitionCount);
  case FrequencyClass::kXTimesZYears:
    return std::string("x") + std::to_string(mPartitionCount) +
           std::string("z") +
           std::to_string(mYearMulti); // partition comes first
  default:
    throw std::logic_error("invalid class type");
  }
}
