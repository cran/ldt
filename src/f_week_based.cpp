/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the LGPL 3.0.
 See accompanying file LICENSE
*/

#include "frequency.h"

using namespace ldt;

FrequencyWeekBased::FrequencyWeekBased(boost::gregorian::date day, bool isWeek,
                                       DayOfWeekRange *range, bool forward,
                                       Ti multi) {
  mMulti = multi;
  if (isWeek)
    mClass =
        multi == 1 ? FrequencyClass::kWeekly : FrequencyClass::kMultiWeekly;
  else if (range) {
    mRange = *range;
    mClass = FrequencyClass::kDailyInWeek;
  } else
    mClass = multi == 1 ? FrequencyClass::kDaily : FrequencyClass::kMultiDaily;

  if ((mClass == FrequencyClass::kWeekly || mClass == FrequencyClass::kDaily) &&
      range)
    throw std::logic_error(
        "Invalid argument: 'range' should be null for a daily or weekly "
        "frequencies.");

  if (range) {
    if (mRange.IsOutsideRange((DayOfWeek)(Ti)day.day_of_week(), forward,
                              mForwardSteps)) {
      day = day + boost::gregorian::days(mForwardSteps);
    } else
      mForwardSteps = 0;
  } else
    mForwardSteps = 0;
  mDay = day;
}

std::unique_ptr<FrequencyWeekBased>
FrequencyWeekBased::Weekly(boost::gregorian::date day) {
  return std::unique_ptr<FrequencyWeekBased>(
      new FrequencyWeekBased(day, true, nullptr, true));
}

std::unique_ptr<FrequencyWeekBased>
FrequencyWeekBased::MultiWeekly(boost::gregorian::date day, Ti multi) {
  return std::unique_ptr<FrequencyWeekBased>(
      new FrequencyWeekBased(day, true, nullptr, true, multi));
}

std::unique_ptr<FrequencyWeekBased>
FrequencyWeekBased::Daily(boost::gregorian::date day) {
  return std::unique_ptr<FrequencyWeekBased>(
      new FrequencyWeekBased(day, false, nullptr, true));
}

std::unique_ptr<FrequencyWeekBased>
FrequencyWeekBased::MultiDaily(boost::gregorian::date day, Ti multi) {
  return std::unique_ptr<FrequencyWeekBased>(
      new FrequencyWeekBased(day, false, nullptr, true, multi));
}

std::unique_ptr<FrequencyWeekBased>
FrequencyWeekBased::DailyInWeek(boost::gregorian::date day,
                                DayOfWeekRange range, bool forward) {
  return std::unique_ptr<FrequencyWeekBased>(
      new FrequencyWeekBased(day, false, &range, forward));
}

std::unique_ptr<Frequency> FrequencyWeekBased::Clone() const {
  return std::unique_ptr<FrequencyWeekBased>(new FrequencyWeekBased(*this));
}

void FrequencyWeekBased::Next(Ti steps) {
  switch (mClass) {
  case FrequencyClass::kWeekly:
  case FrequencyClass::kMultiWeekly:
    mDay += boost::gregorian::days(7 * steps * mMulti);
    break;

  case FrequencyClass::kDaily:
  case FrequencyClass::kMultiDaily:
    mDay += boost::gregorian::days(steps * mMulti);
    break;

  case FrequencyClass::kDailyInWeek: {
    Ti step = 0;
    if (steps > 0) {
      for (Ti i = 0; i < steps; i++) {
        mDay += boost::gregorian::days(1);
        if (mRange.IsOutsideRange((DayOfWeek)(Ti)mDay.day_of_week(), true,
                                  step))
          mDay += boost::gregorian::days(step);
      }
    } else {
      for (Ti i = 0; i < -steps; i++) {
        mDay -= boost::gregorian::days(1);
        if (mRange.IsOutsideRange((DayOfWeek)(Ti)mDay.day_of_week(), false,
                                  step))
          mDay -= boost::gregorian::days(-step);
      }
    }
  } break;

  default:
    throw std::logic_error("not implemented: next: week-based frequency");
  }
}

Ti FrequencyWeekBased::CompareTo(Frequency const &other) {
  CheckClassEquality(*this, other);
  auto second = dynamic_cast<FrequencyWeekBased const &>(other);
  if (mDay < second.mDay)
    return -1;
  else if (mDay > second.mDay)
    return 1;
  else
    return 0;
}

Ti FrequencyWeekBased::Minus(Frequency const &other) {
  CheckClassEquality(*this, other);
  auto second = dynamic_cast<FrequencyWeekBased const &>(other);

  switch (mClass) {
  case FrequencyClass::kWeekly: {
    return ((mDay - second.mDay).days() / (7 * mMulti));
  }
  case FrequencyClass::kDaily: {
    return (mDay - second.mDay).days() / mMulti;
  }
  case FrequencyClass::kDailyInWeek: {

    Ti c0 =
        mRange.Distance((DayOfWeek)(Ti)mDay.day_of_week(), mRange.mEnd, true);
    Ti c1 = mRange.Distance((DayOfWeek)(Ti)second.mDay.day_of_week(),
                            mRange.mEnd, true);

    auto t0 = mDay + boost::gregorian::days(c0);
    auto t1 = second.mDay + boost::gregorian::days(c1);

    Ti span = (t0 - t1).days();
    Ti td = span * mRange.GetLength() / 7;

    return td - c0 + c1;
  }
  default:
    throw std::logic_error("not implemented: minus: week-based frequency");
  }
}

void FrequencyWeekBased::Parse0(const std::string &str,
                                const std::string &classStr,
                                const FrequencyClass &fClass,
                                FrequencyWeekBased &result) {
  result.mClass = fClass;
  try {

    result.mDay = boost::gregorian::date_from_iso_string(str);
    result.mMulti = 1;

    if (fClass == FrequencyClass::kWeekly || fClass == FrequencyClass::kDaily) {
      //
    } else if (fClass == FrequencyClass::kMultiWeekly ||
               fClass == FrequencyClass::kMultiDaily) {
      result.mMulti =
          std::stoi(classStr.substr(1, classStr.length() - 1), nullptr, 10);
    } else if (fClass == FrequencyClass::kDailyInWeek) {
      auto parts = std::vector<std::string>();
      SplitMultiple(classStr, std::string(":"), parts);
      result.mRange = DayOfWeekRange::Parse(parts.at(1));
    } else
      throw std::logic_error("Invalid class for a week-based frequency");
  } catch (...) {
    Rethrow(
        (std::string(
             "Parsing week-based frequency failed. Invalid format. class=") +
         std::to_string((int)fClass) + std::string(", str=") + str +
         std::string(", classStr=") + classStr)
            .c_str(),
        true);
  }
}

std::string FrequencyWeekBased::ToString() const {
  return boost::gregorian::to_iso_string(mDay);
}

std::string FrequencyWeekBased::ToClassString(bool details) const {
  switch (this->mClass) {
  case FrequencyClass::kWeekly:
    return std::string("w");
  case FrequencyClass::kMultiWeekly:
    return std::string("w") + std::to_string(mMulti);
  case FrequencyClass::kDaily:
    return std::string("d");
  case FrequencyClass::kMultiDaily:
    return std::string("d") + std::to_string(mMulti);
  case FrequencyClass::kDailyInWeek:
    return std::string("i:") + mRange.ToString();
  default:
    throw std::logic_error("invalid class type");
  }
}
