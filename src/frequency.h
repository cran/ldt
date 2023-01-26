#pragma once

#include "helpers.h"
#include "ldt_base.h"
#include <string>
#include <vector>

#include <boost/date_time/gregorian/gregorian.hpp>

namespace ldt {

/// @brief Frequency classes
enum class FrequencyClass {

  /// @brief Cross-section frequency class (string: cs)
  kCrossSection = 'c',

  /// @brief Yearly (or annual) frequency (string: y)
  kYearly = 'y',

  /// @brief Quarterly frequency (string: q)
  kQuarterly = 'q',

  /// @brief Monthly frequency (string: m)
  kMonthly = 'm',

  /// @brief X times a year frequency
  kXTimesAYear = 'x',

  /// @brief Every k years
  kMultiYear = 'u',

  /// @brief Every k years, happens x times
  kXTimesZYears = 'z',

  /// @brief Weekly
  kWeekly = 'w',

  /// @brief Multi-Weekly
  kMultiWeekly = 'e',

  /// @brief Daily (day in a complete week)
  kDaily = 'd',

  /// @brief Multi-Daily (day in a complete week)
  kMultiDaily = 'i',

  /// @brief Day in a week
  kDailyInWeek = 'k',

  /// @brief A list of strings
  kListString = 'l',

  /// @brief A list of 'boost' dates
  kListDate = 'L',

  /// @brief Hourly
  kHourly = 'h',

  /// @brief Minutely
  kMinutely = 'n',

  /// @brief Secondly
  kSecondly = 's',

  /// @brief X times a day
  kXTimesADay = 'a'

};

inline const char *ToString(const FrequencyClass &value) {
  switch (value) {
  case FrequencyClass::kCrossSection:
    return "Cross-Section";
  case FrequencyClass::kYearly:
    return "Yearly";
  case FrequencyClass::kQuarterly:
    return "Quarterly";
  case FrequencyClass::kMonthly:
    return "Monthly";
  case FrequencyClass::kMultiYear:
    return "Multi-Yearly";
  case FrequencyClass::kXTimesAYear:
    return "X-Times-A-Year";
  case FrequencyClass::kXTimesZYears:
    return "X-Times-Z-Years";

  case FrequencyClass::kWeekly:
    return "Weekly";
  case FrequencyClass::kDaily:
    return "Daily";
  case FrequencyClass::kMultiWeekly:
    return "Multi-Weekly";
  case FrequencyClass::kMultiDaily:
    return "Multi-Daily";
  case FrequencyClass::kDailyInWeek:
    return "Daily-In-Week";

  case FrequencyClass::kHourly:
    return "Hourly";
  case FrequencyClass::kMinutely:
    return "Minute(ly)";
  case FrequencyClass::kSecondly:
    return "Second(ly)";
  case FrequencyClass::kXTimesADay:
    return "X-Times-A-Day";

  case FrequencyClass::kListString:
    return "List (String)";
  case FrequencyClass::kListDate:
    return "List (Date)";

  default:
    throw std::logic_error("Invalid type: FrequencyClass");
  };
}

/// @brief Represents a day of week
enum class DayOfWeek {
  kSun = 0,
  kMon = 1,
  kTue = 2,
  kWed = 3,
  kThu = 4,
  kFri = 5,
  kSat = 6,
};

/// @brief Converts a day of week to string
/// @param value The day of week
/// @param abb If true, the first three letters are return
/// @return The string
inline const char *ToString(DayOfWeek value, bool abb = true) {
  switch (value) {
  case DayOfWeek::kSun:
    return abb ? "sun" : "Sunday";
  case DayOfWeek::kMon:
    return abb ? "mon" : "Monday";
  case DayOfWeek::kTue:
    return abb ? "tue" : "Tuesday";
  case DayOfWeek::kWed:
    return abb ? "wed" : "Wednesday";
  case DayOfWeek::kThu:
    return abb ? "thu" : "Thursday";
  case DayOfWeek::kFri:
    return abb ? "fri" : "Friday";
  case DayOfWeek::kSat:
    return abb ? "sat" : "Saturday";
  default:
    throw std::logic_error("Invalid day of week");
  }
}

/// @brief Converts a string to a day of week
/// @param str The string
/// @return The day of week
inline DayOfWeek FromString_DayOfWeek(const char *str) {
  if (StartsWith("sun", str))
    return DayOfWeek::kSun;
  if (StartsWith("mon", str))
    return DayOfWeek::kMon;
  if (StartsWith("tue", str))
    return DayOfWeek::kTue;
  if (StartsWith("wed", str))
    return DayOfWeek::kWed;
  if (StartsWith("thu", str))
    return DayOfWeek::kThu;
  if (StartsWith("fri", str))
    return DayOfWeek::kFri;
  if (StartsWith("sat", str))
    return DayOfWeek::kSat;

  throw std::logic_error("Invalid day of week string");
}

/// @brief Represents a day of week range
struct LDT_EXPORT DayOfWeekRange {
public:
  /// @brief Determines where this range starts
  DayOfWeek mStart;

  /// @brief Determines where this range ends
  DayOfWeek mEnd;

  /// @brief Initializes a new instance of this struct
  /// @param start where range starts
  /// @param end where ranges ends
  DayOfWeekRange(DayOfWeek start = DayOfWeek::kMon,
                 DayOfWeek end = DayOfWeek::kFri);

  /// @brief Gets the length of this range
  Ti GetLength() const;

  /// @brief Calculates distance between two days of week; i.e., number of steps
  /// from \p start to \p end
  /// @param start First point
  /// @param end Second point
  /// @param forward It true, we move forward until we reach \p forward
  /// @return The distance
  static Ti Distance(DayOfWeek start, DayOfWeek end, bool forward);

  /// @brief Determines if a given day of week is in this range
  /// @param value The given day of week
  /// @return true if the given day is equal to start, end or is within this
  /// range
  bool IsInRange(DayOfWeek value) const;

  /// @brief Determines if a given day of week is in this range and calculates
  /// the number of steps to exit in any direction
  /// @param value The given day of week
  /// @param forward If true, it moves forward to exit from this range
  /// @param steps Number of steps to exit from this range
  /// @return true if the given day is equal to start, end or is within this
  /// range
  bool IsOutsideRange(DayOfWeek value, bool forward, Ti &steps) const;

  /// @brief Given a day of week, it calculates the next day of week
  /// @param value The given day of week
  /// @return Next day of week
  static DayOfWeek NextDayOfWeek(DayOfWeek value);

  /// @brief Given a day of week, it calculates the previous day of week
  /// @param value The given day of week
  /// @return Previous day of week
  static DayOfWeek PreviousDayOfWeek(DayOfWeek value);

  /// @brief Converts this range to string
  /// @return The string
  std::string ToString() const;

  /// @brief Converts a string to a range
  /// @param str The string
  /// @return The range
  static DayOfWeekRange Parse(std::string str);
};

/// @brief A base class for a frequency
class LDT_EXPORT Frequency {

public:
  /// @brief Gets the frequency class
  FrequencyClass mClass;

  /// @brief Initializes a new instance of the class
  Frequency(){};

  virtual ~Frequency(){};

  /// @brief Checks the equality of classes for two frequencies and throws error
  /// if they are not equal
  /// @param first First frequency
  /// @param second Second frequency
  static void CheckClassEquality(Frequency const &first,
                                 Frequency const &second);

  /// @brief Generates a list of all available frequencies (generally to be used
  /// in testing)
  /// @param values On exit, it contains the examples
  /// @param listItemsString On exit, it contains the list items for the string
  /// list frequency
  /// @param listItemsDate On exit, it contains the list items for the date list
  /// frequency
  static void Examples(std::vector<std::unique_ptr<Frequency>> &values,
                       std::vector<std::string> &listItemsString,
                       std::vector<boost::gregorian::date> &listItemsDate);

  // #pragma region Operations

  /// @brief Creates a copy of this class.
  /// @return A copy
  virtual std::unique_ptr<Frequency> Clone() const = 0;

  /// @brief Moves this frequency a number of steps forward or backward
  /// @param steps The number of steps. If negative, it moves backward.
  /// Otherwise, it moves forward
  virtual void Next(Ti steps = 1) = 0;

  /// @brief Calculates the distance between this frequency and an other one
  /// @param other The other frequency
  /// @returns h>0 if this frequency is h-steps newer
  /// than \p other. h<0 if this frequency is h-steps older than \p other. Zero
  /// means equality.
  virtual Ti Minus(Frequency const &other) = 0;

  /// @brief Gets frequency class from a given string. Throws error if format is
  /// unknown.
  /// @param classStr Frequency class as a string
  /// @return The frequency class
  static FrequencyClass GetClass(const std::string &classStr);

  /// @brief Gets a frequency from a text
  /// @param str The text
  /// @param classStr The frequency class as a string
  /// @param fClass On exit, it is the frequency class
  /// @param classHasDetails If true, e.g. in 'list's, it uses the \p classStr
  /// to build the lists
  /// @return The frequency
  static std::unique_ptr<Frequency> Parse(const std::string &str,
                                          const std::string &classStr,
                                          FrequencyClass &fClass);

  /// @brief Converts this frequency to a string
  /// @return The string
  virtual std::string ToString() const = 0;

  /// @brief Converts the class of this frequency to a string. It is generally
  /// different from string representation of \ref mClass and contains parameter
  /// @param details If true, e.g. for 'list's, it inserts the items
  /// @return The string
  virtual std::string ToClassString(bool details = false) const = 0;

  // #pragma endregion

  // #pragma region Compare

  /// @brief Compares this frequency with an other frequency
  /// @param other The other frequency
  /// @return 0 for equal, 1 if this frequency is newer, -1 if this frequency is
  /// older
  virtual Ti CompareTo(Frequency const &other) = 0;

  /// @brief Determines if this frequency is equal in value to another one
  /// @param other The other frequency
  /// @return true if this is equal to \p other in value
  bool IsEqualTo(Frequency const &other);

  /// @brief Determines which frequency comes after the other one (i.e., is
  /// newer)
  /// @param other The other frequency
  /// @return true if this frequency is strictly newer than \p other
  bool IsNewerThan(Frequency const &other);

  /// @brief Determines which frequency comes after the other one (i.e., is
  /// newer)
  /// @param other The other frequency
  /// @return true if this frequency is strictly older than \p other
  bool IsOlderThan(Frequency const &other);

  /// @brief Determines which frequency comes after the other one (i.e., is
  /// newer)
  /// @param other The other frequency
  /// @return true if this frequency is equal or newer than \p other
  bool IsEqualOrNewerThan(Frequency const &other);

  /// @brief Determines which frequency comes after the other one (i.e., is
  /// newer)
  /// @param other The other frequency
  /// @return true if this frequency is equal or older than \p other
  bool IsEqualOrOlderThan(Frequency const &other);

  // #pragma endregion
};

/// @brief A frequency that represents a cross-section data
class LDT_EXPORT FrequencyCrossSection : public Frequency {

public:
  /// @brief Gets the current position.
  Ti mPosition = 0;

  /// @brief Initializes a new instance of the class
  /// @param position The position in this frequency
  FrequencyCrossSection(Ti position = 0);

  virtual ~FrequencyCrossSection() override{};

  virtual std::unique_ptr<Frequency> Clone() const override;

  virtual void Next(Ti steps = 1) override;

  virtual Ti CompareTo(Frequency const &other) override;

  virtual Ti Minus(Frequency const &other) override;

  /// @brief Parses a string into a cross-section frequency
  /// @param str The string
  /// @param result On exit, it is the updated result
  static void Parse0(const std::string &str, FrequencyCrossSection &result);

  virtual std::string ToString() const override;

  virtual std::string ToClassString(bool details = false) const override;
};

/// @brief A frequency that represents a list
template <class T = std::string>
class LDT_EXPORT FrequencyList : public Frequency {

public:
  /// @brief Gets the current value
  T mValue;

  /// @brief A pointer to the constructor argument. This class is not the
  /// owner.
  std::vector<T> *pItems = nullptr;

  /// @brief Initializes a new instance of the class
  /// @param value Value of this frequency. It is a member of \p items
  /// @param list Items in this frequency. It can be null, but should be set
  /// later.
  FrequencyList(T value, std::vector<T> *items);

  virtual ~FrequencyList() override;

  /// @brief Gets index of \ref Value in \ref Items
  /// @return The index of \ref Value in \ref Items
  Ti GetIndex();

  virtual std::unique_ptr<Frequency> Clone() const override;

  virtual void Next(Ti steps = 1) override;

  virtual Ti CompareTo(Frequency const &other) override;

  virtual Ti Minus(Frequency const &other) override;

  /// @brief Parses a string into a year-based frequency
  /// @param str The string
  /// @param classStr The class string.
  /// @param fClass The class of the frequency
  /// @param result On exit, it gets updated
  /// @param items It not null, it tries to get the items from \p classStr
  static void Parse0(const std::string &str, const std::string &classStr,
                     const FrequencyClass &fClass, FrequencyList<T> &result,
                     std::vector<T> *items = nullptr);

  virtual std::string ToString() const override;

  virtual std::string ToClassString(bool details = false) const override;

  /// @brief Specialized type of parse for list. It gets the items from \p
  /// classStr
  /// @param str
  /// @param classStr
  /// @param fClass
  /// @param items
  /// @return
  static std::unique_ptr<FrequencyList<T>>
  ParseList(const std::string &str, const std::string &classStr,
            FrequencyClass &fClass, std::vector<T> &items);
};

extern template class ldt::FrequencyList<boost::gregorian::date>;
extern template class ldt::FrequencyList<std::string>;

/// @brief A frequency that represents a year-based data
class LDT_EXPORT FrequencyYearBased : public Frequency {

public:
  /// @brief Gets the year of the frequency
  Ti mYear = 2000;

  /// @brief A variable that determines the number of years in each step
  Ti mYearMulti = 1;

  /// @brief Gets the number of partitions in a year
  Ti mPartitionCount = 12;

  /// @brief Gets the current partition of the frequency
  Ti mPosition = 1;

  FrequencyYearBased(){};

  /// @brief Initializes a new instance of the class
  /// @param year Year of the frequency
  /// @param partitionCount Number of partitions in each year (or month, see \p
  /// partitionMonth). It must be positive.
  /// @param position Current partition of the frequency. It must be positive
  /// and equal or less than \p partitionCount
  /// @param yearMulti If 1, happens every 1 year, If k>0, happens every k years
  FrequencyYearBased(Ti year, Ti partitionCount, Ti position, Ti yearMulti = 1);

  virtual ~FrequencyYearBased() override{};

  /// @brief Creates a 'X times Z years' frequency
  /// @param year
  /// @param X
  /// @param position
  /// @param Z
  /// @return
  static std::unique_ptr<FrequencyYearBased> XTimesZYear(Ti year, Ti X,
                                                         Ti position, Ti Z);

  /// @brief Creates an 'X times a year' frequency
  /// @param year
  /// @param X
  /// @param position
  /// @return The frequency
  static std::unique_ptr<FrequencyYearBased> XTimesAYear(Ti year, Ti X,
                                                         Ti position);

  /// @brief Creates a 'yearly' frequency
  /// @param year
  /// @return The frequency
  static std::unique_ptr<FrequencyYearBased> Yearly(Ti year);

  /// @brief Creates a 'multi-year' frequency
  /// @param year
  /// @param yearStep
  /// @return
  static std::unique_ptr<FrequencyYearBased> MultiYearly(Ti year, Ti yearMulti);

  /// @brief Creates a 'quarterly' frequency
  /// @param year
  /// @param quarter
  /// @return The frequency
  static std::unique_ptr<FrequencyYearBased> Quarterly(Ti year, Ti quarter);

  /// @brief Creates a 'monthly' frequency
  /// @param year
  /// @param month
  /// @return The frequency
  static std::unique_ptr<FrequencyYearBased> Monthly(Ti year, Ti month);

  virtual std::unique_ptr<Frequency> Clone() const override;

  virtual void Next(Ti steps = 1) override;

  virtual Ti CompareTo(Frequency const &other) override;

  virtual Ti Minus(Frequency const &other) override;

  /// @brief Parses a string into a year-based frequency
  /// @param str The string
  /// @param classStr The class string. It is needed for 'x-times-a-year' and
  /// 'x-times-a-month' classes
  /// @param fClass The class of the frequency
  /// @param result On exit, it gets updated
  static void Parse0(const std::string &str, const std::string &classStr,
                     const FrequencyClass &fClass, FrequencyYearBased &result);

  virtual std::string ToString() const override;

  virtual std::string ToClassString(bool details = false) const override;
};

/// @brief A frequency that represents a week-based data
class LDT_EXPORT FrequencyWeekBased : public Frequency {

public:
  /// @brief Gets the day for daily frequency or start of the week for weekly
  /// frequency
  boost::gregorian::date mDay;

  /// @brief Gets the day of week range. If this is not a daily in week
  /// frequency, it is not valid.
  DayOfWeekRange mRange;

  /// @brief In a daily in a week frequency, this is the number of steps moved
  /// forward (positive) or backward (negative) to reach the first or last day
  /// of the range
  Ti mForwardSteps = 0;

  /// @brief If 1 it happens every week or day. If k>0, it happens every k weeks
  /// or days
  Ti mMulti = 1;

  FrequencyWeekBased(){};

  /// @brief Initializes a new instance of this class
  /// @param day Day of the frequency
  /// @param isWeek If true, this is a weekly frequency and \p day is the first
  /// day of a week
  /// @param range If not null, this is daily in week frequency and the range
  /// determines the type of week
  /// @param forward If true and current day of week is outside the range, it
  /// will be set to the start of the range. Otherwise, it is set to the end of
  /// the range.
  /// @param multi If 1 it happens every week or day. If k>0, it happens every k
  /// weeks or days. Be careful in using it when the \ref range is not null
  /// (Type is still dailyInWeek)
  FrequencyWeekBased(boost::gregorian::date day, bool isWeek,
                     DayOfWeekRange *range, bool forward, Ti multi = 1);

  virtual ~FrequencyWeekBased() override{};

  /// @brief Gets a weekly frequency
  /// @param day Start day of the week
  /// @return The frequency
  static std::unique_ptr<FrequencyWeekBased> Weekly(boost::gregorian::date day);

  /// @brief Gets a multi-weekly frequency
  /// @param day
  /// @param multi
  /// @return
  static std::unique_ptr<FrequencyWeekBased>
  MultiWeekly(boost::gregorian::date day, Ti multi);

  /// @brief Gets a daily frequency
  /// @param day day in the frequency
  /// @return The frequency
  static std::unique_ptr<FrequencyWeekBased> Daily(boost::gregorian::date day);

  /// @brief Gets a multi-daily frequency
  /// @param day
  /// @param multi
  /// @return
  static std::unique_ptr<FrequencyWeekBased>
  MultiDaily(boost::gregorian::date day, Ti multi);

  /// @brief Gets a daily in a week frequency
  /// @param day day in the frequency
  /// @param range Determines the type of the week
  /// @param forward If true and current day of week is outside the range, it
  /// will be set to the start of the range. Otherwise, it is set to the end of
  /// the range.
  /// @return The frequency
  static std::unique_ptr<FrequencyWeekBased>
  DailyInWeek(boost::gregorian::date day, DayOfWeekRange range, bool forward);

  virtual std::unique_ptr<Frequency> Clone() const override;

  virtual void Next(Ti steps = 1) override;

  virtual Ti CompareTo(Frequency const &other) override;

  virtual Ti Minus(Frequency const &other) override;

  /// @brief Parses a string into a week-based frequency
  /// @param str The string
  /// @param classStr The class string.
  /// @param fClass The class of the frequency
  /// @param result On exit, it gets updated
  static void Parse0(const std::string &str, const std::string &classStr,
                     const FrequencyClass &fClass, FrequencyWeekBased &result);

  virtual std::string ToString() const override;

  virtual std::string ToClassString(bool details = false) const override;
};

/// @brief A frequency that represents a day-based data
class LDT_EXPORT FrequencyDayBased : public Frequency {

public:
  /// @brief Gets the day (in week) definition of the frequency
  FrequencyWeekBased mDay;

  /// @brief Gets the number of partitions in a day
  Ti mPartitionCount = 24;

  /// @brief Gets the current partition of the frequency
  Ti mPosition = 1;

  FrequencyDayBased(){};

  /// @brief Initializes a new instance of the class
  /// @param day 'daily' or 'daily in week' frequency of this class
  /// @param partitionCount Number of partitions in a day
  /// @param position Current position in a day
  FrequencyDayBased(FrequencyWeekBased &day, Ti partitionCount, Ti position);

  virtual ~FrequencyDayBased() override{};

  /// @brief Creates an 'X times a day' frequency
  /// @param day
  /// @param X
  /// @param position
  /// @return The frequency
  static std::unique_ptr<FrequencyDayBased> XTimesADay(FrequencyWeekBased &day,
                                                       Ti X, Ti position);

  /// @brief Creates an 'hourly' frequency
  /// @param day
  /// @param hour
  /// @return The frequency
  static std::unique_ptr<FrequencyDayBased> Hourly(FrequencyWeekBased &day,
                                                   Ti hour = 1);

  /// @brief Creates a 'minutely' frequency
  /// @param minute
  /// @return The frequency
  static std::unique_ptr<FrequencyDayBased> Minutely(FrequencyWeekBased &day,
                                                     Ti minute = 1);

  /// @brief Creates a 'secondly' frequency
  /// @param day
  /// @param second
  /// @return The frequency
  static std::unique_ptr<FrequencyDayBased> Secondly(FrequencyWeekBased &day,
                                                     Ti second = 1);

  virtual std::unique_ptr<Frequency> Clone() const override;

  virtual void Next(Ti steps = 1) override;

  virtual Ti CompareTo(Frequency const &other) override;

  virtual Ti Minus(Frequency const &other) override;

  /// @brief Parses a string into a day-based frequency
  /// @param str The string
  /// @param classStr The class string.
  /// @param fClass The class of the frequency
  /// @param result On exit, it gets updated
  static void Parse0(const std::string &str, const std::string &classStr,
                     const FrequencyClass &fClass, FrequencyDayBased &result);

  virtual std::string ToString() const override;

  virtual std::string ToClassString(bool details = false) const override;
};

} // namespace ldt
