#include "r_ldt.h"

using namespace Rcpp;
using namespace ldt;

// clang-format off

//' Creates a Cross-Section Frequency
//'
//' This frequency is generally for indexed (or, non-time-series) data. It is an integer that represents the position of the observation.
//'
//' @param position Position of the observation
//'
//' @details
//' \itemize{
//' \item **Value String:** \code{"#"} (number is \code{position})
//' \item **Class String:** \code{"cs"}
//' }
//'
//' @return An object of class 'ldtf'
//' @export
// [[Rcpp::export]]
SEXP F_CrossSection(int position)
// clang-format on
{
  List F = List::create(_["class"] = (int)FrequencyClass::kCrossSection,
                        _["position"] = position);
  F.attr("class") = std::vector<std::string>({"ldtf", "list"});
  return F;
}

// clang-format off

//' Creates a \code{Yearly} Frequency
//'
//' Frequency for a series that happens every year
//'
//' @param year Year of the observation
//'
//' @details
//' \itemize{
//' \item **Value String:** \code{"#"} (number is \code{year})
//' \item **Class String:** \code{"y"}
//' }
//'
//' @return An object of class 'ldtf'
//' @export
// [[Rcpp::export]]
SEXP F_Yearly(int year)
// clang-format on
{
  List F =
      List::create(_["class"] = (int)FrequencyClass::kYearly, _["year"] = year);
  F.attr("class") = std::vector<std::string>({"ldtf", "list"});
  return F;
}

// clang-format off

//' Creates a \code{Quarterly} Frequency
//'
//' Frequency for a series that happens every quarter
//'
//' @param year Year of the observation
//' @param quarter Quarter of the observation (1 to 4)
//'
//' @details
//' \itemize{
//' \item **Value String:** \code{"#q#"} (first # is \code{year}, second # is
//'  \code{quarter}; e.g., 2010q3 or 2010q4.
//'  Note that 2000q0 or 2000q5 are invalid.
//' \item **Class String:** \code{"q"}
//' }
//'
//' @return An object of class 'ldtf'
//' @export
// [[Rcpp::export]]
SEXP F_Quarterly(int year, int quarter)
// clang-format on
{
  List F = List::create(_["class"] = (int)FrequencyClass::kQuarterly,
                        _["year"] = year, _["quarter"] = quarter);
  F.attr("class") = std::vector<std::string>({"ldtf", "list"});
  return F;
}

// clang-format off

//' Creates a \code{Monthly} Frequency
//'
//' Frequency for a series that happens every month
//'
//' @param year Year of the observation
//' @param month Month of the observation
//'
//' @details
//' \itemize{
//' \item **Value String:** \code{"#m#"} (first # is the \code{year}, second # is
//'  \code{month} (1 to 12); e.g., 2010m8 or 2010m12.
//'  Note that 2000m0 or 2000m13 are invalid.
//' \item **Class String:** \code{"m"}
//' }
//'
//' @return An object of class 'ldtf'
//' @export
// [[Rcpp::export]]
SEXP F_Monthly(int year, int month)
// clang-format on
{
  List F = List::create(_["class"] = (int)FrequencyClass::kMonthly,
                        _["year"] = year, _["month"] = month);
  F.attr("class") = std::vector<std::string>({"ldtf", "list"});
  return F;
}

// clang-format off

//' Creates a \code{Multi-Yearly} Frequency
//'
//' Frequency for a series that happens every \code{z} years
//'
//' @param year Year of the observation
//' @param z Number of years
//'
//' @details
//' \itemize{
//' \item **Value String:** \code{"#"} (similar to \code{Yearly})
//' \item **Class String:** \code{"z#"} (integer represents the
//' \code{z}; e.g., z3)
//' }
//'
//' @return An object of class 'ldtf'
//' @export
// [[Rcpp::export]]
SEXP F_MultiYearly(int year, int z)
// clang-format on
{
  List F = List::create(_["class"] = (int)FrequencyClass::kMultiYear,
                        _["year"] = year, _["z"] = z);
  F.attr("class") = std::vector<std::string>({"ldtf", "list"});
  return F;
}

// clang-format off

//' Creates an \code{X-Times-A-Year} Frequency
//'
//' Frequency for a series that happens \code{x} times every year
//'
//' @param year Year of the observation
//' @param x Number of observation in each year
//' @param position Position of the current observation
//'
//' @details
//' \itemize{
//' \item **Value String:** \code{"#:#"} (first # is \code{year} and second # is
//'  \code{position};
//'  e.g., 2010:8/12 or 2010:10/10.
//'  Note that 2000:0/2 or 2000:13/12 are invalid.
//' \item **Class String:** \code{"y#"} (the number is \code{x})
//' }
//'
//' @return An object of class 'ldtf'
//' @export
// [[Rcpp::export]]
SEXP F_XTimesAYear(int year, int x, int position)
// clang-format on
{
  List F = List::create(_["class"] = (int)FrequencyClass::kXTimesAYear,
                        _["year"] = year, _["x"] = x, _["position"] = position);
  F.attr("class") = std::vector<std::string>({"ldtf", "list"});
  return F;
}

// clang-format off

//' Creates an \code{X-Times-Z-Years} Frequency
//'
//' Frequency for a series that happens \code{x} times each \code{z} years
//'
//' @param year Year of the observation
//' @param x Number of partitons in each z years
//' @param z Number of years
//' @param position Position of the current observation
//'
//' @details
//' \itemize{
//' \item **Value String:** \code{"#:#"} (Similar to \code{X-Times-A-Year})
//' \item **Class String:** \code{"x#z#"} (first # is \code{x}, second # is \code{z};
//' e.g., x23z4 means 23 times every 4 years)
//' }
//'
//' @return An object of class 'ldtf'
//' @export
// [[Rcpp::export]]
SEXP F_XTimesZYear(int year, int x, int z, int position)
// clang-format on
{
  List F = List::create(_["class"] = (int)FrequencyClass::kXTimesZYears,
                        _["year"] = year, _["x"] = x, _["z"] = z,
                        _["position"] = position);
  F.attr("class") = std::vector<std::string>({"ldtf", "list"});
  return F;
}

// clang-format off

//' Creates a \code{Weekly} Frequency
//'
//' Frequency for a series that happens every week
//'
//' @param year Year of the observation
//' @param month Month of the observation
//' @param day Day of the observation. It points to the first day of the week
//'
//' @details
//' \itemize{
//' \item **Value String:** \code{"YYYYMMDD"} (\code{YYYY} is the \code{year}, \code{MM} is \code{month} and \code{DD} is \code{day})
//' \item **Class String:** \code{"w"}
//' }
//'
//' @return An object of class 'ldtf'
//' @export
// [[Rcpp::export]]
SEXP F_Weekly(int year, int month, int day)
// clang-format on
{
  List F = List::create(_["class"] = (int)FrequencyClass::kWeekly,
                        _["year"] = year, _["month"] = month, _["day"] = day);
  F.attr("class") = std::vector<std::string>({"ldtf", "list"});
  return F;
}

// clang-format off

//' Creates a \code{Multi-Weekly} Frequency
//'
//' Frequency for a series that happens every 'k' weeks
//'
//' @param year Year of the observation
//' @param month Month of the observation
//' @param day First day of the observation. It points to the first day of the week
//' @param k Number of weeks
//'
//' @details
//' \itemize{
//' \item **Value String:** \code{"YYYYMMDD"} (similar to \code{Weekly})
//' \item **Class String:** \code{"w#"} (the number is \code{k}; e.g., w3 means every 3 weeks)
//' }
//'
//' @return An object of class 'ldtf'
//' @export
// [[Rcpp::export]]
SEXP F_MultiWeekly(int year, int month, int day, int k)
// clang-format on
{
  List F = List::create(_["class"] = (int)FrequencyClass::kMultiWeekly,
                        _["year"] = year, _["month"] = month, _["day"] = day,
                        _["k"] = k);
  F.attr("class") = std::vector<std::string>({"ldtf", "list"});
  return F;
}

// clang-format off

//' Creates a \code{Daily} Frequency
//'
//' Frequency for a series that happens every day
//'
//' @param year Year of the observation
//' @param month Month of the observation
//' @param day Day of the observation.
//'
//' @details
//' \itemize{
//' \item **Value String:** \code{"YYYYMMDD"} (similar to \code{Weekly})
//' \item **Class String:** \code{"d"}
//' }
//'
//' @return An object of class 'ldtf'
//' @export
// [[Rcpp::export]]
SEXP F_Daily(int year, int month, int day)
// clang-format on
{
  List F = List::create(_["class"] = (int)FrequencyClass::kDaily,
                        _["year"] = year, _["month"] = month, _["day"] = day);
  F.attr("class") = std::vector<std::string>({"ldtf", "list"});
  return F;
}

// clang-format off

//' Creates an \code{Multi-Daily} Frequency
//'
//' Frequency for a series that happens every \code{k} days
//'
//' @param year Year of the observation
//' @param month Month of the observation
//' @param day First day of the observation
//' @param k Number of the days
//'
//' @details
//' \itemize{
//' \item **Value String:** \code{"YYYYMMDD"} (similar to \code{Weekly})
//' \item **Class String:** \code{"d#"} (the number is \code{k})
//' }
//'
//' @return An object of class 'ldtf'
//' @export
// [[Rcpp::export]]
SEXP F_MultiDaily(int year, int month, int day, int k)
// clang-format on
{
  List F = List::create(_["class"] = (int)FrequencyClass::kMultiDaily,
                        _["year"] = year, _["month"] = month, _["day"] = day,
                        _["k"] = k);
  F.attr("class") = std::vector<std::string>({"ldtf", "list"});
  return F;
}

// clang-format off

//' Creates an \code{Daily-In-Week} Frequency
//'
//' Frequency for a series that happens every in the days of a week
//'
//' @param year Year of the observation
//' @param month Month of the observation
//' @param day First day of the observation
//' @param weekStart First day of the week. It can be \code{sun}, \code{mon},
//' \code{tue}, \code{wed}, \code{thu}, \code{fri}, and \code{sat}
//' @param weekEnd Last day of the week. See \code{weekStart}.
//' Together, they define the week
//' @param forward If current date in not in the week,
//' if true, it moves forward to the first day of the week.
//' Otherwise, it moves backward to the last day of the week.
//'
//' @details
//' \itemize{
//' \item **Value String:** \code{"YYYYMMDD"} (similar to \code{Weekly})
//' \item **Class String:** \code{"i:...-..."} (the first ... is \code{weekStart}
//' and the second ... is \code{weekEnd}; e.g., \code{i:mon-fri} means
//' a week that is from Monday to Friday)
//' }
//'
//' @return An object of class 'ldtf'
//' @export
// [[Rcpp::export]]
SEXP F_DailyInWeek(int year, int month, int day, std::string weekStart,
                   std::string weekEnd, bool forward)
// clang-format on
{
  auto range = DayOfWeekRange::Parse(weekStart + std::string("-") + weekEnd);
  auto freq = FrequencyWeekBased(boost::gregorian::date(year, month, day),
                                 false, &range, forward);
  List F = List::create(
      _["class"] = (int)FrequencyClass::kDailyInWeek,
      _["year"] = (int)freq.mDay.year(), _["month"] = (int)freq.mDay.month(),
      _["day"] = (int)freq.mDay.day(), _["weekStart"] = (int)range.mStart,
      _["weekEnd"] = (int)range.mEnd);
  F.attr("class") = std::vector<std::string>({"ldtf", "list"});
  return F;
}

// clang-format off

//' Creates an \code{List-String} Frequency
//'
//' Frequency for a series that is labeled by string
//'
//' @param items Items of the list
//' @param value Current item
//'
//' @details
//' \itemize{
//' \item **Value String:** \code{"..."} (in which ... is the \code{value})
//' \item **Class String:** \code{Ls} or \code{Ls:...} (in which ...
//' is the semi-colon separated \code{items})
//' }
//'
//' @return An object of class 'ldtf'
//' @export
// [[Rcpp::export]]
SEXP F_ListString(std::vector<std::string> items, std::string value)
// clang-format on
{
  List F = List::create(_["class"] = (int)FrequencyClass::kListString,
                        _["items"] = items, _["value"] = value);
  F.attr("class") = std::vector<std::string>({"ldtf", "list"});
  return F;
}

// clang-format off

//' Creates an \code{List-Date} Frequency
//'
//' Frequency for a series that is labeled by dates
//'
//' @param items Items of the list in string format: \code{YYYYMMDD}
//' @param value Current value in string format: \code{YYYYMMDD}
//'
//' @details
//' \itemize{
//' \item **Value String:** \code{"YYYYMMDD"} (i.e., \code{item})
//' \item **Class String:** \code{Ld} or \code{Ld:...} (in which ...
//' is the semi-colon separated \code{items})
//' }
//'
//' @return An object of class 'ldtf'
//' @export
// [[Rcpp::export]]
SEXP F_ListDate(std::vector<std::string> items, std::string value)
// clang-format on
{
  // auto d = boost::gregorian::date_from_iso_string(value);
  List F = List::create(_["class"] = (int)FrequencyClass::kListDate,
                        _["items"] = items, _["value"] = value);
  F.attr("class") = std::vector<std::string>({"ldtf", "list"});
  return F;
}

// clang-format off

//' Creates an 'Hourly' Frequency
//'
//' Frequency for a series that happens every hour
//'
//' @param day A 'Day-based' frequency such as \code{Daily} or \code{Daily-In-Week}
//' @param hour Index of hour in the day (1 to 24)
//'
//' @details
//' \itemize{
//' \item **Value String:** \code{"YYYYMMDD:#"} (the number is \code{hour})
//' \item **Class String:** \code{ho|...} (the ... is the 'Class String' of \code{day})
//' }
//'
//' @return An object of class 'ldtf'
//' @export
// [[Rcpp::export]]
SEXP F_Hourly(SEXP day, int hour)
// clang-format on
{
  List F = List::create(_["class"] = (int)FrequencyClass::kHourly,
                        _["day"] = day, _["hour"] = hour);
  F.attr("class") = std::vector<std::string>({"ldtf", "list"});
  return F;
}

// clang-format off

//' Creates an 'Minute-ly' Frequency
//'
//' Frequency for a series that happens every minute
//'
//' @param day A 'Day-based' frequency such as daily or daily-in-week
//' @param minute Index of Minute in the day (1 to 1440)
//'
//' @details
//' \itemize{
//' \item **Value String:** \code{"YYYYMMDD:#"} (the number is \code{minute})
//' \item **Class String:** \code{mi|...} (the ... is the 'Class String' of \code{day})
//' }
//'
//' @return An object of class 'ldtf'
//' @export
// [[Rcpp::export]]
SEXP F_Minute_ly(SEXP day, int minute)
// clang-format on
{
  List F = List::create(_["class"] = (int)FrequencyClass::kMinutely,
                        _["day"] = day, _["minute"] = minute);
  F.attr("class") = std::vector<std::string>({"ldtf", "list"});
  return F;
}

// clang-format off

//' Creates an 'Second-ly' Frequency
//'
//' Frequency for a series that happens every second
//'
//' @param day A 'Day-based' frequency such as daily or daily-in-week
//' @param second Index of second in the day (1 to 86400)
//'
//' @details
//' \itemize{
//' \item **Value String:** \code{"YYYYMMDD:#"} (the number is \code{second})
//' \item **Class String:** \code{se|...} (the ... is the 'Class String' of \code{day})
//' }
//'
//' @return An object of class 'ldtf'
//' @export
// [[Rcpp::export]]
SEXP F_Second_ly(SEXP day, int second)
// clang-format on
{
  List F = List::create(_["class"] = (int)FrequencyClass::kSecondly,
                        _["day"] = day, _["second"] = second);
  F.attr("class") = std::vector<std::string>({"ldtf", "list"});
  return F;
}

// clang-format off

//' Creates an 'X-Times-A-Day' Frequency
//'
//' Frequency for a series that happens x times in a day
//'
//' @param day A 'Day-based' frequency such as daily or daily-in-week
//' @param x Number of observations in a day
//' @param position Current position
//'
//' @details
//' \itemize{
//' \item **Value String:** \code{"#"} (the number is \code{hour})
//' \item **Class String:** \code{"da#|..."} (the number is \code{x}
//' and ... is the 'Class String' of \code{day}))
//' }
//'
//' @return An object of class 'ldtf'
//' @export
// [[Rcpp::export]]
SEXP F_XTimesADay(SEXP day, int x, int position)
// clang-format on
{
  List F = List::create(_["class"] = (int)FrequencyClass::kXTimesADay,
                        _["day"] = day, _["x"] = x, _["position"] = position);

  F.attr("class") = std::vector<std::string>({"ldtf", "list"});
  return F;
}

void getCh(CharacterVector f, std::vector<std::string> &result) {
  result.resize(f.size());
  for (int i = 0; i < f.size(); i++)
    result.at(i) = std::string(f[i]);
}

std::unique_ptr<FrequencyWeekBased> GetFreqFromSEXP_week(List f) {
  auto fclass = (FrequencyClass)as<int>(f["class"]);

  switch (fclass) {

  case FrequencyClass::kWeekly:
    return FrequencyWeekBased::Weekly(boost::gregorian::date(
        as<int>(f["year"]), as<int>(f["month"]), as<int>(f["day"])));
  case FrequencyClass::kMultiWeekly:
    return FrequencyWeekBased::MultiWeekly(
        boost::gregorian::date(as<int>(f["year"]), as<int>(f["month"]),
                               as<int>(f["day"])),
        as<int>(f["k"]));
  case FrequencyClass::kDaily:
    return FrequencyWeekBased::Daily(boost::gregorian::date(
        as<int>(f["year"]), as<int>(f["month"]), as<int>(f["day"])));
  case FrequencyClass::kMultiDaily:
    return FrequencyWeekBased::MultiDaily(
        boost::gregorian::date(as<int>(f["year"]), as<int>(f["month"]),
                               as<int>(f["day"])),
        as<int>(f["k"]));
  case FrequencyClass::kDailyInWeek: {
    auto range = DayOfWeekRange((DayOfWeek)as<int>(f["weekStart"]),
                                (DayOfWeek)as<int>(f["weekEnd"]));
    return FrequencyWeekBased::DailyInWeek(
        boost::gregorian::date(as<int>(f["year"]), as<int>(f["month"]),
                               as<int>(f["day"])),
        range, true);
  }
  default:
    throw std::logic_error("use this class for week-based frequency");
  }
}

std::unique_ptr<Frequency>
GetFreqFromSEXP(SEXP value, std::vector<std::string> &listItems,
                std::vector<boost::gregorian::date> &listItemsDate) {
  auto f = as<List>(value);
  auto fclass = (FrequencyClass)as<int>(f["class"]);

  switch (fclass) {
  case FrequencyClass::kCrossSection:
    return std::unique_ptr<FrequencyCrossSection>(
        new FrequencyCrossSection(as<int>(f["position"])));

  case FrequencyClass::kYearly:
    return FrequencyYearBased::Yearly(as<int>(f["year"]));
  case FrequencyClass::kQuarterly:
    return FrequencyYearBased::Quarterly(as<int>(f["year"]),
                                         as<int>(f["quarter"]));
  case FrequencyClass::kMonthly:
    return FrequencyYearBased::Monthly(as<int>(f["year"]), as<int>(f["month"]));
  case FrequencyClass::kMultiYear:
    return FrequencyYearBased::MultiYearly(as<int>(f["year"]), as<int>(f["z"]));
  case FrequencyClass::kXTimesAYear:
    return FrequencyYearBased::XTimesAYear(as<int>(f["year"]), as<int>(f["x"]),
                                           as<int>(f["position"]));
  case FrequencyClass::kXTimesZYears:
    return FrequencyYearBased::XTimesZYear(as<int>(f["year"]), as<int>(f["x"]),
                                           as<int>(f["position"]),
                                           as<int>(f["z"]));

  case FrequencyClass::kWeekly:
  case FrequencyClass::kMultiWeekly:
  case FrequencyClass::kDaily:
  case FrequencyClass::kMultiDaily:
  case FrequencyClass::kDailyInWeek:
    return GetFreqFromSEXP_week(f);

  case FrequencyClass::kListString: {
    getCh(f["items"], listItems);
    return std::unique_ptr<FrequencyList<std::string>>(
        new FrequencyList<std::string>(as<std::string>(f["value"]),
                                       &listItems));
  }
  case FrequencyClass::kListDate: {
    getCh(f["items"], listItems);
    for (auto const &d : listItems)
      listItemsDate.push_back(boost::gregorian::date_from_iso_string(d));
    return std::unique_ptr<FrequencyList<boost::gregorian::date>>(
        new FrequencyList<boost::gregorian::date>(
            boost::gregorian::date_from_iso_string(as<std::string>(f["value"])),
            &listItemsDate));
  }

  case FrequencyClass::kHourly: {
    auto day = GetFreqFromSEXP_week(as<List>(f["day"]));
    return FrequencyDayBased::Hourly(*day.get(), as<int>(f["hour"]));
  }
  case FrequencyClass::kMinutely: {
    auto day = GetFreqFromSEXP_week(f["day"]);
    return FrequencyDayBased::Minutely(*day.get(), as<int>(f["minute"]));
  }
  case FrequencyClass::kSecondly: {
    auto day = GetFreqFromSEXP_week(f["day"]);
    return FrequencyDayBased::Secondly(*day.get(), as<int>(f["second"]));
  }
  case FrequencyClass::kXTimesADay: {
    auto day = GetFreqFromSEXP_week(f["day"]);
    return FrequencyDayBased::XTimesADay(*day.get(), as<int>(f["x"]),
                                         as<int>(f["position"]));
  }

  default:
    throw std::logic_error("not implemeted for this type of frequency");
  }
}

// clang-format off

//' Converts an \code{ldtf} Object to String
//'
//' The format is explained in \code{F_?} functions.
//'
//' @param value value of the frequency. It must be an \code{ldtf}
//' object returned from \code{F_?} functions.
//'
//' @return An object of class 'ldtf'
//' @export
// [[Rcpp::export]]
std::string ToString_F(SEXP value)
// clang-format on
{
  std::vector<std::string> listItems;
  std::vector<boost::gregorian::date> listItemsDate;
  auto F = GetFreqFromSEXP(value, listItems, listItemsDate);
  return F.get()->ToString();
}

// clang-format off

//' Converts an \code{ldtf} Object to String
//'
//' The format is explained in \code{F_?} functions.
//'
//' @param value value of the frequency. It must be an \code{ldtf}
//' object returned from \code{F_?} functions.
//'
//' @return An object of class 'ldtf'
//' @export
// [[Rcpp::export]]
std::string ToClassString_F(SEXP value)
// clang-format on
{
  std::vector<std::string> listItems;
  std::vector<boost::gregorian::date> listItemsDate;
  auto F = GetFreqFromSEXP(value, listItems, listItemsDate);
  return F.get()->ToClassString(true);
}

// clang-format off

//' Similar to \code{ToString_F} and Return Value and Class as String
//'
//' The format is explained in \code{F_?} functions.
//'
//' @param value value of the frequency. It must be an \code{ldtf}
//' object returned from \code{F_?} functions.
//'
//' @return An object of class 'ldtf'
//' @export
// [[Rcpp::export]]
List ToString_F0(SEXP value)
// clang-format on
{
  std::vector<std::string> listItems;
  std::vector<boost::gregorian::date> listItemsDate;
  auto F = GetFreqFromSEXP(value, listItems, listItemsDate);
  List I = List::create(_["value"] = F.get()->ToString(),
                        _["class"] = F.get()->ToClassString(),
                        _["classType"] = ToString(F.get()->mClass));
  return I;
}

SEXP Parse_F_week(FrequencyClass fClass, Frequency *F) {

  switch (fClass) {

  case FrequencyClass::kWeekly: {
    auto f = dynamic_cast<FrequencyWeekBased const &>(*F);
    return F_Weekly(f.mDay.year(), f.mDay.month(), f.mDay.day());
  }
  case FrequencyClass::kMultiWeekly: {
    auto f = dynamic_cast<FrequencyWeekBased const &>(*F);
    return F_MultiWeekly(f.mDay.year(), f.mDay.month(), f.mDay.day(), f.mMulti);
  }
  case FrequencyClass::kDaily: {
    auto f = dynamic_cast<FrequencyWeekBased const &>(*F);
    return F_Daily(f.mDay.year(), f.mDay.month(), f.mDay.day());
  }
  case FrequencyClass::kMultiDaily: {
    auto f = dynamic_cast<FrequencyWeekBased const &>(*F);
    return F_MultiDaily(f.mDay.year(), f.mDay.month(), f.mDay.day(), f.mMulti);
  }
  case FrequencyClass::kDailyInWeek: {
    auto f = dynamic_cast<FrequencyWeekBased const &>(*F);
    return F_DailyInWeek(f.mDay.year(), f.mDay.month(), f.mDay.day(),
                         ToString(f.mRange.mStart), ToString(f.mRange.mEnd),
                         true);
  }

  default:
    throw std::logic_error(
        "Invalid frequency class. week-based frequency is expected");
  }
}

// clang-format off

//' Converts back a String to \code{ldtf} Object
//'
//' The format is explained in \code{F_?} functions.
//'
//' @param str value of the frequency. It must be an \code{ldtf}
//' object returned from \code{F_?} functions.
//' @param classStr class of the frequency
//'
//' @return An object of class 'ldtf'
//' @export
// [[Rcpp::export]]
SEXP Parse_F(std::string str, std::string classStr)
// clang-format on
{
  FrequencyClass fClass;
  auto F = Frequency::Parse(str, classStr, fClass);

  switch (fClass) {
  case FrequencyClass::kCrossSection: {
    auto f = dynamic_cast<FrequencyCrossSection const &>(*F.get());
    return F_CrossSection(f.mPosition);
  }

  case FrequencyClass::kYearly: {
    auto f = dynamic_cast<FrequencyYearBased const &>(*F.get());
    return F_Yearly(f.mYear);
  }
  case FrequencyClass::kQuarterly: {
    auto f = dynamic_cast<FrequencyYearBased const &>(*F.get());
    return F_Quarterly(f.mYear, f.mPosition);
  }
  case FrequencyClass::kMonthly: {
    auto f = dynamic_cast<FrequencyYearBased const &>(*F.get());
    return F_Monthly(f.mYear, f.mPosition);
  }
  case FrequencyClass::kMultiYear: {
    auto f = dynamic_cast<FrequencyYearBased const &>(*F.get());
    return F_MultiYearly(f.mYear, f.mYearMulti);
  }
  case FrequencyClass::kXTimesAYear: {
    auto f = dynamic_cast<FrequencyYearBased const &>(*F.get());
    return F_XTimesAYear(f.mYear, f.mPartitionCount, f.mPosition);
  }
  case FrequencyClass::kXTimesZYears: {
    auto f = dynamic_cast<FrequencyYearBased const &>(*F.get());
    return F_XTimesZYear(f.mYear, f.mPartitionCount, f.mYearMulti, f.mPosition);
  }

  case FrequencyClass::kWeekly:
  case FrequencyClass::kMultiWeekly:
  case FrequencyClass::kDaily:
  case FrequencyClass::kMultiDaily:
  case FrequencyClass::kDailyInWeek:
    return Parse_F_week(fClass, F.get());

  case FrequencyClass::kHourly: {
    auto f = dynamic_cast<FrequencyDayBased const &>(*F.get());
    auto fw = Parse_F_week(f.mDay.mClass, &f.mDay);
    return F_Hourly(fw, f.mPosition);
  }
  case FrequencyClass::kMinutely: {
    auto f = dynamic_cast<FrequencyDayBased const &>(*F.get());
    auto fw = Parse_F_week(f.mDay.mClass, &f.mDay);
    return F_Minute_ly(fw, f.mPosition);
  }
  case FrequencyClass::kSecondly: {
    auto f = dynamic_cast<FrequencyDayBased const &>(*F.get());
    auto fw = Parse_F_week(f.mDay.mClass, &f.mDay);
    return F_Second_ly(fw, f.mPosition);
  }
  case FrequencyClass::kXTimesADay: {
    auto f = dynamic_cast<FrequencyDayBased const &>(*F.get());
    auto fw = Parse_F_week(f.mDay.mClass, &f.mDay);
    return F_XTimesADay(fw, f.mPartitionCount, f.mPosition);
  }

  case FrequencyClass::kListString: {
    auto f0 = FrequencyList<std::string>("", nullptr);
    std::vector<std::string> items0;
    FrequencyList<std::string>::Parse0(str, classStr, fClass, f0, &items0);
    auto dd = F_ListString(items0, f0.mValue);
    return dd;
  }

  case FrequencyClass::kListDate: {
    auto f0 = FrequencyList<boost::gregorian::date>(boost::gregorian::date(),
                                                    nullptr);
    std::vector<boost::gregorian::date> items0;
    FrequencyList<boost::gregorian::date>::Parse0(str, classStr, fClass, f0,
                                                  &items0);
    std::vector<std::string> items1;
    for (auto &d : items0)
      items1.push_back(boost::gregorian::to_iso_string(d));
    auto dd = F_ListDate(items1, boost::gregorian::to_iso_string(f0.mValue));
    return dd;
  }

  default:
    throw std::logic_error("not implemeted for this type of frequency");
  }
}

// clang-format off

//' Generates a Sequence for a frequency
//'
//'
//' @param start first element of the sequence. It must be an \code{ldtf}
//' object returned from \code{F_?} functions.
//' @param length Length of the sequence
//'
//' @return A list of strings
//' @export
// [[Rcpp::export]]
std::vector<std::string> Sequence_F(SEXP start, int length)
// clang-format on
{
  std::vector<std::string> listItems;
  std::vector<boost::gregorian::date> listItemsDate;
  auto F = GetFreqFromSEXP(start, listItems, listItemsDate);
  auto f = F.get();

  std::vector<std::string> list;
  for (Ti i = 0; i < length; i++) {
    list.push_back(f->ToString());
    f->Next(1);
  }

  return list;
}
