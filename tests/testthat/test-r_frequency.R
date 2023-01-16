
YearBased <- list(
  list(A = F_CrossSection(10), B = "10", C = "cs"),
  list(A = F_Yearly(2000), B = "2000", C = "y"),
  list(A = F_Quarterly(2000, 4), B = "2000Q4", C = "q"),
  list(A = F_Monthly(2000, 1), B = "2000M1", C = "m"),
  list(A = F_MultiYearly(2000, 3), B = "2000", C = "z3"),
  list(A = F_XTimesAYear(2000, 3, 1), B = "2000:1", C = "y3"),
  list(A = F_XTimesZYear(2000, 4, 2, 1), B = "2000:1", C = "x4z2")
)

WeekBased <- list(
  list(A = F_Weekly(2000, 10, 11), B = "20001011", C = "w"),
  list(A = F_Daily(2000, 10, 11), B = "20001011", C = "d"),
  list(A = F_MultiWeekly(2000, 10, 11, 4), B = "20001011", C = "w4"),
  list(A = F_MultiDaily(2000, 10, 11, 5), B = "20001011", C = "d5"),
  list(A = F_DailyInWeek(2000, 10, 11, "mon", "sun", TRUE), B = "20001011", C = "i:mon-sun")
)

DayBased_Weekly <- list(
  list(A = F_Hourly(WeekBased[[1]]$A, 5), B = "20001011:5", C = "ho|w"),
  list(A = F_Minute_ly(WeekBased[[1]]$A, 100), B = "20001011:100", C = "mi|w"),
  list(A = F_Second_ly(WeekBased[[1]]$A, 200), B = "20001011:200", C = "se|w"),
  list(A = F_XTimesADay(WeekBased[[1]]$A, 12, 6), B = "20001011:6", C = "da12|w")
)

DayBased_Daily <- list(
  list(A = F_Hourly(WeekBased[[2]]$A, 5), B = "20001011:5", C = "ho|d"),
  list(A = F_Minute_ly(WeekBased[[2]]$A, 100), B = "20001011:100", C = "mi|d"),
  list(A = F_Second_ly(WeekBased[[2]]$A, 200), B = "20001011:200", C = "se|d"),
  list(A = F_XTimesADay(WeekBased[[2]]$A, 12, 6), B = "20001011:6", C = "da12|d")
)

DayBased_MultiWeekly <- list(
  list(A = F_Hourly(WeekBased[[3]]$A, 5), B = "20001011:5", C = "ho|w4"),
  list(A = F_Minute_ly(WeekBased[[3]]$A, 100), B = "20001011:100", C = "mi|w4"),
  list(A = F_Second_ly(WeekBased[[3]]$A, 200), B = "20001011:200", C = "se|w4"),
  list(A = F_XTimesADay(WeekBased[[3]]$A, 12, 6), B = "20001011:6", C = "da12|w4")
)

DayBased_MultiDaily <- list(
  list(A = F_Hourly(WeekBased[[4]]$A, 5), B = "20001011:5", C = "ho|d5"),
  list(A = F_Minute_ly(WeekBased[[4]]$A, 100), B = "20001011:100", C = "mi|d5"),
  list(A = F_Second_ly(WeekBased[[4]]$A, 200), B = "20001011:200", C = "se|d5"),
  list(A = F_XTimesADay(WeekBased[[4]]$A, 12, 6), B = "20001011:6", C = "da12|d5")
)

DayBased_DailyInWeek <- list(
  list(A = F_Hourly(WeekBased[[5]]$A, 5), B = "20001011:5", C = "ho|i:mon-sun"),
  list(A = F_Minute_ly(WeekBased[[5]]$A, 100), B = "20001011:100", C = "mi|i:mon-sun"),
  list(A = F_Second_ly(WeekBased[[5]]$A, 200), B = "20001011:200", C = "se|i:mon-sun"),
  list(A = F_XTimesADay(WeekBased[[5]]$A, 12, 6), B = "20001011:6", C = "da12|i:mon-sun")
)

List_String <- list(
  list(A = F_ListString(c("A", "B", "C", "D"), "C"), B = "C", C = "Ls:A;B;C;D"),
  list(A = F_ListString(c("A", "B", "C", "D"), "D"), B = "D", C = "Ls:A;B;C;D")
)

List_Date <- list(
  list(A = F_ListDate(c("20001001", "20001101", "20001202", "20010104"), "20001001"), B = "20001001", C = "Ld:20001001;20001101;20001202;20010104")
)

test_that_tp <- function(L) {
  for (f in L) {
    s <- ToString_F(f$A)
    expect_equal(f$B, s)

    c <- ToClassString_F(f$A)
    expect_equal(f$C, c)

    g <- Parse_F(s, c)
    expect_equal(f$A, g)
  }
}

test_that("Year-Based ToString and Parse works", {
  test_that_tp(YearBased)
})

test_that("Week-Based ToString and Parse works", {
  test_that_tp(WeekBased)
})

test_that("DayBased_Weekly ToString and Parse works", {
  test_that_tp(DayBased_Weekly)
})

test_that("DayBased_Daily ToString and Parse works", {
  test_that_tp(DayBased_Daily)
})

test_that("DayBased_MultiWeekly ToString and Parse works", {
  test_that_tp(DayBased_MultiWeekly)
})

test_that("DayBased_MultiDaily ToString and Parse works", {
  test_that_tp(DayBased_MultiDaily)
})

test_that("DayBased_DailyInWeek ToString and Parse works", {
  test_that_tp(DayBased_DailyInWeek)
})

test_that("List_String ToString and Parse works", {
  test_that_tp(List_String)
})

test_that("List_Date ToString and Parse works", {
  test_that_tp(List_Date)
})
