% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{GetSearchOptions}
\alias{GetSearchOptions}
\title{Options for 'Search Options'}
\usage{
GetSearchOptions(parallel = FALSE, reportInterval = 2L, printMsg = FALSE)
}
\arguments{
\item{parallel}{(bool) If true, it uses a parallel search. It generally changes the speed and memory usage.}

\item{reportInterval}{(int) Time interval (in seconds) for reporting the progress (if the change is significant). Set zero to disable.}

\item{printMsg}{(bool) Set false to disable printing the details.}
}
\value{
A list with the given options.
}
\description{
Creates a list with predefined Search options.
}
