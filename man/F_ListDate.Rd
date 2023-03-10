% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{F_ListDate}
\alias{F_ListDate}
\title{Creates an \code{List-Date} Frequency}
\usage{
F_ListDate(items, value)
}
\arguments{
\item{items}{Items of the list in string format: \code{YYYYMMDD}}

\item{value}{Current value in string format: \code{YYYYMMDD}}
}
\value{
An object of class 'ldtf'
}
\description{
Frequency for a series that is labeled by dates
}
\details{
\itemize{
\item \strong{Value String:} \code{"YYYYMMDD"} (i.e., \code{item})
\item \strong{Class String:} \code{Ld} or \code{Ld:...} (in which ...
is the semi-colon separated \code{items})
}
}
