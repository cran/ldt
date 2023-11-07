

#' @useDynLib ldt
#' @importFrom Rcpp sourceCpp
NULL


#' IMF's Primary Commodity Prices
#'
#' This is a subset of the IMF's Primary Commodity Prices dataset (non-index data is omitted).
#' The data was generated using the code in the '/data-raw/data-pcp.R' file.
#'
#' @docType data
#' @name data.pcp
#' @format A list with the following items:
#' \describe{
#'   \item{data}{A data frame with monthly variables in the columns.}
#'   \item{descriptions}{A list that describes the columns of \code{data}.}
#'   \item{datatypes}{A character array that describes the type of data in the columns of \code{data}.}
#'   \item{start}{A number that indicates the frequency of the first observation in \code{data}.}
#' }
#' @source \insertCite{datasetPcp;textual}{ldt}
"data.pcp"


#' Berka and Sochorova (1993) Dataset for Loan Default
#'
#' This dataset is a part of the Berka and Sochorova (1993) study, which contains information on loan defaults. The data was generated using the code in the '/data-raw/data-berka.R' file.
#'
#' @docType data
#' @name data.berka
#' @format A list with the following items:
#' \describe{
#'   \item{y}{A vector representing the labels. It contains 1 for default and 0 for non-default. Any observation with default (i.e., 'default' and 'finished with default') is considered to be a positive.}
#'   \item{x}{A matrix with explanatory variables in the columns.}
#'   \item{w}{A vector with the weight of each observation. This is mathematically generated to balance the observations.}
#'   \item{descriptions}{A list that describes the column names.}
#' }
#' @source \insertCite{datasetBerka;textual}{ldt}
"data.berka"


#' Long-run Growth from World Development Indicator Dataset
#'
#' This dataset is derived from the World Development Indicator (WDI) dataset. It contains information on long-run output growth after 2006 and its potential explanatory variables before that year. The data was generated using the code in the '/data-raw/data-wdi.R' file.
#'
#' @docType data
#' @name data.wdi
#' @format A list with the following items:
#' \describe{
#'   \item{y}{A vector representing the long-run output growth after 2006. Each element represents a country.}
#'   \item{x}{A matrix with explanatory variables in the columns. Each row represents a country.}
#'   \item{splitYear}{A number that indicates how \code{y} and \code{x} are calculated.}
#'   \item{names}{A list of pairs that describe the code and the name of variables in \code{y} and \code{x}.}
#' }
#' @source \insertCite{datasetWorldbank;textual}{ldt}
"data.wdi"

