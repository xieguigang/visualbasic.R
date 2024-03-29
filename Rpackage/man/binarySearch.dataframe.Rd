% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binarySearch.R
\name{binarySearch.dataframe}
\alias{binarySearch.dataframe}
\title{Binary Search on a \code{dataframe}}
\usage{
binarySearch.dataframe(dataframe, find, key, compares = function(a, b) a - b)
}
\arguments{
\item{dataframe}{A dataframe object}

\item{key}{The name of the key indexer column. And this key index column value should be ASC ordered.}

\item{compares}{A lambda function that generates the comparision numeric value between the
\code{key} indexer element values.}
}
\description{
Binary Search on a \code{dataframe}
}
\details{
For the \code{compares} lambda function, it should accept two parameter and in format like:

    \code{compares = function(a, b) a - b}

    And this function should generates a numeric value for indicate the object comparision result:

     0: means \code{a} equals to \code{b}\cr
     1: means \code{a} greater than \code{b} \cr
    -1: means \code{a} less than \code{b}
}
\seealso{
\code{\link{binarySearch.impl.generic}}
}
