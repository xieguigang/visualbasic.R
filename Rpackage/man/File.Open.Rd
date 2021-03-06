% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/File.R
\name{File.Open}
\alias{File.Open}
\title{Open a file for write in text mode}
\usage{
File.Open(file.txt, append = FALSE, format = TRUE)
}
\arguments{
\item{file.txt}{Target file path for write in text mode.}

\item{append}{A logical flag to indicate append the data to target file or not?}
}
\value{
A lambda function for write text data to file.
}
\description{
Open a given file and returns the handle for write this file in text mode.
  The returned lambda function is a wrapper for function \code{link{sprintf}} and
  \code{cat} function.
}
