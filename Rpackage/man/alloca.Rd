% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Helpers.R
\name{alloca}
\alias{alloca}
\title{Memory preallocation}
\usage{
alloca(len, list = FALSE)
}
\arguments{
\item{len}{A integer preallocation size.}

\item{list}{Generate a empty list? By default is FALSE, means generate a empty vector.}
}
\description{
Memory preallocation for vector or list. Apply this preallocation operation
  for each vector or list can make great performance improvements.
}
