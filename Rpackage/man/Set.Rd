% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Global.R
\name{Set}
\alias{Set}
\title{Set variable in global}
\usage{
Set(...)
}
\description{
Set variables into the global environment.
}
\examples{
Set(a = 500, b = TRUE, c = list(a= 50, b = FALSE));
list(x11 = 15555, y22 = FALSE) \%=>\% Set
}
\seealso{
\code{\link{Push}}
}
