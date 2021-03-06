% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TextWriter.R
\name{tr}
\alias{tr}
\title{Remove non-printable ASCII characters}
\usage{
tr(file, saveAs = file)
}
\arguments{
\item{file}{The file path of the target text file.}

\item{saveAs}{The clean file save location, by default is the original location.}
}
\description{
Remove non-printable ASCII characters from a file with this Unix command. 
 (https://alvinalexander.com/blog/post/linux-unix/how-remove-non-printable-ascii-characters-file-unix)
}
\details{
This functin only works in unix system.
}
