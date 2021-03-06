% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TextWriter.R
\name{textWriter}
\alias{textWriter}
\title{Open a text writer}
\usage{
textWriter(path, buffer.Size = 4096)
}
\arguments{
\item{path}{The file path string.}

\item{buffer.Size}{Buffer size in number of text lines. This
parameter dramatically affect the writer performance.}
}
\value{
A text writer handler list object, with members:

\enumerate{
\item \code{writeline} Write text line through this text writer helper.
\item \code{println} Write text line with \code{sprintf} format helper.
\item \code{close} Flush the remaining text data in buffer and
     then close the file connection and finally dispose the text writer buffer.
}
}
\description{
Open a text writer
}
