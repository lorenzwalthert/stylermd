% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy.R
\name{tidy_line}
\alias{tidy_line}
\title{Tidy one line}
\usage{
tidy_line(text, width)
}
\arguments{
\item{text}{Text to prettify.}

\item{width}{The maximal lenght of a line. If \code{length(width) > 1}, the
elements are used sequentially, one for each iteration. If there are more
iterations than values of \code{width}, the last value of \code{width} is used for
all remaining iterations.}
}
\description{
Necessary to be called with \code{purrr::map(..., tidy_lines)} only if \code{add_only}
was set to \code{TRUE} because then we have multiple text lines to tidy.
}
\keyword{internal}
