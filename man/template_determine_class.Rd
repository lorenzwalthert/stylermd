% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/determine-class-of-paragraph.R
\name{template_determine_class}
\alias{template_determine_class}
\title{Determine the class and indent of a paragraph}
\usage{
template_determine_class(paragraph, regex, class)
}
\arguments{
\item{paragraph}{A paragraph, i.e. a character vector, as defined in the
vignette "Data structures".}

\item{regex}{The regular expression that matches listing, e.g. bullet of
enumeration. Must the indention must be captured by a \emph{capturing group},
e.g. \code{"^(\\s*)(\\* |\\+ |\\- )"}.}

\item{class}{The class, if matched.}
}
\description{
Determine the class and indent of a paragraph
}
\keyword{internal}
