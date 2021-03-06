% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scaling.r
\name{rescale}
\alias{rescale}
\title{Rescale numeric vector data with the option to clip data to the max and min of the output range}
\usage{
rescale(x, to = c(0, 1), from = range(x, na.rm = TRUE, finite = TRUE),
  clip = FALSE)
}
\arguments{
\item{x}{numeric vector of values to manipulate}

\item{to}{= c(0,1) output range (numeric vector of length two)}

\item{from}{= range(x, na.rm = TRUE, finite = TRUE)}

\item{clip}{= FALSE, if TRUE the result will be clipped to the max and min of the output range.}
}
\value{
rescaled and clipped data vector
}
\description{
Rescale numeric vector data with the option to clip data to the max and min of the output range
}
