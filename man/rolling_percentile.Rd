% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rolling_percentile.R
\name{rolling_percentile}
\alias{rolling_percentile}
\title{Function to calculate a rolling percentile from a vector.}
\usage{
rolling_percentile(
  x,
  width,
  align = "center",
  probs = 0.5,
  na.rm = TRUE,
  round = NA
)
}
\arguments{
\item{x}{Numeric vector}

\item{width}{Width of rolling window}

\item{align}{Should the index of the result be center (default), left or right aligned compared to the rolling window of observations?}

\item{probs}{Percentile to calculate}

\item{na.rm}{Should NAs be removed before the percentile is computed?}

\item{round}{Number of digits to round result to}
}
\value{
Numeric vector of same length as x
}
\description{
Function to calculate a rolling percentile from a vector.
}
\examples{

# define a vector - this is a daily time series
x <- c(41, 36, 12, 18, 28, 23, 19, 8, 7, 16, 11, 14, 18, 14, 34, 6, 30, 11)

# calculate rolling 7-day 99th percentile 
rolling_percentile(
 x,
 width = 7,
 probs = 0.99
)

# round to 1 decimal place only
rolling_percentile(
 x,
 width = 7,
 probs = 0.99,
 round = 1
)#' 

}
\author{
Shona Wilde
}
