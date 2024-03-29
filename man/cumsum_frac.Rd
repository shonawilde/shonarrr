% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cumsum_frac.R
\name{cumsum_frac}
\alias{cumsum_frac}
\title{Cumulative fraction}
\usage{
cumsum_frac(x)
}
\arguments{
\item{x}{Numeric vector.}
}
\value{
Vector of same length as \code{x} 
An NA value in \code{x} causes the corresponding and following elements of the return value to be NA.
}
\description{
Returns a vector whose elements are the cumulative fraction of the total.
}
\author{
Shona Wilde.
}
