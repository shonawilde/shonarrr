#' Function to calculate a rolling percentile from a vector. 
#'
#' @param x Numeric vector
#' @param width Width of rolling window
#' @param align Should the index of the result be center (default), left or right aligned compared to the rolling window of observations?
#' @param probs Percentile to calculate
#' @param na.rm Should NAs be removed before the percentile is computed?
#' @param round Number of digits to round result to
#'
#' @return Numeric vector of same length as x
#' 
#' @author Shona Wilde
#' 
#' @examples 
#' 
#' # define a vector - this is a daily time series
#' x <- c(41, 36, 12, 18, 28, 23, 19, 8, 7, 16, 11, 14, 18, 14, 34, 6, 30, 11)
#' 
#' # calculate rolling 7-day 99th percentile 
#' rolling_percentile(
#'  x,
#'  width = 7,
#'  probs = 0.99
#' )
#'
#' # round to 1 decimal place only
#' rolling_percentile(
#'  x,
#'  width = 7,
#'  probs = 0.99,
#'  round = 1
#' )#' 
#' 
#' @export

rolling_percentile <- function(x, width, align = "center", probs = 0.5, na.rm = TRUE, round = NA) {
  
  x <- zoo::rollapply(
    x,
    width = width,
    align = align,
    quantile,
    probs = probs,
    na.rm = na.rm,
    names = F,
    fill = NA,
    partial = T
  )
  
  if (!is.na(round)){
    
    x <- round(x, digits = round)
    
  }
  
  return(x)
  
}



