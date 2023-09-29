#' Cumulative fraction
#' 
#' Returns a vector whose elements are the cumulative fraction of the total.
#'
#' @param x Numeric vector.
#'
#' @return Vector of same length as \code{x} 
#' An NA value in \code{x} causes the corresponding and following elements of the return value to be NA.
#' 
#' @author Shona Wilde.
#' 
#' @export

cumsum_frac <- function(x) {
  
  
  sum <- sum(x)
  cumsum <- cumsum(x)
  frac <- cumsum/sum
  
  return(frac)
  
  
}

