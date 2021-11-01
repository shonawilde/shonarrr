#' Function to normalise a numeric vector between 0 and 1
#'
#' @param x Numeric vector
#' 
#' @param na.rm A logical indicating whether missing values should be removed. Default is TRUE.
#'
#' @return Numeric vector the same length as \code{x}.
#'
#' @author Shona Wilde
#'
#' @export


normalise <- function(x, na.rm = TRUE)  {
  
  
  x_min <- min(x, na.rm = na.rm)
  x_max <- max(x, na.rm = na.rm)
  
  x <- (x - x_min)/(x_max - x_min)
  
  return(x)
  
}
