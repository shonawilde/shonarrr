#' Function to centre or standardise a vector
#'
#' @param x Numeric vector
#' 
#' @param standardise Should \code{x} be rescaled to have a mean of 0 and a standard deviation of 1? Also known as the z-score
#'
#' @return Vector of same length as \code{x}
#' 
#' @author Shona Wilde
#' 
#' @export

centre <- function(x, standardise = F) {
  
  mean <- mean(x)
  
  x <- x-mean
  
  if (standardise) {
    
    sd <- sd(x)
    
    x <- x/sd
    
  }
  
  return(x)
  
}


