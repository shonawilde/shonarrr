#' Function to replace NAs with unique numbers 
#'
#' @param x Numeric vector.
#' 
#' @param min_number Minimum number to start replacement from.
#'
#' @return Numeric vector of same length as \code{x}.
#' 
#' @export

replace_na_unique <- function(x, min_number = 1) { 
  
  na_indices <- which(is.na(x))
  unique_numbers <- seq(min_number, length.out = length(na_indices))
  x[na_indices] <- unique_numbers
  
  return(x)
  
}
