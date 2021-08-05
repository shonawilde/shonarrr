#' Function to get first and last elements of a vector 
#'
#' @param x Vector
#' 
#' @return Tibble
#' 
#' @author Shona Wilde 
#' 
#' @export



get_first_last <- function(x) {
  
  first <- x[1]
  
  last <- x[length(x)]
  
  # as tibble
  df <- tibble(
    first, last
  )
  
  return(df)
  
}

