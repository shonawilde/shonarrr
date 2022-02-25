#' Functions to replace values with NA
#' 
#' @param x Input vector
#' 
#' @author Shona Wilde
#' 
#' @return Vector of same length as x.
#' 
#' @export


negative_to_na <- function(x) {
  
x <- ifelse(x < 0, NA, x)  

return(x)

}


#' @rdname negative_to_na
#' @export
zero_to_na <- function(x) {
  
  x <- ifelse(x == 0, NA, x)
  
  return(x)
  
}



