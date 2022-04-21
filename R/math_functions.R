#' Basic mathematical functions
#'
#' @param x Numeric value
#' 
#' @param y Numeric value
#'
#' @return Numeric of length 1
#' 
#' @author Shona Wilde
#' 
#' @export

add <- function(x, y) {
  
  x + y
  
}

#' @rdname add
#' @export
subtract <- function(x, y) {
  
  x - y
  
}


#' @rdname add
#' @export
multiply <- function(x, y) {
  
  x * y
  
}

#' @rdname add
#' @export
divide <- function(x, y) {
  
  x / y
  
}
