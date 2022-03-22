#' Create a logical as an ordered factor
#' 
#' Create an ordered factor with levels TRUE and FALSE 
#'
#' @param x Object to convert
#' @param ... Further arguments passed to or from other methods
#'
#' @return Ordered factor
#' 
#' @author Shona Wilde
#' @export

as_logical_factor <- function(x, ...) {
  
  x <- as.logical(x, ...)

  x <- factor(x, levels = c(T, F))
  
  return(x)
}

  

