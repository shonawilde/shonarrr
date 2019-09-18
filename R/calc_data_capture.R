#' Calculate Percentage Data Capture
#'
#' Calculates percentage of data capture over a period of time
#'
#' @param x variable
#' 
#' @author Shona Wilde
#' 
#' @return value
#' 
#' @export

data_capture <- function(x) {
  
  if (all(is.na(x))) {
    capture <- 0
  }
  else {
    capture <- 100 * (1 - sum(is.na(x))/length(x))
  }
  capture
}


