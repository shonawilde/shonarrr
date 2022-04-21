#' Calculate the change along a vector
#'
#' \code{lagged_change()} calculates the change between consecutive lagged values of \code{x}.
#' \code{cumulative_change()} calculates the change between the first value of \code{x} and each subsequent value of \code{x}.
#'  
#' @param x Numeric vector
#' 
#' @param type Type of change to calculate. Either "percent" or "absolute"
#' 
#' @author Shona Wilde
#'
#' @return Numeric vector of the same length as \code{x} 
#' 
#' @export

lagged_change <- function(x, type = "percent") {
  
  lag <- lag(x, 1)
  
  change <- percentage_change(lag, x)
  
  if (type == "absolute") {
    
    change <- x - lag
  }
  
  return(change)
  
}

#' @rdname lagged_change
#' @export
cumulative_change <- function(x, type = "percent") {
  
  start <- x[1]
  
  change <- map_dbl(
    x, 
    ~percentage_change(start, .x)
  )
  
  if (type == "absolute") {
    
    change <- map_dbl(
      x, 
      ~shonarrr::subtract(.x, start)
    )
    
  }
  
  return(change)
  
}


