#' Function to find the nearest date within a vector
#'
#' @param date Date to match to.
#' 
#' @param date_match Date vector for which the closest date to \code{date} should be found.
#' 
#' @author Shona Wilde & Stuart K. Grange
#' 
#' @return Date vector of class \code{POSIXct}.
#' 
#' @export


find_nearest_date <- function(date, date_match){  
  
  date_nearest <- date %>% 
    map_dbl(
      find_nearest_date_worker, date_match
    ) %>% 
    as.POSIXct(tz = "UTC", origin = "1970-01-01")
  
  return(date_nearest)
    
}

find_nearest_date_worker <- function(date, date_vector) {
  
  delta <- abs(date - date_vector)
  index <- which.min(delta)
  x <- date_vector[index]
  return(x)
  
}


