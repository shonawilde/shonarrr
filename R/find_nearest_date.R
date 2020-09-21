#' Function to find the nearest date for two date vectors
#'
#' @param date Date to match to   
#' 
#' @param date_match Date vector for which the closest date to \code{date} should be found
#' 
#' @author Shona Wilde & Stuart K. Grange
#' 
#' @return Date vector of class \code{POSIXct}
#' 
#' @export

find_nearest_date <- function(date, date_match){
  
  return_closest_date <- function(date, date_vector) {
    
    delta <- abs(date - date_vector)
    index <- which.min(delta)
    x <- date_vector[index]
    return(x)
    
  }
  
  date_nearest <- date %>% 
    map_dbl(return_closest_date, date_match) %>% 
    as.POSIXct(tz = "UTC", origin = "1970-01-01")
  
  return(date_nearest)
    
}

