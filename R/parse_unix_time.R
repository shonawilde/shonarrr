#' Function to parse unix time to a POSIXct date
#' 
#' @param x Unix time
#' 
#' @param tzone Character string containing the time zone to convert to
#' 
#' @param origin Origin of epoch. For unix time this is "1970-01-01"
#' 
#' @return POSIXct date
#' 
#' @author Shona Wilde
#' 
#' @export

parse_unix_time <- function(x, tzone = "UTC", origin = "1970-01-01") {
  
  y <- as.POSIXct(x, tz = tzone, origin = origin)
  
  return(y)
  
}
