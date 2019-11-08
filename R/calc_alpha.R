#' Calculate alpha
#'
#' Calculate alpha which is the angle between aircraft track and direction normal to wind direction
#'
#' @param wind_direction Wind direction in degrees
#' 
#' @param heading Aircraft heading in degrees
#' 
#' @author Shona Wilde
#' 
#' @return value
#' 
#' @export



calc_alpha <- function(wind_direction, heading){
  
  alpha = heading-(wind_direction - 90)
  
  return(alpha)
  
}



