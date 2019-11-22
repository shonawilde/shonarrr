#' Calculate wind speed and direction
#'
#' Calculates wind speed and direction from u and v components of wind
#'
#' @param u
#' 
#' @param v 
#' 
#' @author Shona Wilde
#' 
#' @return vector
#' 
#' @export


calc_wind_speed <- function(u, v){
  
  ws = sqrt(u^2 + v^2)
  
}


#' @export

calc_wind_direction <- function(u, v){
  
  wd <- (atan2(u, v) * 180/pi)+180
  
  return(wd)
  
}
