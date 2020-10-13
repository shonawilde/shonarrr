#' Calculate perpendicular wind component
#'
#' Function to calculate the component of horizontal wind speed perpendicular to the flight track
#' 
#' @param ws  Wind speed
#' 
#' @param alpha Angle between wind direction and the line perpendicular to the flight track 
#' calculated using \code{\link{calc_alpha}}. 
#' 
#' @author Shona Wilde
#' 
#' @return Numeric.
#' 
#' @export


calc_perpendicular_wind <- function(ws = "ws", alpha){

# ANGLE IN RADIANS FOR TRIG FUNCTIONS!
angle_rad <- pracma::deg2rad(alpha)

# Component of wind perpendicular to flight track
ws_perp <- ws*cos(angle_rad)

return(ws_perp)

}
