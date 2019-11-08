#' Calculate mass flux
#'
#' Calculate emission rate of species using simplified mass flux technique in molecules per second
#'
#' @param wind_speed Wind speed in ms-1
#' 
#' @param ground_speed Speed over ground in ms-1
#' 
#' @param alpha Angle between aircraft track and direction normal to wind direction 
#' 
#' @param scale_height Scale height in m
#' Scale height is the increase in altitude for which the atmospheric pressure decreases by a factor of e
#' 
#' @param mixing_height Mixing height of observed plume in m
#' 
#' @param plume_integral Area under observed plume
#'  This must be the enhancement above background concentrations
#' 
#' @author Shona Wilde
#' 
#' @return value
#' 
#' @export

calc_flux <- function(wind_speed = ws, 
                      ground_speed = gspd_gin,
                      alpha = alpha,
                      scale_height = scale_height, 
                      mixing_height = mixing_height, 
                      plume_integral = plume_integral) {

  flux = wind_speed*scale_height*air_number_density()*
    (1-exp(-mixing_height/scale_height))*
    plume_integral*ground_speed*
    sqrt((cos((alpha*pi)/180)^2))*0.001
  
  return(flux)
    
}


