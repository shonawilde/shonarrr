#' Calculate potential temperature
#'
#' Calculate potential temperature in Kelvin using Poissons Equation.
#'  \link{http://tornado.sfsu.edu/geosciences/classes/m201/PrimtiveEquations/DerivedRelationships.html}
#'
#' @param temp Tempertaure in Kelvin
#' 
#' @param pressure Pressure in mb or hPa
#' 
#' @param ref_pressure Reference pressure to use in equation in mb or hPa
#' 
#' @author Shona Wilde
#' 
#' @return value
#' 
#' @export



calc_potential_temp <- function(temp, pressure, ref_pressure = 1000) {
  
  pot_temp <- temp*((ref_pressure/pressure)**0.286)
  
  return(pot_temp)
  
}