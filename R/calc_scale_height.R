#' Calculate scale height
#'
#' Calculate scale height for use in mass flux calculation. 
#' Scale height is the increase in altitude for which the atmospheric pressure decreases by a factor of e
#' 
#' @param temperature Non de-iced temperature in Kelvin
#' 
#' @author Shona Wilde
#' 
#' @return value
#' 
#' @export



calc_scale_height <- function(temperature){
  
  scale_height = (temperature*specific_gas_constant())/gravitational_constant()
  
  return(scale_height)
  
}
