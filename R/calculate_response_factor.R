
#' Calculate reponse factor
#'
#' @param area Peak area
#' 
#' @param concentration concentration in standard
#' 
#' @author Shona Wilde
#' 
#' @return Numeric
#' 
#' @export



calculate_response_factor <- function(area, concentration) {
  
  rf <- area/concentration
  
  return(rf)
  
}