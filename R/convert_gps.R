#' Convert GPS
#'
#' Convert GPS data from WACL van to decimal degrees
#'
#' @param coord latitude or longitude column to be converted
#' 
#' @param direction column containing directional component of coordinates (N, S, E, W)
#' 
#' @author Shona Wilde
#' 
#' @return value
#' 
#' @export


# function to convert coordinates
conv_gps <- function(coord, direction) {
  
  # is directional component negative
  is_negative <- ifelse(direction %in% c("S", "W"), TRUE, FALSE)
  
  int <- floor(coord/100)
  
  new_coord <- (((coord/100 - int)*100)/60) + int
  
  suppressWarnings(if (is_negative)
    
    new_coord <- ((((coord/100 - int)*100)/60) + int) *-1)
  
  
  return(new_coord)
  
}

