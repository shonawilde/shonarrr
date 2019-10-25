#' Convert GPS
#'
#' Convert GPS data from WACL van to decimal degrees
#'
#' @param coord lat/long
#' 
#' @author Shona Wilde
#' 
#' @return value
#' 
#' @export


# function to convert coordinates
conv_gps <- function(coord) {
  
  int = floor(coord/100)
  
  new_coord = (((coord/100 - int)*100)/60) + int
  
  return(new_coord)
  
}
