#' Function to extract geometry coordinates as columns
#'
#' @param df Data frame to be converted to spatial data frame - currently only works for `POINTS`
#' 
#' @param latitude Latitude varaible in df
#' 
#' @param longitude Longitude varaible in df
#' 
#' @param crs Coordinate reference system of df
#' 
#' @author Shona Wilde
#' 
#' @return `sf`object
#' 
#' @export

st_from_df <- function(df, latitude = "latitude", longitude = "longitude", crs = 4326){
  
  sf <- sf::st_as_sf(df, 
           coords = c(longitude, latitude),
           crs = crs)
  
  return(sf)
}

