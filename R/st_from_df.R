#' Function for promoting a data frame to an \code{sf} object
#'
#' @param df Data frame to be converted to spatial data frame - currently only works for `POINTS`
#' 
#' @param latitude Latitude variable in df
#' 
#' @param longitude Longitude variable in df
#' 
#' @param crs Coordinate reference system of df
#' 
#' @author Shona Wilde
#' 
#' @return `sf`object
#' 
#' @export

st_from_df <- function(df, latitude = "latitude", longitude = "longitude", crs = 4326){
  
  
  if (any(is.na(c(df[, latitude, drop = T], df[, longitude, drop = T]))))
    
    df <- df %>% 
      filter(
        across(c(!!latitude, !!longitude),
               ~ !is.na(.x)
        )
      )
  
    warning("Missing values in coordinates detected and have been removed...", 
            call. = FALSE)
  
  
  sf <- sf::st_as_sf(df, 
           coords = c(longitude, latitude),
           crs = crs)
  
  return(sf)
  
}



