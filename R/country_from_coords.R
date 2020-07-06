#' Function to obtain country from pair of coordinates
#'
#' @param data Data with class \code{sp} or \code{sf}
#' 
#' @author Shona Wilde
#' 
#' @return Either an `sp` or `sf` object
#' 
#' @export


country_from_coords <- function(data) {
  
  # must be spatial object
  if(!stringr::str_detect(class(data)[1], "sf|Spatial")) 
    
    stop("data object must of class `sp` or `sf`." )
    
  
  # get country data
  st_country <- rnaturalearth::ne_countries(returnclass = 'sf') %>% 
    dplyr::rename(country = name) %>% 
    select(country, geometry)
  
  if (class(data)[1] %in% "sf") {
    
  st_countries_joined <- data %>%
    sf::st_join(st_country, join = st_within)
  
  }
  
  
  if (stringr::str_detect(class(data)[1], "Spatial")) {
    
    st_countries_joined <- data %>%
      st_as_sf() %>% 
      sf::st_join(st_country, join = st_within)
    
  }
  
  return(st_countries_joined)
  
}





