#' Function to extract world map as an \code{sf} object
#' 
#' @author Shona Wilde
#' 
#' @return object of  class \code{sf} object
#' 
#' @export

map_as_st <- function() {
  
  map <- rnaturalearth::ne_countries(scale = "medium")
  
  st_map <- map %>% 
    sf::st_as_sf()
  
  st_map_filt <- st_map %>% 
    dplyr::select(name, continent, adm0_a3, pop_est) %>% 
    dplyr::rename(iso_code = adm0_a3,
           population_estimate = pop_est)
  
  return(st_map_filt)
  
}

