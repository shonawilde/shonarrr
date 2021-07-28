#' Function to extract geometry coordinates as columns
#'
#' @param data Data with class \code{sf} or \code{sfc}
#' 
#' @param names Names of new columns
#' 
#' @param drop_geometry Should the `geometry` column be dropped?
#' 
#' @author Shona Wilde
#' 
#' @return Either a `tibble` or `sf` object
#' 
#' @export


sfc_to_columns <- function(data, names = c("longitude", "latitude"), drop_geometry = FALSE) {
  
  # check class
  stopifnot("class must be `sf` & geometry must be of type `sfc_POINT`" = 
              inherits(data,"sf") && inherits(sf::st_geometry(data),"sfc_POINT"))
  
  # get coordinates as tibble
  coords <- sf::st_coordinates(data) %>% 
    tibble::as_tibble()
  
  stopifnot("number of columns must be the same length as `names" = length(names) == ncol(coords))
  
  # remove columns with same names
  data <- data[ ,!names(data) %in% names]
  
  coords <- purrr::set_names(coords, names)
  
  data <- dplyr::bind_cols(data, coords)
  
  if(drop_geometry){
    
    data <- data %>% 
      sf::st_drop_geometry() %>% 
      tibble::as_tibble()
  }
  
  return(data)
  
}


