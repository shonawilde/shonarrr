#' Calculate Reads core van
#'
#' Reads core data fromm WACL van
#'
#' @param file file
#' 
#' @author Shona Wilde
#' 
#' @return tibble
#' 
#' @export


read_core_van <- function(file) {
  
  # function to convert coordinates
  conv_gps <- function(coord) {
    
    int = floor(coord/100)
    
    new_coord = (((coord/100 - int)*100)/60) + int
    
    return(new_coord)
    
  }
  
  
  df <- read.table(file,
                   sep = ",",
                   header = T) %>% 
    rename_all(. %>% tolower()) %>% 
    rename(date = thetime) %>% 
    mutate(date = mdy_hms(date)) %>% 
    mutate(lat = conv_gps(gpsr_lat),
           long = conv_gps(gpsr_long)) %>% 
    select(-gpsr_lat, -gpsr_long) %>% 
    as_tibble()
  
  return(df)
}

