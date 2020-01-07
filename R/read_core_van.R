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
  
  
  df <- read.table(file,
                   sep = ",",
                   header = T) %>% 
    rename_all(. %>% tolower()) %>% 
    rename(date = thetime) %>% 
    mutate(date = mdy_hms(date)) %>% 
    mutate(lat = conv_gps(gpsr_lat, gpsr_lat_direction),
           long = conv_gps(gpsr_long, gpsr_long_direction)) %>% 
    select(-gpsr_lat, -gpsr_long) %>% 
    as_tibble()
  
  return(df)
}

