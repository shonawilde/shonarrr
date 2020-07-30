#' Function to calculate sunrise and sunset times at a particular location
#' 
#' @param latitude Latitude of location
#' 
#' @param longitude Longitude of location
#'
#' @author Shona Wilde
#' 
#' @return Tibble. 
#' 
#' @seealso \code{\link{sunriset}}, \code{\link{gissr::calculate_sunrise}}
#' 
#' @examples 
#' # append to data frame
#' df %>% 
#'   mutate(sunrise_sunset(date, 54.20092, -0.81852))
#'
#' @export

 sunrise_sunset <- function(date, latitude, longitude) {
  
   # MUST BE LONG THEN LAT
   coords <- tribble(
     ~longitude, ~latitude,
     longitude, latitude
   )
   
   # make spatial
   sp <- coords %>% 
     sp::SpatialPoints(proj4string = sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))
   
   sunrise <- maptools::sunriset(sp, date, direction = "sunrise", POSIXct.out = TRUE) %>% 
     pull(time)
   
   sunset <- maptools::sunriset(sp, date, direction = "sunset", POSIXct.out = TRUE) %>% 
     pull(time)
   
   # Build tibble and do test here
   df <- tibble(date, sunrise, sunset) %>% 
      mutate(day_time = if_else(date >= sunrise & date <= sunset, "day", "night"))
   
   # Between seemed to be behaving wrong
   # between(date, sunrise, sunset)
   
   return(df)
   
 }

  