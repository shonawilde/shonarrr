#' Function to plot Strava GPS tracks from personal database
#'
#' @param activity_date Date of activity/activities to plot
#' 
#' @author Shona Wilde
#' 
#' @return \code{leaflet} map
#' 
#' @export


plot_strava_gps <- function(activity_date = "2020-01-01") {
  
  strava_data <- read_rds("C:/Users/Shona/Google Drive/Other/running/st_strava.rds")
  
  stopifnot("Data must be of class `sf`" =  stringr::str_detect(class(strava_data)[1], "sf|Spatial"))
  
  if (!"date" %in% names(strava_data)) 
    stop("Data must contain a date variable called 'date'.", 
         call. = FALSE)
  
  activity <- strava_data %>% 
    dplyr::mutate(date = lubridate::date(date)) %>% 
    dplyr::filter(date %in% lubridate::ymd(activity_date))
  
  if(nrow(activity) == 0)
    stop("There were no recorded activities on this date...",
         call. = FALSE)
  
  plot <- activity %>% 
    sf::as_Spatial() %>% 
    gissr::leaflet_plot(popup = "name")
  
  return(plot)
  
}
