#' Download current weather data from OpenWeatherMap
#' 
#' Download data from the \href{https://openweathermap.org/}{OpenWeatherMap API}
#'
#' @param api_key API key for OpenWeatherMap
#'
#' @param lat Latitude of location to get data for
#' 
#' @param lon Longitude of location to get data for
#' 
#' @return Tibble
#' 
#' @author Shona Wilde
#' 
#' @export



get_current_weather <- function(api_key, lat = 53.9600, lon = -1.0873) {
  
  # build api URL
  base_url <- "api.openweathermap.org/data/2.5/weather?"
  
  url <- paste0(
    base_url, "lat=", lat, "&lon=", lon, "&appid=", api_key, "&units=metric"
  )
  
  message(lubridate::now(), " Requesting data from API...")
  
  # request data
  response <- httr::GET(url)
  
  # tidy
  data <- jsonlite::fromJSON(
    rawToChar(response$content), 
    flatten = T
  )
  
  # as df
  df <- as.data.frame(data) %>% 
    as_tibble() %>% 
    janitor::clean_names()
    
  df_tidy <- df %>% 
    select(
      any_of(c(
        "name", 
        "sys_country",
        "id",
        "dt",
        "timezone",
        "coord_lat",
        "coord_lon",
        "weather_id",
        "weather_main",
        "weather_description",
        "main_temp",
        "main_feels_like",
        "main_temp_min",
        "main_temp_max",
        "main_pressure",
        "main_humidity",
        "visibility",
        "wind_speed",
        "wind_deg",
        "wind_gust",
        "sys_country",
        "sys_sunrise",
        "sys_sunset"))
    ) %>% 
    rename_with(
      ~str_remove(., "coord_|main_|sys_")
    ) %>% 
    rename(
      date_utc = dt,
      wind_dir = wind_deg,
      temp_feels_like = feels_like,
      shift_from_utc = timezone
    ) %>% 
    mutate(
      across(c(date_utc, sunrise, sunset), parse_unix_time)
    ) %>% 
    mutate(
      date_local = date_utc + shift_from_utc, 
      .after = date_utc,
      name = if_else(str_detect(name, "Zürich"), "Zurich", name)
    )
  
  return(df_tidy)
  
}


