#' Read core van data
#'
#' Reads core data from the WACL 
#'
#' @param file_list files to read
#' 
#' @param tz a character string that specifies which time zone to parse the date with
#' 
#' @param force_utc should the date be converted to UTC?
#' 
#' @param verbose display message to user?
#' 
#' @author Shona Wilde
#' 
#' @return Tibble.
#' 
#' @export



read_core_van <- function(file_list, tz = "UTC", force_utc = T, verbose = T) {
  
  df <- imap_dfr(
    file_list,
    ~read_core_van_worker(
      file = .x,
      tz = tz,
      force_utc = force_utc,
      index = .y,
      verbose = verbose
    )
  )
  
  return(df)
  
}


read_core_van_worker <- function(file, tz, force_utc, index, verbose) {
  
  if (verbose) {
    message(lubridate::now(tzone = Sys.timezone()), " `", basename(file), "`...")
  }  
  
  
  df <- read_csv(file, show_col_types = F) %>%  
    janitor::clean_names() %>% 
    rename(date = the_time) %>% 
    mutate(
      date = mdy_hms(date, tz = tz),
      file_index = index,
      gpsr_track_angle = as.numeric(gpsr_track_angle),
      gpsr_time = as.numeric(gpsr_time),
      lat = conv_gps(gpsr_lat, gpsr_lat_direction),
      lon = conv_gps(gpsr_long, gpsr_long_direction)
    ) %>% 
    relocate(file_index) %>% 
    select(-gpsr_lat, -gpsr_long) %>% 
    as_tibble()
  
  
  if (force_utc) {
    
    df$date <- with_tz(df$date, tz = "UTC")
    
  }
  
  return(df)
  
}


