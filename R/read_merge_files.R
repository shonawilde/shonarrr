#' Read FAAM merge files
#' 
#'Reads and cleans FAAM merge files
#'
#'#' @param file_list list of merge files to read
#' 
#' @param verbose should messgaes print in console
#' 
#' @author Shona Wilde
#' 
#' @return tibble
#' 
#' @export



read_merge_file <- function(file_list, verbose = T) {
  
  map_dfr(file_list,
          read_merge_file_worker,
          verbose = verbose)
  
  
}

read_merge_file_worker <- function(file, verbose = verbose) {
  
  if (verbose) message(threadr::date_message(), "`", file, "`...")
  
  
  # clean name
  file_clean <- file %>% 
    str_remove("Flight") %>% 
    str_to_lower()
  
  # get flight number
  flight_no = str_sub(file_clean, 
                      start = 1,
                      end = 4)
  
  # read all formats of date
  df <- read.csv(file) %>% 
    rename_all(. %>% tolower()) %>% 
    mutate(flight_no = flight_no,
           ws = calc_wind_speed(u_c, v_c),
           wd = calc_wind_direction(u_c, v_c),
           date = if_else(
             ymd_hms(date) %>% is.na(),
             dmy_hms(date),
             ymd_hms(date)))
  
  # Determine if file contains high and reduced quality ICL data
  is_hq_rq <- grepl_all(names(df), "ethane_rq")
  
  # combine all ethane data into one column
  if (is_hq_rq) {
    
    df <- df %>% 
      mutate(ethane_icl = if_else(
        !is.na(ethane_hq_icl),
        ethane_hq_icl, 
        ethane_rq_icl),
        ethane_flag = if_else(
          !is.na(ethane_hq_flag),
          ethane_hq_flag,
          ethane_rq_flag
        )
      )
    
  }
  

  
  # set and replace names
  variable_names <- names(df) %>% 
    enframe(value = "name_orig") %>% 
    left_join(aircraft_variables_lookup(),
              by = "name_orig") %>% 
    mutate(name_replace = if_else(
      is.na(name_replace), 
      name_orig,
      name_replace
    ))

  # set df names and re-arrange
  df <- df %>% 
    set_names(variable_names %>% 
                pull(name_replace)) %>% 
    select(date, flight_no, everything()) %>% 
    as_tibble()

  
  return(df)
  
}


#' @export
aircraft_variables_lookup <- function(){
  
  tribble(
    ~name_orig, ~name_replace,
    "o3_teco", "o3",
    "co_aero", "co",
    "lat_gin", "latitude",
    "lon_gin", "longitude",
    "alt_gin", "altitude",
    "hdg_gin", "heading",
    "gspd_gin", "ground_speed",
    "ps_rvsm", "air_pressure",
    "tat_di_r", "air_temp",
    "tat_nd_r", "air_temp_nd",
    "tdew_ge", "dew_point",
    "so2_teco", "so2",
    "no.pptv", "no",
    "no2.ppt", "no2",
    "no_pptv", "no",
    "no2_pptv", "no2",
    "picarro_h2o", "h2o",
    "co2_ppm", "co2",
    #"ch4_ppm", "ch4",
    "flight", "flight_no"
    
  )
  
}



