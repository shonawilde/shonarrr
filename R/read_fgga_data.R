#' Read NASA AMES FGGA file from FAAM aircraft
#'
#' Reads and cleans FGGA files from FAAM aircraft
#'
#' @param core_file_list List containing FGGA files
#' 
#' @author Shona Wilde
#' 
#' @return Tibble
#' 
#' @export

read_fgga_data <- function(fgga_file_list, verbose = T) {
  
  map_dfr(fgga_file_list,
          read_fgga_data_worker,
          verbose = verbose)
}


read_fgga_data_worker <- function(file, verbose) {
  
  file <- file
  
  # message
  if (verbose) message(threadr::date_message(), "`", basename(file), "`...")
  
  flight_no <- stringr::str_sub(file, start = -7, end = -4) %>% 
    str_to_lower()

    text <- readr::read_lines(file)
  
    # Isolate meta data
    length_meta <- stringr::str_split_fixed(text[1], " ", 2) %>% 
      .[, 1] %>% 
      as.integer()
    
    meta_data <- text[1:length_meta]
    
    # Get start date for file
    date_start <- meta_data %>% 
      stringr::str_subset("Time ") %>% 
      stringr::str_extract("\\([^()]+\\)") %>% 
      stringr::str_sub(2L, -2L) %>% 
      stringr::str_remove("seconds since ") %>% 
      lubridate::ymd_hms(tz = "UTC")
    
    index_start_names <- stringr::str_which(meta_data, "^99")[1] + 1L
    index_end_names <- stringr::str_which(meta_data, "THIS-FILE")[1] -2L
    
    variable_names <- meta_data[index_start_names:index_end_names] %>% 
      stringr:: str_to_lower() %>% 
      variable_names_cleaner()
    
    # add date column heading manually
    variable_names <- c("date", variable_names)
    
    # read data
    df <- read_table2(file,
                      skip = length_meta,
                      col_names = F) %>% 
      purrr::set_names(variable_names) %>% 
      mutate(date = date_start + date,
             date = floor_date(date, "seconds"),
             flight_no = flight_no) %>% 
      select(date, flight_no, everything())
    
    return(df)
    
}
    
    
variable_names_cleaner <- function(x){
  
  # Switch names when needed
  x <- dplyr::case_when(
    x == "carbon dioxide (co2) dry mole fraction, in ppm" ~ "co2",
    x == "co2 error flag" ~ "co2_flag",
    x == "methane (ch4) dry mole fraction, in ppb" ~ "ch4",
    x == "ch4 error flag" ~ "ch4_flag",
    TRUE ~ x
  )
  
}
      