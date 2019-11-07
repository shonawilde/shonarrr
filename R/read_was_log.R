#' Read WAS Log
#'
#' Reads and cleans WAS log file from FAAM aircraft
#'
#' @param file file to read of type ".WAS"
#' 
#' @author Stuart Grange / Shona Wilde
#' 
#' @return df
#' 
#' @export

read_was_log <- function(file) {
  
  # Read file as text
  text <- readr::read_lines(file)
  
  # Get end of preable
  index_end <- stringr::str_which(text, "^Compressed gas")
  
  # Get flight number
  flight_no <- text[1] %>% 
    stringr::str_split_fixed("flight | on", 3) %>% 
    .[, 2]
  
  # Get day of flight
  day <- text[1] %>% 
    stringr::str_split_fixed("on ", 2) %>% 
    .[, 2] %>% 
    lubridate::dmy(tz = "UTC")
  
  # Get set number
  set_no <- text[2] %>% 
    stringr::str_split_fixed(" = ", 3) %>% 
    .[, 3] %>% 
    as.integer()
  
  # Parse tabular data  
  df <- readr::read_table(text[-1:-index_end], col_names = F) %>% 
    purrr::set_names(
      c("bottle_no", "date_start", "date_end", "fill_time", "fill_type")
    ) %>% 
    mutate(flight_no = !!flight_no,
           day = !!day,
           set_no = !!set_no,
           date_start = stringr::str_c(day, " ", date_start),
           date_start = lubridate::ymd_hms(date_start, tz = "UTC"),
           date_end = stringr::str_c(day, " ", date_end),
           date_end = lubridate::ymd_hms(date_end, tz = "UTC")) %>% 
    select(flight_no,
           set_no,
           bottle_no,
           date_start,
           date_end,
           fill_time)
  
  return(df)
  
}
