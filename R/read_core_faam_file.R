#' Read Core FAAM NetCDF
#'
#' Reads and cleans core FAAM files
#'
#' @param file path to file
#' 
#' @author Shona Wilde
#' 
#' @return data frame
#' 
#' @export


read_core_faam_file <- function(file) {
  
  file <- file
  
  # pull out date and number of flight
  flight_date <- str_sub(file, start = 11, end = 18) %>% 
    paste0(" 00:00") %>% 
    ymd_hm()
  
  flight_no <- str_sub(file, start = -11, end = -8)
  
  # read in data
  df <- wsdmiscr::read.1D_ncdf(file) %>% 
    rename_all(. %>% tolower())
  
  # format date and rearrange
  df <- df %>% 
    mutate(date = time + flight_date,
           flight_no = flight_no) %>% 
    select(date, flight_no, everything()) %>% 
    as_tibble()
  
  return(df)
  
}