#' Read Core FAAM NetCDF
#'
#' Reads and cleans core FAAM files
#'
#' @param core_file_list List containing core FAAM file names
#' 
#' @author Shona Wilde
#' 
#' @return Tibble
#' 
#' @export


read_core_faam_data <- function(core_file_list, verbose = T) {
  
  map_dfr(core_file_list, read_core_faam_worker,
          verbose = verbose)
  
}


read_core_faam_worker <- function(file, verbose) {
  
  file <- file
  
  # message
  if (verbose) message(threadr::date_message(), "`", basename(file), "`...")
  
  # pull out date and number of flight from file name
  flight_date <- stringr::str_sub(file, start = 11, end = 18) %>% 
    paste0(" 00:00") %>% 
    lubridate::ymd_hm()
  
  flight_no <- stringr::str_sub(file, start = -11, end = -8)
  
  # read in data
  df <- tidync::tidync(file) %>% 
    tidync::hyper_tibble() %>% 
    dplyr::rename_all(. %>% tolower())
  
  # format date and rearrange
  df <- df %>% 
    dplyr::mutate(date = time + flight_date,
                  flight_no = flight_no) %>% 
    dplyr::select(date, flight_no, dplyr::everything(), -time) %>% 
    as_tibble()
  
  return(df)
  
}
