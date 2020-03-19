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
  
  # message
  if (verbose) message(threadr::date_message(), "`", basename(file), "`...")
  
  # Connection to ncdf file
  nc <- ncdf4::nc_open(file)
  
  # get meta data from ncdf
  meta_data <- nc %>% 
    capture.output() 
  
  # Get and format dates
  flight_date <- nc$dim$Time$units %>% 
    stringr::str_remove("seconds since ") %>% 
    lubridate::ymd_hms(tz = "UTC")
  
  # get flight number
  flight_no <- meta_data[str_which(meta_data, "! FLIGHT")] %>% 
    str_sub(start = -4, end = -1) %>% 
    str_to_lower()
  
  # close connection
  ncdf4::nc_close(nc)

  
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
