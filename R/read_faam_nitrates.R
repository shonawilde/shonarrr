#' Read  FAAM core nitrates file
#'
#' Reads and cleans core nitrates FAAM file
#'
#' @param core_file_list List containing core nitrates file names
#' 
#' @author Shona Wilde
#' 
#' @return Tibble
#' 
#' @export


read_core_nitrates_data <- function(nitrtes_file_list, verbose = TRUE) {
  
  map_dfr(nitrtes_file_list, read_core_nitrates_worker,
          verbose = verbose)
}



read_core_nitrates_worker <- function(file, verbose) {
  
  # message
  if (verbose) message(threadr::date_message(), "`", basename(file), "`...")
  
# open connection
  nc <- ncdf4::nc_open(file)
  
  # get meta data from ncdf
  meta_data <- nc %>% 
    capture.output()  
  
  # close connection
  ncdf4::nc_close(nc)
  
  # pull flight number and date
  flight_no <- meta_data[str_which(meta_data, "flight_number")] %>% 
    str_split(":", simplify = F)  %>% 
    map_chr(`[` (2)) %>% 
    str_remove(" ") %>% 
    str_to_lower()
  
  flight_date <- meta_data[str_which(meta_data, "flight_date")] %>% 
    str_split(":", simplify = F)  %>% 
    map_chr(`[` (2)) %>% 
    str_remove(" ") %>% 
    paste0(" 00:00") %>% 
    lubridate::ymd_hm()
  
  
  # read tidy nc
  nc_tidy <- tidync::tidync(file)
  
  
  # read grid with core data
  data_core <- tidync(file) %>% 
    activate("D0") %>% 
    hyper_tibble()
  
  # read grid with mixing ratios
  data_conc <- tidync(file) %>%
    activate("D1,D0") %>% 
    hyper_tibble()
  
  # merge
  data_all <- data_core %>% 
    left_join(data_conc, "time") %>% 
    mutate(date = flight_date + time,
           flight_no = flight_no) %>%  
    select(date, flight_no, everything(), -time)
  
}


