
#' Read FAAM core nitrates file
#'
#' Reads and cleans core nitrates FAAM file
#'
#' @param nitrates_file_list List containing file names
#' 
#' @author Shona Wilde
#' 
#' @return Tibble
#' 
#' @export


read_core_nitrates_data <- function(nitrates_file_list, verbose = TRUE) {
  
  map_dfr(nitrates_file_list, read_core_nitrates_worker,
          verbose = verbose)
}

read_core_nitrates_worker <- function(file, verbose) {
 
   # message
  if (verbose) message(threadr::date_message(), "`", basename(file), "`...")
  
  # Connection to ncdf file
  nc <- ncdf4::nc_open(file)
  
  # get meta data from ncdf
  meta_data <- nc %>% 
    capture.output() 
  
  # flight_date <- meta_data[str_which(meta_data, "flight_date")] %>% 
  #   str_split(":", simplify = F)  %>% 
  #   map_chr(`[` (2)) %>% 
  #   str_remove(" ") %>% 
  #   paste0(" 00:00") %>% 
  #   lubridate::dmy_hm()
  
  # Get and format dates
  date_start <- nc$dim$time$units %>% 
    stringr::str_remove("seconds since ") %>% 
    lubridate::ymd_hms(tz = "UTC")
  
  date <- date_start + as.vector(nc$dim$time$vals) 
  
  # get flight number
  flight_no <- meta_data[str_which(meta_data, "flight_number")] %>% 
    str_split(":", simplify = F)  %>% 
    map_chr(`[` (2)) %>% 
    str_remove(" ") %>% 
    str_to_lower()
  
  # Get variable names
  variables <- names(nc$var)
  
  # Extract all data
  list_nc <- variables %>% 
    purrr::set_names(variables) %>% 
    purrr::map(~ncdf4::ncvar_get(nc, varid = .))
  
  class_list_nc <- list_nc %>% 
    purrr::set_names(variables) 
  
  # Function to change colnames
  change_colname_value <- function(df, new_colname){
    df %>% rename(!!new_colname := value)
  }
  
  # extract all 10 Hz matrix elements
  nc_matrix <- list_nc %>% 
    keep(is.matrix)
  
  # get matrix names
  names_matrix <- names(nc_matrix)

# clean and bind all 10 Hz data  
  data_10hz <- nc_matrix %>% 
    map(reshape2::melt) %>% 
    map(as_tibble) %>% 
    map2(names_matrix, change_colname_value) %>% 
    map(select, -1:-2) %>%  
    reduce(bind_cols) %>% 
    mutate(date = rep(date, each = 10)) 
  
  # extract all 1 Hz elements
  nc_array <- list_nc %>% 
    discard(is.matrix)
  
  # get array names
  names_array <- names(nc_array)
  
  # close connection
  ncdf4::nc_close(nc)
  
  # clean and bind 1 Hz data
  data_1hz <- nc_array %>% 
    map(as_tibble) %>% 
    map(mutate, date = date) %>% 
    map2(names_array, change_colname_value) %>% 
    reduce(left_join, "date") 
    
  
  # merge all data
  data_all <- data_10hz %>% 
    left_join(data_1hz, "date") %>% 
    mutate(flight_no = flight_no) %>% 
    select(date, flight_no, everything())
  
  return(data_all)

}


