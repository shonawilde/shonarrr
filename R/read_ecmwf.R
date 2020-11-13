#' Read ECMWF
#'
#' Reads and cleans data from the Copernicus Climate Data Store (CDS)
#'
#' @param file_list Files to read
#' 
#' @param verbose Should a message to the user be displayed?
#' 
#' @author Shona Wilde
#' 
#' @return Tibble
#' 
#' @export


read_ecmwf <- function(file_list, verbose = T) {
  
  df <- purrr::map_dfr(
    file_list,
    read_ecmwf_worker,
    verbose = verbose
  )
  
  return(df)
  
}


read_ecmwf_worker <- function(file, verbose) {
  
  if (verbose) message(threadr::date_message(), "`", file, "`...")
  
  
  df <- tidync::tidync(file) %>% 
    tidync::activate("D0,D1,D2") %>% 
    tidync::hyper_tibble() %>% 
    mutate(time = time*3600,
           datetime = as.POSIXct(time, 
                             tz = "UTC",
                             origin = "1900-01-01 00:00")) %>% 
    select(-time) %>% 
    select(datetime, everything()) 
    
  
  return(df)
  
}



