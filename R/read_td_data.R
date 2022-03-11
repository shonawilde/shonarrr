#' Read TD data
#'
#' Reads and cleans raw data files from the WACL thermal desorption (TD) units
#'
#' @param file_list List containing file names
#' 
#' @param verbose Should a message to the user be displayed?
#' 
#' @author Shona Wilde
#' 
#' @return Tibble
#' 
#' @export

read_td_data <- function(file_list, verbose = T) {
  
  df <- map_dfr(
    file_list, 
    read_td_data_worker, 
    verbose = verbose
  )
  
  return(df)
}


read_td_data_worker <- function(file, verbose) {
  
  if (verbose) message(threadr::date_message(), "`", file, "`...")
  
  df <- read.delim2(file) %>%
    as_tibble() %>%
    janitor::clean_names() %>%
    mutate(
      across(c("trap_fire_time":"sampling_end_time"), lubridate::ymd_hms)
    )
  
  return(df)
  
}
