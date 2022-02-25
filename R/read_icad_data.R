#' Read data from the ICAD instrument
#'  
#' @param file_list List of files to read
#' 
#' @param pad Should the time series be padded? If TRUE the time series is interpolated to give an observation every second
#' 
#' @param verbose Display message to user? 
#' 
#' @return Tibble
#' 
#' @author Shona Wilde
#' 
#' @export

read_icad_data <- function(file_list, pad = T, verbose = T) {
  
  df <- imap_dfr(
    file_list,
    ~read_icad_data_worker(
      file = .x,
      index = .y,
      pad = pad,
      verbose = verbose
    )
  )
  
  return(df)
  
}


read_icad_data_worker <- function(file, index, pad, verbose) {
  
  if (verbose) {
    message(lubridate::now(tzone = Sys.timezone()), " `", basename(file), "`...")
  }  
  
  # load file
      df <- read_delim(
        file, 
        na = c("nan",intToUtf8(8734),intToUtf8(c(45,8734))),
        show_col_types = F
        ) %>%
        janitor::clean_names(parsing_option = 3) %>% 
        rename(date = 1) %>% 
        rename_with(~str_replace(.x, "no2_x", "nox")) %>% 
        mutate(file_index = index, .before = date)
  
  # pad time series
  if (pad) {
    
    df <- df %>% 
      mutate(date = floor_date(date, "second")) %>% 
      threadr::time_pad(interval = "second") %>%
      mutate(
        across(where(is.numeric), zoo::na.approx, na.rm = F)
      )
    
    message("Interpolating time series...")
    
  }
    
    return(df)
    
  
}


