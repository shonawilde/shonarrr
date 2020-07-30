
#' Read Markes data
#'
#' Reads and cleans raw data files from Markes pre-concentration units
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


read_markes_data <- function(file_list, verbose = T) {
  
  map_dfr(file_list,
          read_markes_data_worker,
          verbose = verbose)
}


read_markes_data_worker <- function(file, verbose = verbose) {
  
  # Message to user
  if (verbose) message(threadr::date_message(), "`", file, "`...")
  
  # read data
  df <- suppressMessages(suppressWarnings(read_table2(file))) %>% 
    janitor::remove_empty("cols")
  
  #set anmes
  col_names <- c("preconcentrator_method_start_date",
                 "preconcentrator_method_start_time",
                 "spare_date",
                 "spare_time",
                 "sample_start_date",
                 "sample_start_time",
                 "sample_end_date",
                 "sample_end_time",
                 "sample_volume",
                 "channel",
                 "sample_pressure_start",
                 "sample_pressure_end"
  )
  
  names(df) <- col_names
  
  # tidy data and format dates
  df_clean <- df %>% 
    mutate(preconcentrator_method_start_time = paste(preconcentrator_method_start_date, preconcentrator_method_start_time) %>% 
             ymd_hms() %>% 
             floor_date(uni = "minute"),
           sample_start_time = paste(sample_start_date, sample_start_time) %>% 
             ymd_hms() %>% 
             floor_date(uni = "minute"),
           sample_end_time = paste(sample_end_date, sample_end_time) %>% 
             ymd_hms() %>% 
             floor_date(uni = "minute"),
           sample_mid_time = threadr::date_centre(sample_start_time, sample_end_time)
    ) %>%  
    select(preconcentrator_method_start_time,
           sample_start_time, 
           sample_end_time,
           sample_mid_time,
           channel,
           sample_volume,
           sample_pressure_start,
           sample_pressure_end
    ) %>% 
    janitor::clean_names()
  
  return(df_clean)
  
  
}



