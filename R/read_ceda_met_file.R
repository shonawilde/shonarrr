#' Read CEDA Meteorology
#'
#' Reads and cleans CEDA met files
#'
#' @param file path to file
#' 
#' @param site "KM" or "LP"
#' 
#' @author Shona Wilde
#' 
#' @return data frame
#' 
#' @export

read_ceda_met_file <- function(file, site){
  
  # read lines
  text <- readr::read_lines(file) %>% 
    str_to_lower()
  
  # isolate preamble
  df_preamble <- text[2:(str_which(text, "bad"))]
  
  # get date
  date <- text[str_which(text, "time in minutes")] %>% 
    str_split_fixed("since", 2) %>% 
    nth(2) %>% 
    str_remove(" ") %>% 
    ymd_hm()
  
  # get variable names
  variable_names <- text[str_which(text, "time in minutes"): str_which(text, "bad") + 4L] %>% 
    str_rm_round_brackets()  %>% 
    str_trim() %>% 
    str_to_lower() %>% 
    str_replace_all(" ", "_") %>% 
    str_replace("wind_direction", "wd") %>% 
    str_replace("wind_speed", "ws") %>%
    str_replace("windspeed", "ws") %>% 
  #  str_replace("air_pressure", "air_pressure") %>% 
    str_replace("temperature", "air_temp")
  
  # drop extra variable names
  if (site %in% "LP"){
    
    drop_vals = (length(variable_names)-9):length(variable_names)
    variable_names <- c("mins", variable_names[-drop_vals])
  }
  
  else {
    drop_vals = (length(variable_names)-8):length(variable_names)
    variable_names <- c("mins", variable_names[-drop_vals])
    
  }
  
  # read tablular data
  df <- text[(stringr::str_which(text, "bad") + 4L):length(text)] %>% 
    read_table2(col_names = F) %>% 
    purrr::set_names(variable_names) %>% 
    mutate(date = date + mins) %>% 
    select(date, everything(), -mins)
  
  return(df)
  
}
