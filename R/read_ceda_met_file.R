#' Read CEDA met
#'
#' Reads and cleans CEDA met Files
#'
#' @param file path to file
#' 
#' @author Shona Wilde
#' 
#' @return data frame
#' 
#' @export

read_ceda_met_files <- function(file) {
  
  
  # read lines
  text <- readr::read_lines(file)
  
  # isolate preamble
  df_preamble <- text[2:(which(text %in% "4-bad data"))]
  
  # get date
  date <- text[str_which(text, "Time in minutes")] %>% 
    str_split_fixed("since", 2) %>% 
    nth(2) %>% 
    str_remove(" ") %>% 
    ymd_hm()
  
  # get variable names
  variable_names <- text[str_which(text, "Time in minutes"): str_which(text, "4-bad data") + 4L] %>% 
    str_to_lower() %>% 
    str_remove_all(" ") %>% 
    str_replace("winddirection", "wd") %>% 
    str_replace("windspeed", "ws") %>%
    str_replace("airpressure", "air_pressure") %>% 
    str_replace("temperature", "air_temp") %>% 
    str_rm_round_brackets()
  
  # drop extra variable names
  drop_vals = (length(variable_names)-8):length(variable_names)
  variable_names <- c("secs", variable_names[-drop_vals])
  
  # read tablular data
  df <- text[(stringr::str_which(text, "4-bad data") + 4L):length(text)] %>% 
    read_table2(col_names = F) %>% 
    purrr::set_names(variable_names) %>% 
    mutate(date = date + secs) %>% 
    select(date, everything(), -secs)
  
  return(df)
}
