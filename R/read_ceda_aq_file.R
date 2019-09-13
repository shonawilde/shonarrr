#' Read CEDA AQ
#'
#' Reads and cleans CEDA AQ Files
#'
#' @param file path to file
#' 
#' @author Shona Wilde
#' 
#' @export

read_ceda_aq_files <- function(file) {
  
  
  # read all of text
  text <- readr::read_lines(file)
  
  # isolate preamble
  df_preamble <- text[2:(which(text %in% "4 - bad data"))]
  
  # get date
  date <- text[str_which(text, "Time in minutes")] %>% 
    str_split_fixed("since", 2) %>% 
    nth(2) %>% 
    str_remove(" ") %>% 
    ymd_hm()
  
  # get variable names
  variable_names <- text[str_which(text, "Time in minutes"): str_which(text, "Ozone precision") + 4L] %>% 
    str_to_lower() %>% 
    str_rm_round_brackets()
  
  # drop extra variable names
  drop_vals = (length(variable_names)-5):length(variable_names)
  variable_names <- c("secs", variable_names[-drop_vals])
  
  # read tablular data
  df <- text[(stringr::str_which(text, "4 - bad data") + 3L):length(text)] %>% 
    read_table2(col_names = F) %>% 
    purrr::set_names(variable_names) %>% 
    mutate(date = date + secs) %>% 
    select(date, everything())
  
  return(df)
}


