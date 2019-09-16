#' Read CEDA Greenhouse Gas
#'
#' Reads and cleans CEDA GHG files
#'
#' @param file path to file
#' 
#' @author Shona Wilde
#' 
#' @return data frame
#' 
#' @export

read_ceda_ghg_file <- function(file) {
  
  
  # read all of text
  text <- readr::read_lines(file)
  
  # isolate preamble
  df_preamble <- text[2:(str_which(text, "Bad/missing data"))]
  
  # get date
  date <- text[str_which(text, "Time in minutes")] %>% 
    str_split_fixed("since", 2) %>% 
    nth(2) %>% 
    str_remove(" ") %>% 
    ymd_h()
  
  # col names
  variable_names <- c("mins", "co2", "co2_flag", "ch4", "ch4_flag")
  
  # read tablular data
  df <- text[(stringr::str_which(text, "Bad/missing data") + 1L):length(text)] %>% 
    read_table2(col_names = F) %>% 
    purrr::set_names(variable_names) %>% 
    mutate(date = date + mins) %>% 
    select(date, everything(), -mins)
  
  return(df)
}

