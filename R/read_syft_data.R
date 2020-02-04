#' Read SYFT data
#'
#' Reads worked up SYFT data 
#'
#' @param file file
#' 
#' @author Shona Wilde
#' 
#' @return tibble
#' 
#' @export

read_syft_data <- function(file, guess_max = 10000) {
  
  df <- read_xlsx(file, guess_max = guess_max) %>% 
    rename_all(. %>% tolower()) %>% 
    rename_all(. %>% str_rm_round_brackets()) %>% 
    rename_all(. %>% str_replace_all(" ", "")) %>% 
    rename_all(. %>% str_remove("corr")) %>%
    rename_all(. %>% str_replace("-", "_")) %>% 
    rename(date = actualtime) %>% 
    mutate(date = floor_date(date, "secs")) %>%
    select(date, everything()) %>% 
    as_tibble()
  
  if (suppressWarnings(!class(df$date)[1] %in% "POSIXct"))
    
    df$date <- ymd_hms(df$date)
  
  return(df)
  
}


