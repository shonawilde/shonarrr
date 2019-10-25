#' Calculate Read raw ICL file
#'
#' Reads raw data str data file from Aerodyne ICL
#'
#' @param file file
#' 
#' @author Shona Wilde
#' 
#' @return tibble
#' 
#' @export

read_icl_str_file <- function(file) {
  
  names_icl <- c("secs", "c2h6")
  
  origin <- ymd_hms("1904-01-01 00:00:00")
  
  df <- read_table2(file, 
                    skip = 1) %>% 
    select(1:2) %>% 
    set_names(names_icl) %>% 
    mutate(date = origin + secs,
           date = floor_date(date, "secs")) %>% 
    select(date, c2h6) %>% 
    as_tibble()
  
  return(df)
  
}

