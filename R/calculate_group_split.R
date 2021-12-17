#' Function to calculate the percentage of observations in each group
#'
#' @param df Data frame
#' 
#' @param ... Grouping variable(s)
#' 
#' @return Summary tibble
#'
#' @author Shona Wilde
#'
#' @export



calculate_group_split <- function(df, ...) {
  
  group_by <- quos(...) 
  
  df_percent <- df %>% 
    ungroup() %>% 
    mutate(total = n()) %>% 
    group_by(!!!group_by, total) %>% 
    summarise(n = n()) %>% 
    mutate(percent = 100*(n/total))
  
  return(df_percent)
  
}


