#' Slice min and max rows
#'
#' Select rows of highest and lowest values based on variables 
#'
#' @param df data
#' 
#' @param order_by variable to order by
#' 
#' @param n number of rows to select, default is 1
#' 
#' @author Shona Wilde
#' 
#' @return tibble
#' 
#' @export


slice_min_max <- function(df, order_by = value, n = 1) {
  
  order_by = enquo(order_by)
  
  min <- slice_min(df, !!order_by, n = n) %>%
    mutate(type = "min")

  max <- slice_max(df, !!order_by, n = n) %>%
    mutate(type = "max")

  df <- bind_rows(min, max) %>%
    as_tibble()
  
  return(df)
  
}




