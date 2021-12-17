#' Function to create empty tibble with 0 rows
#'
#' @param names Vector of names for tibble
#' 
#' @return Empty tibble with column names
#'
#' @author Shona Wilde
#'
#' @export


create_empty_tibble <- function(names) {
  
  ncol = length(names)
  
  df <- data.frame(matrix(0, ncol = ncol,  nrow = 0)) %>% 
    purrr::set_names(names) %>% 
    as_tibble()
  
  return(df)
  
}

