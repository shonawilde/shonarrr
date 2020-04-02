#' Converts strings to a tidy format
#' 
#' @param x Input string
#' 
#' @author Shona Wilde
#' 
#' @return Cleaned string
#' 
#' @export

str_tidy <- function(x) {
  
  x_clean <- x %>% 
    str_to_lower() %>% 
    str_replace_all("-", "_") %>% 
    str_replace_all(" ", "_") %>% 
    str_replace_all(",", "_") 
  
  return(x_clean)
  
}
