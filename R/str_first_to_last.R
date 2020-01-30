#' Extract first word of string and move to end
#'
#' @param x Input string
#' 
#' @author Shona Wilde
#' 
#' @return Output string
#' 
#' @export

str_first_to_last <- function(x) {
  
  x_remove <- str_split(x, "_", 2) %>% 
    map_chr(1) %>% 
    paste0("_")
  
  x_add <- paste0("_", str_remove(x_remove,  "_"))
  
  x <- x %>% 
    str_remove(x_remove) %>% 
    paste0(x_add)
  
  return(x)
  
}
