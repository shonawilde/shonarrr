#'Function to extract a specific element from \code{str_split}
#'
#' @param x Input string
#' 
#' @param pattern Pattern to split string on
#' 
#' @param n Number of pieces to return. Default (Inf) uses all possible split positions
#' 
#' @param element Element of split string to return
#' 
#' @param as_character Should the string be returned as a character?
#' 
#' @author Shona Wilde
#' 
#' @export


str_split_extract_element <- function(x, pattern, n = Inf, element = 1, as_character = T) {
  
  split <- str_split(
    x,
    pattern = pattern,
    n = n
  )
  
  x_keep <- split[[1]][element] 
  
  if (as_character)
    
    x_keep <- as.character(x_keep)
  
  return(x_keep)
  
}
  
