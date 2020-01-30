#' Extract text within round brackets
#'
#' Extracts texts contained within round brackets
#' 
#' @param x Input string
#' 
#' @author Shona Wilde
#' 
#' @return Output string
#' 
#' @export


str_extract_round_brackets <- function(x) {
  
    stringr::str_extract(x, "\\([^()]+\\)") %>% 
    stringr::str_sub(2L, -2L)
  
}

