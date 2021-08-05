#' Functions for string manipulation
#' 
#' @param x Input string.
#' 
#' @author Shona Wilde.
#' 
#' @return Output string.
#' 
#' @export


str_extract_round_brackets <- function(x) {
  
    stringr::str_extract(x, "\\([^()]+\\)") %>% 
    stringr::str_sub(2L, -2L)
  
}


#' @rdname str_extract_round_brackets
#' @export
str_extract_square_brackets <- function (x) 
{
  stringr::str_extract(x, "\\[[^()]+\\]") %>% stringr::str_sub(2L, -2L)
}




#' @rdname str_extract_round_brackets
#' @export
str_rm_unicode <- function (x)
  
{
  stringr::str_replace_all(x, "[^\u0001-\u007F]+|<U\\+\\w+>", "")
}




