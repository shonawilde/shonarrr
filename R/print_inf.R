#' Print infinate values
#'
#' Wrapper for \code{print(n = Inf)}
#'
#' @param x Input to print
#'
#' @author Shona Wilde
#' 
#' @return Tibble
#' 
#' @export


print_inf <- function(x){
  
  x <- x %>% 
    as_tibble() %>% 
    print(n = Inf)
  
}
