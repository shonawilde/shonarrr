#' Function to add a group index to data
#' 
#' \code{add_group_index} creates groups within data based on a repeating index.
#'
#' @param x Vector.
#' 
#' @param each Size of groups.
#' 
#' @return Value.
#' 
#' @author Shona Wilde 
#' 
#' @export

add_group_index <- function(x, each) {
  
  n <- length(x)
  m <- n %/% each + 1
  index <- rep(1:m, each = each)[1:n]
  
  if (n %% each == 1 && n > 1) 
    
    index[n-1] <- index[n]
  
  return (as.factor(index))
  
}


