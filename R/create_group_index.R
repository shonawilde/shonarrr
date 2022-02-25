#' Function to create groups within data
#' 
#' \code{create_group_index} creates groups within data based on a repeating index
#'
#' @param df Input data frame
#' 
#' @param group_size Size of groups to create
#' 
#' @return Tibble
#' 
#' @author Shona Wilde 
#' 
#' @export

create_group_index <- function(df, group_size) {
  
  df <- as_tibble(df)
  
  n <- nrow(df)
  m <- n %/% group_size + 1
  index <- rep(1:m, each = group_size)[1:n]
  
  if (n %% group_size == 1 && n > 1) {
    
    index[n-1] <- index[n]
    
  }
  
  df$group <- index
  
  return(df)
  
}




