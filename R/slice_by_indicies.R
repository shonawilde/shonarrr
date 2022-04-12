#' Slice by a range of indexes
#'
#' @param df Input data frame or tibble 
#' 
#' @param index_start Start row value
#' 
#' @param index_end End row value
#'
#' @return Tibble
#' 
#' @author Shona Wilde
#' 
#' @export

slice_by_indices <- function(df, index_start, index_end) {
  
  
  slice(df, index_end:index_start)
  
}
