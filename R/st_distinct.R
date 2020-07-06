
#' Function to find distinct rows from a spatial object
#'
#' @param data Data with class \code{sf} or \code{sfc}
#' 
#' @param ... Optional variables to use when determining uniqueness.
#' 
#' @param .keep_all If \code{TRUE}, keep all variables in \code{data}.
#' 
#' @author Shona Wilde
#' 
#' @return df
#' 
#' @export

st_distinct <- function(data, ..., .keep_all = FALSE){
  
  distinct_rows <- data %>% 
    st_set_geometry(NULL) %>% 
    distinct(...)
  
  return(distinct_rows)
  
}


