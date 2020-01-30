
#' Function to find distinct rows from a spatial object
#'
#' @param obj object of class \code{sf} or \code{sfc}
#' 
#' @param ... Optional variables to use when determining uniqueness.
#' 
#' @param .keep_all If \code{TRUE}, keep all variables in \code{obj}.
#' 
#' @author Shona Wilde
#' 
#' @return df
#' 
#' @export

st_distinct <- function(obj, ..., .keep_all = FALSE){
  
  distinct_rows <- obj %>% 
    st_set_geometry(NULL) %>% 
    distinct(...)
  
  return(distinct_rows)
  
}


