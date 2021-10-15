#' Function to return bounding box of spatial geometries
#'
#' @param st Object of class \code{sf}.
#' 
#' @param group_by Optional grouping variable.
#' 
#' @param buffer Buffer distance in m.
#' 
#' @param crs Projection of output. Defualt is to convert to WGS84.
#' 
#' @author Shona Wilde
#' 
#' @return An object of the same class as \code{st}, with manipulated geometry.
#' 
#' @export


st_get_boundary <- function(st, group_by, buffer = 0, crs = 4326) {
  
  # check class
  stopifnot(
    "Object class must be `sf`" = 
      inherits(st,"sf") 
  )
  
  group_by <- enquo(group_by)
  
  # get boundary
  st_boundary <- st %>% 
    group_by(!!group_by) %>% 
    summarize(
      geometry = st_as_sfc(st_bbox(geometry))
    ) %>% 
    st_transform(crs = 7801) %>% 
    st_buffer(dist = buffer, joinStyle = "ROUND") %>% 
    st_transform(crs = crs)
  
  return(st_boundary)
  
}
  

  
  