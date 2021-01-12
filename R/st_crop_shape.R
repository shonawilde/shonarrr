

st_crop_shape <- function(st, st_polygon, inverse = F) {
  
  st_crop <- st[lengths(st_intersects(st, st_polygon)), ]
  
  if (inverse)
    
    st_crop <- st[!lengths(st_intersects(st, st_polygon)), ]
  
  return(st_crop)
  
}
