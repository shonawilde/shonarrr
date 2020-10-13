#' Calculate alpha
#'
#' Function to calculate the angle between wind direction and the line perpendicular to the flight track
#' 
#' @param heading Aircraft heading in degrees
#' 
#' @param wd  Wind direction in degrees
#' 
#' @author Shona Wilde
#' 
#' @return Angle in degrees.
#' 
#' @export

calc_alpha <- function(heading, wd = "wd"){
  
  # normalise to heading - rotate to make heading 0
  wd_rotate <- wd - heading
  
  if (wd_rotate < 0) {
    
    wd_rotate <- 360-(abs(wd-heading))
    
  }
  
  # always take wd in first 2 quadrants
  if (wd_rotate > 180) {
    
    wd_rotate <- wd_rotate-180
  }
  
  #  alpha is the angle between wind direction and direction perpendicular to flight track
  angle_perpendicular <- 90
  
  alpha <- angle_perpendicular-wd_rotate
  
  if (wd_rotate>angle_perpendicular){
    
    alpha <- wd_rotate-angle_perpendicular
  }
  
  return(alpha)
}







