#' Function to detect breakpoints
#'
#' \code{find_breakpoints} can be used to find breakpoints which can be extracted as a vector by \code{get_break_numbers}
#'
#' @param df Data frame to detect breakpoints in
#' 
#' @param h minimal segment size either given as fraction relative to the sample 
#' size or as an integer giving the minimal number of observations in each segment.
#' 
#' @param breaks Maximum number of breaks to be calculated
#' 
#' @param x Object of class \code{"breakpoints"}
#' 
#' @author Shona Wilde
#' 
#' @return An object of class \code{"breakpoints"}
#' 
#' @export 




# define functions ---- 
find_breakpoints <- function(df,  h = 0.15, breaks = NULL){
  
  breakpoints <- strucchange::breakpoints(value ~ date, data = df, 
                             breaks = breaks,
                             h = h)
  
  return(breakpoints)
  
}

#' @rdname find_breakpoints
#' @export 
get_break_numbers <- function(x){
  
  break_numbers <- x$breakpoints %>% 
    map_dbl(`[`)
  
  return(break_numbers)
  
}
