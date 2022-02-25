#' Assign time of day to a date
#' 
#' Function to assign a date as either 'morning', 'afternoon', 'evening' or 'night'
#' 
#' @param date POSIXct date
#' 
#' @return Tibble
#' 
#' @author Shona Wilde
#' 
#' @export

assign_time_of_day <- function(date) {
  
  stopifnot(
    "Date must be a 'POSIXct' object." = class(date)[[1]] %in% "POSIXct"
  )
  
  tod <- case_when(
    between(hour(date), 6, 11) ~ "morning",
    between(hour(date), 12, 17) ~ "afternoon",
    between(hour(date), 18, 22) ~ "evening",
    TRUE ~ "night"
  )
  
  return(tod)
  
}

