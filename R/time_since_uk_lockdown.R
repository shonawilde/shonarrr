#' Function to calculate time since UK lockdown
#' 
#' @author Shona Wilde 
#' 
#' @param format Type of return. Can be "seconds", "hms", "duration", "period" or "days" (default).
#' 
#' @return Vector 
#' 
#' @export


time_since_uk_lockdown <- function(format = "days") {
  
  date_one <- ymd("2020-03-23")
  date_two <- today()
  
  x <- threadr::calculate_time_span(date_one, date_two,
                      format = format)
  
  return(x)
  
}


  