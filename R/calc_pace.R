

  
# Pace as seconda and minutes 
calc_pace <- function(seconds, kilometres){
  
  pace <- map2_chr(seconds, kilometres, calc_pace_worker) %>% 
    as.numeric()
  
  return(pace)
  
}


calc_pace_worker <- function(seconds, kilometres) {
  
  
  metres <- kilometres * 1000
  
  min_per_km <- threadr::calculate_pace(seconds = seconds, 
                                        metres = metres)
  
  full_minutes <- floor(min_per_km)
  remaining_secs <- ((min_per_km) - full_minutes) * 60
  
  if (remaining_secs == 0 | round(remaining_secs) < 10) {
    
    min_secs <- paste0(full_minutes, ".0", round(remaining_secs))
    
  } 
  
  else {
    
    min_secs <- paste(full_minutes, round(remaining_secs), sep = ".")
  }
  
  
  return(min_secs)
  
}



pace_to_period <- function(pace){
  
  period <- lubridate::ms(pace)
  
  return(period)
  
}


