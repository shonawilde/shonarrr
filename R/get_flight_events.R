
#' Split flight into range events
#'
#' Splits up flight data into straight level runs and profiles which are defined in flight summary
#'
#' @param flight_data merge file containing flight data. Date must be called "date" and of class \code{POSICXct}
#' 
#' @param flight_sum FAAM flight summary read using \code{\link{read_faam_flight_summary}}
#' 
#' @author Will Drysdale / Shona Wilde
#' 
#' @return Tibble
#' 
#' @export



get_flight_events <- function (flight_data, flight_sum) {

  range_rows = which(flight_sum$range_event == 1)
  
  range_names = trimws(flight_sum$event[range_rows], "right")
  
  range_flight_list = list()
  
  for (j in 1:length(range_rows)) {
    range_flight_list[[j]] = flight_data[(flight_data$date > 
                                            flight_sum$date_start[range_rows[j]]) & (flight_data$date < 
                                                                                       flight_sum$date_end[range_rows[j]]), ]
  }
  names(range_flight_list) = range_names
  
  # reduce list to single dataframe and assign event name
  df <- purrr::map2(range_flight_list, names(range_flight_list), 
              function(df, event_name) mutate(df, event = event_name)) %>% 
    reduce(bind_rows) %>% 
    mutate(event = str_remove(event, " "))
  
  df <- df %>% 
    group_by(flight_no, event) %>% 
    summarise(mean_alt = mean(alt_gin, na.rm = T) %>% round()) %>% 
    left_join(df, c("flight_no", "event")) %>% 
    select(date, flight_no, event, mean_alt, everything()) %>% 
    as_tibble()
  
  
  return(df)
  
}
