#' Interpolate values within a time series
#' 
#' This function is typically used to increase the resolution of time series data e.g hourly to minute. All numeric variables within the data will be interpolated. 
#' Character or factor variables are filled down.
#'  
#' @param df Input data frame or tibble
#' 
#' @param by Increment of the new time series. Options are 'year', 'month', 'day', 'hour', 'min', or 'sec'
#' 
#' @return Tibble
#' 
#' @author Shona Wilde
#' 
#' @export

interpolate_time_series <- function(df, by = "hour") {
  
  if (!"date" %in% names(df)) {
    stop("`date` must be present in data frame.", call. = FALSE)
  }
  if (!lubridate::is.POSIXct(df$date)) {
    stop("`date` must be a POSIXct object.", call. = FALSE)
  }
  
  date_min <- min(df$date)
  date_max = max(df$date)
  
  date_seq <- seq(as.POSIXct(date_min), as.POSIXct(date_max), by = by)
  
  df_date <- tibble(
    date = date_seq
  )
  
  df_join <- df_date %>% 
    left_join(
      df,
      by = "date"
    )
  
  df_interp <- df_join %>% 
    mutate(
      across(where(is.numeric), zoo::na.approx, na.rm = F)
    ) %>% 
    fill(
      where(is.character), where(is.factor)
    )
  
  return(df_interp)
}



    