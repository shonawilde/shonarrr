#' Filter a data frame between two dates
#'  
#' @param df Input data frame or tibble
#' 
#' @param start_date Start date
#' 
#' @param end_date End date
#' 
#' @param id Name of ID column to add to data. Useful when using with \code{\link{purrr::map}}
#' 
#' @return Tibble
#' 
#' @author Shona Wilde
#' 
#' @export

filter_by_date_range <- function(df, start_date, end_date, id = NA) {
  
  if (!"date" %in% names(df)) {
    stop("`date` must be present in data frame.", call. = FALSE)
  }
  if (!lubridate::is.POSIXct(df$date)) {
    stop("`date` must be a POSIXct object.", call. = FALSE)
  }
  
  df_filt <- df %>% 
    filter(
      between(date, ymd_hms(start_date), ymd_hms(end_date))
    )
  
  if (!is.na(id)) {
    
    df_filt <- df %>% 
      filter(
        between(date, ymd_hms(start_date), ymd_hms(end_date))
      ) %>% 
      mutate(
        id = id
      )
    }
  
  return(as_tibble(df_filt))
  
}


