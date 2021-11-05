#' Function to find and add nearest date in another data frame 
#' 
#' @param df1 Data frame.
#' 
#' @param df2 Data frame to find nearest date in.
#' 
#' @author Shona Wilde 
#' 
#' @return Date vector of class \code{POSIXct}.
#' 
#' @export

add_nearest_date <- function(df1, df2) {
  
  df_nearest <- df1 %>% 
    mutate(
      nearest_date = find_nearest_date(.$date, df2$date),
      date_delta = as.numeric(nearest_date - date)  
    )
  
  return(df_nearest)
  
}
