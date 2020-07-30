
#' Function to remove outliers in a data frame. 
#' 
#' @param df Data frame
#' 
#' @param x Numeric vector within \code{df}
#' 
#' @param coef A coefficient which determines the classification of an outlier. Use 0 for no outliers to be identified.
#' 
#' @author Shona Wilde
#' 
#' @examples
#' \dontrun{
#' 
#' df %>% group_by(variable) %>% remove_outliers(value)
#' 
#' }
#' 
#' @export


remove_outliers <- function(df, x, coef = 10) {
  
  x <- enquo(x)
  
  df <- df %>% 
    mutate(
      outlier = threadr::is_outlier(!!x, coef = coef)
    )
  
  df_filt <- df %>% 
    filter(outlier == F)
  
  return(df_filt)
  
}



