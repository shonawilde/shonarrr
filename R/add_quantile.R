#' Add quantile
#'
#' Calculate single quantile for a dataset. Useful if wanting to filter below a certain threshold of values, ie lowest 1%.
#'
#' @param df Data frame
#' 
#' @param variable Variable to calculate quantile for 
#' 
#' @param percent Quantile to calculate
#' 
#' @param na.rm Should NAs be removed?
#' 
#' @author Shona Wilde & Stuart Grange
#' 
#' @return Tibble
#' 
#' @export

# Define a function
add_quantile <- function(df, variable = "value", percent = 0.01, na.rm = FALSE) {
  
  # Calculate quantile
  quantile <- df %>% 
    pull(!!variable) %>% 
    quantile(probs = percent, na.rm = na.rm, names = TRUE)
  
  # Add to tibble
  df <- df %>% 
    mutate(quantile_name = names(!!quantile),
           quantile = !!quantile)
  
  return(df)
  
} 
