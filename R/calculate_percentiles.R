#' Calculate percentiles
#'
#' Function to calculate summary statistics 
#'
#' @param x numeric vector 
#' 
#' @param probs numeric vector of quantiles to be calculated
#' 
#' @author Shona Wilde
#' 
#' @return Tibble
#' 
#' @export

calculate_percentiles <- function(x, probs = c(0.01, 0.1, 0.25, 0.5, 0.75, 0.95, 0.99)) {
  
  # check data
  stopifnot(is.numeric(x) | lubridate::is.POSIXt(x))
  
  # basic summary stats
  x <- x[!is.na(x)]
  n <- length(x)
  sd <- sd(x)
  mean <- mean(x)
  se <- sd/sqrt(n)
  
  # calculate quantiles
  quantiles <- quantile(x, probs = probs) %>% 
    enframe() %>% 
    spread(name, value)
  
  # bind into tibble
  df <- tibble(n, sd, mean, se) %>% 
    bind_cols(quantiles)
  
  return(df)

}


