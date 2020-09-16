#' Correlation summary 
#'
#' Function to calculate correlation coefficients and tidy output
#'
#' @param df Data frame containing model variables
#' 
#' @param x x-component for model
#' 
#' @param y y-component for model
#' 
#' @param method a character string indicating which correlation coefficient is to be used for the test.
#'  One of "pearson", "kendall", or "spearman".
#'  
#' @param conf_level confidence level for the returned confidence interval
#' 
#' @author Shona Wilde
#' 
#' @return Tibble
#' 
#' @export

correlation_summary <- function(df, 
                                x = "x",
                                y = "y",
                                method = "pearson",
                                conf_level = 0.95) {
  
  x <- df %>% 
    pull(!!x)
  
  y <- df %>% 
    pull(!!y)
  
  nrow = length(x)
  
  cor.test(x, y, 
           method = method,
           conf.level = conf_level,
           exact = F) %>% 
    broom::tidy() %>% 
    mutate(nrow = nrow,
           conf_level = conf_level,
           critical_r = calc_critical_r(nrow,
                                        alpha = 1-conf_level),
           signif = if_else(estimate > critical_r, T, F))
  
}

#' @export
calc_critical_r <- function(n, alpha = .05) {
  
  # formula from:
  #https://stats.stackexchange.com/questions/281801/how-to-calculate-critical-values-for-r-for-a-large-dataset
  
  df <- n-2
  
  critical_t <- qt(alpha/2, df, lower.tail = F)
  
  critical_r <- sqrt((critical_t^2) / ((critical_t^2) + df))
  
  return(critical_r)
  
}
