#' Model2 regression  
#'
#' Generic wrapper for the \code{lmodel2} function
#'
#' @param df Data frame containing model variables
#' 
#' @param x x-component for model
#' 
#' @param y y-component for model
#' 
#' @author Shona Wilde
#' 
#' @return Regression output
#' 
#' @export

rma_model <- function(df, x = "x", y = "y") {
  
  df <- data.frame(df)
  
  lmodel2::lmodel2(df[,y]~df[,x], data = df, "interval", "interval", 99)
}
