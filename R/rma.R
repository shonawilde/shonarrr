#' Model2 regression  
#'
#' Generic wrapper for the \code{lmodel2} function. Should be used when there is an error associated with both the x and y variables in the regression
#'
#' @param df Data frame containing model variables
#' 
#' @param formula A formula specifying the bivariate model, as in \code{\link[stats]{lm} and \code{\link[lmodel2]{lmodel2}}}
#' 
#' @author Shona Wilde
#' 
#' @return Regression output
#' 
#' @export


rma <- function(df, formula) {
  
  df <- data.frame(df)
  
  suppressMessages(
    
    model <- lmodel2::lmodel2(formula = formula, data = df)
  )
  
  return(model)
  
}


