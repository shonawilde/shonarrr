#' Model2 regression  
#'
#' Generic wrapper for the \code{lmodel2} function. Should be used when there is an error associated with both the x and y variables in the regression
#' 
#' @param formula A formula specifying the bivariate model, as in \code{\link[stats]{lm} and \code{\link[lmodel2]{lmodel2}}}
#' 
#' @param data Input data containing model variables
#' 
#' @param ... Additional arguments to be passed to \code{lmodel2}
#' 
#' @author Shona Wilde
#' 
#' @return Model object
#' 
#' @export


rma <- function(formula, data, ...) {
  
  data <- data.frame(data)
  
  suppressMessages(
    
    model <- lmodel2::lmodel2(formula = formula, data = data, ...)
  )
  
  return(model)
  
}



