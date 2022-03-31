#' Extract column names from a formula
#'  
#' @param formula Formula object
#' 
#' @return Vector of names
#' 
#' @author Shona Wilde
#' 
#' @export

cols_from_formula <- function(formula) {
  
  formula <- format(formula)
  
  cols <- str_split(formula, "~|\\+") %>% 
    unlist() %>% 
    str_trim()
  
  return(cols)
  
}
