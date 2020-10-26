#' Tidy banres output
#'
#' Reads and re-formats MatLab data from Barnes interpolation
#'
#' @param file File to read
#' 
#' @author Shona Wilde
#' 
#' @return Tibble
#' 
#' @export


tidy_barnes_output <- function(file){
  
  data <- R.matlab::readMat(file)
  
  matrix <- data[[1]]
  xax <- data[[2]]
  yax <- data[[3]]
  
  matrix_named <- matrix %>% 
    trqwe::set_rownames(yax) %>% 
    trqwe::set_colnames(xax)
  
  df <- matrix_named %>% 
    reshape2::melt(c("altitude", "distance"),
                   value.name = "conc") %>% 
    tibble::as_tibble()
  
  return(df)
    
  
}


