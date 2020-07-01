

tidy_barnes_output <- function(file){
  
  data <- R.matlab::readMat(file)
  
  matrix <- data[[1]]
  xax <- data[[2]]
  yax <- data[[3]]
  
  matrix_named <- matrix %>% 
    set_rownames(yax) %>% 
    set_colnames(xax)
  
  df <- matrix_named %>% 
    reshape2::melt(c("altitude", "distance"),
                   value.name = "conc") %>% 
    as_tibble()
  
  return(df)
    
  
}


