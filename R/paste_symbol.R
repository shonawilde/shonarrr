#' Function to paste symbols. Useful for labelling axis.
#'
#' @param x R object to be converted into a character vector
#' 
#' @param symbol Symbol. Default is %. 
#' 
#' @param position Position to paste symbol relative to vector. Either "start" or "end".
#'
#' @return A character vector of the concatenated values.
#' 
#' @author Shona Wilde
#' 
#' @export

paste_symbol <- function(x, symbol = "%", position = "end", remove_space = T) {
  
  
  if (position == "start")  
    
    text <- paste(symbol, x)
  
  else
    
    text <- paste(x, symbol)
  
  
  if (remove_space)
    
    text <- str_remove(text, " ")
  
  return(text)
  
}




