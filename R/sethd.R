#' Set home directory
#' 
#' Set home directory based on the current operating system
#'
#' @return \code{sethd} returns the current directory before the change as in \code{setwd}.
#' 
#' @author Shona Wilde
#' 
#' @export

sethd <- function() {
  
  system <- Sys.info()[1]
  
  if (system == "Windows") {
    
    dir <- setwd("G:/My Drive/")
    
  }
  
  if (system == "Darwin") {
    
    dir <- setwd("~/Google Drive/My Drive/")
  }

  return(invisible(dir))
  
}
  
