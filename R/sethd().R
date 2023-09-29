#' Set home directory
#' 
#' Set home directory based on the current operating system
#'
#' @return \code{sethd} returns the current directory before the change as in \code{setwd}.
#' 
#' @author Shona Wilde
#' 
#' @export

sethd <- function(drive = "google") {
  
  system <- Sys.info()[1]
  
  if (drive == "google")
  {
    
    if (system == "Windows") {
      
      dir <- setwd("G:/My Drive/")
      
    }
    
    if (system == "Darwin") {
      
      dir <- setwd("~/Google Drive/My Drive/")
    }
    
  }
  
  if (drive == "one") {
    
    if (system == "Windows") {
      
      dir <- setwd("C:/Users/Shona/OneDrive - Carleton University/")
      
    }
    
    if (system == "Darwin") {
      
      dir <- setwd("~/OneDrive - Carleton University/")
    }
    
  }
    
  
  
  return(invisible(dir))
  
}

