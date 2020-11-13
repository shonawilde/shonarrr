#' Tidy banres output
#'
#' Reads and re-formats MatLab data from Barnes interpolation
#' 
#' @param file_list Files to read
#' 
#' @param verbose Should a message to the user be displayed?
#' 
#' @author Shona Wilde
#' 
#' @return Tibble
#' 
#' @export

tidy_barnes_output <- function(file_list, verbose = T) {
  
  df <- purrr::map_dfr(
    file_list,
    tidy_barnes_output_worker,
    verbose = verbose
  )
  
  return(df)
  
}


tidy_barnes_output_worker <- function(file, verbose){
  
  if (verbose) message(threadr::date_message(), "`", file, "`...")
  
  # pull meta data out of file name
  string <- file %>% 
    basename() %>% 
    stringr::str_split("_") %>% 
    purrr:::flatten_chr()
  
  flight_no <- string[4]
  plume <- string[5]
  gas <- string[length(string)-1]
  
  # read Matlab data
  data <- R.matlab::readMat(file)
  
  matrix <- data[[1]]
  xax <- data[[2]]
  yax <- data[[3]]
  
  matrix_named <- matrix %>% 
    trqwe::set_rownames(yax) %>% 
    trqwe::set_colnames(xax)
  
  # to data frame
  df <- matrix_named %>% 
    reshape2::melt(c("altitude", "distance"),
                   value.name = "conc") %>%
    mutate(flight_no = flight_no,
           plume = plume,
           gas = gas) %>% 
    select(flight_no, plume, gas, everything()) %>% 
    tibble::as_tibble()
  
  return(df)
  
}


