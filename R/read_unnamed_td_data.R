#' Read unnamed TD data
#' 
#' Function to read data from the WACL thermal desorption units with unnamed columns.
#'
#' @param file_list List of files to read
#' 
#' @param verbose Display message to user?
#'
#' @return Tibble
#' 
#' @author Shona Wilde
#' 
#' @export

read_unnamed_td_data <- function(file_list, verbose = T) {
  
  df <- map_dfr(
    file_list,
    read_unnamed_td_data_worker,
    verbose = verbose
  )
  
  return(df)
}

read_unnamed_td_data_worker <- function(file, verbose) {
  
  # user message
  if (verbose) message(threadr::date_message(), "`", file, "`...")
  
  # manual vector of names
  names = c("gc_start_time",
            "td_start_time",
            "sample_start_time",
            "sample_end_time",
            "sample_volume_ml",
            "port",
            "start_psi",
            "end_psi")
  
  # read
  df <- read.delim2(file) %>%
    janitor::remove_empty("cols") %>%
    set_names(names) %>%
    as_tibble()
  
  # format dates and column types
  df_clean <- df %>%
    mutate(
      across(
        contains("time"),
        ymd_hms
      ),
      across(
        sample_volume_ml:end_psi,
        as.numeric
      )
    )
  
  return(df_clean)
  
}

