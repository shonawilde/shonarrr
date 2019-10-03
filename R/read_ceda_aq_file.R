#' Read CEDA Air Quality
#'
#' Reads and cleans CEDA AQ files
#'
#' @param file path to file.
#' 
#' @param longer Should the return be reshaped to be in a "tidy data" format? 
#' This operation is a bit slow. 
#' 
#' @param verbose Should the function give messages on what file is being 
#' processed? 
#' 
#' @author Shona Wilde
#' 
#' @return Data frame/tibble
#' 
#' @export
read_ceda_aq_file <- function(file, longer = FALSE, verbose = FALSE) {
  
  # Message to user
  if (verbose) message("`", file, "`...")
  
  # Read entire file as text
  text <- readr::read_lines(file)
  
  # Isolate preamble, not used here
  # df_preamble <- text[2:(which(text %in% "4 - bad data"))]
  
  # get date
  date <- text[stringr::str_which(text, "Time in minutes")] %>% 
    stringr::str_split_fixed("since", 2) %>% 
    dplyr::nth(2) %>% 
    stringr::str_remove(" ") %>% 
    lubridate::ymd_hm(tz = "UTC")
  
  # get variable names
  index_start_names <- stringr::str_which(text, "^99")[1] + 1L
  index_end_names <- stringr::str_which(text, "precision")[1] - 2L
  
  variable_names <- text[index_start_names:index_end_names] %>% 
    stringr::str_to_lower() %>% 
    threadr::str_rm_round_brackets() %>% 
    stringr::str_replace(" ", "_") %>% 
    c("mins", .)
  
  # Find where the table is located
  index_start_table <- stringr::str_which(text, "4 - bad data") + 3L
  index_end_table <- length(text)
  
  # For when the file contains no data
  if (index_end_table <= index_start_table) {
    # Raise warning
    warning("File contains no data table.", call. = FALSE)
    return(tibble())
  }
  
  # Read tablular data
  df <- text[index_start_table:index_end_table] %>% 
    readr::read_table2(
      col_names = variable_names, 
      na = "9999.99",
      guess_max = 50000,
      progress = FALSE
    ) %>% 
    mutate(date = date + mins * 60) %>% 
    select(date, 
           everything(), 
           -mins)
  
  # Check for duplicate dates
  if (!anyDuplicated(df$date) == 0L) {
    
    # Raise warning
    warning("Duplicate dates detected.", call. = FALSE)
    
    if (longer) {
      warning("Observations removed for reshaping.", call. = FALSE)
      df <- distinct(df, date, .keep_all = TRUE)
    }
    
  }
  
  # Make tidy data
  if (longer) {
    df <- df %>% 
      tidyr::pivot_longer(-date, names_to = "variable") %>% 
      mutate(
        variable_type = if_else(
          stringr::str_detect(variable, "flag$"), "flag", "value"
        ),
        variable = stringr::str_remove(variable, "_flag$")
      ) %>% 
      tidyr::pivot_wider(names_from = variable_type, values_from = value) %>% 
      mutate(flag = as.integer(flag))
  }
  
  return(df)
  
}
