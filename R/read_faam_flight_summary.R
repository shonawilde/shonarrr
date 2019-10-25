#' Read FAAM flight summary
#'
#' Reads and cleans flight summary from aircraft
#'
#' @param x file
#' 
#' @author Stuart Grange
#' 
#' @return df
#' 
#' @export

read_faam_flight_summary <- function(file, verbose) {
  
  # Message to user
  if (verbose) message(threadr::date_message(), "`", file, "`...")
  
  # Read file as text
  text <- readr::read_lines(file)
  
  # Isolate preamble and clean
  df_preamble <- text[2:(which(text == "") - 1L)] %>% 
    stringr::str_split_fixed(":|No", 2) %>% 
    tibble::as_tibble(.name_repair = "minimal") %>% 
    purrr::set_names(c("variable", "value")) %>% 
    dplyr::mutate_all(stringr::str_trim) %>% 
    mutate(variable = stringr::str_to_lower(variable)) %>% 
    tidyr::spread(variable, value, convert = TRUE) %>% 
    mutate(date = dmy(date, tz = "UTC")) %>% 
    dplyr::rename(flight_no = flight)
  
  # Get variable names in preamble
  variable_names <- text %>% 
    stringr::str_subset("^Time") %>% 
    stringr::str_split("  ") %>% 
    .[[1]] %>% 
    .[. != ""] %>% 
    stringr::str_trim() %>% 
    threadr::str_rm_round_brackets() %>% 
    threadr::str_to_underscore()
  
  # Set a name
  variable_names[2] <- if_else(variable_names[2] == "time", "time_end", variable_names[2])
  
  # Parse tabular data
  suppressWarnings(
    df <- text[(stringr::str_which(text, "^----") + 1L):length(text)] %>% 
      readr::read_fwf(col_positions = readr::fwf_widths(c(7, 7, 19, 17, 30))) %>% 
      purrr::set_names(variable_names) %>% 
      dplyr::mutate(date_start = stringr::str_c(df_preamble$date, " ", time),
             date_start = lubridate::ymd_hms(date_start, tz = "UTC"),
             date_end = stringr::str_c(df_preamble$date, " ", time_end),
             date_end = lubridate::ymd_hms(date_end, tz = "UTC")) %>% 
      dplyr::select(-time,
             -time_end)
  )
  
  # Bind the preamble and tabular data together
  df <- df_preamble %>% 
    dplyr::rename(day = date) %>% 
    dplyr::slice(rep(1:dplyr::n(), each = nrow(df))) %>% 
    bind_cols(df)
  
  return(df)
  
}