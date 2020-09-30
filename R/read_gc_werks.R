#' Read GCwerks
#'
#' Reads and cleans raw data files from the GCwerks program
#'
#' @param file_list List containing file names
#' 
#' @param verbose Should a message to the user be displayed?
#' 
#' @author Shona Wilde
#' 
#' @return Tibble
#' 
#' @export

read_gc_werks <- function(file_list, verbose = T) {
  
  purrr::map_dfr(file_list,
          read_gc_works_worker,
          verbose = verbose)
}

read_gc_works_worker <- function(file, verbose) {
  
  # Message to user
  if (verbose) message(threadr::date_message(), "`", file, "`...")
  
  # read text
  text <- readr::read_lines(file)
  
  # get names
  compound_names <- text[2] %>% 
    stringr::str_squish() %>% 
    stringr::str_split(" ") %>%
    unlist() %>% 
    stringi::stri_remove_empty()
  
  # get extra information about names
  other_names <- text[3] %>%
    stringr::str_squish() %>% 
    stringr::str_split(" ") %>% 
    unlist() %>% 
    stringi::stri_remove_empty()
  
  # create single vector for names
  col_names <- compound_names %>% 
    purrr::map2(other_names, str_c, sep = "_") %>% 
    stringr::str_to_lower() %>% 
    stringr::str_remove("^[-_]*") %>% # removes "-_" at start of strings
    stringr::str_replace_all("-", "_") %>% 
    unlist() 

  # read tabular data and set columns names
  df <- text %>% 
    readr::read_table2(skip = 3, col_names = F, na = "nan", guess_max = 999999) %>% 
    dplyr::select(-last_col()) %>% # empty for some reason...
    purrr::set_names(col_names)
  
  # format date, clean and filter for area columns
  df_clean <- df %>% 
    dplyr::mutate(gc_start_date = paste0(date, time) %>% ymd_hm()) %>% 
    dplyr::select(gc_start_date,
                 # type,
                #  sample,
                #  standard,
                #  port,
                  contains("area")) %>% 
    dplyr::rename_with(str_remove, everything(), pattern = "_area") %>% 
    as_tibble() %>% 
    clean_names() %>% 
    unique()
  
  return(df_clean)
  
}



