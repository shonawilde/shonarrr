#' Function to clean variable names in data frame
#'
#' @param df data 
#' 
#' @param lookup_table A look-up table to be used for cleaning names
#' 
#' @param join_by Joining key in look-up table
#' 
#' @usage clean_variable_names(lookup, join_by = c("variable" = "variable_lookup"))
#' 
#' @author Shona Wilde
#' 
#' @return Tibble
#' 
#' @export

clean_variable_names <- function(df, lookup_table, join_by = "variable") {
  
  # clean names first
  df <- df %>% 
    janitor::clean_names()
  
  names <- names(df)
  df_names <- tibble::enframe(names,
                      value = "variable")
  
  # join to lookup table
  names_join <- df_names %>% 
    left_join(lookup_table,
              by = join_by)
  
  # get cleaned names
  names_clean <- names_join %>% 
    mutate(variable = if_else(!is.na(variable_clean),
                              variable_clean, 
                              variable)) %>% 
    select(-variable_clean) %>% 
    pull(variable)
  
  # assign new names
  df_clean <- df %>% 
    purrr::set_names(names_clean)
  
  return(df_clean)
  
  
}


#' Function to clean variable strings contained in a column
#'
#' @param x vector of names to clean
#' 
#' @param lookup_table A look-up table to be used for cleaning names
#' 
#' @param join_by Joining key in look-up table
#' 
#' @usage df %>% 
#' mutate(variable = clean_variable_column(variable,
#'  join_by = c("variable" = "variable_lookup"))
#' 
#' @author Shona Wilde
#' 
#' @return Tibble
#' 
#' @rdname clean_variable_names
#' 
#' @export

clean_variable_column <- function(x, lookup_table, join_by = "variable") {
  
  x <- tibble::enframe(x, 
                       value = "variable") %>% 
    mutate(variable = stringr::str_to_lower(variable))
  
  df <- x %>% 
    left_join(lookup_table,
              by = join_by)
  
  names_clean <- df %>% 
    mutate(variable = threadr::str_to_underscore(variable)) %>% 
    mutate(variable = if_else(!is.na(variable_clean),
                              variable_clean, 
                              variable)) %>% 
    select(-variable_clean) %>% 
    pull(variable)
  
  return(names_clean)
  
  
}




