#' NA in row
#'
#' Adds identifier based on whether rows contain NA in any column
#'
#' @param df data frame
#' 
#' @param name name of new column
#' 
#' @author Shona Wilde
#' 
#' @return tibble
#' 
#' @export

na_in_row <- function(df, name = "na_in_row") {
  
  
  # add row id
  df <- df %>% 
    rowid_to_column()
  
  # find rows with NA in
  na_rows <- df %>% 
    filter_all(
      any_vars(is.na(.))
    ) %>% 
    pull(rowid)
  
  # add column to df
  df_na <- df %>% 
    mutate(
      !! (name) := if_else(
        rowid %in% na_rows, T,  F
      )
    )
  
  return(df_na)
  
}
