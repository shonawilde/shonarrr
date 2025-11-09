#' Function to give unique identifiers to groups within data
#'
#' @param df Input data frame or tibble
#' 
#' @param type Type of identifier. Either 'numbers' or 'letters'
#' 
#' @param as_factor Should the new column be a factor?
#' 
#' @return Tibble
#' 
#' @author Shona Wilde 
#' 
#' @export

add_group_identifier <- function(df, type = "numbers", name = "group", as_factor = F) {
  
  stopifnot(
    "Type must be one of 'numbers' or 'letters'" = type %in% c("numbers", "letters")
  )
  
  df_group <- df %>% 
    mutate(
      group = cur_group_id()
      )
  
  if (type == "letters") {
    
    letters <- extend(LETTERS)
    
    df_group$group <- map_chr(
      df_group$group,
      letters
    )
  }
  
  if (as_factor) {
    df_group$group <- as.factor(df_group$group)
  }
  
  # rename
  df_group_rename <- df_group %>% 
    relocate(
      group, .before = 1
    ) %>%
    rename(
      {{name}} := group
    )
  
  return(as_tibble(df_group_rename))
  
  
}


extend <- function(alphabet) function(i) {
  base10toA <- function(n, A) {
    stopifnot(n >= 0L)
    N <- length(A)
    j <- n %/% N 
    if (j == 0L) A[n + 1L] else paste0(Recall(j - 1L, A), A[n %% N + 1L])
  }   
  vapply(i-1L, base10toA, character(1L), alphabet)
}

