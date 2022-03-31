#' Function to summarise the output from \code{\link[stats]{lm}} in a tidy format
#'
#' @param model Model object to summarise.
#'
#' @return Tibble containing model results.
#'
#' @author Shona Wilde
#'
#' @export


tidy_lm_output <- function(model){

  suppressWarnings(
    summary <- summary(model)
  )

  df_model <- summary$coefficients %>%
    as_tibble(rownames = "term") %>%
    clean_names() %>%
    rename(
      value = estimate,
      p_value = pr_t,
    ) %>%
    mutate(
      term = str_replace(term, "(\\(Intercept)\\)", "intercept"),
      r_squared = summary$r.squared
    )

  return(df_model)

}
