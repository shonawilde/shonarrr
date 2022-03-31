#' Function to summarise the output from \code{\link[quantreg]{rq}} in a tidy format
#'
#' @param model Model object to summarise.
#'
#' @param se Method to use for calculating standard errors. One of NULL, "nid", "rank", "iid", "ker", or "boot".
#'
#' @return Tibble containing model results.
#'
#' @author Shona Wilde
#'
#' @export


tidy_rq_output <- function(model, se = NULL){

  suppressWarnings(

    summary <- summary(model, se = se)

  )

  # if single model object create list
  if (class(summary) == "summary.rq") {

    summary <- list(summary)

  }

  # do
  df <- map_dfr(
    summary,
    ~tidy_rq_output_worker(
      summary = .x
    )
  )

  return(df)

}


tidy_rq_output_worker <- function(summary){


  df_model <- summary$coefficients %>%
    as_tibble(rownames = "term") %>%
    clean_names() %>%
    rename(
      p_value = pr_t,
    ) %>%
    mutate(
      term = str_replace(term, "(\\(Intercept)\\)", "intercept"),
      quantile = summary$tau,
      quantile_name = paste0("Q", quantile*100)
    )


  return(df_model)

}

