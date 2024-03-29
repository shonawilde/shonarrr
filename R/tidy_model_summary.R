#' Tidy model summary 
#'
#' Combines useful model summary functions \code{broom::glance} and \code{broom::tidy} to produce a single tidy summary 
#' for linear models
#'
#' @param model Model for summarising
#' 
#' @author Shona Wilde
#' 
#' @return Tibble
#' 
#' @export


tidy_model_summary <- function(model) {
  
  lifecycle::deprecate_warn("1.0.0", "shonarrr::tidy_model_summary()", "shonarrr::tidy_rma_output()")
  
  
  tidy <- broom::tidy(model) %>% 
    janitor::clean_names() %>% 
    mutate(across(c(term), str_to_lower)) %>% 
    pivot_wider(id_cols = method,
                names_from = "term",
                values_from = c("estimate", "conf_low", "conf_high")) %>% 
    rename_with(str_remove, starts_with("estimate"), "estimate_")
  
  
  glance <- broom::glance(model) %>% 
    janitor::clean_names()

  
  model_summary <- tidy %>% 
    bind_cols(glance) %>% 
    select(method, slope, intercept, r_squared, everything()) %>% 
    as_tibble()
  
  return(model_summary)

  }



