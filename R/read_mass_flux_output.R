#' Read mass flux output
#'
#' Reads and cleans data files from the MatLab mass flux script
#'
#' @param file_list List containing file names
#' 
#' @author Shona Wilde
#' 
#' @return Tibble
#' 
#' @export


read_mass_flux_output <- function(file_list) {
  
  purrr::map_dfr(file_list,
                 read_mass_flux_output_worker)
}



read_mass_flux_output_worker <- function(file) {

  # read lines and account for rig names with more than one word
  lines <- readr::read_lines(file) %>% 
    str_replace("glen_lyon", "glen-lyon")

  
  # get flight and variable info
  meta_data <- lines[1] %>% 
    stringr::str_remove("# CASE:") %>% 
    stringr::str_split("_") %>% 
    purrr::map(stringr::str_squish) %>% 
    purrr::reduce(dplyr::bind_rows) %>% 
    purrr::set_names(c("flight_no", "plume", "variable")) %>% 
    tibble::enframe() %>% 
    tidyr::pivot_wider()
  
  # get calculation results and parameters
  data <- lines[7:11] %>% 
    map(stringr::str_squish) %>% 
    map(stringr::str_split, " ")%>% 
    purrr::flatten() %>% 
    purrr::reduce(c) %>% 
    as.numeric() %>% 
    purrr::set_names(c("flux_lower",
                "flux_mean",
                "flux_upper",
                "lat_lon_min", 
                "lat_lon_max",
                "track_distance",
                "ymin",
                "ymax",
                "smooth_param",
                "background"
    )) %>% 
    tibble::enframe() %>% 
    tidyr::pivot_wider()
  
  # combine and re-format plume names
  # convert to tonnes per year
  df <- meta_data %>% 
    dplyr::bind_cols(data) %>%   
    dplyr::mutate(across(contains("flux"),
                         kgs_to_tonne_day,
                  .names = "{.col}_tonnes_day"),
           .after = "flux_upper") %>%
    dplyr::mutate(plume = shonarrr::str_tidy(plume)) %>% 
    tibble::as_tibble()
  
  return(df)
  
}





