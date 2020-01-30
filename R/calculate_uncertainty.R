
#' Calculate uncertainty
#'
#' @param x Value for which uncertainty should be calculated
#' 
#' @param blank_value Concentration of variable in GC blanks in ppb
#' 
#' @param cal_rel_error Relative error from calibration standards calculated as the
#' calibration error in ppb divided by the concentration in ppb
#' 
#' @param detection_limit Limit of detection in ppb
#' 
#' @param work_std_precision Precision of woking standard calculated by dividing the standard deviation of the working standard 
#'  by the mean concentration of the working standard in ppb
#'
#' @param blank_error Relative error from blank values calculated using \code{calc_relative_error}
#' 
#' @author Shona Wilde
#' 
#' @return value
#' 
#' @export

calc_relative_error <- function(x, blank_value){
  
  # avoid dividing by zero
  blank_value[blank_value == 0] <- NA
  
  # get blank ratio
  blank_ratio <- x/blank_value
  
  relative_error <- 0.1989 * exp(-0.348 * blank_ratio)
  
  return(relative_error)
  
  
  # lookup table for blank ratios 
  # exponential equation from table
  df_blank_ratio <- tribble(
    ~blank_ratio, ~percent_error,
    0, 20,
    2, 10,
    4, 5,
    6, 2.5, 
    8, 1.2,
    10, 0.6,
    12, 0.3,
    14, 0.15,
    16, 0.08
  ) %>% 
    mutate(relative_error = percent_error/100)
  
}


#' @export
calc_uncertainty <- function(x, cal_rel_error, detection_limit, work_std_precision, blank_error) {
  
  #blank_error[is.na(blank_error)] <- 0 
  
  uncertainty <- 2 * sqrt(((cal_rel_error*x)^2) + ((1/3)*detection_limit + work_std_precision*x)^2 + (blank_error*x)^2)
  
  return(uncertainty)
  
}

