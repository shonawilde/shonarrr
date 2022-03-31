#' Function to calculate acceleration from speed and date variables
#'
#' @param speed Speed in \eqn{m~s^{-1}}.
#'
#' @param date Date and time variable.
#'
#' @return Value
#'
#' @author Shona Wilde
#'
#' @export


# define custom function
calculate_acceleration <- function(speed, date) {

  if (!lubridate::is.POSIXt(date)) {
    stop("`date` must be a POSIXt object.", call. = FALSE)
  }

  diff_s <- c(NA, diff(speed))
  diff_t = c(NA, diff(date))

  a <- diff_s/diff_t

  return(a)

}


