#' Plot colour palette
#'  
#' Function to visualize a colour palette
#'
#' @param cols Vector of colours
#' 
#' @author Shona Wilde
#'
#' @export


plot_colours <- function(cols) {
  
  qplot(
    x = 1:length(cols),
    y = 1, 
    fill = factor(1:length(cols)),
    geom = "tile"
  ) +
    scale_fill_manual(values = cols) +
    theme_void() +
    theme(
      legend.position = "none"
    )
  
} 




