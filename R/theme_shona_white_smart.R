#' A smart, spacy theme for a ggplot
#'
#' Designed to produce publication-quality figures
#' 
#' @param legend_position Location of legend - choices are "top", "bottom", "right", or  "left".
#' 
#' @param grid_type Line type of \code{panel.grid}. An integer (0:8), a name (blank, solid, dashed, dotted, dotdash, longdash, twodash), 
#' or a string with an even number (up to eight) of hexadecimal digits which give the lengths in consecutive positions in the string.
#' 
#' @author Shona Wilde
#' 
#' @export

theme_shona_white_smart <-  function(legend_position = "right", grid_type = "dashed") {
  theme_minimal() +
    theme(
      panel.background = element_blank(),
      plot.background = element_rect(colour = NA),
      plot.margin = margin(1, 1, 1, 1, "cm"),
      axis.line = element_line(),
      strip.background = element_blank(),   
      text = element_text(size = 15),
      axis.text = element_text(size = 13),
      axis.ticks = element_line(),
      strip.text = element_text(size = 15),
      axis.title.y = element_text(
        size = 15,
        margin = margin(t = 0, r = 20, b = 0, l = 0)
      ),
      axis.title.x = element_text(
        size = 15,
        margin = margin(t = 20, r = 0, b = 0, l = 0)
      ),
      panel.spacing = unit(1, "lines"),
      legend.position = legend_position,
      panel.grid.minor = element_blank(),
      panel.grid = element_line(linetype = grid_type),
      legend.background = element_blank(),
      legend.text = element_text(size = 15),
      legend.title = element_text(size = 15)
    ) +
    ggeasy::easy_all_text_colour(colour = "black")
  
}
