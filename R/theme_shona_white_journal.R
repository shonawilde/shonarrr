#' A smart, spacy theme for a ggplot
#'
#' Designed to produce publication-quality figures with appropriate text sizes
#' 
#' @param legend_position Location of legend - choices are "top", "bottom", "right", or  "left".
#' 
#' @param grid_type Line type of \code{panel.grid}. An integer (0:8), a name (blank, solid, dashed, dotted, dotdash, longdash, twodash), 
#' or a string with an even number (up to eight) of hexadecimal digits which give the lengths in consecutive positions in the string.
#' If grid_type = "none", no grid will be displayed
#' 
#' @param text_size Size of plot text.
#' 
#' @author Shona Wilde
#' 
#' @export

theme_shona_white_journal <- function(legend_position = "right", grid_type = "dashed", strip = T, text_size = 9) {
  
  # switch grid type
  if (grid_type == "none") {
    
    panel_grid = element_blank()
    
  }
  
  else {
    panel_grid = element_line(linetype = grid_type)
  }
  
  
  if (strip) {
    strip_background = element_rect(fill = NA, color = "black") 
    panel.background = element_rect(fill = NA, color = "black")
  }
  
  else {
    strip_background = element_blank()
    panel.background = element_blank()
  }
    
    # build theme
    theme_minimal() +
      theme(
        plot.background = element_rect(colour = NA),
        plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
        axis.line = element_line(),
        strip.background = strip_background,   
        panel.background = strip_background,   
        text = element_text(size = text_size),
        axis.text = element_text(size = text_size),
        axis.ticks = element_line(),
        strip.text = element_text(size = text_size),
        axis.title.y = element_text(
          size = text_size,
          margin = margin(t = 0, r = 5, b = 0, l = 0)
        ),
        axis.title.x = element_text(
          size = text_size,
          margin = margin(t = 5, r = 0, b = 0, l = 0)
        ),
        panel.spacing = unit(1, "lines"),
        legend.margin=margin(c(1,1,1,1)),
        legend.position = legend_position,
        panel.grid.minor = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(size = text_size),
        legend.title = element_text(size = text_size),
        panel.grid = panel_grid
      ) +
      ggeasy::easy_all_text_colour(colour = "black")
    
}

  

