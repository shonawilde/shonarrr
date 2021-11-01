#' Theme Shona Void
#'
#' Void theme designed for use with maps
#' 
#' @author Shona Wilde
#' 
#' @export
#' 



theme_shona_void <- function(legend_position = "top") {
  
  theme_minimal() +
    theme(
      plot.background = element_rect(fill = "white", colour = NA),
      legend.position = legend_position,
      plot.margin = margin(1, 1, 1, 1, "cm"),
      text = element_text(size = 15),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      strip.text = element_text(size = 15),
      legend.title = element_text(size = 13,
                                  vjust = 1),
      legend.text = element_text(size = 13)
    ) +
    ggeasy::easy_all_text_colour(colour = "black")
  
  
}
