#' Theme Less Minimal Big
#'
#' theme_minimal but bigger and less minimal
#' 
#' @author Shona Wilde
#' 
#' @export
#' 



theme_shona_less_minimal <- function(legend_position = "right", grid_type = "dashed") {
  theme_minimal() +
    theme(plot.background = element_rect(colour = NA))+
    theme(plot.margin = margin(1, 1, 1, 1, "cm"),
          axis.line = element_line(colour = "black")) +
    theme(strip.background= element_rect(fill = NA),
          panel.background = element_rect(fill = NA),
          axis.ticks = element_line()) +               
    theme(text = element_text(colour = "black", size = 12)) +
    theme(axis.text = element_text(colour = "black", size = 12)) +
    theme(plot.title = element_text(colour = "black")) +
    theme(strip.text = element_text(colour = "black", face = "bold", size = 12)) +
    theme(axis.title.y = element_text(size = 12,
                                      margin = margin(t = 0, r = 20, b = 0, l = 0))) +
    theme(axis.title.x = element_text(size = 12,
                                      margin = margin(t = 20, r = 0, b = 0, l = 0))) +
    theme(panel.spacing = unit(1, "lines")) +
    theme(panel.grid = element_line(linetype = grid_type))+
    theme(panel.grid.minor = element_blank()) +
    theme(legend.background = element_blank()) +
    theme(legend.text = element_text(size = 12)) +
    theme(legend.title = element_text(size = 12)) +
    theme(legend.position = legend_position) +
    ggeasy::easy_all_text_colour(colour = "black")
}

