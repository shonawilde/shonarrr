#' Theme Minimal Big
#'
#' theme_minimal but bigger
#' 
#' @author Shona Wilde
#' 
#' @export

theme_shona_white_big <- function(legend_position = "right") {
  theme_minimal() +
    theme(panel.background = element_blank()) +
    theme(plot.background = element_rect(colour = NA))+
    theme(plot.margin = margin(1, 1, 1, 1, "cm"),
          axis.line = element_line()) + 
    theme(strip.background = element_blank()) +             
    theme(text = element_text(colour = "gray13", size = 15)) +
    theme(axis.text = element_text(colour = "gray13", size = 15)) +
    theme(axis.ticks = element_line()) +
    theme(plot.title = element_text(colour = "gray13")) +
    theme(strip.text.x = element_text(colour = "gray13", face = "bold")) +
    theme(axis.title.y = element_text(size = 20,
                                      margin = margin(t = 0, r = 20, b = 0, l = 0))) +
    theme(axis.title.x = element_text(size = 20,
                                      margin = margin(t = 20, r = 0, b = 0, l = 0))) +
    theme(panel.spacing = unit(1, "lines")) +
    theme(legend.position = legend_position) +
    theme(panel.grid.minor = element_blank()) +
    theme(legend.background = element_blank()) +
    theme(legend.text = element_text(size = 15)) +
    theme(legend.title = element_text(size = 15)) +
    ggeasy::easy_all_text_colour(colour = "black")
  
}
