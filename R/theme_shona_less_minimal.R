#' Theme Less Minimal Big
#'
#' theme_minimal but bigger and less minimal
#' 
#' @author Shona Wilde
#' 
#' @export
#' 



theme_shona_less_minimal <- function() {
  theme_minimal() +
    theme(plot.background = element_rect(colour = NA))+
    theme(plot.margin = margin(1, 1, 1, 1, "cm"),
          axis.line = element_line(colour = "black")) +                     # facet border
    theme(strip.background= element_rect(fill = NA),
          panel.background = element_rect(fill = NA),
          axis.ticks = element_line()) +               # facet title background
    theme(text = element_text(colour = "black", size = 15)) +
    theme(axis.text = element_text(colour = "black", size = 15)) +
    theme(plot.title = element_text(colour = "black")) +
    theme(strip.text.x = element_text(colour = "black", face = "bold")) +
    theme(axis.title.y = element_text(size = 20,
                                      margin = margin(t = 0, r = 20, b = 0, l = 0))) +
    theme(axis.title.x = element_text(size = 20,
                                      margin = margin(t = 20, r = 0, b = 0, l = 0))) +
    theme(panel.spacing = unit(1, "lines")) +
    theme(panel.grid.minor = element_blank()) +
    theme(legend.background = element_blank()) +
    theme(legend.text = element_text(size = 20)) +
    theme(legend.title = element_text(size = 20)) +
    ggeasy::easy_all_text_colour(colour = "black")
  
  
}

