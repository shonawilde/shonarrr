#' Draw horizontal pointrange key glyph in a \code{ggplot} legend
#'
#' @param data Data to be displayed in layer
#' 
#' @param params Additional parameters to the geom and stat
#' 
#' @param size Size
#' 
#' @author Shona Wilde
#' 
#' @export
#' 

draw_key_pointrange_h <- function(data, params, size) {
  grid::grobTree(
    grid::segmentsGrob(0.1, 0.5, 0.9, 0.5,
                 gp = grid::gpar(
                   col = alpha(data$colour, data$alpha),
                   lwd = data$size * .pt,
                   lty = data$linetype,
                   lineend = "butt"
                 ),
                 arrow = params$arrow
    ),
    draw_key_point(transform(data, size = data$size * 4), params)
  )
}



