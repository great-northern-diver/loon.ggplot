#' @export
#' @import ggmulti
#' @rdname loon2ggplot
loon2ggplot.l_layer_scatterplot <- function(target, asAes = TRUE, selectedOnTop = TRUE, ...) {

  widget <- loon::l_create_handle(attr(target, "widget"))
  swapAxes <- widget["swapAxes"]

  # layer scatterplot shares the same data set with the ggplot model layer
  ggObj <- list(...)$ggObj
  data <- ggObj$data

  if (!any(data$active)) return(ggObj)

  # No active points in scatterplot
  displayOrder <- if(selectedOnTop) {
    get_model_display_order(widget)
  } else {
    seq(widget['n'])
  }

  active <- data$active[displayOrder]
  selected <- data$selected[displayOrder][active]

  s_a <- list(
    x = if(swapAxes) data$y[displayOrder][active] else data$x[displayOrder][active],
    y = if(swapAxes) data$x[displayOrder][active] else data$y[displayOrder][active],
    glyph = data$glyph[displayOrder][active],
    color = get_display_color(data$color[displayOrder][active], selected),
    size = data$size[displayOrder][active],
    index = displayOrder[active]
  )

  x <- as.numeric(s_a$x)
  y <- as.numeric(s_a$y)
  glyph <- s_a$glyph
  color <- fill <-s_a$color
  size <- s_a$size
  index <- s_a$index

  if(asAes) {
    scatterplotAsAesTRUE(ggObj, widget, x, y, glyph, color, size, index, selectedOnTop)
  } else {
    scatterplotAsAesFALSE(ggObj, widget, x, y, glyph, color, size, index, selectedOnTop)
  }
}
