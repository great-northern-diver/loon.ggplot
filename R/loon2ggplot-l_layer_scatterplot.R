#' @export
#' @import ggmulti
#' @rdname loon2ggplot
loon2ggplot.l_layer_scatterplot <- function(target, asAes = TRUE, selectedOnTop = TRUE,
                                            showNearestColor = FALSE, ...) {

  widget <- loon::l_create_handle(attr(target, "widget"))
  ggObj <- list(...)$ggObj
  n <- widget['n']
  if(n == 0) return(ggObj)

  swapAxes <- widget["swapAxes"]

  states <- get_layer_states(widget, native_unit = FALSE)
  states$color <- l_colorName(states$color, error = FALSE,
                              precise = !showNearestColor)
  if (!any(states$active)) return(ggObj)

  # No active points in scatterplot
  displayOrder <- if(selectedOnTop) {
    get_model_display_order(widget)
  } else {
    seq(n)
  }

  active <- states$active[displayOrder]
  selected <- states$selected[displayOrder][active]

  s_a <- list(
    x = if(swapAxes) states$y[displayOrder][active] else states$x[displayOrder][active],
    y = if(swapAxes) states$x[displayOrder][active] else states$y[displayOrder][active],
    glyph = states$glyph[displayOrder][active],
    color = get_display_color(states$color[displayOrder][active], selected),
    size = states$size[displayOrder][active],
    index = displayOrder[active]
  )

  x <- as.numeric(s_a$x)
  y <- as.numeric(s_a$y)
  glyph <- s_a$glyph
  color <- fill <- s_a$color
  size <- s_a$size
  index <- s_a$index

  if(asAes) {
    scatterplotAsAesTRUE(ggObj, widget, x, y, glyph, color, size, index, selectedOnTop)
  } else {
    scatterplotAsAesFALSE(ggObj, widget, x, y, glyph, color, size, index, selectedOnTop)
  }
}
