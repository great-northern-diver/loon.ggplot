#' @export
#' @importFrom ggmulti geom_polygon_glyph geom_serialaxes_glyph geom_image_glyph coord_serialaxes
#' @rdname loon2ggplot
loon2ggplot.l_layer_scatterplot <- function(target, asAes = TRUE, ...) {

  widget <- loon::l_create_handle(attr(target, "widget"))
  states <- get_layer_states(widget)
  swapAxes <- widget["swapAxes"]

  ggObj <- list(...)$ggObj

  if (!any(states$active)) return(ggObj)

  # No active points in scatterplot
  display_order <- get_model_display_order(widget)

  active <- states$active[display_order]
  selected <- states$selected[display_order][active]

  s_a <- list(
    x = if(swapAxes) states$y[display_order][active] else states$x[display_order][active],
    y = if(swapAxes) states$x[display_order][active] else states$y[display_order][active],
    glyph = states$glyph[display_order][active],
    color = get_display_color(states$color[display_order][active], selected),
    size = states$size[display_order][active],
    index = display_order[active]
  )

  x <- as.numeric(s_a$x)
  y <- as.numeric(s_a$y)
  glyph <- s_a$glyph
  color <- fill <- s_a$color
  size <- s_a$size
  index <- s_a$index

  if(asAes) {
    scatterplotAsAesTRUE(ggObj, widget, x, y, glyph, color, size, index)
  } else {
    scatterplotAsAesFALSE(ggObj, widget, x, y, glyph, color, size, index)
  }
}

selection_color_labels <- function(x, name = "select") {

  select_color <- loon::l_getOption("select-color")
  if(select_color %in% x || as_hex6color(select_color) %in% x) {
    x[x == select_color] <- name
  }

  x
}
