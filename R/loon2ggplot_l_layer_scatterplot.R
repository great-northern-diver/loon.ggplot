#' @export
#' @import ggmulti
#' @rdname loon2ggplot
loon2ggplot.l_layer_scatterplot <- function(target, asAes = TRUE, ...) {

  widget <- loon::l_create_handle(attr(target, "widget"))
  swapAxes <- widget["swapAxes"]

  # layer scatterplot shares the same data set with the ggplot model layer
  ggObj <- list(...)$ggObj
  data <- ggObj$data

  if (!any(data$active)) return(ggObj)

  # No active points in scatterplot
  display_order <- get_model_display_order(widget)

  active <- data$active[display_order]
  selected <- data$selected[display_order][active]

  s_a <- list(
    x = if(swapAxes) data$y[display_order][active] else data$x[display_order][active],
    y = if(swapAxes) data$x[display_order][active] else data$y[display_order][active],
    glyph = data$glyph[display_order][active],
    color = get_display_color(data$color[display_order][active], selected),
    size = data$size[display_order][active],
    index = display_order[active]
  )

  x <- as.numeric(s_a$x)
  y <- as.numeric(s_a$y)
  glyph <- s_a$glyph
  color <- fill <-s_a$color
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
