ggplot.loon.l_layer_histogram <- function(target, ...) {

  widget <- loon::l_create_handle(attr(target, "widget"))
  states <- loon:::get_layer_states(widget, native_unit = FALSE)

  ggObj <- list(...)$ggObj

  # No active points in scatterplot
  active <- states$active
  if(any(active)) {

    display_order <- loon:::get_model_display_order(widget)
    selected <- states$selected[display_order][active]

    s_a <- list(
      x = states$x[display_order][active],
      y = as.numeric(selected),
      fill = loon:::get_display_color(states$color[display_order][active], selected)
    )
browser()
    ggObj <- ggObj +
      geom_hist(
        data = data.frame(
          x = s_a$x,
          y = s_a$y
        ),
        mapping = aes(x = x, y = y),
        fill = s_a$fill,
        colour = states$colorOutline,
        yshows = states$yshows,
        showStackedColors = states$showStackedColors,
        showOutlines = states$showOutlines,
        colourFill = states$colorFill, # colourFill is default colour
        origin = states$origin,
        binwidth = states$binwidth
      )
  }
  ggObj
}
