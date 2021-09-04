ggCompound <- function(plots,
                       setBackground = TRUE,
                       fill.bg = loon::l_getOption("background"),
                       colour.bg = loon::l_getOption("background")) {

  if(setBackground) {
    do.call(patchwork::wrap_plots, plots) &
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = fill.bg,
                                                colour = colour.bg)
      )
  } else {
    do.call(patchwork::wrap_plots, plots)
  }
}
