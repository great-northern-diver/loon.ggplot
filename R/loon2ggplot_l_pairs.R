#' @rdname loon2ggplot
#' @export
loon2ggplot.l_pairs <- function(target, asAes = TRUE, ...) {

  locations <- g_getLocations(target)

  gm <- GGally::ggmatrix(
    plots = g_getPlots(target, asAes = asAes),
    nrow = locations$nrow,
    ncol = locations$ncol,
    byrow = FALSE,
    showXAxisPlotLabels = FALSE,
    showYAxisPlotLabels = FALSE) +
    theme(plot.background = ggplot2::element_rect(fill = loon::l_getOption("canvas_bg_guides")))
  return(gm)
}
