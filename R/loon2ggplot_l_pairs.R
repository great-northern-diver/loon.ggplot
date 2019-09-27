#' @rdname loon2ggplot
#' @export
#' @examples
#' p <- l_pairs(iris, showHistograms = TRUE)
#' g <- loon2ggplot(p)
#' g + ggtitle("Iris pairs plot")
loon2ggplot.l_pairs <- function(target, ...) {

  locations <- g_getLocations(target)

  gm <- GGally::ggmatrix(
    plots = g_getPlots(target),
    nrow = locations$nrow,
    ncol = locations$ncol,
    byrow = FALSE,
    showXAxisPlotLabels = FALSE,
    showYAxisPlotLabels = FALSE) +
    theme(plot.background = ggplot2::element_rect(fill = loon::l_getOption("canvas_bg_guides")))
  return(gm)
}
