#' @rdname loon2ggplot
#' @export
loon2ggplot.l_ts <- function(target, asAes = TRUE, selectedOnTop = TRUE, ...) {

  locations <- g_getLocations(target)

  GGally::ggmatrix(plots = g_getPlots(target, asAes = asAes, selectedOnTop = selectedOnTop),
                   title = loon_title(target),
                   showYAxisPlotLabels = TRUE,
                   yAxisLabels = names(target),
                   nrow = locations$nrow,
                   ncol = locations$ncol)
}
