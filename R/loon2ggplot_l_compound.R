#' @rdname loon2ggplot
#' @export
loon2ggplot.l_compound <- function(target, asAes = TRUE, selectedOnTop = TRUE,
                                   showNearestColor = FALSE, ...) {

  locations <- g_getLocations(target)

  GGally::ggmatrix(plots = g_getPlots(target, asAes = asAes,
                                      selectedOnTop = selectedOnTop,
                                      showNearestColor = showNearestColor),
                   nrow = locations$nrow,
                   ncol = locations$ncol,
                   byrow = FALSE)
}
