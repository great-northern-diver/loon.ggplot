#' @rdname loon2ggplot
#' @export
loon2ggplot.l_compound <- function(target, ...) {

  locations <- g_getLocations(target)

  GGally::ggmatrix(plots = g_getPlots(target),
                   nrow = locations$nrow,
                   ncol = locations$ncol)
}
