#' @rdname loon2ggplot
#' @export
loon2ggplot.l_compound <- function(target, asAes = TRUE, ...) {

  locations <- g_getLocations(target)

  GGally::ggmatrix(plots = g_getPlots(target, asAes = asAes),
                   nrow = locations$nrow,
                   ncol = locations$ncol,
                   byrow = FALSE)
}
