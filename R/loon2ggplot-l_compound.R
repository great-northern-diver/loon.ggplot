#' @rdname loon2ggplot
#' @import patchwork
#' @export
loon2ggplot.l_compound <- function(target, asAes = TRUE, selectedOnTop = TRUE,
                                   showNearestColor = FALSE, ...) {

  # locations <- g_getLocations(target)
  #
  # GGally::ggmatrix(plots = g_getPlots(target, asAes = asAes,
  #                                     selectedOnTop = selectedOnTop,
  #                                     showNearestColor = showNearestColor),
  #                  nrow = locations$nrow,
  #                  ncol = locations$ncol,
  #                  byrow = FALSE)

  locations <- loon::l_getLocations(target)
  layout_matrix <- locations$layout_matrix
  plots <- lapply(target,
                  function(x) {
                    loon2ggplot(x, asAes = asAes, selectedOnTop = selectedOnTop,
                                showNearestColor = showNearestColor, ...)
                  })

  positions <- layout_matrix2positions(layout_matrix, n = length(plots))

  plots$design <- do.call(c,
                          lapply(seq(nrow(positions)),
                                 function(i) {
                                   do.call(patchwork::area,
                                           positions[i, ])
                                 }))
  plots$heights <- locations$heights
  plots$widths <- locations$widths
  ggCompound(plots)
}
