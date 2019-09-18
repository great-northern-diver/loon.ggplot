#' @rdname ggplot2.loon
#' @export
ggplot2.loon.l_ts <- function(target, ...) {

  plots <- lapply(target,
                  function(p) {
                    ggplot2.loon(p)
                  })
  GGally::ggmatrix(plots,
                   title = loon_title(target),
                   showYAxisPlotLabels = TRUE,
                   yAxisLabels = names(target),
                   nrow = length(plots), ncol = 1)
}
