#' @export
ggplot2::aes

GeomFreqpoly <- ggplot2::ggproto("GeomFreqpoly", GeomPath)

#' @export
#' @inherit ggplot2::geom_freqpoly
geom_freqpoly <- function (mapping = NULL, data = NULL, stat = "bin", position = "identity",
                           ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  params <- list(na.rm = na.rm, ...)
  if (identical(stat, "bin")) {
    params$pad <- TRUE
  }
  ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = GeomFreqpoly,
                 position = position, show.legend = show.legend, inherit.aes = inherit.aes,
                 params = params)
}
