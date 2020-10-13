#' @export
#' @inherit ggmulti::geom_image_glyph
geom_imageGlyph <- function(mapping = NULL, data = NULL, stat = 'identity',
                            position = 'identity', ...,
                            images, width = 4, height = 3, na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  rlang::warn(
    "Function `geom_imageGlyph` is deprecated, please use function `geom_image_glyph` in package 'ggmulti'"
  )

  if(missing(images)) {
    images <- NULL
  }

  ggmulti::geom_image_glyph(
    mapping = mapping, data = data, stat = stat,
    position = position, ...,
    images = images, imagewidth = width, imageheight = height, units = "cm",
    na.rm = na.rm, show.legend = show.legend,
    inherit.aes = inherit.aes
  )
}

#' @export
#' @inherit ggmulti::geom_polygon_glyph
geom_polygonGlyph <- function(mapping = NULL, data = NULL, stat = 'identity',
                              position = 'identity', ...,
                              polygon_x, polygon_y, showArea = TRUE, linewidth = 1,
                              na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE) {
  rlang::warn(
    "Function `geom_polygonGlyph` is deprecated, please use function `geom_polygon_glyph` in package 'ggmulti'"
  )

  if(missing(polygon_x) || missing(polygon_y)) {
    polygon_x <- NULL
    polygon_y <- NULL
  }

  ggmulti::geom_polygon_glyph(
    mapping = mapping, data = data, stat = stat,
    position = position, ...,
    polygon_x = polygon_x, polygon_y = polygon_y,
    linewidth = linewidth,
    na.rm = na.rm, show.legend = show.legend,
    inherit.aes = inherit.aes
  )
}

#' @export
#' @inherit ggmulti::geom_serialaxes_glyph
geom_serialAxesGlyph <- function(mapping = NULL, data = NULL, stat = 'identity',
                                 position = 'identity', ...,
                                 serialAxesData, sequence = NULL, linewidth = 1,
                                 scaling = c('variable', 'data', 'observation', 'none'),
                                 axesLayout = c("parallel", "radial"),
                                 showAxes = FALSE, showArea = FALSE,  showEnclosing = FALSE,
                                 axesColor = "black", bboxColor = 'black',na.rm = FALSE, show.legend = NA,
                                 inherit.aes = TRUE) {
  rlang::warn(
    "Function `geom_polygonGlyph` is deprecated, please use function `geom_polygon_glyph` in package 'ggmulti'"
  )

  if(missing(serialAxesData)) {
    serialAxesData <- NULL
  }

  ggmulti::geom_serialaxes_glyph(
    mapping = mapping, data = data, stat = stat,
    position = position, ..., serialaxes.data = serialAxesData,
    axes.sequence = sequence,
    scaling = match.arg(scaling),
    axes.layout = match.arg(axesLayout),
    show.axes = showAxes, show.enclosing = showEnclosing,
    linewidth = linewidth,
    axescolour = axesColor, bboxcolour = bboxColor, na.rm = na.rm,
    show.legend = show.legend,
    inherit.aes = inherit.aes
  )
}

#' @export
#' @inherit ggplot2::geom_pointrange
geom_pointrangeGlyph <- function(mapping = NULL, data = NULL, stat = 'identity',
                                 position = 'identity', ...,
                                 ymin, ymax, showArea = TRUE, linewidth = 1,
                                 na.rm = FALSE, show.legend = NA,
                                 inherit.aes = TRUE) {
  rlang::warn(
    "Function `geom_pointrangeGlyph` is deprecated,
    please use function `ggplot2::geom_pointrange` instead."
  )

  ggplot2::geom_pointrange(
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    ...,
    na.rm = na.rm,
    show.legend = show.legend,
    inherit.aes = inherit.aes
  )
}

#' @export
#' @inherit ggplot2::geom_text
geom_textGlyph <- function(mapping = NULL, data = NULL, stat = 'identity',
                           position = 'identity', ..., text,
                           na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE) {
  rlang::warn(
    "Function `geom_textGlyph` is deprecated,
    please use function `ggplot2::geom_text` instead."
  )

  ggplot2::geom_text(
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    ...,
    na.rm = na.rm,
    show.legend = show.legend,
    inherit.aes = inherit.aes
  )
}
