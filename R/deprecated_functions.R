#' @title Add image glyphs on scatter plot
#' @description Each point glyph can be a image (png, jpeg, etc) object.
#' @export
#' @inheritParams ggmulti::geom_image_glyph
#' @param width Numerical; width of image
#' @param height Numerical; height of image
geom_imageGlyph <- function(mapping = NULL, data = NULL, stat = 'identity',
                            position = 'identity', ...,
                            images, width = 4, height = 3, na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  warning(
    "Function `geom_imageGlyph` is deprecated, ",
    "please use function `geom_image_glyph` in package 'ggmulti'", call. = FALSE
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

#' @title Add polygon glyphs on scatter plot
#' @description Each point glyph can be a polygon object. Available polygons coords can be achieved in \code{\link{polygon_glyph}}
#' @export
#' @inheritParams ggmulti::geom_polygon_glyph
#' @param showArea show area; deprecated now, please set `fill` or `colour` to control the shown area.
geom_polygonGlyph <- function(mapping = NULL, data = NULL, stat = 'identity',
                              position = 'identity', ...,
                              polygon_x, polygon_y, showArea = TRUE, linewidth = 1,
                              na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE) {
  warning(
    "Function `geom_polygonGlyph` is deprecated, ",
    "please use function `geom_polygon_glyph` in package 'ggmulti'", call. = FALSE
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

#' @title Add serialaxes glyphs on scatter plot
#' @description To visualize high dimensional data on scatterplot. Each point glyph is surrounded by a serialaxes object.
#' @export
#' @inheritParams ggmulti::geom_serialaxes_glyph
#' @param serialAxesData a serial axes numerical data set. If not provided, `geom_point()` will be called.
#' @param sequence A vector with variable names that defines the axes sequence
#' @param axesLayout either "radial" or "parallel"
#' @param showAxes boolean to indicate whether axes should be shown or not
#' @param showArea show area; deprecated now, please set `fill` or `colour` to control the shown area.
#' @param showEnclosing boolean to indicate whether enclosing should be shown or not
#' @param axesColor axes color
#' @param bboxColor bounding box color
geom_serialAxesGlyph <- function(mapping = NULL, data = NULL, stat = 'identity',
                                 position = 'identity', ...,
                                 serialAxesData, sequence = NULL, linewidth = 1,
                                 scaling = c('variable', 'data', 'observation', 'none'),
                                 axesLayout = c("parallel", "radial"),
                                 showAxes = FALSE, showArea = FALSE,  showEnclosing = FALSE,
                                 axesColor = "black", bboxColor = 'black',na.rm = FALSE, show.legend = NA,
                                 inherit.aes = TRUE) {
  warning(
    "Function `geom_polygonGlyph` is deprecated, ",
    "please use function `geom_polygon_glyph` in package 'ggmulti'",
    call. = FALSE
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

#' @title Pointrange
#' @export
#' @inheritParams ggplot2::geom_pointrange
#' @param ymin y min
#' @param ymax y max
#' @param showArea show area; deprecated now, please set `fill` or `colour` to control the shown area.
#' @param linewidth line width
geom_pointrangeGlyph <- function(mapping = NULL, data = NULL, stat = 'identity',
                                 position = 'identity', ...,
                                 ymin, ymax, showArea = TRUE, linewidth = 1,
                                 na.rm = FALSE, show.legend = NA,
                                 inherit.aes = TRUE) {
  warning(
    "Function `geom_pointrangeGlyph` is deprecated, ",
    "please use function `ggplot2::geom_pointrange` instead.", call. = FALSE
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

#' @title Text
#' @export
#' @inheritParams ggplot2::geom_text
#' @param text The test to display
geom_textGlyph <- function(mapping = NULL, data = NULL, stat = 'identity',
                           position = 'identity', ..., text,
                           na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE) {
  warning(
    "Function `geom_textGlyph` is deprecated, please use function `ggplot2::geom_text` instead.", call. = FALSE)

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
