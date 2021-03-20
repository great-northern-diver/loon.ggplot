#' Automatically create a loon widget (deprecated)
#' @export
#' @inheritParams l_ggplot
#' @keywords internal
#' @name lggplot-deprecated
#' @section \code{lggplot}:
#' For \code{lggplot}, use \code{\link{l_ggplot}}.
#'
lggplot <- function(data = NULL, mapping = aes(), ...,
                    environment = parent.frame()) {

  .Deprecated("l_ggplot", package= "loon.ggplot")
  l_ggplot(data = data, mapping = mapping, ...,
           environment = environment)
}

#' image glyph (deprecated)
#' @export
#' @inheritParams ggmulti::geom_image_glyph
#' @param width Numerical; width of image
#' @param height Numerical; height of image
#' @name geom_imageGlyph-deprecated
#' @keywords internal
#' @section \code{geom_imageGlyph}:
#' For \code{geom_imageGlyph}, use \code{\link{geom_image_glyph}}.
#'
geom_imageGlyph <- function(mapping = NULL, data = NULL, stat = 'identity',
                            position = 'identity', ...,
                            images, width = 4, height = 3, na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {

  .Deprecated("geom_image_glyph", package= "loon.ggplot")

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

#' polygon glyph (deprecated)
#' @export
#' @inheritParams ggmulti::geom_polygon_glyph
#' @param showArea show area; deprecated now, please set `fill` or `colour` to control the shown area.
#' @name geom_polygonGlyph-deprecated
#' @keywords internal
#' @section \code{geom_polygonGlyph}:
#' For \code{geom_polygonGlyph}, use \code{\link{geom_polygon_glyph}}.
#'
geom_polygonGlyph <- function(mapping = NULL, data = NULL, stat = 'identity',
                              position = 'identity', ...,
                              polygon_x, polygon_y, showArea = TRUE, linewidth = 1,
                              na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE) {

  .Deprecated("geom_polygon_glyph", package= "loon.ggplot")

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

#' serialaxes glyph (deprecated)
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
#'
#' @name geom_serialAxesGlyph-deprecated
#' @keywords internal
#' @section \code{geom_serialAxesGlyph}:
#' For \code{geom_serialAxesGlyph}, use \code{\link{geom_serialaxes_glyph}}.
#'
#'
geom_serialAxesGlyph <- function(mapping = NULL, data = NULL, stat = 'identity',
                                 position = 'identity', ...,
                                 serialAxesData, sequence = NULL, linewidth = 1,
                                 scaling = c('variable', 'data', 'observation', 'none'),
                                 axesLayout = c("parallel", "radial"),
                                 showAxes = FALSE, showArea = FALSE,  showEnclosing = FALSE,
                                 axesColor = "black", bboxColor = 'black',na.rm = FALSE, show.legend = NA,
                                 inherit.aes = TRUE) {

  .Deprecated("geom_serialaxes_glyph", package= "loon.ggplot")

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

#' pointrange glyph (deprecated)
#' @export
#' @inheritParams ggplot2::geom_pointrange
#' @param ymin y min
#' @param ymax y max
#' @param showArea show area; deprecated now, please set `fill` or `colour` to control the shown area.
#' @param linewidth line width
#'
#' @name geom_pointrangeGlyph-deprecated
#' @keywords internal
#' @section \code{geom_pointrangeGlyph}:
#' For \code{geom_pointrangeGlyph}, use \code{\link{geom_pointrange}}.
#'
geom_pointrangeGlyph <- function(mapping = NULL, data = NULL, stat = 'identity',
                                 position = 'identity', ...,
                                 ymin, ymax, showArea = TRUE, linewidth = 1,
                                 na.rm = FALSE, show.legend = NA,
                                 inherit.aes = TRUE) {
  .Deprecated("geom_pointrange", package= "loon.ggplot")

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

#' text glyph (deprecated)
#' @export
#' @inheritParams ggplot2::geom_text
#' @param text The test to display
#'
#' @name geom_textGlyph-deprecated
#' @keywords internal
#' @section \code{geom_textGlyph}:
#' For \code{geom_textGlyph}, use \code{\link{geom_text}}.
#'
geom_textGlyph <- function(mapping = NULL, data = NULL, stat = 'identity',
                           position = 'identity', ..., text,
                           na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE) {

  .Deprecated("geom_text", package= "loon.ggplot")

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
