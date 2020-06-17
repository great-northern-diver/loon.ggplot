#' @title Add polygon glyph on scatter plot
#' @description The glyph geom is used to create scatterplots with a variety glyphs such as polygon glyph, serialaxes glyph, image glyph, point range glyph and text glyph.
#'
#' @inheritParams ggplot2::layer
#' @param polygon_x nested list of x-coordinates of polygons, one list element for each scatterplot point.
#' If not provided, \code{geom_point()} will be called.
#' @param polygon_y nested list of y-coordinates of polygons, one list element for each scatterplot point.
#' If not provided, \code{geom_point()} will be called.
#' @param showArea boolean to indicate whether area should be shown or not
#' @param linewidth line width of polygon
#' @param na.rm If \code{FALSE}, the default, missing values are removed with a warning.
#' If `TRUE`, missing values are silently removed.
#' @param ... Other arguments passed on to \code{ggplot2::layer}.
#' These are often aesthetics, used to set an aesthetic to a fixed value,
#' like \code{colour = "red"} or \code{size = 3}.
#' They may also be parameters to the paired geom/stat.
#'
#' @section Aesthetics:
#' geom_...Glyph() understands the following aesthetics (required aesthetics are in bold):
#' \itemize{
#' \item{\strong{x}}
#' \item{\strong{y}}
#' \item{alpha}
#' \item{colour}
#' \item{fill}
#' \item{group}
#' \item{shape}
#' \item{size}
#' \item{stroke}
#' \item{linetype}
#' }
#'
#' @return a \code{geom} layer
#' @export
#' @seealso \code{\link{geom_imageGlyph}}, \code{\link{geom_pointrangeGlyph}},
#' \code{\link{geom_serialAxesGlyph}}, \code{\link{geom_textGlyph}}
#' @examples
#' # polygon glyph
#' p <- ggplot(data = data.frame(x = 1:4, y = 1:4),
#'             mapping = aes(x = x, y = y)) +
#'   geom_polygonGlyph(polygon_x = list(x_star, x_cross, x_hexagon, -x_airplane),
#'                     polygon_y = list(y_star, y_cross, y_hexagon, y_airplane),
#'                     colour = 'black', fill = 'red')
#' p
geom_polygonGlyph <- function(mapping = NULL, data = NULL, stat = 'identity',
                              position = 'identity', ...,
                              polygon_x, polygon_y, showArea = TRUE, linewidth = 1,
                              na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE) {
  if(missing(polygon_x) || missing(polygon_y)) {

    ggplot2::layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = ggplot2::GeomPoint,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = na.rm,
        ...
      )
    )
  } else {
    ggplot2::layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomPolygonGlyph,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        polygon_x = polygon_x,
        polygon_y = polygon_y,
        showArea = showArea,
        linewidth = linewidth,
        na.rm = na.rm,
        ...
      )
    )
  }
}

GeomPolygonGlyph <- ggplot2::ggproto('GeomPolygonGlyph', Geom,
                                     required_aes = c('x', 'y'),
                                     default_aes = aes(colour = 'black',
                                                       fill = 'black', size = 4,
                                                       linetype = 1, alpha = 1,
                                                       shape = 19, stroke = 0.5),
                                     draw_key = ggplot2::draw_key_point,
                                     draw_panel = function(data, panel_params, coord,
                                                           polygon_x, polygon_y, showArea, linewidth, na.rm) {

                                       data <- coord$transform(data, panel_params)
                                       n <- dim(data)[1]

                                       if(is.list(polygon_x)) {
                                         if(n != length(polygon_x)) {
                                           polygon_x <- rep(polygon_x, n)[1:n]
                                         }
                                       } else {
                                         polygon_x <- lapply(1:n, function(i) polygon_x)
                                       }
                                       if(is.list(polygon_y)) {
                                         if(n != length(polygon_y)) {
                                           polygon_y <- rep(polygon_y, n)[1:n]
                                         }
                                       } else {
                                         polygon_y <- lapply(1:n, function(i) polygon_y)
                                       }

                                       poly_x <- lapply(seq_len(n),
                                                        function(i) {
                                                          if(showArea) {
                                                            grid::unit(data$x[i], 'native') +
                                                              grid::unit(polygon_x[[i]] * as_r_polygonGlyph_size(data$size[i]), "mm")
                                                          } else {
                                                            grid::unit(data$x[i], 'native') +
                                                              grid::unit(c(polygon_x[[i]], polygon_x[[i]][1]) * as_r_polygonGlyph_size(data$size[i]), "mm")
                                                          }

                                                        })
                                       poly_y <- lapply(seq_len(n),
                                                        function(i) {
                                                          if(showArea) {

                                                            grid::unit(data$y[i], 'native') +
                                                              grid::unit(-polygon_y[[i]] * as_r_polygonGlyph_size(data$size[i]), "mm")

                                                          } else {

                                                            grid::unit(data$y[i], 'native') +
                                                              grid::unit(c(polygon_y[[i]], polygon_y[[i]][1]) * as_r_polygonGlyph_size(data$size[i]), "mm")
                                                          }
                                                        })

                                       grob <- if(showArea) {
                                         grid::polygonGrob(
                                           x = do.call(grid::unit.c, poly_x),
                                           y = do.call(grid::unit.c, poly_y),
                                           id = rep(seq(length(poly_x)), lengths(poly_x)),
                                           gp = grid::gpar(
                                             fill = data$fill,
                                             col =  data$colour,
                                             lwd = as_r_line_size(linewidth),
                                             alpha = data$alpha
                                           )
                                         )
                                       } else {
                                         grid::polylineGrob(
                                           x = do.call(grid::unit.c, poly_x),
                                           y = do.call(grid::unit.c, poly_y),
                                           id = rep(seq(length(poly_x)), lengths(poly_x)),
                                           gp = gpar(
                                             fill = NA,
                                             col =  data$colour,
                                             lwd = as_r_line_size(linewidth),
                                             alpha = data$alpha
                                           )
                                         )
                                       }

                                       ggplot2:::ggname("geom_polygonGlyph", grob)
                                     }
)
