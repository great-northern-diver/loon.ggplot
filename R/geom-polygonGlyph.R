#' @title Add polygon glyph on scatter plot
#' @inheritParams ggplot2::layer
#' @param polygon_x nested list of x-coordinates of polygons, one list element for each scatterplot point.
#' If not provided, `geom_point()` will be called.
#' @param polygon_y nested list of y-coordinates of polygons, one list element for each scatterplot point.
#' If not provided, `geom_point()` will be called.
#' @param showArea boolean, show a filled polygon or just the outline
#' @param linewidth linewidth of outline.
#' @param na.rm If FALSE, the default, missing values are removed with a warning.
#' If TRUE, missing values are silently removed.
#' @param ... Other arguments passed on to `layer()`.
#'
#' @section Aesthethics:
#' geom_polygonGlyph() understands the following aesthetics (required aesthetics are in bold):
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
#' @export
#'
#' @seealso \code{\link{geom_serialAxesGlyph}}, \code{\link{geom_pointrangeGlyph}},
#' \code{\link{geom_imageGlyph}}, \code{\link{geom_textGlyph}}
#'
#' @examples
#' x_star <-
#'   c(-0.000864304235090734, 0.292999135695765, 0.949870354364736,
#'     0.474503025064823, 0.586862575626621, -0.000864304235090734,
#'     -0.586430423509075, -0.474070872947277, -0.949438202247191, -0.29256698357822)
#' y_star <-
#'   c(-1, -0.403630077787381, -0.308556611927398, 0.153846153846154,
#'     0.808556611927398, 0.499567847882455, 0.808556611927398,
#'     0.153846153846154, -0.308556611927398, -0.403630077787381)
#' x_cross <-
#'   c(-0.258931143762604, -0.258931143762604, -0.950374531835206,
#'     -0.950374531835206, -0.258931143762604, -0.258931143762604,
#'     0.259651397291847, 0.259651397291847, 0.948934024776722,
#'     0.948934024776722, 0.259651397291847, 0.259651397291847)
#' y_cross <-
#'   c(-0.950374531835206, -0.258931143762604, -0.258931143762604,
#'     0.259651397291847, 0.259651397291847, 0.948934024776722,
#'     0.948934024776722, 0.259651397291847, 0.259651397291847,
#'     -0.258931143762604, -0.258931143762604, -0.950374531835206)
#' x_hexagon <-
#'   c(0.773552290406223, 0, -0.773552290406223, -0.773552290406223,
#'     0, 0.773552290406223)
#' y_hexagon <-
#'   c(0.446917314894843, 0.894194756554307, 0.446917314894843,
#'     -0.447637568424085, -0.892754249495822, -0.447637568424085)
#' p <- ggplot(data = data.frame(x = 1:3, y = 1:3),
#'             mapping = aes(x = x, y = y)) +
#'   geom_polygonGlyph(polygon_x = list(x_star, x_cross, x_hexagon),
#'                     polygon_y = list(y_star, y_cross, y_hexagon),
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
                                     setup_data = function(data, params) {

                                       n <- dim(data)[1]

                                       if(is.list(params$polygon_x) && is.list(params$polygon_y)) {
                                         stopifnot(exprs = {
                                           n == length(params$polygon_x)
                                           n == length(params$polygon_y)
                                         })
                                       }

                                       tryCatch(
                                         {
                                           lapply(names(params),
                                                  function(name) {

                                                    switch(name,
                                                           "polygon_x" = {
                                                             polygon_x <- params[[name]]

                                                             data[, "polygon_x"] <<- if(is.list(polygon_x) && length(polygon_x) == n) {
                                                               sapply(polygon_x, function(x) paste0(x, collapse = "\\"))
                                                             } else {
                                                               paste0(polygon_x, collapse = "\\")
                                                             }
                                                           },
                                                           "polygon_y" = {
                                                             polygon_y <- params[[name]]

                                                             data[, "polygon_y"] <<- if(is.list(polygon_y) && length(polygon_y) == n) {
                                                               sapply(polygon_y, function(y) paste0(y, collapse = "\\"))
                                                             } else {
                                                               paste0(polygon_y, collapse = "\\")
                                                             }
                                                           },
                                                           {
                                                             data[, name] <<- params[[name]]
                                                           }
                                                    )
                                                  })
                                         }, error = function(e) warning("Try to set a list for polygon_x and polygon_y")
                                       )
                                       data
                                     },
                                     draw_panel = function(data, panel_params, coord,
                                                           polygon_x, polygon_y, showArea, linewidth, na.rm) {


                                       data <- coord$transform(data, panel_params)
                                       poly_x <- lapply(1:length(polygon_x),
                                                        function(i) {
                                                          if(showArea) {
                                                            grid::unit(data$x[i], 'native') +
                                                              grid::unit(polygon_x[[i]] * as_r_polygonGlyph_size(data$size[i]), "mm")
                                                          } else {
                                                            grid::unit(data$x[i], 'native') +
                                                              grid::unit(c(polygon_x[[i]], polygon_x[[i]][1]) * as_r_polygonGlyph_size(data$size[i]), "mm")
                                                          }

                                                        })
                                       poly_y <- lapply(1:length(polygon_y),
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
