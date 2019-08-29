#' @export
geom_polygonGlyph <- function(mapping = NULL, data = NULL, stat = 'identity',
                              polygon_x, polygon_y, showArea = TRUE, linewidth = 1,
                              position = 'identity', na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE, ...) {
  if(missing(polygon_x)) stop('no polygon_x exist')
  if(missing(polygon_y)) stop('no polygon_y exist')

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
                                                              grid::unit(polygon_x[[i]] * loon:::as_r_polygonGlyph_size(data$size[i]), "mm")
                                                          } else {
                                                            grid::unit(data$x[i], 'native') +
                                                              grid::unit(c(polygon_x[[i]], polygon_x[[i]][1]) * loon:::as_r_polygonGlyph_size(data$size[i]), "mm")
                                                          }

                                                        })
                                       poly_y <- lapply(1:length(polygon_y),
                                                        function(i) {
                                                          if(showArea) {

                                                            grid::unit(data$y[i], 'native') +
                                                              grid::unit(-polygon_y[[i]] * loon:::as_r_polygonGlyph_size(data$size[i]), "mm")

                                                          } else {

                                                            grid::unit(data$y[i], 'native') +
                                                              grid::unit(c(polygon_y[[i]], polygon_y[[i]][1]) * loon:::as_r_polygonGlyph_size(data$size[i]), "mm")
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
