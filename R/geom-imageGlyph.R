#' @title Add image glyph on scatter plot
#' @description The glyph geom is used to create scatterplots with a variety glyphs such as polygon glyph, serialaxes glyph, image glyph, point range glyph and text glyph.
#'
#' @inheritParams ggplot2::layer
#' @param images a list of images (a raster object, bitmap image). If not provided, \code{geom_point()} will be called.
#' @param width width of image
#' @param height height of image
#' @param na.rm If \code{FALSE}, the default, missing values are removed with a warning.
#' If `TRUE`, missing values are silently removed.
#' @param ... Other arguments passed on to \code{ggplot2::layer}.
#' These are often aesthetics, used to set an aesthetic to a fixed value,
#' like \code{colour = "red"} or \code{size = 3}.
#' They may also be parameters to the paired geom/stat.
#'
#' @return a \code{geom} layer
#'
#' @export
#' @seealso \code{\link{geom_polygonGlyph}}, \code{\link{geom_pointrangeGlyph}},
#' \code{\link{geom_serialAxesGlyph}}, \code{\link{geom_textGlyph}}
#' @examples
#' # image glyph
#' \donttest{
#' if(requireNamespace("png")) {
#'   img_paths <- list.files(file.path(find.package(package = 'loon'), "images"), full.names = TRUE)
#'   images <- lapply(img_paths, function(path) png::readPNG(path))
#'   p <- ggplot(data = data.frame(x = 1:6, y = 1:6),
#'               mapping = aes(x = x, y = y)) +
#'          geom_imageGlyph(images = images, alpha = 0.4, width = 2, height = 1.5)
#'   p
#' }
#' }
#'

geom_imageGlyph <- function(mapping = NULL, data = NULL, stat = 'identity',
                            position = 'identity', ...,
                            images, width = 4, height = 3, na.rm = FALSE, show.legend = NA,
                            inherit.aes = TRUE) {

  if(missing(images)) {

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
      geom = GeomImageGlyph,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        images = images,
        width = width,
        height = height,
        na.rm = na.rm,
        ...
      )
    )
  }
}

GeomImageGlyph <- ggplot2::ggproto('GeomImageGlyph', Geom,
                                   required_aes = c('x', 'y'),
                                   default_aes = aes(colour = NA,
                                                     fill = 'black', size = 4,
                                                     linetype = 1, alpha = 1,
                                                     shape = 19, stroke = 0.5),
                                   draw_key = ggplot2::draw_key_point,
                                   # TODO setup_data
                                   draw_panel = function(data, panel_params, coord, images, width, height, na.rm) {

                                     data <- coord$transform(data, panel_params)
                                     width_p <- grid::unit(as_r_image_size(width) * data$size, "cm")
                                     height_p <- grid::unit(as_r_image_size(height) * data$size, "cm")

                                     ggplot2:::ggname("geom_imageGlyph",
                                                      grid::gTree(
                                                        children = do.call(grid::gList,
                                                                           lapply(1:length(images),
                                                                                  function(i) {
                                                                                    grid::gList(
                                                                                      grid::rectGrob(x = grid::unit(data$x[i], "native"),
                                                                                                     y = grid::unit(data$y[i], "native"),
                                                                                                     just = "centre",
                                                                                                     width = width_p[i] + unit(2, "mm"),
                                                                                                     height = height_p[i] + unit(2, "mm"),
                                                                                                     gp = grid::gpar(
                                                                                                       fill = data$fill[i],
                                                                                                       col = data$colour[i],
                                                                                                       alpha = data$alpha[i]
                                                                                                     )
                                                                                      ),
                                                                                      grid::rasterGrob(image = if(is.list(images)) images[[i]] else images,
                                                                                                       x = grid::unit(data$x[i], "native"),
                                                                                                       y = grid::unit(data$y[i], "native"),
                                                                                                       just = "centre",
                                                                                                       width = width_p[i],
                                                                                                       height = height_p[i],
                                                                                                       gp = grid::gpar(
                                                                                                         alpha = data$alpha[i]
                                                                                                       )
                                                                                      )
                                                                                    )
                                                                                  })
                                                        )
                                                      )
                                     )
                                   }
)

as_r_image_size <- function(x) x/10
