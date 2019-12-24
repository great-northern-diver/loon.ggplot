#' @title Add image glyph on scatter plot
#' @inheritParams ggplot2::layer
#' @param images a list of images (a raster object, bitmap image). If not provided, `geom_point()` will be called.
#' @param width width of image
#' @param height height of image
#' @param na.rm If FALSE, the default, missing values are removed with a warning.
#' If TRUE, missing values are silently removed.
#' @param ... Other arguments passed on to `layer()`.
#'
#' @section Aesthetics:
#' geom_imageGlyph() understands the following aesthetics (required aesthetics are in bold):
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
#' \code{\link{geom_polygonGlyph}}, \code{\link{geom_textGlyph}}
#'
#' @examples
#' if(requireNamespace("png")) {
#'   img_paths <- list.files(file.path(find.package(package = 'loon'), "images"), full.names = TRUE)
#'   images <- lapply(img_paths, function(path) png::readPNG(path))
#'   p <- ggplot(data = data.frame(x = 1:6, y = 1:6),
#'               mapping = aes(x = x, y = y)) +
#'          geom_imageGlyph(images = images, alpha = 0.4, width = 2, height = 1.5)
#'   p
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
