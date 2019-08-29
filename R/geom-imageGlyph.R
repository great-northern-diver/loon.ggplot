#' @export
geom_imageGlyph <- function(mapping = NULL, data = NULL, stat = 'identity',
                            images, width = 4, height = 3,
                            position = 'identity', na.rm = FALSE, show.legend = NA,
                            inherit.aes = TRUE, ...) {

  if(missing(images)) stop('no images exist')

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
