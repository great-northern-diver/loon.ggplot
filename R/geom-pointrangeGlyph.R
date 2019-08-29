#' @export
geom_pointrangeGlyph <- function(mapping = NULL, data = NULL, stat = 'identity',
                                 ymin, ymax, linewidth = 1, showArea = TRUE,
                                 position = 'identity', na.rm = FALSE, show.legend = NA,
                                 inherit.aes = TRUE, ...) {
  if(missing(ymin)) stop('no ymin exist')
  if(missing(ymax)) stop('no ymax exist')
  if(!is.numeric(linewidth)) stop('numerical linewidth is required')

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPointrangeGlyph,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      ymin = ymin,
      ymax = ymax,
      linewidth = linewidth,
      showArea = showArea,
      na.rm = na.rm,
      ...
    )
  )
}

GeomPointrangeGlyph <- ggplot2::ggproto('GeomPointrangeGlyph', Geom,
                                        required_aes = c('x', 'y'),
                                        default_aes = aes(colour = "black", linetype = 1, size = 4,
                                                          fill = NA, alpha = 1, stroke = 1),
                                        draw_key = ggplot2::draw_key_point,
                                        setup_data = function(data, params) {

                                          tryCatch(
                                            {
                                              lapply(names(params),
                                                     function(name) {
                                                       data[, name] <<- params[[name]]
                                                     })
                                            }, error = function(e) warning("welcome to report issues in https://github.com/z267xu/loon.ggplot")
                                          )

                                          data
                                        },
                                        draw_panel = function(data, panel_params, coord, ymin, ymax, linewidth, showArea, na.rm) {

                                          data$ymin <- ymin
                                          data$ymax <- ymax

                                          if(showArea) data$shape <- 1 else data$shape <- 19

                                          grob <- if(is.null(data$y)) {

                                            ggplot2::GeomLinerange$draw_panel(transform(data, size = as_r_line_size(linewidth)),
                                                                              panel_params, coord)
                                          } else {

                                            grid::gTree(
                                              children = grid::gList(
                                                ggplot2::GeomLinerange$draw_panel(transform(data, size = as_r_line_size(linewidth)),
                                                                                  panel_params, coord),
                                                ggplot2::GeomPoint$draw_panel(transform(data, size = as_r_point_size(size)),
                                                                              panel_params, coord)
                                              )
                                            )
                                          }

                                          ggplot2:::ggname("geom_pointrangeGlyph", grob)
                                        }
)
