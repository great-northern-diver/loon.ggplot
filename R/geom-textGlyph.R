#' @export
geom_textGlyph <- function(mapping = NULL, data = NULL, stat = 'identity',
                           text,
                           position = 'identity', na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE, ...) {

  if(missing(text)) stop('no texts exist')

  ggplot2::layer(
    data = data,
    mapping = mapping,
    geom = GeomTextGlyph,
    position = position,
    stat = stat,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      text = text,
      ...
    )
  )
}


GeomTextGlyph <- ggplot2::ggproto('GeomTextGlyph', Geom,
                                  required_aes = c('x', 'y'),
                                  default_aes = ggplot2::aes(colour = "black", size = 4,
                                                             angle = 0, hjust = 0.5, stroke = 0.5,
                                                             shape = 19, fill = NA,
                                                             vjust = 0.5, alpha = NA, family = "",
                                                             fontface = 1, lineheight = 1.2),
                                  draw_key = ggplot2::draw_key_point,
                                  setup_data = function(data, params) {
                                    data[, "text"] <- params$text
                                    data
                                  },
                                  draw_panel = function(data, panel_params, coord, text, na.rm) {

                                    data <- coord$transform(data, panel_params)
                                    if (is.character(data$vjust)) {
                                      data$vjust <- ggplot2:::compute_just(data$vjust, data$y)
                                    }
                                    if (is.character(data$hjust)) {
                                      data$hjust <- ggplot2:::compute_just(data$hjust, data$x)
                                    }

                                    ggplot2:::ggname(
                                      "geom_textGlyph",
                                      grid::textGrob(
                                        text,
                                        data$x,
                                        data$y,
                                        default.units = "native",
                                        hjust = data$hjust, vjust = data$vjust,
                                        rot = data$angle,
                                        gp = grid::gpar(
                                          col = alpha(data$colour, data$alpha),
                                          fontsize = data$size * .pt,
                                          fontfamily = data$family,
                                          fontface = data$fontface,
                                          lineheight = data$lineheight
                                        )
                                      )
                                    )
                                  }
)
