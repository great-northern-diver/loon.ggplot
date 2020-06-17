#' @title Add pointrange glyph on scatter plot
#' @description The glyph geom is used to create scatterplots with
#' a variety glyphs such as polygon glyph, serialaxes glyph, image glyph, point range glyph and text glyph.
#'
#' @inheritParams ggplot2::layer
#' @param ymin vector with lower y-value of the point range. If not provided, \code{geom_point()} will be called.
#' @param ymax vector with upper y-value of the point range. If not provided, \code{geom_point()} will be called.
#' @param showArea If \code{TRUE}, the point pch is 21, else it is 1.
#' @param linewidth line width of whisker
#' @param na.rm If \code{FALSE}, the default, missing values are removed with a warning.
#' If `TRUE`, missing values are silently removed.
#' @param ... Other arguments passed on to \code{ggplot2::layer}
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
#' @export
#' @seealso \code{\link{geom_imageGlyph}}, \code{\link{geom_pointrangeGlyph}},
#' \code{\link{geom_serialAxesGlyph}}, \code{\link{geom_textGlyph}}
#'
#' @details \code{geom_pointrangeGlyph()} is very close to \code{\link{geom_pointrange}} but with `loon` API
#' @export
#' @seealso \code{\link{geom_polygonGlyph}}, \code{\link{geom_imageGlyph}},
#' \code{\link{geom_serialAxesGlyph}}, \code{\link{geom_textGlyph}}
#'
#' @return a \code{geom} layer
#'
#' @examples
#' # point range glyph
#' p <- ggplot(data = data.frame(x = 1:3, y = 1:3),
#'             mapping = aes(x = x, y = y)) +
#'   geom_pointrangeGlyph(ymin=(1:3)-(1:3)/5, ymax=(1:3)+(1:3)/5)
#' p
geom_pointrangeGlyph <- function(mapping = NULL, data = NULL, stat = 'identity',
                                 position = 'identity', ...,
                                 ymin, ymax, showArea = TRUE, linewidth = 1,
                                 na.rm = FALSE, show.legend = NA,
                                 inherit.aes = TRUE) {

  if(missing(ymin) || missing(ymax)) {

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

    if(!is.numeric(linewidth)) linewidth <- 1

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
                                            }, error = function(e) warning("welcome to report issues in https://github.com/great-northern-diver/loon.ggplot", call. = FALSE)
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
