#' @title loon.ggplot
#'
#' @description A bridge between \code{loon} widgets and \code{gg} objects. It can take either a \code{loon} widget
#' or a \code{gg} object (\code{ggplot} or \code{ggmatrix}), then create a corresponding \code{gg} (or \code{loon}) graphics.
#'
#' @param x A \code{loon} widget or a \code{ggplot} object.
#' @param ... arguments used in either \code{loon2ggplot()} or \code{ggplot2loon()}
#'
#' @return If the input is a \code{ggplot} object, the output would be a \code{loon} widget; conversely, if the
#' input is a \code{loon} widget, then it returns a \code{ggplot} object.
#'
#' @seealso \code{\link{loon2ggplot}}, \code{\link{ggplot2loon}}
#'
#' @export
#' @examples
#'
#' if(interactive()) {
#' ######### loon --> gg #########
#'   # loon 3D plot
#'   l <- with(quakes,
#'     l_plot3D(long, lat, depth, linkingGroup = "quakes")
#'   )
#'   # equivalent to `loon2ggplot(l)`
#'   g <- loon.ggplot(l)
#'   g # a ggplot object
#'
#' ######### gg --> loon #########
#'
#'   # ggplot histogram
#'   g <- ggplot(iris, mapping = aes(Sepal.Length, fill = Species)) +
#'     geom_histogram()
#'   # equivalent to `ggplot2loon(g)`
#'   l <- loon.ggplot(g)
#'   l # a loon widget
#' }

loon.ggplot <- function(x, ...) {
  UseMethod('loon.ggplot', x)
}

#' @export
#' @rdname loon.ggplot
loon.ggplot.gg <- function(x, ...) {
  ggplot2loon(x, ...)
}

#' @export
#' @rdname loon.ggplot
loon.ggplot.loon <- function(x, ...) {
  loon2ggplot(x, ...)
}
