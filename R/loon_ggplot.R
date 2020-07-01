#' @title loon.ggplot
#'
#' @description A bridge between \code{loon} widgets and \code{gg} objects. It can take either a \code{loon} widget,
#' a \code{gg} object (\code{ggplot} or \code{GGally::ggmatrix}) or
#' a \code{lggplot} object, then create a corresponding \code{gg} (or \code{loon}) graphics.
#'
#' @param x A \code{loon} widget, a \code{ggplot} object or a \code{lggplot} object.
#' @param ... arguments used in either \code{loon2ggplot()} or \code{ggplot2loon()}
#'
#' @return If the input is a \code{ggplot} object, the output would be a \code{loon} widget; conversely, if the
#' input is a \code{loon} widget, then it returns a \code{ggplot} object. If it is a \code{lggplot} object,
#' \code{loon.ggplot} helps to return a \code{loon} widget.
#'
#' @seealso \code{\link{loon2ggplot}}, \code{\link{ggplot2loon}}, \code{\link{l_ggplot}}
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
#'
#' ######### lggplot #########
#'   p <- l_ggplot(mpg, aes(displ, fill = factor(cyl))) +
#'        geom_histogram()
#'   class(p)
#'   # Function `print.lggplot` is called automatically
#'   p
#'   # Function `loon.ggplot` helps to return a loon widget
#'   q <- loon.ggplot(p)
#'   q
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

#' @export
#' @rdname loon.ggplot
# object `lggplot` and `l_ggplot` are two different animals.
################## `lggplot` ##################
# A `lggplot` object is a kinda `ggplot` object.
# Users can use `+` to build a loon plot. The magic comes from the class `lggplot`.
# When we start with function `l_ggplot` (which is confusing) instead of the `ggplot` function,
# the returned object has a new class `lggplot`.
# Then, function `print.lggplot` automatically draws the `loon` plot.

################## `l_ggplot` ##################
# when we transform a `ggplot` object (with multiple facets) to a `loon` plot. We will return a list with several plots.
# the returned list is a `l_compound` object, called `l_ggplot`.

loon.ggplot.lggplot <- function(x, ...) {

  params <- list()
  if(!is.null(x$interactivity)) {
    interactivity <- x$interactivity
    # Function `params_check` does not do any assignment (No side-effect here)
    # It is mainly used to check parameters and generate warnings
    params_check(interactivity, data = x$data)
    params <- interactivity$params
  }

  p <- do.call(
    ggplot2loon,
    c(list(ggObj = x), params)
  )

  p
}
