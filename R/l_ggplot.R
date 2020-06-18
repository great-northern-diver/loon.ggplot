#' @title Automatically create a loon widget
#' @description \code{l_ggplot()} wraps function \code{ggplot} with assigning a new class "lggplot" to the output
#' \code{ggplot} object and returns a \code{lggplot} object. When a \code{ggplot} object is processed,
#' S3 method \code{print.ggplot} is rendered, however, if a \code{lggplot} object is processed,
#' S3 method \code{print.lggplot} will be rendered which will return a \code{loon} widget
#'
#' @param data Default dataset to use for plot. If not already a data.frame, will be converted to one by fortify().
#' If not specified, must be supplied in each layer added to the plot.
#' @param mapping Default list of aesthetic mappings to use for plot.
#' If not specified, must be supplied in each layer added to the plot.
#' @param ... Other arguments passed on to methods. Not currently used.
#' @param environment DEPRECATED. Used prior to tidy evaluation.
#' @seealso \code{\link{ggplot}}, \code{\link{ggplot2loon}}, \code{\link{print.lggplot}}
#' @export
#' @return It will return a \code{lggplot} object with class \code{c("lggplot", "gg", "ggplot")}.
#' Then print a \code{loon} plot automatically.
#' @examples
#' if(interactive()) {
#'   p <- l_ggplot(mpg, aes(displ, cty)) +
#'      geom_point() +
#'      facet_grid(rows = vars(drv))
#'   # p is a `lggplot` object, `print.lggplot(p)` is called automatically.
#'   # Then, the `lggplot` object will be transformed to a `loon` widget
#'   p
#' }
#' \dontrun{
#'   # get widgets from current path
#'   # suppose the path of `p` is '.l0.ggplot'
#'   q <- l_getFromPath('.l0.ggplot')
#'   # q is a `loon` widget
#'   q
#' }
l_ggplot <- function(data = NULL, mapping = aes(), ...,
                    environment = parent.frame()) {

  p <- ggplot(data = data, mapping = mapping, ...,
              environment = environment)

  structure(p,
            class = c("lggplot", "gg", "ggplot"))
}

#' Explicitly draw plot
#'
#' @param x plot to display
#' @param ... other arguments used to modify function \code{ggplot2loon}
#' @return Invisibly returns a \code{loon} widget
#' @export
print.lggplot <- function(x, ...) {
  p <- ggplot2loon(x,
                   ...)
  invisible()
}

#' @title Automatically create a loon widget
#' @export
#' @description It is retired. See \code{\link{l_ggplot}}
#' @inheritParams l_ggplot
lggplot <- function(data = NULL, mapping = aes(), ...,
                    environment = parent.frame()) {
  warning("`lggplot()` is deprecated now. Please use `l_ggplot()`",
          call. = FALSE)
  l_ggplot(data = data, mapping = mapping, ...,
           environment = environment)
}
