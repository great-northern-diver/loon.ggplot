#' @title Automatically create a loon widget
#' @description Create a \code{loon} widget with \code{ggplot} syntax
#'
#' @param data Default dataset to use for plot. If not already a data.frame, will be converted to one by fortify().
#' If not specified, must be supplied in each layer added to the plot.
#' @param mapping Default list of aesthetic mappings to use for plot.
#' If not specified, must be supplied in each layer added to the plot.
#' @param ... Other arguments passed on to methods. Not currently used.
#' @param environment DEPRECATED. Used prior to tidy evaluation.
#' @seealso \code{\link{ggplot}}, \code{\link{ggplot2loon}}, \code{\link{print.l_ggplot}}
#' @export
#' @return It will return an \code{l_ggplot} object with class \code{c("l_ggplot", "gg", "ggplot")}.
#' Then print a \code{loon} plot automatically.
#'
#' @details function \code{l_ggplot()} wraps function \code{ggplot()} with assigning an additional class "l_ggplot"
#' to the output. The returned object is called an \code{l_ggplot} object.
#' To draw a \code{ggplot} object, S3 method \code{print.ggplot} will be rendered
#' so that a static graphic is displayed. While, for an \code{l_ggplot()} object,
#' S3 method \code{print.l_ggplot} will be rendered
#' which will return an interactive \code{loon} widget.
#'
#' @seealso \code{\link{loon.ggplot}}
#' @examples
#' if(interactive()) {
#' p <- l_ggplot(mpg, aes(displ, cty)) +
#'     geom_point(
#'       size = 4,
#'       mapping = aes(color = factor(cyl))
#'     )
#' # p is an `l_ggplot` object, `print.l_ggplot(p)` will be called automatically.
#' # Then, at printing time, an `l_ggplot` object will be transformed to a `loon` widget
#' p
#'
#' \dontrun{
#' # Assign a widget from current path
#' # suppose the path of `p` is '.l0.ggplot'
#' q <- l_getFromPath('.l0.ggplot')
#' # q is a `loon` widget
#' q
#' }
#'
#' # An alternative way to return a real loon widget from `p` (a `l_ggplot` object)
#' # is to call the function `loon.ggplot()`.
#' q <- loon.ggplot(p)
#' q
#'
#' # pipe more components
#' p +
#'   facet_grid(rows = vars(drv)) +
#'   linking(linkingGroup = "mpg") +
#'   ggtitle("displ versus cty")
#' # a linked bar plot
#' l_hist(mpg$class, linkingGroup = "mpg")
#'
#' # a 3D object
#' # press the button key `R` to rotate the plot
#' l_ggplot(mtcars,
#'          mapping = aes(x = wt, y = hp, z = drat)) +
#'    geom_point(size = 4) +
#'    scale_multi()
#' }
l_ggplot <- function(data = NULL, mapping = aes(), ...,
                     environment = parent.frame()) {

  p <- ggplot(data = data,
              mapping = mapping,
              ...,
              environment = environment)

  structure(p,
            class = c("l_ggplot", "gg", "ggplot"))
}

#' Explicitly draw plot
#'
#' @param x plot to display
#' @param message logical; if \code{TRUE}, the way to create handle will be
#' printed out.
#' @param ... other arguments used to modify function \code{ggplot2loon}
#' @return Invisibly returns a \code{loon} widget
#' @export
print.l_ggplot <- function(x, message = TRUE, ...) {

  p <- loon.ggplot(x, ...)

  # this message will be executed if and only if
  # 1. message is set as TRUE
  # 2. `x` is executed in the global environment
  if(message && is.GlobalEnv(x$plot_env)) {
    tryCatch(
      expr = {
        if(is.l_compound(p)) {
          p_ <- p[[1L]]
        } else p_ <- p

        path <- as.character(tcltk::tkwinfo("parent", as.character(p_)))

        message_wrap("The loon plot can be accessed as ",
                     "l_getFromPath(\"",
                     path,
                     "\")")
      },
      error = function(e) NULL
    )
  }

  print(p)
}

#' @title Reports whether x is a \code{l_ggplot} object
#' @param x An object to test
is.l_ggplot <- function(x) {
  inherits(x, "is.l_ggplot")
}
