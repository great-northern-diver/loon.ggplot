#' @title ggplot serialaxes
#'
#' @description The ggplot serialaxes graphics displays multivariate data either as
#' a stacked star glyph plot, or as a parallel coordinate plot.
#'
#' @param ggObj A `ggplot` object
#' @param data A data frame for serialaxes. If `NULL`, data must be set in `ggObj`
#' @param axesLabels A vector with variable names that defines the axes sequence.
#' @param showAxes Logical value to indicate whether axes should be shown or not
#' @param showAxesLabels Logical value to indicate whether axes labels should be shown or not
#' @param scaling one of 'variable', 'data', 'observation' or 'none' to
#' specify how the data is scaled. See Details for more information
#' @param axesLayout either "radial" or "parallel"
#' @param displayOrder The display order of the observations.
#' @param title title of the display
#' @param showLabels Logical value to indicate whether label (mainly **title**) should be shown or not
#' @param color Line color
#' @param size Line width
#' @param alpha Transparency
#' @param showGuides Logical value to indicate whether guides should be shown or not
#' @param showArea Logical value to indicate whether to display lines or area
#' @param ymin A vector or a data.frame. When \code{showArea = TRUE}, it is the lower boundary of the ribbon.
#'
#' @return a ggplot object
#'
#' @details It is deprecated now. Please check \code{\link{coord_serialaxes}}.
#'
#' @export
#' @examples
#' # Blank plot
#' p <- ggplot(data = mtcars, mapping = aes(colour = factor(cyl)))
#' # Add serial axes (returns a ggplot object)
#' g <- ggSerialAxes(p)
#' g
#' # An eulerian path of iris variables
#' # ordSeq <- PairViz::eulerian(4)
#' ordSeq <- c(1, 2, 3, 1, 4, 2, 3, 4)
#' ggSerialAxes(
#'        ggObj = ggplot(data = iris, mapping = aes(colour = Species)),
#'        axesLabels = colnames(iris)[ordSeq],
#'        axesLayout = "radial"
#' )

ggSerialAxes <- function(ggObj,
                         data = NULL, axesLabels = NULL,
                         showAxes = TRUE, showAxesLabels = TRUE,
                         scaling = c("variable", "observation", "data", "none"),
                         axesLayout = c("parallel", "radial"), displayOrder = NULL,
                         title = "", showLabels = TRUE,
                         color = NULL, size = NULL, alpha = NULL,
                         showGuides = TRUE, showArea = FALSE, ymin = NULL) {

  # check arguments
  if(!ggplot2::is.ggplot(ggObj)) {
    stop(paste(deparse(substitute(ggObj)), "is not a ggplot object"), call. = FALSE)
  }

  scaling <- match.arg(scaling)
  axesLayout <- match.arg(axesLayout)

  data <- data %||% ggObj$data %||% stop("No data found", call. = FALSE)

  ggObj <- switch(
    axesLayout,
    "parallel" = {
      ggObj %>%
        ggParallelAes(
          axesLabels = axesLabels,
          title = title,
          showLabels = showLabels,
          showAxesLabels = showAxesLabels,
          showGuides = showGuides,
          showAxes = showAxes) %>%
        ggParallelSerialAxes(
          data = data,
          axesLabels = axesLabels,
          displayOrder = displayOrder,
          scaling = scaling,
          color = color,
          lineWidth = size,
          alpha = alpha,
          showArea = showArea,
          ymin = ymin)
    },
    "radial" = {
      ggObj %>%
        ggRadialAes(
          axesLabels = axesLabels,
          title = title,
          showLabels = showLabels,
          showAxesLabels = showAxesLabels,
          showGuides = showGuides,
          showAxes = showAxes) %>%
        ggRadialSerialAxes(
          data = data,
          axesLabels = axesLabels,
          displayOrder = displayOrder,
          scaling = scaling,
          color = color,
          lineWidth = size,
          alpha = alpha,
          showArea = showArea,
          ymin = ymin)
    },
    {NULL}
  )

  return(ggObj)
}
