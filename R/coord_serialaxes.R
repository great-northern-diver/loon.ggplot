#' @title Serial axes coordinates
#' @description It is used to visualize high dimensional data set.
#' @param axesLayout Serial axes layout, either "parallel" or "radial".
#' @param scaling One of 'variable', 'data', 'observation' or 'none' to specify how the data is scaled.
#' @param displayOrder The order of the display
#' @param axesLabels A vector with variable names that defines the axes sequence.
#' @param right The layer ('density', 'histogram', ...) is displayed on the left or right.
#' @param ... other arguments used to modify layers
#' @details Serial axes coordinate system is different from other conventional coordinate system (Cartesian, Polar, ...).
#' It does not have a formal transformation (i.e. in polar coordinate system, "x = rcos(theta)",
#' "y = rsin(theta)"). In serial axes coordinate system, mapping aesthetics does not really require "x" or "y".
#' To project a \code{geom} layer, users can customize function \code{\link{add_serialaxes_layers}}.
#' @importFrom utils getFromNamespace
#' @examples
#' p <- ggplot(iris) +
#'        geom_path(alpha = 0.2) +
#'        coord_serialaxes()
#' # an 'iris' parallel coordinate plot.
#' p
#' # histogram layer (parallel coord)
#' p + geom_histogram(alpha = 0.8, mapping = aes(fill = factor(Species)))
#' # density layer
#' p + geom_density(alpha = 0.8)
#' @export
coord_serialaxes <- function(axesLayout = c("parallel", "radial"),
                             scaling = c("variable", "observation", "data", "none"),
                             displayOrder = NULL, axesLabels = NULL,
                             right = TRUE, ...) {

  axesLayout <- match.arg(axesLayout)
  scaling <- match.arg(scaling)

  ggplot2::ggproto("CoordSerialaxes",
                   ggplot2::CoordCartesian,
                   axesLayout = axesLayout,
                   scaling = scaling,
                   displayOrder = displayOrder,
                   axesLabels = axesLabels,
                   right = right,
                   ...
  )
}

#' @export
ggplot_build.gg <- function(plot) {

  object <- plot$coordinates
  # regular call
  if(!inherits(object, "CoordSerialaxes")) return(ggplot_build_ggplot(plot))

  plot$coordinates <- ggplot2::coord_cartesian()
  plot <- undate_CoordSerialaxes(plot, object)
  ggplot_build_ggplot(plot)
}

undate_CoordSerialaxes <- function(p, object) {

  if(length(p$layers) > 0) {
    p <- serialaxes_layers(p, object)
  } else {
    warning("No layers are detected. Did you forget to add the `geom_path()` object?",
            call. = FALSE)
  }

  suppressMessages(

    switch(
      object$axesLayout,
      "parallel" = {
        ggParallelAes(p,
                      axesLabels = object$axesLabels)
      },
      "radial" = {
        ggRadialAes(p,
                    axesLabels = object$axesLabels)
      }
    )
  )
}

serialaxes_layers <- function(p, object) {

  layers <- p$layers
  # remove all layers
  p$layers <- list()

  if(length(layers) == 0) return(p)

  n <- length(layers)

  for(i in seq(n)) {
    layer <- layers[[i]]
    p <- add_serialaxes_layers(layer, p, object)
  }

  return(p)
}
