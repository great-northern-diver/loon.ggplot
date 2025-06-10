#' @title active geom layers
#' @description `get_activeGeomLayers` will return the geom layer index which can be active
#' @param ggObj a ggplot object
#' @details `ggplot2loon` has an argument called `activeGeomLayers`. It is a vector to determine which geom layers can be active.
#' The default setting is `integer(0)`, however, `ggplot2loon` will automatically search the first `geom_histogram` or `geom_point` layer
#' to make it active. `get_activeGeomLayers` is more like a guidance and give us a hint which one can be set as active.
#'
#' @seealso \code{\link{ggplot2loon}}
#'
#' @return a numerical vector of indicies (which layer can be interactive)
#'
#' @examples
#' if(interactive()) {
#' \donttest{
#'   df <- data.frame(x = 1:3, y = 1:3, colour = c(1,3,5))
#'   xgrid <- with(df, seq(min(x), max(x), length = 50))
#'   interp <- data.frame(
#'     x = xgrid,
#'     y = approx(df$x, df$y, xout = xgrid)$y,
#'     colour = approx(df$x, df$colour, xout = xgrid)$y
#'   )
#'   p1 <- ggplot(data = df, aes(x, y, colour = colour)) +
#'     geom_line(interp, mapping = aes(x, y, colour = colour), size = 2) +
#'     geom_point(size = 5)
#'   agL <- get_activeGeomLayers(p1)
#'   ggplot2loon(p1, activeGeomLayers = agL)
#'
#'   p2 <- ggplot(economics) +
#'     geom_rect(
#'       aes(xmin = start, xmax = end, fill = party),
#'       ymin = -Inf, ymax = Inf, alpha = 0.2,
#'       data = presidential
#'     ) +
#'     geom_text(
#'       aes(x = start, y = 2500, label = name),data = presidential,
#'       size = 3, vjust = 0, hjust = 0, nudge_x = 50
#'     ) +
#'     geom_line(aes(date, unemploy)) +
#'     scale_fill_manual(values = c("blue", "red"))
#'   # none can be interactive
#'   agL <- get_activeGeomLayers(p2)
#'   #transparency is not allowed in tcltk
#'   ggplot2loon(p2, ggGuides = TRUE, activeGeomLayers = agL)
#' }
#' }
#' @export
get_activeGeomLayers <- function(ggObj) {

  if(!ggplot2::is.ggplot(ggObj)) {
    stop(deparse(substitute(ggObj)), " is not a ggplot object.", call. = FALSE)
  }
  len_layers <- length(ggObj$layers)

  modelLayers <- get_modelLayers(len_layers, ggObj,
                                 isCoordPolar = is.CoordPolar(ggObj$coordinates),
                                 isCoordSerialaxes = is.CoordSerialaxes(ggObj$coordinates))

  np <- length(modelLayers$pointLayers)
  nh <- length(modelLayers$histogramLayers)

  if(np == 0 & nh == 0) {
    message("no layers can be interactive")
    integer(0)
  } else if(np > 0 & nh == 0) {

    setNames(modelLayers$pointLayers, rep("l_point", np))

  } else if(np == 0 & nh > 0) {

    setNames(modelLayers$histogramLayers, rep("l_hist", nh))

  } else {

    setNames(c(modelLayers$histogramLayers, modelLayers$histogramLayers),
             c(rep("l_point", np), rep("l_hist", nh)))
  }
}
