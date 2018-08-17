#' @title active geom layers
#' @description `active_geomLayers` will return the geom layer index which can be active
#' @param ggplotObject a ggplot object
#' @details `loon.ggplot` has an argument called `active_geomLayers`. It is a vector to determine which geom layers can be active.
#' The default setting is `integer(0)`, however, `loon.ggplot` will automatically search the first `geom_histogram` or `geom_point` layer
#' to make it active. `active_geomLayers` is more like a guide and give you an idea which one can be set as active.
#'
#' @seealso \code{\link{loon.ggplot}}
#'
#' @examples
#' df <- data.frame(x = 1:3, y = 1:3, colour = c(1,3,5))
#' xgrid <- with(df, seq(min(x), max(x), length = 50))
#' interp <- data.frame(
#'   x = xgrid,
#'   y = approx(df$x, df$y, xout = xgrid)$y,
#'   colour = approx(df$x, df$colour, xout = xgrid)$y
#' )
#' p1 <- ggplot(data = df, aes(x, y, colour = colour)) +
#'   geom_line(interp, mapping = aes(x, y, colour = colour), size = 2) +
#'   geom_point(size = 5)
#' agL <- active_geomLayers(p1)
#' loon.ggplot(p1, active_geomLayers = agL$geom_point)
#'
#'
#' p2 <- ggplot(economics) +
#'   geom_rect(
#'     aes(xmin = start, xmax = end, fill = party),
#'     ymin = -Inf, ymax = Inf, alpha = 0.2,
#'     data = presidential
#'   ) +
#'   geom_text(
#'     aes(x = start, y = 2500, label = name),data = presidential,
#'     size = 3, vjust = 0, hjust = 0, nudge_x = 50
#'   ) +
#'   geom_line(aes(date, unemploy)) +
#'   scale_fill_manual(values = c("blue", "red"))
#' # none can be active
#' active_geomLayers(p2)
#' #transparency is not allowed in tcltk
#' loon.ggplot(p2, ggGuides = TRUE, active_geomLayers = integer(0))
#'
#' @export
active_geomLayers <- function(ggplotObject){
  len_layers <- length(ggplotObject$layers)
  importantLayers <- importantLayers(len_layers, ggplotObject)

  if(length(importantLayers$pointLayers) == 0 & length(importantLayers$histogramLayers) == 0) {
    message("no layers can be active")
    list()
  } else if(length(importantLayers$pointLayers) > 0 & length(importantLayers$histogramLayers) == 0){
    list(
      geom_point = importantLayers$pointLayers
    )
  }else if(length(importantLayers$pointLayers) == 0 & length(importantLayers$histogramLayers) > 0){
    list(
      geom_histogram = importantLayers$histogramLayers
    )
  } else {
    list(
      geom_point = importantLayers$pointLayers,
      geom_histogram = importantLayers$histogramLayers
    )
  }
}
