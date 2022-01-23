#' @rdname loon2ggplot
#' @export
loon2ggplot.l_facet_ggplot <- function(target, asAes = TRUE, selectedOnTop = TRUE,
                                       showNearestColor = FALSE, ...) {

  tryCatch(
    expr = {
      subtitles <- l_getSubtitles(target)
      if(subtitles$FacetWrap) {
        loon2ggplot.l_facet_wrap(target, asAes = asAes, selectedOnTop = selectedOnTop,
                                 showNearestColor = showNearestColor, subtitles = subtitles,
                                 ...)
      } else if(subtitles$FacetGrid) {
        loon2ggplot.l_facet_grid(target, asAes = asAes, selectedOnTop = selectedOnTop,
                                 showNearestColor = showNearestColor, subtitles = subtitles,
                                 ...)
      } else stop("It is not an l_facet_ggplot object", call. = FALSE)
    },
    error = function(e) {

      warning(e$message,
              ". The plots will be constructed by `patchwork`.", call. = FALSE)

      loon2ggplot.l_compound(target, asAes = asAes, selectedOnTop = selectedOnTop,
                             showNearestColor = showNearestColor, ...)
    }
  )
}
