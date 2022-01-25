#' @export
#' @import ggmulti
#' @rdname loon2ggplot
loon2ggplot.l_layer_scatterplot <- function(target, asAes = TRUE, selectedOnTop = TRUE,
                                            showNearestColor = FALSE, ...) {


  widget <- loon::l_create_handle(attr(target, "widget"))

  args <- list(...)
  ggObj <- args$ggObj
  facets <- args$facets
  facetsLabels <- args$facetsLabels
  levels <- args$levels

  if(is.null(facets)) {
    n <- widget['n']
    if(n == 0) return(ggObj)

    states <- ggStates(widget = widget, ggObj = ggObj,
                       showNearestColor = showNearestColor,
                       selectedOnTop = selectedOnTop)

  } else {
    n <- sum(vapply(facets, function(facet) facet['n'], numeric(1L)))
    if(n == 0) return(ggObj)
    facetsVar <- rownames(facetsLabels)

    states <- do.call(rbind,
                      lapply(seq_along(facets),
                             function(i) {

                               facet <- facets[[i]]
                               states <- ggStates(widget = facet, ggObj = ggObj,
                                                  showNearestColor = showNearestColor,
                                                  selectedOnTop = selectedOnTop)
                               do.call(cbind,
                                       c(list(states),
                                         stats::setNames(as.list(facetsLabels[, i]),
                                                         facetsVar),
                                         facetGroup = i))
                             })
    )

    for (i in seq_along(facetsVar)) {
      states[[facetsVar[i]]] <- factor(states[[facetsVar[i]]],
                                       levels = levels[[i]])
    }
  }

  ggObj <- if(asAes) {
    scatterplotAsAesTRUE(ggObj = ggObj, widget = widget,
                         states = states,
                         selectedOnTop = selectedOnTop, facets = facets)
  } else {
    scatterplotAsAesFALSE(ggObj = ggObj, widget = widget,
                          states = states,
                          selectedOnTop = selectedOnTop, facets = facets)
  }

  return(ggObj)
}

ggStates <- function(widget, ggObj, showNearestColor = FALSE,
                     selectedOnTop = TRUE) {

  n <- widget['n']
  if (n == 0 || !any(widget['active'])) {
    return(
      data.frame(
        x = NA,
        y = NA,
        glyph = NA,
        color = NA,
        size = NA,
        index = NA
      )
    )
  }
  states <- get_layer_states(widget, native_unit = FALSE)
  states$color <- l_colorName(states$color, error = FALSE,
                              precise = !showNearestColor)

  # No active points in scatterplot
  displayOrder <- if(selectedOnTop) {
    get_model_display_order(widget)
  } else {
    seq(widget['n'])
  }

  active <- states$active[displayOrder]
  selected <- states$selected[displayOrder][active]

  if (widget['swapAxes']) {
    x <- as.numeric(states$y[displayOrder][active])
    y <- as.numeric(states$x[displayOrder][active])
  } else {
    x <- as.numeric(states$x[displayOrder][active])
    y <- as.numeric(states$y[displayOrder][active])
  }

  data.frame(
    x = x,
    y = y,
    glyph = states$glyph[displayOrder][active],
    color = get_display_color(states$color[displayOrder][active], selected),
    size = states$size[displayOrder][active],
    index = displayOrder[active]
  )
}
