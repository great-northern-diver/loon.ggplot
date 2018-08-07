loonPlot_configure <- function(isCoordPolar, loonPlot, ggGuides, panelIndex, ggplotPanel_params,
                               swapAxes, theme, panX, panY, deltaX, deltaY, zoomX, zoomY){
  # draw ggGuides?
  if (isCoordPolar) {
    if ("l_hist" %in% class(loonPlot)) {
      warning("l_hist only works with Cartesian coordinates\n")
    } else {
      if (ggGuides) {
        polarGuides <- polarGuides(loonPlot, ggplotPanel_params[[panelIndex]], swapAxes, theme)
        # lower to bottom
        children <- l_layer_getChildren(loonPlot)
        # the length of children is at least two
        sapply(1:(length(children) - 1),
               function(l){
                 l_layer_lower(loonPlot, polarGuides)
               })
      } else {
        message("Is it hard to underatand this graphics? Try \"ggGuides = TRUE\"!\n")
      }

      l_scaleto_world(loonPlot)
    }
  } else {
    if (ggGuides) {
      CartesianGuides <- CartesianGuides(loonPlot, ggplotPanel_params[[panelIndex]], swapAxes, theme)
      # lower to bottom
      children <- l_layer_getChildren(loonPlot)
      # the length of children is at least two
      sapply(seq_len(length(children) - 1),
             function(l){
               l_layer_lower(loonPlot, CartesianGuides)
             })
      l_scaleto_world(loonPlot)
    }
  }

  # in polar coord, scales are fixed; ggGuides do not need to set scales
  if (!isCoordPolar & !ggGuides) {
    l_configure(loonPlot,
                panX=panX,
                panY=panY,
                deltaX= deltaX,
                deltaY=deltaY,
                zoomX = zoomX,
                zoomY = zoomY)
  }

  # set theme
  background.color <- if (is.null(theme$plot.background$colour)) {
    loonPlot['background']
  } else hex6to12(theme$plot.background$colour)

  text.color <- if (is.null(theme$text$colour)) {
    loonPlot['foreground']
  } else hex6to12(theme$text$colour)

  panel.background_fill <- if(is.null(theme$panel.background$fill))  {
    loonPlot['guidesBackground']
  } else hex6to12(theme$panel.background$fill)

  panel.guideline_color <- if(is.null(theme$panel.grid$colour)) {
    loonPlot['guidelines']
  } else hex6to12(theme$panel.grid$colour)

  l_configure(loonPlot,
              background = background.color,
              foreground = text.color,
              guidesBackground = panel.background_fill,
              guidelines = panel.guideline_color)
}
