loonPlot_configure <- function(isCoordPolar, loonPlot, ggGuides, panelIndex, ggplotPanel_params,
                               swapAxes, theme, panX, panY, deltaX, deltaY, zoomX, zoomY){
  # draw ggGuides?
  if (isCoordPolar) {
    if ("l_hist" %in% class(loonPlot)) {
      warning("l_hist only works with Cartesian coordinates", call. = FALSE)
    } else {
      if (ggGuides) {
        polarGuides <- polarGuides(loonPlot, ggplotPanel_params[[panelIndex]], swapAxes, theme)
        # lower to bottom
        children <- loon::l_layer_getChildren(loonPlot)
        # the length of children is at least two
        sapply(1:(length(children) - 1),
               function(l){
                 loon::l_layer_lower(loonPlot, polarGuides)
               })
      } else {
        message("Is it hard to understand this graphics? Try \"ggGuides = TRUE\"!\n")
      }

      loon::l_scaleto_world(loonPlot)
    }
  } else {
    if (ggGuides) {
      CartesianGuides <- CartesianGuides(loonPlot, ggplotPanel_params[[panelIndex]], swapAxes, theme)
      # lower to bottom
      children <- loon::l_layer_getChildren(loonPlot)
      # the length of children is at least two
      sapply(seq_len(length(children) - 1),
             function(l){
               loon::l_layer_lower(loonPlot, CartesianGuides)
             })
      loon::l_scaleto_world(loonPlot)
    }
  }

  # in polar coord, scales are fixed; ggGuides do not need to set scales
  if (!isCoordPolar & !ggGuides) {
    loon::l_configure(loonPlot,
                      panX=panX,
                      panY=panY,
                      deltaX= deltaX,
                      deltaY=deltaY,
                      zoomX = zoomX,
                      zoomY = zoomY)
  }

  # set theme
  if(length(theme) == 0) {
    # default theme: white guide lines, grey92 guidesBackground
    background.color <- loonPlot['background']
    text.color <- loonPlot['foreground']
    panel.background_fill <- loonPlot['guidesBackground']
    panel.guideline_color <- loonPlot['guidelines']
  } else {
    background.color <- if (is.null(theme$plot.background$colour)) {
      loonPlot['background']
    } else hex6to12(theme$plot.background$colour)

    text.color <- if (is.null(theme$text$colour)) {
      loonPlot['foreground']
    } else hex6to12(theme$text$colour)

    panel.background_fill <- if(is.null(theme$panel.background$fill)) {
      if(is(theme$panel.background, "NULL")) {
        if(is(theme$rect, "element_blank")) "white" else "grey92"
      } else "white"
    } else hex6to12(theme$panel.background$fill)

    panel.guideline_color <- if(is.null(theme$panel.grid$colour)) {
      major_guideline_color <- hex6to12(theme$panel.grid.major$colour)
      minor_guideline_color <- hex6to12(theme$panel.grid.minor$colour)
      if(major_guideline_color != "") {
        major_guideline_color
      } else if(minor_guideline_color != "") {
        minor_guideline_color
      } else loonPlot['guidelines']
    } else {
      if(length(theme$panel.grid.major) == 0 & length(theme$panel.grid.minor) == 0) {
        "white"
      } else hex6to12(theme$panel.grid$colour)
    }
  }

  loon::l_configure(loonPlot,
                    background = background.color,
                    foreground = text.color,
                    guidesBackground = panel.background_fill,
                    guidelines = panel.guideline_color)
}
