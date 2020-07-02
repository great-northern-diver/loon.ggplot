loonPlot_configure <- function(isCoordPolar, loonPlot, ggGuides, panelIndex, ggplotPanel_params,
                               swapAxes, theme, panX, panY, deltaX, deltaY, zoomX, zoomY){
  # draw ggGuides?
  if (isCoordPolar) {
    if (inherits(loonPlot, "l_hist")) {
      if(panelIndex == 1)
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
        if(panelIndex == 1)
          message("Is it hard to understand the graphics? Try \"ggGuides = TRUE\"!")
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

  ######################################## set theme ########################################
  if(length(theme) == 0) {
    # default theme: white guide lines, grey92 guidesBackground
    background.color <- loonPlot['background']
    text.color <- loonPlot['foreground']
    panel.background_fill <- loonPlot['guidesBackground']
    panel.guideline_color <- loonPlot['guidelines']
  } else {

    background.color <- theme$plot.background$colour %||% loonPlot['background']
    text.color <- theme$text$colour %||% loonPlot['foreground']

    panel.background_fill <- theme$panel.background$fill %||% {
      if(is(theme$panel.background, "NULL")) {
        if(is(theme$rect, "element_blank"))
          loon::l_getOption("background")
        else
          loon::l_getOption("guidesBackground")
      } else
        loon::l_getOption("background")
    }

    panel.guideline_color <- if(is.null(theme$panel.grid$colour)) {
      major_guideline_color <- hex6to12(theme$panel.grid.major$colour)
      minor_guideline_color <- hex6to12(theme$panel.grid.minor$colour)
      if(major_guideline_color != "") {
        major_guideline_color
      } else if(minor_guideline_color != "") {
        minor_guideline_color
      } else loonPlot['guidelines']
    } else {
      if(length(theme$panel.grid.major) == 0 & length(theme$panel.grid.minor) == 0)
        loon::l_getOption("background")
      else
        hex6to12(theme$panel.grid$colour)
    }
  }

  if(is.na(background.color)) background.color <- loonPlot['background']
  if(is.na(text.color)) text.color <- loonPlot['foreground']
  if(is.na(panel.background_fill)) panel.background_fill <- loonPlot['guidesBackground']
  if(is.na(panel.guideline_color)) panel.guideline_color <- loonPlot['guidelines']

  loon::l_configure(loonPlot,
                    background = background.color,
                    foreground = text.color,
                    guidesBackground = panel.background_fill,
                    guidelines = panel.guideline_color)
}
