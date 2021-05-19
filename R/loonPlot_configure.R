loonPlot_configure <- function(isCoordPolar, loonPlot, loonLayers,
                               layerId, scaleToFun, activeGeomLayers,
                               ggGuides, panelIndex, ggplotPanelParams,
                               swapAxes, theme, panX, panY, deltaX, deltaY, zoomX, zoomY) {
  # draw ggGuides?
  if (isCoordPolar) {
    if (inherits(loonPlot, "l_hist")) {
      if(panelIndex == 1)
        warning("l_hist only works with Cartesian coordinates", call. = FALSE)
    } else {
      if (ggGuides) {
        polarGuides <- polarGuides(loonPlot, ggplotPanelParams[[panelIndex]], swapAxes, theme)
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

      if(layerId > 0) {
        warning("`layerId` only works on Cartesian Coordinate", call. = FALSE)
      }
      loon::l_scaleto_world(loonPlot)
    }
  } else {
    if (ggGuides) {
      CartesianGuides <- CartesianGuides(loonPlot, ggplotPanelParams[[panelIndex]], swapAxes, theme)
      # lower to bottom
      children <- loon::l_layer_getChildren(loonPlot)
      # the length of children is at least two
      sapply(seq_len(length(children) - 1),
             function(l){
               loon::l_layer_lower(loonPlot, CartesianGuides)
             })

      if(layerId == 0)
        loon::l_scaleto_world(loonPlot)
    }
  }

  # in polar coord, scales are fixed; ggGuides do not need to set scales
  if(layerId == 0) {
    if (!isCoordPolar && !ggGuides) {
      loon::l_configure(loonPlot,
                        panX=panX,
                        panY=panY,
                        deltaX= deltaX,
                        deltaY=deltaY,
                        zoomX = zoomX,
                        zoomY = zoomY)
    }
  } else {

    if(length(loonLayers) > 0) {

      for(j in seq(length(loonLayers))) {


        if(layerId != j) next

        layer <- loonLayers[[j]]

        if(j %in% activeGeomLayers) {
          # scale to interactive layer
          scaleToFun <- scaleToFun %||% loon::l_scaleto_plot
          tryCatch(
            scaleToFun(loonPlot),
            error = function(e) {
              warning("Not valid `scaleToFun`",
                      call. = FALSE)
            }
          )
        } else {
          scaleToFun <- scaleToFun %||% loon::l_scaleto_layer
          # layer could be NULL, if the input geom layer data is 0 dimension
          if(!is.null(layer))
            tryCatch(
              scaleToFun(loonPlot, layer),
              error = function(e) {
                warning("This `scaleToFun` cannot be applied on a layer geometric visual",
                        call. = FALSE)
              }
            )
        }
      }
    }
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

  if(is.na(background.color))
    background.color <- loonPlot['background']
  if(is.na(text.color))
    text.color <- loonPlot['foreground']
  if(is.na(panel.background_fill))
    panel.background_fill <- loonPlot['background']
  if(is.na(panel.guideline_color))
    panel.guideline_color <- loonPlot['guidelines']

  loon::l_configure(loonPlot,
                    background = background.color,
                    foreground = text.color,
                    guidesBackground = panel.background_fill,
                    guidelines = panel.guideline_color)
}
