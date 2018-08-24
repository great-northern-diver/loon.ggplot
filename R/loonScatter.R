loonScatter <- function(ggBuild, ggplotObject, ggplotPanel_params, panelIndex, mapping.x, mapping.y, dataFrame,
                        activeGeomLayers, isCoordPolar, toplevel, showGuides, showScales, swapAxes, linkingKey,
                        args, showLabels, xlabel, ylabel, loonTitle){
  if(length(activeGeomLayers) !=0) {
    # combine points data
    combined.pointsData <- lapply(activeGeomLayers,
                                  function(k){
                                    Layer.k <- ggBuild$data[[k]]
                                    data <- Layer.k[Layer.k$PANEL == panelIndex, ]
                                    x <- data$x
                                    y <- data$y
                                    if(length(x) == 0 & length(y) == 0){
                                      label <- NULL
                                      color <- NULL
                                      glyph <- NULL
                                      size <- NULL
                                    } else {
                                      label <- data$label
                                      color <- sapply(1:dim(data)[1],
                                                      function(j){
                                                        if(data$shape[j] %in% 21:24 ){
                                                          hex6to12(data$fill[j])
                                                        }else {
                                                          hex6to12(data$colour[j])
                                                        }
                                                      })
                                      glyph <- pch_to_glyph(data$shape, data$alpha)
                                      size <- as_loon_size( data$size , "points" )
                                    }


                                    if (is.null(label)) {
                                      if(length(x) != 0 & length(y) != 0) {
                                        label <- paste0("item", seq_len(length(x)) - 1, "panel", panelIndex)
                                        warning("item label may not match\n")
                                      }
                                    }

                                    data.frame(x = x, y = y, label = label, color = color, glyph = glyph, size = size)
                                  })

    combined.pointsData <- do.call(rbind, combined.pointsData)
    combined.pointsData$color <- as.character( combined.pointsData$color)
    combined.pointsData$glyph <- as.character( combined.pointsData$glyph)
    # linkingKey
    combined.pointsData$itemLabel <- combined.pointsData$linkingKey <- as.character(combined.pointsData$label)

  } else {
    combined.pointsData <- data.frame(x = with(dataFrame, eval(parse(text = mapping.x))),
                                      y = with(dataFrame, eval(parse(text = mapping.y))))
    # some default settings, need more thought
    if(length(combined.pointsData$x) > 0 & length(combined.pointsData$y) > 0) {
      combined.pointsData$x <- as.numeric(combined.pointsData$x)
      combined.pointsData$y <- as.numeric(combined.pointsData$y)
      combined.pointsData$size <- 3
      combined.pointsData$color <- "black"
      combined.pointsData$glyph <- "circle"
      # linkingKey
      combined.pointsData$itemLabel <- combined.pointsData$linkingKey <- linkingKey
    }
  }

  if(dim(combined.pointsData)[1] > 0) {
    # TODO a loon bug cannot handle a single point
    if(dim(combined.pointsData)[1] == 1) {
      combined.pointsData <- combined.pointsData[rep(1,2), ]
      combined.pointsData[1, ]$linkingKey <- combined.pointsData[1, ]$itemLabel <- dim(do.call(rbind, ggBuild$data))[1] + panelIndex
    }
    if(isCoordPolar) {
      coordPolarxy <- Cartesianxy2Polarxy(NULL,
                                          coordinates = ggplotObject$coordinates,
                                          data = combined.pointsData,
                                          ggplotPanel_params = ggplotPanel_params[[panelIndex]])
      x <- coordPolarxy$x
      y <- coordPolarxy$y
    } else {
      x <- combined.pointsData$x
      y <- combined.pointsData$y
    }
    # loon scatter plot
    l_plot(parent = toplevel,
           x = x,
           y = y,
           size = combined.pointsData$size,
           color = combined.pointsData$color,
           glyph = combined.pointsData$glyph,
           itemLabel = combined.pointsData$itemLabel,
           linkingKey = combined.pointsData$linkingKey,
           showGuides = showGuides,
           showScales = showScales,
           showLabels = showLabels,
           showItemLabels = TRUE,
           swapAxes = swapAxes,
           linkingGroup = args$linkingGroup,
           xlabel = if(is.null(xlabel)) "" else xlabel,
           ylabel = if(is.null(ylabel)) "" else ylabel,
           title = loonTitle)
  } else {
    l_plot(parent = toplevel,
           showGuides = showGuides,
           showScales = showScales,
           showLabels = showLabels,
           showItemLabels = TRUE,
           swapAxes = swapAxes,
           linkingGroup = args$linkingGroup,
           xlabel = if(is.null(xlabel)) "" else xlabel,
           ylabel = if(is.null(ylabel)) "" else ylabel,
           title = loonTitle)
  }

}
