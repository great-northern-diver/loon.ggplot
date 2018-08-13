loonScatter <- function(ggBuild, ggplotObject, ggplotPanel_params, panelIndex, mapping.x, mapping.y, dataFrame,
                        active_geomLayers, isCoordPolar, toplevel, showGuides, showScales, swapAxes, linkingKey,
                        args, showLabels, xlabel, ylabel, subtitle, title){
  if(length(active_geomLayers) !=0) {
    # combine points data
    combined.pointsData <- lapply(active_geomLayers,
                                  function(k){
                                    Layer.k <- ggBuild$data[[k]]
                                    data <- Layer.k[Layer.k$PANEL == panelIndex, ]
                                    x <- data$x
                                    y <- data$y
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

                                    if (is.null(label)) {
                                      label <- paste0("item", seq_len(length(x)) - 1, "panel", panelIndex)
                                      warning("item label may not match\n")
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
    # in case
    combined.pointsData$x <- as.numeric(combined.pointsData$x)
    combined.pointsData$y <- as.numeric(combined.pointsData$y)
    # some default settings, need more thought
    combined.pointsData$size <- 3
    combined.pointsData$color <- "black"
    combined.pointsData$glyph <- "circle"
    # linkingKey
    combined.pointsData$itemLabel <- combined.pointsData$linkingKey <- linkingKey
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
         xlabel = xlabel,
         ylabel = ylabel,
         title = paste(c(title, subtitle), collapse = "%+%"))
}
