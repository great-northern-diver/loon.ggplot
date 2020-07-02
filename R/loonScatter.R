loonScatter <- function(ggBuild, ggObj, ggplotPanel_params, panelIndex, mapping, dataFrame,
                        activeGeomLayers, isCoordPolar, parent, showGuides, showScales, swapAxes, linkingKey,
                        itemLabel, showLabels, xlabel, ylabel, loonTitle) {

  if(length(activeGeomLayers) > 0) {
    # combine points data
    count <- 0
    activeGeomDim <- activeGeomDim(ggBuild, activeGeomLayers, panelIndex)
    len_linkingKey <- length(linkingKey)
    len_itemLabel <- length(itemLabel)
    combined.pointsData <- lapply(activeGeomLayers,
                                  function(activeGeomLayer) {

                                    activeLayer <- ggBuild$data[[activeGeomLayer]]
                                    data <- activeLayer[activeLayer$PANEL == panelIndex, ]
                                    num <- dim(data)[1]
                                    x <- data$x
                                    y <- data$y

                                    if(num != 0) {
                                      activeLayer_itemLabel <- data$itemLabel
                                      activeLayer_linkingKey <- data$linkingKey
                                      color <- sapply(1:dim(data)[1],
                                                      function(j){
                                                        if(data$shape[j] %in% 21:24 ){
                                                          data$fill[j]
                                                        }else {
                                                          data$colour[j]
                                                        }
                                                      })
                                      glyph <- pch_to_glyph(data$shape, data$alpha)
                                      size <- as_loon_size( data$size , "points" )
                                    } else {
                                      activeLayer_linkingKey <- NULL
                                      activeLayer_itemLabel <- NULL
                                      color <- NULL
                                      glyph <- NULL
                                      size <- NULL
                                    }

                                    if (is.null(activeLayer_itemLabel) | is.null(activeLayer_linkingKey)) {
                                      if(num != 0) {
                                        activeLayer_linkingKey <- if(activeGeomDim == len_linkingKey) {
                                          linkingKey[(count : (count + num - 1)) + 1]
                                        } else {
                                          count : (count + num - 1)
                                        }
                                        activeLayer_itemLabel <- if(activeGeomDim == len_itemLabel) {
                                          itemLabel[(count : (count + num - 1)) + 1]
                                        } else {
                                          paste0("item", activeLayer_linkingKey)
                                        }
                                        count <<- count + num
                                      }
                                    }

                                    data.frame(x = x,
                                               y = y,
                                               itemLabel = activeLayer_itemLabel,
                                               color = color,
                                               glyph = glyph,
                                               size = size,
                                               linkingKey = activeLayer_linkingKey)
                                  }
    )
    combined.pointsData <- do.call(rbind, combined.pointsData)
    combined.pointsData$color <- as.character( combined.pointsData$color)
    combined.pointsData$glyph <- as.character( combined.pointsData$glyph)
    combined.pointsData$itemLabel <- as.character(combined.pointsData$itemLabel)
    combined.pointsData$linkingKey <- as.character(combined.pointsData$linkingKey)

    isDuplicated_linkingKey <- duplicated(combined.pointsData$linkingKey)
    if(any(isDuplicated_linkingKey)) {
      combined.pointsData$linkingKey <- 0:(dim(combined.pointsData)[1] - 1)
      # generate warning once
      if(panelIndex == 1)
        warning("linkingKey may not match and will be set as the default loon one", call. = FALSE)
    }
  } else {
    # used for boxplot
    if(is.null(mapping$x) & !is.null(mapping$y)) {

      combined.pointsData <- data.frame(x = rep(0, dim(dataFrame)[1]),
                                        y = rlang::eval_tidy(rlang::quo(!!mapping$y),  dataFrame))
    } else if(!is.null(mapping$x) & is.null(mapping$y)) {

      combined.pointsData <- data.frame(x = rlang::eval_tidy(rlang::quo(!!mapping$x),  dataFrame),
                                        y = rep(0, dim(dataFrame)[1]))
    } else {
      # both zero or both non zero
      combined.pointsData <- data.frame(x = rlang::eval_tidy(rlang::quo(!!mapping$x),  dataFrame),
                                        y = rlang::eval_tidy(rlang::quo(!!mapping$y),  dataFrame))
    }

    # some default settings, need more thought
    if(length(combined.pointsData$x) > 0 & length(combined.pointsData$y) > 0) {

      if(!is.numeric(combined.pointsData$x))
        combined.pointsData$x <- as.numeric(factor(combined.pointsData$x))
      if(!is.numeric(combined.pointsData$y))
        combined.pointsData$y <- as.numeric(factor(combined.pointsData$y))

      combined.pointsData$y <- as.numeric(combined.pointsData$y)
      combined.pointsData$size <- loon_default_setting("size")
      combined.pointsData$color <- loon_default_setting("color")
      combined.pointsData$glyph <- loon_default_setting("glyph")
      combined.pointsData$itemLabel <- itemLabel
      combined.pointsData$linkingKey <- linkingKey
    }
  }

  # remove NA
  combined.pointsData <- na.omit(combined.pointsData)

  if(dim(combined.pointsData)[1] > 0) {

    if(isCoordPolar) {
      coordPolarxy <- Cartesianxy2Polarxy(NULL,
                                          coordinates = ggObj$coordinates,
                                          data = combined.pointsData,
                                          ggplotPanel_params = ggplotPanel_params[[panelIndex]])
      x <- coordPolarxy$x
      y <- coordPolarxy$y

    } else {
      x <- combined.pointsData$x
      y <- combined.pointsData$y
    }

    # loon scatter plot
    loon::l_plot(parent = parent,
                 x = x,
                 y = y,
                 size = combined.pointsData$size,
                 color = hex6to12(combined.pointsData$color),
                 glyph = combined.pointsData$glyph,
                 itemLabel = combined.pointsData$itemLabel,
                 linkingKey = combined.pointsData$linkingKey,
                 showGuides = showGuides,
                 showScales = showScales,
                 showLabels = showLabels,
                 showItemLabels = TRUE,
                 swapAxes = swapAxes,
                 xlabel = if(is.null(xlabel)) "" else xlabel,
                 ylabel = if(is.null(ylabel)) "" else ylabel,
                 title = loonTitle)
  } else {
    loon::l_plot(parent = parent,
                 showGuides = showGuides,
                 showScales = showScales,
                 showLabels = showLabels,
                 showItemLabels = TRUE,
                 swapAxes = swapAxes,
                 xlabel = if(is.null(xlabel)) "" else xlabel,
                 ylabel = if(is.null(ylabel)) "" else ylabel,
                 title = loonTitle)
  }
}

activeGeomDim <- function(ggBuild, activeGeomLayers, panelIndex) {
  length(l_plot_indices(ggBuild, activeGeomLayers, panelIndex))
}
