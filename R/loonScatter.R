loonScatter <- function(ggBuild, ggObj, ggplotPanel_params, panelIndex, mapping, dataFrame,
                        activeGeomLayers, isCoordPolar, parent, showGuides, showScales, swapAxes, linkingKey,
                        itemLabel, showLabels, xlabel, ylabel, loonTitle){
  if(length(activeGeomLayers) > 0) {
    # combine points data
    count <- 0
    activeGeomDim <- activeGeomDim(ggBuild, activeGeomLayers, panelIndex)
    len_linkingKey <- length(linkingKey)
    len_itemLabel <- length(itemLabel)
    combined.pointsData <- lapply(activeGeomLayers,
                                  function(activeGeomLayer){
                                    thisLayer <- ggBuild$data[[activeGeomLayer]]
                                    data <- thisLayer[thisLayer$PANEL == panelIndex, ]
                                    num <- dim(data)[1]
                                    x <- data$x
                                    y <- data$y
                                    if(num != 0){
                                      thisLayer_itemLabel <- data$itemLabel
                                      thisLayer_linkingKey <- data$linkingKey
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
                                      thisLayer_linkingKey <- NULL
                                      thisLayer_itemLabel <- NULL
                                      color <- NULL
                                      glyph <- NULL
                                      size <- NULL
                                    }

                                    if (is.null(thisLayer_itemLabel) | is.null(thisLayer_linkingKey)) {
                                      if(num != 0) {
                                        thisLayer_linkingKey <- if(activeGeomDim == len_linkingKey) {
                                          linkingKey[(count : (count + num - 1)) + 1]
                                        } else {
                                          count : (count + num - 1)
                                        }
                                        thisLayer_itemLabel <- if(activeGeomDim == len_itemLabel) {
                                          itemLabel[(count : (count + num - 1)) + 1]
                                        } else {
                                          paste0("item", thisLayer_linkingKey)
                                        }
                                        count <<- count + num
                                      }
                                    }

                                    data.frame(x = x,
                                               y = y,
                                               itemLabel = thisLayer_itemLabel,
                                               color = color,
                                               glyph = glyph,
                                               size = size,
                                               linkingKey = thisLayer_linkingKey)
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
      combined.pointsData$x <- as.numeric(combined.pointsData$x)
      combined.pointsData$y <- as.numeric(combined.pointsData$y)
      combined.pointsData$size <- 3
      combined.pointsData$color <- "black"
      combined.pointsData$glyph <- "circle"
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
  dim <- 0
  lapply(activeGeomLayers,
         function(activeGeomLayer){
           thisLayer <- ggBuild$data[[activeGeomLayer]]
           data <- thisLayer[thisLayer$PANEL == panelIndex, ]
           dim <<- dim + dim(data)[1]
         }
  )
  dim
}
