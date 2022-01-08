l_scatterplot <- function(ggBuild, ggObj, ggplotPanelParams, panelIndex, mapping, dataFrame,
                          activeGeomLayers, isCoordPolar, parent, showGuides, showScales, swapAxes, showLabels,
                          showItemLabels, linkingKey, itemLabel, nDimStates, xlabel, ylabel, loonTitle, args) {

  if(length(activeGeomLayers) > 0) {
    # combine points data
    count <- 0
    activeGeomDim <- activeGeomDim(ggBuild, activeGeomLayers, panelIndex)
    len_linkingKey <- length(linkingKey)
    len_itemLabel <- length(itemLabel)

    combinedPointsData <- lapply(activeGeomLayers,
                                 function(activeGeomLayer) {

                                   layerData <- ggBuild$data[[activeGeomLayer]]
                                   id <- layerData$PANEL == panelIndex
                                   data <- layerData[id, ]
                                   num <- nrow(data)
                                   x <- data$x
                                   y <- data$y
                                   z <- data$z

                                   if(num > 0) {
                                     activeLayer_itemLabel <- data$itemLabel
                                     activeLayer_linkingKey <- data$linkingKey
                                     color <- sapply(seq(num),
                                                     function(j) {

                                                       if(is.null(data$shape[j]))
                                                         return(data$colour[j] %||% data$fill[j])

                                                       if(data$shape[j] %in% 21:24) {
                                                         if (is.na(data$fill[j]) || is.null(data$fill[j]))
                                                           data$colour[j]
                                                         else
                                                           data$fill[j]
                                                       } else {
                                                         if (is.na(data$colour[j]) || is.null(data$colour[j]))
                                                           data$fill[j]
                                                         else
                                                           data$colour[j]
                                                       }
                                                     })
                                     glyph <- pch_to_glyph(data$shape, data$alpha)
                                     size <- as_loon_size(data$size , "points",
                                                          stroke = data$stroke)
                                   } else {
                                     activeLayer_linkingKey <- NULL
                                     activeLayer_itemLabel <- NULL
                                     color <- NULL
                                     glyph <- NULL
                                     size <- NULL
                                   }

                                   if (is.null(activeLayer_itemLabel) || is.null(activeLayer_linkingKey)) {
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

                                   if(is.null(z)) {
                                     # n (n > 0) dimension
                                     if(length(x) > 0) z <- NA
                                   }

                                   d <- data.frame(x = x,
                                                   y = y,
                                                   z = z,
                                                   itemLabel = activeLayer_itemLabel,
                                                   color = color,
                                                   glyph = glyph,
                                                   size = size,
                                                   linkingKey = activeLayer_linkingKey,
                                                   stringsAsFactors = FALSE)
                                   modify_n_dim_data(nDimStates, d, id)
                                 }
    )

    combinedPointsData <- do.call(rbind, combinedPointsData)
    if(!is.null(combinedPointsData$color) && all(is.na(combinedPointsData$color))) {
      # ALL NA
      if(panelIndex == 1)
        warning("No legal colors. The color will be set as the default loon color ",
                loon::l_getOption("color"),
                call. = FALSE)
      combinedPointsData$color <- loon::l_getOption("color")
    }

    isDuplicated_linkingKey <- duplicated(combinedPointsData$linkingKey)
    if(any(isDuplicated_linkingKey)) {
      combinedPointsData$linkingKey <- 0:(dim(combinedPointsData)[1] - 1)
      # generate warning once
      if(panelIndex == 1)
        warning("linkingKey may not match and will be set as the default loon one",
                call. = FALSE)
    }
  } else {
    # activeGeomLayers is 0
    # used for boxplot
    # an issue may occur
    # considering this
    # ggplot() + geom_boxplot(data, mapping = aes())
    # the boxplot may not be interactive
    if(is.null(mapping$x) && !is.null(mapping$y)) {

      combinedPointsData <- data.frame(x = rep(0, dim(dataFrame)[1]),
                                       y = rlang::eval_tidy(rlang::quo(!!mapping$y),  dataFrame))
    } else if(!is.null(mapping$x) && is.null(mapping$y)) {

      combinedPointsData <- data.frame(x = rlang::eval_tidy(rlang::quo(!!mapping$x),  dataFrame),
                                       y = rep(0, dim(dataFrame)[1]))
    } else {
      # both zero or both non zero
      combinedPointsData <- data.frame(x = rlang::eval_tidy(rlang::quo(!!mapping$x),  dataFrame),
                                       y = rlang::eval_tidy(rlang::quo(!!mapping$y),  dataFrame))
    }

    # some default settings, need more thought
    if(length(combinedPointsData$x) > 0 & length(combinedPointsData$y) > 0) {

      if(!is.numeric(combinedPointsData$x))
        combinedPointsData$x <- as.numeric(factor(combinedPointsData$x))
      if(!is.numeric(combinedPointsData$y))
        combinedPointsData$y <- as.numeric(factor(combinedPointsData$y))

      combinedPointsData$y <- as.numeric(combinedPointsData$y)
      combinedPointsData$size <- loon_default_setting("size")
      combinedPointsData$color <- loon_default_setting("color")
      combinedPointsData$glyph <- loon_default_setting("glyph")
      combinedPointsData$itemLabel <- itemLabel
      combinedPointsData$linkingKey <- linkingKey
    }
  }

  # 3D plot
  if(is.null(combinedPointsData$z)) {

    plot <- loon::l_plot
    zlabel <- NULL

  } else {

    if(all(is.na(combinedPointsData$z))) {

      # a loon 2D plot
      combinedPointsData$z <- NULL

      plot <- loon::l_plot
      zlabel <- NULL

    } else {

      plot <- loon::l_plot3D
      zlabel <- rlang::as_label(mapping$z)
    }
  }

  # remove NA
  # combinedPointsData <- na.omit(combinedPointsData)

  if(dim(combinedPointsData)[1] > 0) {

    if(isCoordPolar) {
      coordPolarxy <- Cartesianxy2Polarxy(NULL,
                                          coordinates = ggObj$coordinates,
                                          data = combinedPointsData,
                                          ggplotPanelParams = ggplotPanelParams[[panelIndex]])
      x <- coordPolarxy$x
      y <- coordPolarxy$y

      combinedPointsData$x <- x
      combinedPointsData$y <- y

      if(!is.null(combinedPointsData$z)) {
        warning("The `l_plot3D` object does not accommodate the Cartesian coordinate",
                call. = FALSE)
        combinedPointsData$z <- NULL
      }
    }

    plotList <- remove_null(
      c(
        list(
          parent = parent,
          showGuides = showGuides,
          showScales = showScales,
          showLabels = showLabels,
          showItemLabels = showItemLabels,
          swapAxes = swapAxes,
          xlabel = if(is.null(xlabel)) "" else xlabel,
          ylabel = if(is.null(ylabel)) "" else ylabel,
          zlabel = zlabel,
          title = loonTitle
        ),
        combinedPointsData
      ), as_list = FALSE)

    # loon scatter plot (2D or 3D)
    p <- do.call(plot, plotList)

    # widget is returned
    tryCatch(
      expr = {
        add_glyph(p, ggBuild, activeGeomLayers)
      },
      error = function(e) {

        if(length(activeGeomLayers) > 1) {
          warning("If the non-primitive glyphs are set (e.g. serialaxes, polygon, text, ...), ",
                  "the length of the `activeGeomLayers` can only be set as 1. Currently, it is ",
                  length(activeGeomLayers), ".",
                  call. = FALSE
          )
        }
      }
    )

    p

  } else {

    loon::l_plot(parent = parent,
                 showGuides = showGuides,
                 showScales = showScales,
                 showLabels = showLabels,
                 showItemLabels = showItemLabels,
                 swapAxes = swapAxes,
                 xlabel = if(is.null(xlabel)) "" else xlabel,
                 ylabel = if(is.null(ylabel)) "" else ylabel,
                 title = loonTitle)
  }
}

activeGeomDim <- function(ggBuild, activeGeomLayers, panelIndex) {
  length(l_plot_indices(ggBuild, activeGeomLayers, panelIndex))
}
