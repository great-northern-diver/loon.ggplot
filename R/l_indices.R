l_indices <- function(ggObj, panelIndex, args,
                      plotInfo, numOfSubtitles, modelLayers, activeInfo) {

  # grab objects from the input
  isCoordSerialaxes <- plotInfo$isCoordSerialaxes
  buildggObj <- plotInfo$buildggObj

  # gg build
  ggplotPanelParams <- buildggObj$ggplotPanelParams
  ggBuild <- buildggObj$ggBuild
  layout <- buildggObj$layout

  lenLayers <- length(ggObj$layers)
  isCoordPolar <- is.CoordPolar(ggObj$coordinates)
  dataFrame <- ggObj$data

  activeGeomLayers <- activeInfo$activeGeomLayers
  activeModel <- activeInfo$activeModel

  # boxplot has a hidden scatterplot model layer
  boxplotPointLayers <- c(modelLayers$boxplotLayers, activeGeomLayers)

  if(activeModel == "l_serialaxes" & length(activeGeomLayers) > 0) {

    l_serialaxes_indices(ggBuild = ggBuild,
                         activeGeomLayers = activeGeomLayers,
                         panelIndex = panelIndex,
                         ggObj = ggObj)

  } else if (activeModel == "l_hist" & length(activeGeomLayers) > 0) {

    FacetWrap <- plotInfo$FacetWrap
    FacetGrid <- plotInfo$FacetGrid
    ggLayout <- buildggObj$ggLayout

    if (is.data.frame(dataFrame) & !is.waive(dataFrame)) {
      mapping <- ggObj$mapping
    } else {
      if(length(activeGeomLayers) == 1) {
        dataFrame <- ggObj$layers[[activeGeomLayers]]$data
        mapping <- ggObj$layers[[activeGeomLayers]]$mapping
      } else mapping <- ggplot2::aes() # activeGeomLayers > 1 not implemented so far
    }

    l_hist_indices(ggBuild = ggBuild,
                   activeGeomLayers = activeGeomLayers,
                   panelIndex = panelIndex,
                   mapping = mapping,
                   dataFrame = dataFrame,
                   numOfSubtitles = numOfSubtitles,
                   FacetWrap = FacetWrap,
                   FacetGrid = FacetGrid,
                   layout = layout,
                   ggLayout = ggLayout)


  } else if(activeModel == "l_plot" & length(boxplotPointLayers) > 0) {

    l_plot_indices(ggBuild = ggBuild,
                   activeGeomLayers = activeGeomLayers,
                   panelIndex = panelIndex)

  } else NULL
}

l_plot_indices <- function(ggBuild, activeGeomLayers, panelIndex) {
  unlist(
    lapply(activeGeomLayers,
           function(activeGeomLayer){
             activeLayer <- ggBuild$data[[activeGeomLayer]]
             which(activeLayer$PANEL == panelIndex)
           }
    )
  )
}

l_serialaxes_indices <- function(ggBuild, activeGeomLayers, panelIndex, ggObj) {

  # The warning is given ahead; See function `loon.ggplot:::get_modelLayers`
  activeGeomLayers <- activeGeomLayers[1]
  indices <- which(ggBuild$data[[activeGeomLayers]]$PANEL == panelIndex)
  aesData <- ggBuild$data[[activeGeomLayers]][indices, ]

  layer <- ggObj$layers[[activeGeomLayers]]
  if(inherits(layer$geom, "GeomRibbon") || inherits(layer$geom, "GeomPath")) {

    # Each line is repeated by the same times
    offset <- sum(aesData$group == aesData$group[1], na.rm = TRUE)
    unique(ceiling(indices/offset))

  } else NULL
}

# it is a wrapper of the function `catch_facet_id`
l_hist_indices <- function(ggBuild, activeGeomLayers, panelIndex, mapping, dataFrame,
                           numOfSubtitles, FacetWrap, FacetGrid,
                           layout, ggLayout) {

  hist_data <- ggBuild$data[[activeGeomLayers]]

  flipped_aes <- any(hist_data$flipped_aes) %||% FALSE

  hist_x <- if(flipped_aes) {
    rlang::eval_tidy(rlang::quo(!!mapping$y),  dataFrame)
  } else {
    rlang::eval_tidy(rlang::quo(!!mapping$x),  dataFrame)
  }

  catch_facet_id(numOfSubtitles, hist_x, FacetWrap, FacetGrid,
                 layout, ggLayout, panelIndex, dataFrame)
}
