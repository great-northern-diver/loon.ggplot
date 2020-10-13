l_loonPlot <- function(ggObj, panelIndex, args,
                       plotInfo, numOfSubtitles, modelLayers, activeInfo, index, parent,
                       showGuides, showScales, swapAxes, xlabel, ylabel, loonTitle) {


  # grab objects from the input
  isCoordSerialaxes <- plotInfo$isCoordSerialaxes
  buildggObj <- plotInfo$buildggObj
  linkingKey <- plotInfo$linkingKey
  itemLabel <- plotInfo$itemLabel

  # gg build
  ggplotPanel_params <- buildggObj$ggplotPanel_params
  ggBuild <- buildggObj$ggBuild
  layout <- buildggObj$layout

  lenLayers <- length(ggObj$layers)
  isCoordPolar <- is.CoordPolar(ggObj$coordinates)
  dataFrame <- ggObj$data

  activeGeomLayers <- activeInfo$activeGeomLayers
  activeModel <- activeInfo$activeModel

  # boxplot has a hidden scatterplot model layer
  boxplot_point_layers <- c(modelLayers$boxplotLayers, activeGeomLayers)

  if (is.data.frame(dataFrame) & !is.waive(dataFrame)) {
    mapping <- ggObj$mapping
  } else {
    if(length(activeGeomLayers) == 1) {
      dataFrame <- ggObj$layers[[activeGeomLayers]]$data
      linkingKey <- loonLinkingKey(dataFrame, args)
      itemLabel <- loonItemLabel(dataFrame, args)

      mapping <- ggObj$layers[[activeGeomLayers]]$mapping
    } else mapping <- ggplot2::aes() # activeGeomLayers > 1 not implemented so far
  }

  if(activeModel == "l_serialaxes" & length(activeGeomLayers) > 0) {

    loonSerialaxes(ggBuild = ggBuild,
                   index = index,
                   ggObj = ggObj,
                   activeGeomLayers = activeGeomLayers,
                   panelIndex = panelIndex,
                   dataFrame = dataFrame,
                   parent = parent,
                   showGuides = showGuides,
                   linkingKey = linkingKey,
                   showLabels = plotInfo$showLabels,
                   loonTitle = loonTitle)

  } else if (activeModel == "l_hist" & length(activeGeomLayers) > 0) {

    if(length(activeGeomLayers) > 1) {
      activeGeomLayers <- activeGeomLayers[1]
      warning("Two histogram layers are detected and only the ", activeGeomLayers,
              "th one is the interactive one.", call. = FALSE)
    }

    FacetWrap <- plotInfo$FacetWrap
    FacetGrid <- plotInfo$FacetGrid
    ggLayout <- buildggObj$ggLayout

    loonHistogram(ggBuild = ggBuild,
                  ggLayout = ggLayout,
                  layout = layout,
                  ggplotPanel_params = ggplotPanel_params,
                  ggObj = ggObj,
                  activeGeomLayers = activeGeomLayers,
                  panelIndex = panelIndex,
                  dataFrame = dataFrame,
                  mapping = mapping,
                  numOfSubtitles = numOfSubtitles,
                  parent = parent,
                  showGuides = showGuides,
                  showScales = showScales,
                  swapAxes = swapAxes,
                  linkingKey = linkingKey,
                  showLabels = plotInfo$showLabels,
                  xlabel = xlabel,
                  ylabel = ylabel,
                  loonTitle = loonTitle,
                  FacetWrap = FacetWrap,
                  FacetGrid = FacetGrid)


  } else if(activeModel == "l_plot" & length(boxplot_point_layers) > 0) {

    loonScatter(ggBuild = ggBuild,
                ggObj = ggObj,
                ggplotPanel_params = ggplotPanel_params,
                panelIndex = panelIndex,
                mapping = mapping,
                dataFrame = dataFrame,
                activeGeomLayers = activeGeomLayers,
                isCoordPolar = isCoordPolar,
                parent = parent,
                showGuides = showGuides,
                showScales = showScales,
                swapAxes = swapAxes,
                linkingKey = linkingKey,
                itemLabel = itemLabel,
                showLabels = plotInfo$showLabels,
                xlabel = xlabel,
                ylabel = ylabel,
                loonTitle = loonTitle,
                args = args)

  } else {

    loon::l_plot(parent = parent,
                 showGuides = showGuides,
                 showScales = showScales,
                 showLabels = plotInfo$showLabels,
                 swapAxes = swapAxes,
                 xlabel = if(is.null(xlabel)) "" else xlabel,
                 ylabel = if(is.null(ylabel)) "" else ylabel,
                 title = loonTitle)

  }
}
