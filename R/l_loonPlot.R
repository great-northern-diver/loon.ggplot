l_loonPlot <- function(ggObj, panelIndex, args,
                       plotInfo, numOfSubtitles, modelLayers, activeInfo, index, parent,
                       showGuides, showScales, swapAxes, xlabel, ylabel, loonTitle) {


  # grab objects from the input
  isCoordSerialaxes <- plotInfo$isCoordSerialaxes
  buildggObj <- plotInfo$buildggObj
  linkingKey <- plotInfo$linkingKey
  itemLabel <- plotInfo$itemLabel

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

  if (is.data.frame(dataFrame) && !is.waive(dataFrame)) {
    mapping <- ggObj$mapping
  } else {
    if(length(activeGeomLayers) == 1) {
      dataFrame <- ggObj$layers[[activeGeomLayers]]$data
      linkingKey <- loonLinkingKey(dataFrame, args)
      itemLabel <- loonItemLabel(dataFrame, args)

      mapping <- ggObj$layers[[activeGeomLayers]]$mapping
    } else mapping <- ggplot2::aes() # activeGeomLayers > 1 not implemented so far
  }

  # l_serialaxesplot, l_histgoram and l_scatterplot are loon.ggplot customized plots
  # they are wrappers of `loon::l_serialaxes`, `loon::l_hist`, `loon::l_plot`
  if(activeModel == "l_serialaxes" & length(activeGeomLayers) > 0) {

    l_serialaxesplot(ggBuild = ggBuild,
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
      warning(
        "Two histogram layers are detected and the ",
        activeGeomLayers,
        "th one will be taken as the interactive one.", call. = FALSE)
    }

    FacetWrap <- plotInfo$FacetWrap
    FacetGrid <- plotInfo$FacetGrid
    ggLayout <- buildggObj$ggLayout

    h <- l_histogram(ggBuild = ggBuild,
                     ggLayout = ggLayout,
                     layout = layout,
                     ggplotPanelParams = ggplotPanelParams,
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

    sync <- plotInfo$sync
    linkingGroup <- args$linkingGroup

    if(is.null(linkingGroup)) return(h)
    # sync can only be pull or push
    if(sync == "push") return(h)

    h['colorStackingOrder'] <- "selected"
    return(h)

  } else if(activeModel == "l_plot" & length(boxplotPointLayers) > 0) {

    l_scatterplot(ggBuild = ggBuild,
                  ggObj = ggObj,
                  ggplotPanelParams = ggplotPanelParams,
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
