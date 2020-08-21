loonSerialaxes <- function(ggBuild,
                           index,
                           ggObj,
                           activeGeomLayers,
                           panelIndex,
                           dataFrame,
                           parent,
                           showGuides,
                           linkingKey,
                           showLabels,
                           loonTitle) {


  # The warning is given ahead; See function `loon.ggplot:::get_modelLayers`
  activeGeomLayers <- activeGeomLayers[1]
  layer <- ggObj$layers[[activeGeomLayers]]

  coordSerialAxes <- ggObj$coordinates
  axesLabels <- coordSerialAxes$axesLabels %||% colnames(dataFrame)
  displayOrder <- coordSerialAxes$displayOrder
  axesLayout <- coordSerialAxes$axesLayout
  scaling <- coordSerialAxes$scaling

  showArea <- if(inherits(layer$geom, "GeomRibbon")) TRUE else FALSE

  activeLayer <- ggBuild$data[[activeGeomLayers]]
  aesData <- activeLayer[activeLayer$PANEL == panelIndex, ]

  color <- aesData$colour[which(!duplicated(aesData$group))]
  size <- aesData$size[which(!duplicated(aesData$group))]

  loon::l_serialaxes(
    data = dataFrame[index, ],
    sequence = axesLabels,
    scaling = scaling,
    axesLayout = axesLayout ,
    showArea = showArea,
    showGuides = showGuides,
    linkingKey = linkingKey[index],
    showLabels = showLabels,
    linewidth = size,
    color = color,
    parent  = parent,
    title = loonTitle
  )
}
