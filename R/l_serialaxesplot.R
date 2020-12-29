l_serialaxesplot <- function(ggBuild,
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

  if(!is.CoordSerialaxes(coordSerialAxes))
   stop("No serialaxes coordinate is found", call. = FALSE)

  axes.sequence <- char2null(coordSerialAxes$axes.sequence) %||% colnames(dataFrame)
  axesLayout <- coordSerialAxes$axes.layout
  scaling <- coordSerialAxes$scaling

  showArea <- if(inherits(layer$geom, "GeomRibbon")) TRUE else FALSE

  activeLayer <- ggBuild$data[[activeGeomLayers]]
  aesData <- activeLayer[activeLayer$PANEL == panelIndex, ]

  color <- aesData$colour[which(!duplicated(aesData$group))]
  size <- aesData$size[which(!duplicated(aesData$group))]

  loon::l_serialaxes(
    data = dataFrame[index, ],
    sequence = axes.sequence,
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
