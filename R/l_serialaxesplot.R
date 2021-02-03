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

  axes.sequence <- get_axes.sequence(ggObj, activeGeomLayers)
  axesLayout <- coordSerialAxes$axes.layout
  scaling <- coordSerialAxes$scaling

  showArea <- if(inherits(layer$geom, "GeomRibbon")) TRUE else FALSE

  activeLayerData <- ggBuild$data[[activeGeomLayers]]
  aesData <- activeLayerData[activeLayerData$PANEL == panelIndex, ]

  color <- aesData$colour[which(!duplicated(aesData$group))]
  size <- aesData$size[which(!duplicated(aesData$group))]

  args <- list(
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

  if(utils::packageVersion("loon") >= "1.3.2") {
    args$andrews <- is.andrews(ggObj, activeGeomLayers)
  }

  do.call(loon::l_serialaxes, args)
}


get_axes.sequence <- function(ggObj, activeGeomLayers) {

  coordSerialAxes <- ggObj$coordinates
  if(!is.null(char2null(coordSerialAxes$axes.sequence)))
    return(coordSerialAxes$axes.sequence)

  layer <- ggObj$layers[[activeGeomLayers]]

  # aesthetics will not be treated as the axes
  axes.sequence <- setdiff(unique(c(names(ggObj$mapping), names(layer$mapping))),
                           names(ggplot2::GeomPath$default_aes))

  if(length(axes.sequence) == 0) {
    warning("No legal axes found")
    return(NULL)
  }

  axes.sequence
}

is.andrews <- function(ggObj, activeGeomLayers) {
 layer <- ggObj$layers[[activeGeomLayers]]
 stat <- layer$stat
 inherits(stat, "StatDotProduct")
}
