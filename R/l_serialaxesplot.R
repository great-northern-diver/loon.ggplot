l_serialaxesplot <- function(ggBuild,
                             index,
                             ggObj,
                             activeGeomLayers,
                             panelIndex,
                             dataFrame,
                             parent,
                             showGuides,
                             linkingKey,
                             itemLabel,
                             nDimStates,
                             showLabels,
                             showItemLabels,
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
  id <- activeLayerData$PANEL == panelIndex
  aesData <- activeLayerData[id, ]

  color <- aesData$colour[which(!duplicated(aesData$group))]
  size <- aesData$size[which(!duplicated(aesData$group))]


  dat <- modify_n_dim_data(nDimStates,
                           data.frame(
                             linkingKey = linkingKey[index],
                             itemLabel = itemLabel[index],
                             linewidth = size,
                             color = color
                           ), id[which(!duplicated(aesData$group))])

  args <- remove_null(
    c(
      list(
        data = dataFrame[index, ],
        sequence = axes.sequence,
        scaling = scaling,
        axesLayout = axesLayout ,
        showArea = showArea,
        showGuides = showGuides,
        showLabels = showLabels,
        showItemLabels = showItemLabels,
        parent  = parent,
        title = loonTitle
      ),
      dat
    ), as_list = FALSE)

  args$andrews <- is.andrews(ggObj, activeGeomLayers)

  do.call(loon::l_serialaxes, args)
}


get_axes.sequence <- function(ggObj, activeGeomLayers) {

  coordSerialAxes <- ggObj$coordinates
  if(!is.null(char2null(coordSerialAxes$axes.sequence)))
    return(coordSerialAxes$axes.sequence)

  layer <- ggObj$layers[[activeGeomLayers]]

  mapping <- new_aes(ggObj$mapping, layer$mapping)

  # aesthetics will not be treated as the axes
  axes.sequence.names <- setdiff(names(mapping),
                                 names(ggplot2::GeomPath$default_aes))

  axes.sequence <- vapply(axes.sequence.names,
                          function(name) {
                            rlang::as_label(mapping[[name]])
                          }, character(1L))

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
