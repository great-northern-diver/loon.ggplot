get_modelLayers <- function(len_layers, ggObj, isCoordPolar = FALSE, isCoordSerialaxes = FALSE) {

  layerNames <- lapply(seq_len(len_layers),
                       function(j) {
                         className <- class(ggObj$layers[[j]]$geom)
                         className[-which(className %in% c("ggproto"  ,"gg" ,"Geom"))]
                       })

  if(length(layerNames) == 0)
    return(
      list(pointLayers = numeric(0L),
           histogramLayers = numeric(0L),
           boxplotLayers = numeric(0L),
           curveLayers = numeric(0L),
           serialaxesLayers = numeric(0L))
    )

  # check the coord
  if(isCoordSerialaxes) {
    serialaxesLayers <- which(
      sapply(layerNames,
             function(layerName) {
               (length(layerName) == 1) && (("GeomPath" %in% layerName) || ("GeomRibbon" %in% layerName))
             })
    )

    if(length(serialaxesLayers) > 1) {
      warning("Only one layer can be active in serialaxes. The first layer will be picked by default", call. = FALSE)
      serialaxesLayers <- serialaxesLayers[1]
    }

    return(list(pointLayers = numeric(0L),
                histogramLayers = numeric(0L),
                boxplotLayers = numeric(0L),
                curveLayers = numeric(0L),
                serialaxesLayers = serialaxesLayers))
  }

  # take the point layer as l_plot
  pointLayers <- which(
    sapply(layerNames,
           function(layerName){
             any(layerName %in% c("GeomPoint", "GeomPolygonGlyph",
                                  "GeomText", "GeomTextGlyph",
                                  "GeomPointrange", "GeomPointrangeGlyph",
                                  "GeomImageGlyph", "GeomSerialAxesGlyph"))
           })
  )

  histogramLayers <- which(
    sapply(layerNames,
           function(layerName){
             if("GeomBar" %in% layerName) {
               if(isCoordPolar) {
                 warning("loon `l_hist` is built based on Cartesian coordinate system ",
                 "and does not accommodate polar coordinate system yet. ",
                 "If polar coords are used, the histograms or bar plots ",
                 "will be created as static polygons and will **not** be interactive." , call. = FALSE)

                 FALSE
               } else TRUE
               # TODO `l_hist` only accommodate one dimensional histogram
               # "GeomBar_" would not be interactive
               if("GeomBar_" %in% layerName) {
                 warning("If `GeomBar_` object is called, the histograms or bar plots ",
                 "will be created as static polygons and will **not** be interactive.", call. = FALSE)
                 FALSE
               } else TRUE
             } else FALSE
           })
  )

  # boxlayer
  boxplotLayers <- which(sapply(layerNames,
                                function(layerName){
                                  "GeomBoxplot" %in% layerName
                                })
  )

  # curvelayer
  curveLayers <- which(sapply(layerNames,
                              function(layerName){
                                "GeomCurve" %in% layerName
                              })
  )
  list(pointLayers = pointLayers,
       histogramLayers = histogramLayers,
       boxplotLayers = boxplotLayers,
       curveLayers = curveLayers,
       serialaxesLayers = numeric(0L))
}

get_activeInfo <- function(modelLayers, activeGeomLayers, len_layers){

  pointLayers <- modelLayers$pointLayers
  histogramLayers <- modelLayers$histogramLayers
  serialaxesLayers <- modelLayers$serialaxesLayers

  boxplotLayers <- modelLayers$boxplotLayers

  point_hist_layers <- c(pointLayers, histogramLayers)

  if (length(activeGeomLayers) == 0) {
    if(length(c(point_hist_layers, serialaxesLayers)) > 0) {
      activeGeomLayers <- if(length(serialaxesLayers) > 0) serialaxesLayers else min(point_hist_layers)
      activeModel <- if (activeGeomLayers %in% serialaxesLayers) {
        "l_serialaxes"
      } else if(activeGeomLayers %in% pointLayers)
        "l_plot"
      else "l_hist"
    } else {
      activeModel <- "l_plot"
    }
  } else {
    if(max(activeGeomLayers, na.rm = TRUE) > len_layers)
      stop("the activeGeomLayers is out of bound", call. = FALSE)
    canBeActive <- activeGeomLayers %in% c(point_hist_layers, serialaxesLayers, boxplotLayers)
    if(all(canBeActive)) {

      if(all(activeGeomLayers %in% serialaxesLayers)) {
        activeModel <- "l_serialaxes"
      } else if(all(activeGeomLayers %in% pointLayers)) { # all point layers?
        activeModel <- "l_plot"
      } else if(all(activeGeomLayers %in% histogramLayers)) { # all histogram layers?
        activeModel <- "l_hist"
        if (length(activeGeomLayers) > 1) {
          # multiple histograms?
          message("only one histogram layer is drawn as active (model) layer\n",
                  "the rest will be added as l_layer_rectangles")
          activeGeomLayers <- activeGeomLayers[1]
        }
      } else if(any(activeGeomLayers %in% pointLayers) & any(activeGeomLayers %in% histogramLayers)) {
        stop("histogram layer and point layer cannot be active at the same time", call. = FALSE)
      } else if(any(activeGeomLayers %in% boxplotLayers) & any(activeGeomLayers %in% histogramLayers)){
        stop("histogram layer and boxplot layer cannot be active at the same time", call. = FALSE)
      } else {
        # boxplot Layer?
        activeGeomLayers <- integer(0)
        activeModel <- "l_plot"
      }
    } else {
      stop("This layer cannot be active", call. = FALSE)
    }
  }
  list(activeModel = activeModel,
       activeGeomLayers = activeGeomLayers)
}

get_subtitle <- function(layoutByROWS, layoutByCOLS, layout, ggLayout, numOfSubtitles,
                         byROWS, byCOLS ,panelNum, FacetWrap, FacetGrid){
  if(FacetWrap) {
    colSubtitle <- if (numOfSubtitles > 0) {
      paste(
        sapply(
          layout[panelNum, names(ggLayout$facet_params$facets)],
          as.character
        ),
        collapse = "\n"
      )
    } else NULL
    rowSubtitle <- NULL
  } else if(FacetGrid) {
    if(byROWS & !byCOLS) {
      rowSubtitle <- paste(sapply(layout[panelNum, layoutByROWS], as.character), collapse = "\n")
      colSubtitle <- NULL
    } else if(!byROWS & byCOLS){
      rowSubtitle <- NULL
      colSubtitle <- paste(sapply(layout[panelNum, layoutByCOLS], as.character), collapse = "\n")
    } else if(byROWS & byCOLS){
      rowSubtitle <- paste(sapply(layout[panelNum, layoutByROWS], as.character), collapse = "\n")
      colSubtitle <- paste(sapply(layout[panelNum, layoutByCOLS], as.character), collapse = "\n")
    } else {
      rowSubtitle <- NULL
      colSubtitle <- NULL
    }
  } else {
    rowSubtitle <- NULL
    colSubtitle <- NULL
  }
  list(colSubtitle = colSubtitle,
       rowSubtitle = rowSubtitle)
}


get_showScales <- function(theme) {

  # do not set theme, default showScales is TRUE
  if(length(theme) == 0) return(TRUE)

  # any of them is turned off --> showScales is FALSE
  if(any(names(theme) %in% c("axis.text.x", "axis.text.y", "axis.ticks"))) {
    !(
      is.element_blank(theme$axis.text.x) ||
        is.element_blank(theme$axis.text.y) ||
        is.element_blank(theme$axis.ticks)
    )
  } else TRUE
}

get_showLabels <- function(theme) {

  # do not set theme, default showLabels is TRUE
  if(length(theme) == 0) return(TRUE)

  # any of them is turned off --> showLabels is FALSE
  if(any(names(theme) %in% c("axis.title.y", "axis.title.x", "axis.title"))) {
    !(
      is.element_blank(theme$axis.title.x) ||
        is.element_blank(theme$axis.title.y) ||
        is.element_blank(theme$axis.title)
    )
  } else TRUE
}

