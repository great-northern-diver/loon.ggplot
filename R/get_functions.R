get_importantLayers <- function(len_layers, ggObj){
  layerNames <- lapply(seq_len(len_layers),
                       function(j) {
                         className <- class(ggObj$layers[[j]]$geom)
                         className[-which(className %in% c("ggproto"  ,"gg" ,"Geom"))]
                       })

  # take the point layer as l_plot
  pointLayers <- which(sapply(layerNames,
                              function(layerName){
                                "GeomPoint" %in% layerName
                              }) == TRUE
  )

  # take the histogram layer as l_hist
  histogramLayers <- which(sapply(seq_len(length(layerNames)),
                                  function(j){
                                    # it could be bar plot
                                    is.histogram_condition1 <- all(c("GeomBar", "GeomRect") %in% layerNames[[j]])
                                    # stat class of geom_bar is StatCount
                                    is.histogram_condition2 <- "StatBin" %in% class(ggObj$layers[[j]]$stat)
                                    is.histogram_condition1 & is.histogram_condition2
                                  }) == TRUE
  )

  # boxlayer
  boxplotLayers <- which(sapply(layerNames,
                                function(layerName){
                                  "GeomBoxplot" %in% layerName
                                }) == TRUE
  )

  # curvelayer
  curveLayers <- which(sapply(layerNames,
                              function(layerName){
                                "GeomCurve" %in% layerName
                              }) == TRUE
  )
  list(pointLayers = pointLayers,
       histogramLayers = histogramLayers,
       boxplotLayers = boxplotLayers,
       curveLayers = curveLayers)
}

get_activeInfo <- function(importantLayers, activeGeomLayers, len_layers){

  pointLayers <- importantLayers$pointLayers
  histogramLayers <- importantLayers$histogramLayers
  boxplotLayers <- importantLayers$boxplotLayers

  point_hist_layers <- c(pointLayers, histogramLayers)
  if (length(activeGeomLayers) == 0) {
    if(length(point_hist_layers) != 0) {
      activeGeomLayers <- min(point_hist_layers)
      activeModel <- if(activeGeomLayers %in% pointLayers) "l_plot" else "l_hist"
    } else {
      activeModel <- "l_plot"
    }
  } else {
    if(max(activeGeomLayers) > len_layers)
      stop("the activeGeomLayers is out of bound")
    canBeActive <- activeGeomLayers %in% c(point_hist_layers, boxplotLayers)
    if(all(canBeActive)) {
      if(all(activeGeomLayers %in% pointLayers)) { # all point layers?
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
        stop("histogram layer and point layer cannot be active at the same time")
      } else if(any(activeGeomLayers %in% boxplotLayers) & any(activeGeomLayers %in% histogramLayers)){
        stop("histogram layer and boxplot layer cannot be active at the same time")
      } else {
        # boxplot Layer?
        activeGeomLayers <- integer(0)
        activeModel <- "l_plot"
      }
    } else {
      stop("This layer cannot be active")
    }
  }
  list(activeModel = activeModel,
       activeGeomLayers = activeGeomLayers)
}

get_subtitle <- function(layoutByROWS, layoutByCOLS, layout, ggLayout, numOfSubtitles,
                         byROWS, byCOLS ,panelNum, is_facet_wrap, is_facet_grid, tkLabels){
  if(is_facet_wrap | !tkLabels) {
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
  } else if(is_facet_grid) {
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

  if(any(names(theme) %in% c("axis.text.x", "axis.text.y", "axis.ticks"))) {
    !(
      inherits(theme$axis.text.x, "element_blank") ||
        inherits(theme$axis.text.y, "element_blank") ||
        inherits(theme$axis.ticks, "element_blank")
    )
  } else TRUE
}

