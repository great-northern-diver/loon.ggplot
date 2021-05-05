pack_layers <- function(loonPlot, ggObj, buildggObj,
                        panelIndex, activeInfo, modelLayers) {

  lenLayers <- length(ggObj$layers)
  if(lenLayers == 0) return(NULL)

  ggplotPanelParams <- buildggObj$ggplotPanelParams
  ggBuild <- buildggObj$ggBuild

  curveLayers <- modelLayers$curveLayers
  boxplotLayers <- modelLayers$boxplotLayers

  activeGeomLayers <- activeInfo$activeGeomLayers
  activeModel <- activeInfo$activeModel

  # adding layers
  loonLayers <- lapply(seq_len(lenLayers),
                       function(j){
                         if(j %in% activeGeomLayers)
                           return(NULL)
                         loonLayer(widget = loonPlot,
                                   layerGeom = ggObj$layers[[j]],
                                   data =  ggBuild$data[[j]][ggBuild$data[[j]]$PANEL == panelIndex, ],
                                   ggplotPanelParams = ggplotPanelParams[[panelIndex]],
                                   ggObj = ggObj,
                                   curveAdditionalArgs = list(curve = list(which_curve = j,
                                                                           curveLayers = curveLayers))
                         )
                       })

  # reset the points or histogram layer to the original position
  if(length(activeGeomLayers) != lenLayers && length(activeGeomLayers) > 0 && all(activeGeomLayers != 0L)) {

    otherLayerId <- seq(lenLayers)[-activeGeomLayers]
    minOtherLayerId <- min(otherLayerId)
    max_hist_points_layerId <- max(activeGeomLayers)

    if(max_hist_points_layerId > minOtherLayerId){
      modelLayerup <- sapply(seq_len(length(which(otherLayerId < max_hist_points_layerId) == TRUE)),
                             function(j){
                               loon::l_layer_raise(loonPlot, "model")
                             }
      )
    }
  }

  if (length(boxplotLayers) != 0 && activeModel == "l_plot" && length(activeGeomLayers) == 0) {
    # hidden points layer
    loon::l_layer_hide(loonPlot, "model")
    # move the hidden layer on the top
    modelLayerup <- sapply(seq_len(lenLayers),
                           function(j){
                             loon::l_layer_raise(loonPlot, "model")
                           })
  }

  loonLayers
}
