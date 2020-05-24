get_loon_plots_info <- function(plots_info = list(),
                                ggObj,
                                parent = NULL,
                                activeGeomLayers = integer(0),
                                ggGuides = FALSE,
                                pack = FALSE,
                                tkLabels = NULL,
                                canvasHeight = 700,
                                canvasWidth = 850) {

  buildggObj <- plots_info$buildggObj
  args <- plots_info$args


  # ggplot object
  dataFrame <- ggObj$data
  linkingKey <- plots_info$linkingKey
  itemLabel <- plots_info$itemLabel

  # ggbuild
  ggBuild <- buildggObj$ggBuild
  layout <- buildggObj$layout
  ggplotPanel_params <- buildggObj$ggplotPanel_params
  panelNum <- dim(layout)[1]

  # facet and location
  span <- plots_info$span
  start.ypos <- plots_info$start.ypos
  start.xpos <- plots_info$start.xpos
  start.subtitlepos <- plots_info$start.subtitlepos
  newspan <- span

  # loon setting
  swapAxes <- FALSE
  # set zoomX zoomY and linkingGroup
  zoomX <- if (is.null(args[['zoomX']])) {
    5/6
  } else {
    args[['zoomX']]
  }
  zoomY <- if (is.null(args[['zoomY']])) {
    5/6
  } else {
    args[['zoomY']]
  }
  # labels
  xlabel <- plots_info$xlabel
  ylabel <- plots_info$ylabel
  title <- plots_info$title
  colSubtitles <- c()
  rowSubtitles <- c()

  # length layers
  len_layers <- length(ggObj$layers)

  plots <- lapply(seq_len(panelNum),
                  function(i){
                    # subtitle
                    # if wrap number is larger than 0, multiple facets are displayed
                    numOfSubtitles <- wrap_num(buildggObj$ggLayout,
                                               plots_info$is_facet_wrap,
                                               plots_info$is_facet_grid,
                                               tkLabels)

                    subtitle <- get_subtitle(plots_info$layoutByROWS,
                                             plots_info$layoutByCOLS,
                                             layout = layout,
                                             ggLayout = buildggObj$ggLayout,
                                             numOfSubtitles = numOfSubtitles,
                                             byROWS = plots_info$byROWS,
                                             byCOLS = plots_info$byCOLS,
                                             panelNum = i,
                                             is_facet_wrap = plots_info$is_facet_wrap,
                                             is_facet_grid = plots_info$is_facet_grid,
                                             tkLabels = tkLabels)
                    colSubtitle <- subtitle$colSubtitle
                    rowSubtitle <- subtitle$rowSubtitle
                    colSubtitles <<- c(colSubtitles, colSubtitle)
                    rowSubtitles <<- c(rowSubtitles, rowSubtitle)

                    if(!is.null(colSubtitle) & !plots_info$is_facet_grid & tkLabels & pack) {
                      sub <- as.character(tcltk::tcl('label',
                                                     as.character(loon::l_subwin(parent,'label')),
                                                     text= colSubtitle, background = "grey90"))
                      tcltk::tkgrid(sub,
                                    row = (layout[i,]$ROW - 1) * span + start.ypos,
                                    column = (layout[i,]$COL - 1) * span + start.xpos,
                                    rowspan = numOfSubtitles,
                                    columnspan = span,
                                    sticky="nesw")
                      start.subtitlepos <<- start.ypos + numOfSubtitles
                      newspan <- span - numOfSubtitles
                      if(newspan <= 0) stop(paste0("pick a larger span, at least larger than ", numOfSubtitles), call. = FALSE)
                    }

                    # is polar coord?
                    isCoordPolar <- is.CoordPolar(ggplotPanel_params[[i]])

                    if(isCoordPolar){
                      # theta can be only "x" or "y"
                      if(ggObj$coordinates$theta == "y")  swapAxes <<- TRUE
                      showGuides <- FALSE
                      showScales <- FALSE
                    } else {
                      # if not polar coord
                      # swap or not
                      if( which( names(ggplotPanel_params[[i]]) %in% "y.range"  == TRUE ) <
                          which( names(ggplotPanel_params[[i]]) %in% "x.range"  == TRUE ) ) swapAxes <<- TRUE
                      # show ggGuides or not
                      if (ggGuides) {
                        showGuides <- FALSE
                        showScales <- FALSE

                      } else {
                        # set panX, panY, deltaX, deltaY
                        showGuides <- TRUE
                        showScales <- get_showScales(ggObj$theme)
                        x.range <- if (swapAxes) {
                          ggplotPanel_params[[i]]$y.range
                        } else {
                          ggplotPanel_params[[i]]$x.range
                        }
                        y.range <- if (swapAxes) {
                          ggplotPanel_params[[i]]$x.range
                        } else {
                          ggplotPanel_params[[i]]$y.range
                        }
                        panY <- y.range[1]
                        panX <- x.range[1]
                        deltaY <- diff(y.range) * zoomX
                        deltaX <- diff(x.range) * zoomY
                      }
                    }

                    loonTitle <- paste(c(title,
                                         colSubtitle,
                                         rowSubtitle), collapse = "\n")

                    if (len_layers != 0) {
                      importantLayers <- get_importantLayers(len_layers, ggObj, isCoordPolar)

                      boxplotLayers <- importantLayers$boxplotLayers
                      curveLayers <- importantLayers$curveLayers

                      # set active geom layer and active model
                      activeInfo <- get_activeInfo(importantLayers, activeGeomLayers, len_layers)
                      activeGeomLayers <- activeInfo$activeGeomLayers
                      activeModel <- activeInfo$activeModel

                      # boxplot has a hidden scatterplot model layer
                      boxplot_point_layers <- c(boxplotLayers, activeGeomLayers)

                      if (is.data.frame(dataFrame) & !"waiver" %in% class(dataFrame)) {
                        mapping <- ggObj$mapping
                      } else {
                        if(length(activeGeomLayers) == 1) {
                          dataFrame <- ggObj$layers[[activeGeomLayers]]$data
                          linkingKey <- loonLinkingKey(dataFrame, args)
                          itemLabel <- loonItemLabel(dataFrame, args)

                          mapping <- ggObj$layers[[activeGeomLayers]]$mapping
                        } else NULL # activeGeomLayers > 1 not implemented so far
                      }

                      if (activeModel == "l_hist" & length(activeGeomLayers) != 0) {
                        loonPlot <- loonHistogram(ggBuild = ggBuild,
                                                  ggLayout = buildggObj$ggLayout,
                                                  layout = layout,
                                                  ggplotPanel_params = ggplotPanel_params,
                                                  ggObj = ggObj,
                                                  activeGeomLayers = activeGeomLayers,
                                                  panelIndex = i,
                                                  dataFrame = dataFrame,
                                                  mapping = mapping,
                                                  numOfSubtitles = numOfSubtitles,
                                                  parent = parent,
                                                  showGuides = showGuides,
                                                  showScales = showScales,
                                                  swapAxes = swapAxes,
                                                  linkingKey = linkingKey,
                                                  showLabels = plots_info$showLabels,
                                                  xlabel = xlabel,
                                                  ylabel = ylabel,
                                                  loonTitle = loonTitle,
                                                  is_facet_wrap = plots_info$is_facet_wrap,
                                                  is_facet_grid = plots_info$is_facet_grid)


                      } else if(activeModel == "l_plot" & length(boxplot_point_layers) != 0) {
                        loonPlot <- loonScatter(ggBuild = ggBuild,
                                                ggObj = ggObj,
                                                ggplotPanel_params = ggplotPanel_params,
                                                panelIndex = i,
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
                                                showLabels = plots_info$showLabels,
                                                xlabel = xlabel,
                                                ylabel = ylabel,
                                                loonTitle = loonTitle)

                      } else {
                        loonPlot <- loon::l_plot(parent = parent,
                                                 showGuides = showGuides,
                                                 showScales = showScales,
                                                 showLabels = plots_info$showLabels,
                                                 swapAxes = swapAxes,
                                                 xlabel = if(is.null(xlabel)) "" else xlabel,
                                                 ylabel = if(is.null(ylabel)) "" else ylabel,
                                                 title = loonTitle)

                      }
                      # adding layers
                      loon_layers <- lapply(seq_len(len_layers),
                                            function(j){
                                              if(! j %in% activeGeomLayers){
                                                loonLayer(widget = loonPlot,
                                                          layerGeom = ggObj$layers[[j]],
                                                          data =  ggBuild$data[[j]][ggBuild$data[[j]]$PANEL == i, ],
                                                          ggplotPanel_params = ggplotPanel_params[[i]],
                                                          ggObj = ggObj,
                                                          special = list(curve = list(which_curve = j,
                                                                                      curveLayers = curveLayers))
                                                )
                                              }
                                            }
                      )

                      # recover the points or histogram layer to the original position
                      if(length(activeGeomLayers) != len_layers & length(activeGeomLayers) != 0) {
                        otherLayerId <- (1:len_layers)[-activeGeomLayers]
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

                      # special case
                      if (length(boxplotLayers) != 0 & activeModel == "l_plot" & length(activeGeomLayers) == 0) {
                        # hidden points layer
                        loon::l_layer_hide(loonPlot, "model")
                        # move the hidden layer on the top
                        modelLayerup <- sapply(seq_len(len_layers),
                                               function(j){
                                                 loon::l_layer_raise(loonPlot, "model")
                                               })
                      }

                    } else loonPlot <- loon::l_plot(parent = parent,
                                                    showGuides = showGuides,
                                                    showScales = showScales,
                                                    showLabels = plots_info$showLabels,
                                                    swapAxes = swapAxes,
                                                    xlabel = if(is.null(xlabel)) "" else xlabel,
                                                    ylabel = if(is.null(ylabel)) "" else ylabel,
                                                    title = loonTitle)

                    # resize loon plot
                    if(pack) {

                      tcltk::tkconfigure(paste(loonPlot,'.canvas',sep=''),
                                         width = canvasWidth/plots_info$column,
                                         height = canvasHeight/plots_info$row)
                      # tk pack
                      row.start <- (layout[i,]$ROW - 1) * span + start.subtitlepos
                      col.start <- (layout[i,]$COL - 1) * span + start.xpos

                      tcltk::tkgrid(loonPlot,
                                    row = row.start,
                                    column= col.start,
                                    rowspan = newspan,
                                    columnspan = span,
                                    sticky="nesw")
                      # facet wrap will have multiple column names
                      for (j in row.start:(row.start + newspan - 1)) {
                        tcltk::tkgrid.rowconfigure(parent, j, weight=1)
                      }
                      for(j in col.start:(col.start + span - 1)) {
                        tcltk::tkgrid.columnconfigure(parent, j, weight=1)
                      }
                    }
                    # loonPlot_configure does not produce anything but just configure the loon plot
                    loonPlot_configure <- loonPlot_configure(isCoordPolar = isCoordPolar,
                                                             loonPlot = loonPlot,
                                                             ggGuides = ggGuides,
                                                             panelIndex = i,
                                                             ggplotPanel_params = ggplotPanel_params,
                                                             swapAxes = swapAxes,
                                                             theme = ggObj$theme,
                                                             panX=panX,
                                                             panY=panY,
                                                             deltaX= deltaX,
                                                             deltaY=deltaY,
                                                             zoomX = zoomX,
                                                             zoomY = zoomY)
                    loonPlot
                  })

  names(plots) <- sapply(
    seq_len(panelNum),
    function(j){
      paste0(
        c("x", "y"),
        layout[j, c(which(colnames(layout) == "ROW"),
                    which(colnames(layout) == "COL"))],
        collapse = ""
      )
    }
  )

  return(
    list(
      plots = plots,
      display_info = list(
        colSubtitles = colSubtitles,
        rowSubtitles = rowSubtitles,
        start.subtitlepos = start.subtitlepos,
        swapAxes = swapAxes
      )
    )
  )
}
