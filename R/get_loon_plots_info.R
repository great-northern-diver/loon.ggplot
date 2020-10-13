get_loon_plotInfo <- function(plotInfo = list(),
                              ggObj,
                              parent = NULL,
                              activeGeomLayers = integer(0),
                              ggGuides = FALSE,
                              pack = FALSE,
                              tkLabels = NULL,
                              canvasHeight = 700,
                              canvasWidth = 850) {

  buildggObj <- plotInfo$buildggObj
  args <- plotInfo$args

  # ggplot object
  dataFrame <- ggObj$data
  linkingKey <- plotInfo$linkingKey
  itemLabel <- plotInfo$itemLabel
  # is serialaxes coord?
  isCoordSerialaxes <- plotInfo$isCoordSerialaxes

  layout <- buildggObj$layout
  ggplotPanel_params <- buildggObj$ggplotPanel_params
  panelNum <- dim(layout)[1]

  # facet and location
  span <- plotInfo$span
  start.ypos <- plotInfo$start.ypos
  start.xpos <- plotInfo$start.xpos
  start.subtitlepos <- plotInfo$start.subtitlepos
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
  xlabel <- plotInfo$xlabel
  ylabel <- plotInfo$ylabel
  title <- plotInfo$title

  # length layers
  lenLayers <- length(ggObj$layers)

  # initial settings
  colSubtitles <- c()
  rowSubtitles <- c()
  indices <- list()
  plots <- list()
  # is polar coord?
  isCoordPolar <- is.CoordPolar(ggObj$coordinates)

  # subtitle
  # if wrap number is larger than 0, multiple facets are displayed
  numOfSubtitles <- wrap_num(buildggObj$ggLayout,
                             plotInfo$FacetWrap,
                             plotInfo$FacetGrid,
                             tkLabels)

  for(i in seq_len(panelNum)) {

    subtitle <- get_subtitle(plotInfo$layoutByROWS,
                             plotInfo$layoutByCOLS,
                             layout = layout,
                             ggLayout = buildggObj$ggLayout,
                             numOfSubtitles = numOfSubtitles,
                             byROWS = plotInfo$byROWS,
                             byCOLS = plotInfo$byCOLS,
                             panelNum = i,
                             FacetWrap = plotInfo$FacetWrap,
                             FacetGrid = plotInfo$FacetGrid,
                             tkLabels = tkLabels)
    colSubtitle <- subtitle$colSubtitle
    rowSubtitle <- subtitle$rowSubtitle
    colSubtitles <- c(colSubtitles, colSubtitle)
    rowSubtitles <- c(rowSubtitles, rowSubtitle)

    if(!is.null(colSubtitle) & !plotInfo$FacetGrid & tkLabels & pack) {
      sub <- as.character(tcltk::tcl('label',
                                     as.character(loon::l_subwin(parent,'label')),
                                     text= colSubtitle,
                                     bg = set_tkLabel()$labelBackground,
                                     fg = set_tkLabel()$labelForeground,
                                     borderwidth = set_tkLabel()$labelBorderwidth,
                                     relief = set_tkLabel()$labelRelief))
      tcltk::tkgrid(sub,
                    row = (layout[i,]$ROW - 1) * span + start.ypos,
                    column = (layout[i,]$COL - 1) * span + start.xpos,
                    rowspan = numOfSubtitles,
                    columnspan = span,
                    sticky="nesw")
      start.subtitlepos <- start.ypos + numOfSubtitles
      newspan <- span - numOfSubtitles
      if(newspan <= 0) rlang::abort(paste0("pick a larger span, at least larger than ", numOfSubtitles))
    }

    if(isCoordPolar) {
      # theta can be only "x" or "y"
      theta <- ggObj$coordinates$theta %||% "x"
      if(theta == "y")  swapAxes <- TRUE
      showGuides <- FALSE
      showScales <- FALSE
    } else {
      # if not polar coord
      # swap or not
      swapAxes <- is.CoordFlip(ggObj$coordinates)
      # show ggGuides or not
      if (ggGuides) {
        showGuides <- FALSE
        showScales <- FALSE

      } else {
        # set panX, panY, deltaX, deltaY
        showGuides <- TRUE
        showScales <- get_showScales(ggObj$theme)
        x.range <- plot_range("x.range", ggplotPanel_params[[i]], swapAxes)
        y.range <- plot_range("y.range", ggplotPanel_params[[i]], swapAxes)

        panY <- y.range[1]
        panX <- x.range[1]
        deltaY <- diff(y.range) * zoomX
        deltaX <- diff(x.range) * zoomY
      }
    }

    loonTitle <- paste(c(title,
                         colSubtitle,
                         rowSubtitle), collapse = "\n")
    if (lenLayers > 0) {

      modelLayers <- get_modelLayers(lenLayers, ggObj, isCoordPolar, isCoordSerialaxes)

      # set active geom layer and active model
      activeInfo <- get_activeInfo(modelLayers, activeGeomLayers, lenLayers)

      index <- l_indices(ggObj = ggObj,
                         panelIndex = i, args = args, plotInfo = plotInfo,
                         numOfSubtitles = numOfSubtitles,  activeInfo = activeInfo,
                         modelLayers = modelLayers)

      # update indicies
      indices[[i]] <- index

      loonPlot <- l_loonPlot(ggObj = ggObj,
                             panelIndex = i, args = args, plotInfo = plotInfo,
                             numOfSubtitles = numOfSubtitles, activeInfo = activeInfo,
                             modelLayers = modelLayers, index = index,
                             parent = parent, showGuides = showGuides, showScales = showScales,
                             swapAxes = swapAxes, xlabel = xlabel, ylabel = ylabel,
                             loonTitle = loonTitle)

      pack_layers(loonPlot = loonPlot, ggObj = ggObj, buildggObj = buildggObj,
                  panelIndex = i, activeInfo = activeInfo, modelLayers = modelLayers)

    } else {

      loonPlot <- loon::l_plot(parent = parent,
                               showGuides = showGuides,
                               showScales = showScales,
                               showLabels = plotInfo$showLabels,
                               swapAxes = swapAxes,
                               xlabel = if(is.null(xlabel)) "" else xlabel,
                               ylabel = if(is.null(ylabel)) "" else ylabel,
                               title = loonTitle)
    }

    # resize loon plot
    if(pack) {

      tcltk::tkconfigure(paste(loonPlot,'.canvas',sep=''),
                         width = canvasWidth/plotInfo$column,
                         height = canvasHeight/plotInfo$row)
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

    if(!isCoordSerialaxes) {
      ## After version 3.3.0, ggplot has a significant change.
      ## In the past
      ## p <- ggplot(mtcars, aes(y = hp)) +
      ##        geom_histogram()
      ## this would not work (give an error).
      ## However, after version 3.3.0
      ## this will produce a swapAxes histogram
      ## In other words, if we want to flip a histogram,
      ## rather than pipe through function `coord_flip()`
      ## we can set mapping systems as `aes(y = variable)`.
      if(loonPlot['swapAxes'] != swapAxes) {
        # the situation we described before happens
        swapAxes <- loonPlot['swapAxes']

        if(!isCoordPolar && !ggGuides) {
          temp <- panX
          panX <- panY
          panY <- temp

          temp <- zoomX
          zoomX <- zoomY
          zoomY <- temp

          temp <- deltaX
          deltaX <- deltaY
          deltaY <- temp
        }
      }
      # loonPlot_configure does not produce anything but just configure the loon plot
      loonPlot_configure(isCoordPolar = isCoordPolar,
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
    }

    plots[[i]] <- loonPlot
  }

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

  list(
    plots = plots,
    indices = indices,
    display_info = list(
      colSubtitles = colSubtitles,
      rowSubtitles = rowSubtitles,
      start.subtitlepos = start.subtitlepos,
      swapAxes = swapAxes
    )
  )
}
