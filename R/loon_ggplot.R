#' @title Create a loon plot from a ggplot2 object
#'
#' @description Interactive loon plots from ggplots
#'
#' @param ggplotObject a ggplot object
#' @param ggGuides logical (default \code{FALSE}) to determine whether to draw a ggplot background or not.
#' @param activeGeomLayers to determine which geom layer is active. Only `geom_point()` and `geom_histogram()` can be set as active geom layer(s).
#' (Notice, more than one `geom_point()` layers can be set as active layers, but only one `geom_histogram()` can be set as an active geom layer)
#' @param tkLabels logical (default \code{TRUE}) to show tk packing labels or not
#' @param span the span of canvas
#' @param canvasHeight the height of canvas
#' @param canvasWidth the width of canvas
#' @param ... named arguments to modify loon plot states
#'
#'
#' @import ggplot2 loon tcltk methods grid
#' @importFrom stats quantile approxfun integrate setNames
#' @importFrom utils packageVersion menu data
#' @importFrom grDevices extendrange
#' @importFrom stringr str_detect
#'
#' @export
#'
#' @examples
#'  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'  g <- loon.ggplot(p)
#'
#'  # show ggGuides
#'  p <- ggplot(mpg, aes(class, hwy)) + geom_boxplot()
#'  g <- loon.ggplot(p, ggGuides = TRUE)
#'
#'  # which one is active?
#'  ## l_hist and l_plot, which one should be the priority?
#'  den <- density(mtcars$mpg)
#'  pp <- ggplot(mtcars, aes(x = mpg)) +
#'    geom_histogram(mapping = aes(y = ..density..)) +
#'    geom_point(data = data.frame(x = den$x, y = den$y), mapping = aes(x, y))
#'  pp
#'  ## check difference
#'  g <- loon.ggplot(pp)
#'  suppressWarnings(g <- loon.ggplot(pp, activeGeomLayers = 2))
#'
#' \dontrun{
#' p <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg,
#'    colour = factor(gear))) + facet_wrap(~am)
#' g1 <- loon.ggplot(p)
#' g2 <- loon.ggplot(p, tkLabels = TRUE)
#' }


loon.ggplot <- function(ggplotObject, ggGuides = FALSE, activeGeomLayers = integer(0), tkLabels = FALSE,
                        span = 5, canvasHeight = 850, canvasWidth = 700, ...){
  # check arguments
  args <- list(...)
  if (is.null(args[['linkingGroup']])) {
    args[['linkingGroup']] <- "none"
  }
  if (is.null(args[['scalesMargins']]) & tkLabels) {
    args[['scalesMargins']] <- c(30, 50, 0, 0)
  }
  if (is.null(args[['labelMargins']]) & tkLabels) {
    args[['labelMargins']] <- c(30, 60, 60, 0)
  }
  if (is.null(args[['minimumMargins']]) & tkLabels) {
    args[['minimumMargins']] <- c(20, 20, 10, 10)
  }
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

  dataFrame <- ggplotObject$data
  linkingKey <- loonLinkingKey(dataFrame, args)

  # ggplot_build
  buildggplotObject <-  ggBuild2Loon(ggplotObject, linkingKey)
  ggBuild <- buildggplotObject$ggBuild
  ggLayout <- buildggplotObject$ggLayout
  ggplotPanel_params <- buildggplotObject$ggplotPanel_params

  # labels
  title <- ggplotObject$labels$title
  ylabel <- ggplotObject$labels$y
  xlabel <- ggplotObject$labels$x
  newspan <- span
  # number of panels
  panelNum <- dim(ggLayout)[1]


  if (tkLabels) {
    # two ways to separate facets, facet_wrap or facet_grid
    is_facet_wrap <- !is.null(ggBuild$layout$facet_params$facets)
    is_facet_grid <- !is.null(ggBuild$layout$facet_params$cols) & !is.null(ggBuild$layout$facet_params$rows)
    if(is_facet_wrap) {
      byCOLS <- TRUE
      byROWS <- FALSE
    } else if(is_facet_grid) {
      # layout multiple facets by rows or by cols
      layoutByROWS <- names(ggBuild$layout$facet_params$rows)
      layoutByCOLS <- names(ggBuild$layout$facet_params$cols)
      # by columns or by rows?
      byCOLS <- if(length(layoutByCOLS) > 0) TRUE else FALSE
      byROWS <- if(length(layoutByROWS) > 0) TRUE else FALSE
    } else {
      byCOLS <- FALSE
      byROWS <- FALSE
    }

    start.xpos <- if(!is.null(ylabel)) 1 else 0
    start.ypos <- start.subtitlepos <- if(!is.null(title)){
      if(is_facet_grid & byCOLS) 2 else 1
    } else {
      if(is_facet_grid & byCOLS) 1 else 0
    }
    showLabels <- FALSE
  } else {
    is_facet_wrap <- FALSE
    is_facet_grid <- FALSE
    byCOLS <- FALSE
    byROWS <- FALSE

    start.xpos <- start.ypos <- start.subtitlepos <- 0
    showLabels <- TRUE
  }
  colSubtitles <- c()
  rowSubtitles <- c()

  # theme
  theme <- ggplotObject$theme

  tt <- tktoplevel(background = "white")
  tktitle(tt) <- paste0("loon.ggplot", as.character(tktitle(tt)))

  column <- max(ggLayout$COL)
  row <- max(ggLayout$ROW)
  row.span <- span * row
  column.span <- span * column

  # length layers
  len_layers <- length(ggplotObject$layers)

  swapAxes <- FALSE

  # the parttern of ggLayout
  # PANEL, ROW, COL, ..., SCALE_X, SCALE_Y. In general, it is 3
  ggLayout_COL_pos <- which(colnames(ggLayout) == "COL")
  ggLayout_ROW_pos <- which(colnames(ggLayout) == "ROW")
  ggLayout_start_pos <- max(ggLayout_COL_pos,
                            ggLayout_ROW_pos)

  # set scales: free (free_x, free_y) or fixed?
  scales_free_x <- if(is.null(ggplotObject$facet$params$free$x)) FALSE else ggplotObject$facet$params$free$x
  scales_free_y <- if(is.null(ggplotObject$facet$params$free$y)) FALSE else ggplotObject$facet$params$free$y

  plots <- lapply(seq_len(panelNum),
                  function(i){
                    # subtitle
                    # if wrap number is larger than 0, multiple facets are displayed
                    numOfSubtitles <- wrap_num(ggLayout)
                    getSubtitle <- getSubtitle(layoutByROWS, layoutByCOLS,
                                               ggLayout = ggLayout, ggLayout_start_pos = ggLayout_start_pos,
                                               numOfSubtitles = numOfSubtitles, byROWS = byROWS, byCOLS = byCOLS,
                                               panelNum = i, is_facet_wrap = is_facet_wrap, is_facet_grid = is_facet_grid,
                                               tkLabels = tkLabels)
                    colSubtitle <- getSubtitle$colSubtitle
                    rowSubtitle <- getSubtitle$rowSubtitle
                    colSubtitles <<- c(colSubtitles, colSubtitle)
                    rowSubtitles <<- c(rowSubtitles, rowSubtitle)

                    if(!is.null(colSubtitle) & !is_facet_grid & tkLabels) {
                      sub <- as.character(tcl('label', as.character(l_subwin(tt,'label')),
                                              text= colSubtitle, background = "grey90"))
                      tkgrid(sub,
                             row = (ggLayout[i,]$ROW - 1) * span + start.ypos,
                             column = (ggLayout[i,]$COL - 1) * span + start.xpos,
                             rowspan = numOfSubtitles,
                             columnspan = span,
                             sticky="nesw")
                      start.subtitlepos <<- start.ypos + numOfSubtitles
                      newspan <- span - numOfSubtitles
                      if(newspan <= 0) stop("pick a larger span")
                    }

                    # is polar coord?
                    isCoordPolar <- is.CoordPolar(ggplotPanel_params[[i]])

                    if(isCoordPolar){
                      # theta can be only "x" or "y"
                      if(ggplotObject$coordinates$theta == "y")  swapAxes <<- TRUE
                      showGuides <- FALSE
                      showScales <- FALSE
                    } else {
                      # if not polar coord
                      # swap or not
                      if( which( names(ggplotPanel_params[[i]]) %in% "y.range"  == T ) <
                          which( names(ggplotPanel_params[[i]]) %in% "x.range"  == T ) ) swapAxes <<- TRUE
                      # show ggGuides or not
                      if (ggGuides) {
                        showGuides <- FALSE
                        showScales <- FALSE

                      } else {
                        # set panX, panY, deltaX, deltaY
                        showGuides <- TRUE
                        showScales <- TRUE
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

                    loonTitle <- paste(c(title, colSubtitle, rowSubtitle), collapse = "\n")

                    if (len_layers != 0) {
                      importantLayers <- importantLayers(len_layers, ggplotObject)

                      boxplotLayers <- importantLayers$boxplotLayers
                      curveLayers <- importantLayers$curveLayers

                      # set active geom layer and active model
                      activeInfo <- activeInfo(importantLayers, activeGeomLayers, len_layers)
                      activeGeomLayers <- activeInfo$activeGeomLayers
                      activeModel <- activeInfo$activeModel

                      # boxplot has a hidden scatterplot model layer
                      boxplot_point_layers <- c(boxplotLayers, activeGeomLayers)

                      if (is.data.frame(dataFrame) & !"waiver" %in% class(dataFrame)) {
                        mapping.x <- as.character(ggplotObject$mapping$x)
                        mapping.x <- mapping.x[-which("~" %in% mapping.x)]
                        mapping.y <- as.character(ggplotObject$mapping$y)
                        mapping.y <- mapping.y[-which("~" %in% mapping.y)]
                        column_names <- colnames(dataFrame)
                      } else {
                        if(length(activeGeomLayers) == 1) {
                          dataFrame <- ggplotObject$layers[[activeGeomLayers]]$data
                          linkingKey <- loonLinkingKey(dataFrame, args)
                          mapping.x <- as.character(ggplotObject$layers[[activeGeomLayers]]$mapping$x)
                          mapping.x <- mapping.x[-which("~" %in% mapping.x)]
                          mapping.y <- as.character(ggplotObject$layers[[activeGeomLayers]]$mapping$y)
                          mapping.y <- mapping.y[-which("~" %in% mapping.y)]
                          column_names <- colnames(dataFrame)
                        } else NULL # activeGeomLayers > 1 not implemented so far
                      }

                      if (activeModel == "l_hist" & length(activeGeomLayers) != 0) {
                        loonPlot <- loonHistogram(ggBuild = ggBuild, ggLayout_start_pos = ggLayout_start_pos,
                                                  ggLayout = ggLayout, ggplotPanel_params = ggplotPanel_params,
                                                  ggplotObject = ggplotObject, activeGeomLayers = activeGeomLayers,
                                                  panelIndex = i, column_names = column_names, dataFrame = dataFrame,
                                                  mapping.x = mapping.x, mapping.y = mapping.y,  numOfSubtitles = numOfSubtitles,
                                                  toplevel = tt, showGuides = showGuides,
                                                  showScales = showScales, swapAxes = swapAxes, linkingKey = linkingKey,
                                                  args = args, showLabels = showLabels, xlabel = xlabel, ylabel = ylabel,
                                                  loonTitle = loonTitle)


                      } else if(activeModel == "l_plot" & length(boxplot_point_layers) != 0) {
                        loonPlot <- loonScatter(ggBuild = ggBuild, ggplotObject = ggplotObject,
                                                ggplotPanel_params = ggplotPanel_params,
                                                panelIndex = i, mapping.x = mapping.x, mapping.y = mapping.y,
                                                dataFrame = dataFrame, activeGeomLayers = activeGeomLayers,
                                                isCoordPolar = isCoordPolar, toplevel = tt,
                                                showGuides = showGuides, showScales = showScales,
                                                swapAxes = swapAxes, linkingKey = linkingKey, args = args,
                                                showLabels = showLabels, xlabel = xlabel, ylabel = ylabel,
                                                loonTitle = loonTitle)

                      } else {
                        loonPlot <- l_plot(parent = tt,
                                           showGuides = showGuides,
                                           showScales = showScales,
                                           showLabels = showLabels,
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
                                                          layerGeom = ggplotObject$layers[[j]],
                                                          data =  ggBuild$data[[j]][ggBuild$data[[j]]$PANEL == i, ],
                                                          ggplotPanel_params = ggplotPanel_params[[i]],
                                                          ggplotObject = ggplotObject,
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
                          modelLayerup <- sapply(seq_len(length(which(otherLayerId < max_hist_points_layerId) == T)),
                                                 function(j){
                                                   l_layer_raise(loonPlot, "model")
                                                 })
                        }
                      }

                      # special case
                      if (length(boxplotLayers) != 0 & activeModel == "l_plot" & length(activeGeomLayers) == 0) {
                        # hidden points layer
                        l_layer_hide(loonPlot, "model")
                        # move the hidden layer on the top
                        modelLayerup <- sapply(seq_len(len_layers),
                                               function(j){
                                                 l_layer_raise(loonPlot, "model")
                                               })
                      }

                    } else loonPlot <- l_plot(parent = tt,
                                              showGuides = showGuides,
                                              showScales = showScales,
                                              showLabels = showLabels,
                                              swapAxes = swapAxes,
                                              xlabel = if(is.null(xlabel)) "" else xlabel,
                                              ylabel = if(is.null(ylabel)) "" else ylabel,
                                              title = loonTitle)

                    # resize loon plot
                    tkconfigure(paste(loonPlot,'.canvas',sep=''),
                                width = canvasHeight/column,
                                height = canvasWidth/row)
                    # tk pack
                    tkgrid(loonPlot,
                           row = (ggLayout[i,]$ROW - 1) * span + start.subtitlepos,
                           column= (ggLayout[i,]$COL - 1) * span + start.xpos,
                           rowspan = newspan,
                           columnspan = span,
                           sticky="nesw")
                    # loonPlot_configure does not produce anything but just configure the loon plot
                    loonPlot_configure <- loonPlot_configure(isCoordPolar = isCoordPolar,
                                                             loonPlot = loonPlot,
                                                             ggGuides = ggGuides,
                                                             panelIndex = i,
                                                             ggplotPanel_params = ggplotPanel_params,
                                                             swapAxes = swapAxes,
                                                             theme = theme,
                                                             panX=panX,
                                                             panY=panY,
                                                             deltaX= deltaX,
                                                             deltaY=deltaY,
                                                             zoomX = zoomX,
                                                             zoomY = zoomY)
                    loonPlot
                  })

  names(plots) <- sapply(seq_len(panelNum),
                         function(j){
                           paste0(c("x", "y"), ggLayout[j, c(ggLayout_ROW_pos, ggLayout_COL_pos)], collapse = "")
                         }
  )
  # tk column row configure
  for (j in 0:(column.span + start.xpos)) {
    tkgrid.columnconfigure(tt, j, weight=1)
  }
  for(j in 0:(row.span + start.ypos)) {
    tkgrid.rowconfigure(tt, j, weight=1)
  }

  # synchronize binding
  scalesSynchronize(plots, scales_free_x, scales_free_y)

  if(swapAxes) {
    label <- ylabel
    ylabel <- xlabel
    xlabel <- label
  }

  # pack xlabel and ylabel
  if(!is.null(xlabel) & tkLabels){
    xlab <- as.character(tcl('label', as.character(l_subwin(tt,'label')),
                             text= xlabel, background = "white"))
    tkgrid(xlab, row = row.span + start.ypos, column = start.xpos,
           rowspan = 1, columnspan = column.span,
           sticky="nesw")
  }
  if(!is.null(ylabel) & tkLabels){
    ylab <- as.character(tcl('label', as.character(l_subwin(tt,'label')),
                             text= paste(c( strsplit(ylabel, "")[[1]], " "), collapse = "\n"),
                             background = "white")
    )
    tkgrid(ylab, row = start.ypos, column = 0,
           rowspan = row.span, columnspan = 1,
           sticky="nesw")
  }

  # is_facet_grid; subtitle by row?
  if(!is.null(rowSubtitles) & is_facet_grid & tkLabels) {
    uniqueRowSubtitles <- unique(rowSubtitles)
    for(i in 1:length(uniqueRowSubtitles)){
      rowSub <- as.character(tcl('label', as.character(l_subwin(tt,'label')),
                                 text= uniqueRowSubtitles[i], background = "grey90"))
      tkgrid(rowSub, row = start.ypos + (i - 1)* span,
             column = start.xpos + column.span,
             rowspan = span, columnspan = 1,
             sticky="nesw")
    }
  }
  # is_facet_grid; subtitle by col?
  if(!is.null(colSubtitles) & is_facet_grid & tkLabels) {
    uniqueColSubtitles <- unique(colSubtitles)
    for(i in 1:length(uniqueColSubtitles)){
      colSub <- as.character(tcl('label', as.character(l_subwin(tt,'label')),
                                 text= uniqueColSubtitles[i], background = "grey90"))
      tkgrid(colSub, row = start.ypos - 1,
             column = start.xpos + (i - 1) * span,
             rowspan = 1, columnspan = span,
             sticky="nesw")
    }
  }

  if(!is.null(title) & tkLabels) {
    titleFont <- if(start.subtitlepos == start.ypos) tkfont.create(size = 16) else tkfont.create(size = 16, weight="bold")
    tit <- as.character(tcl('label', as.character(l_subwin(tt,'label')),
                            text= title, background = "white"))
    tkconfigure(tit, font = titleFont)
    tkgrid(tit, row = 0, column = start.xpos,
           rowspan = 1, columnspan = column.span,
           sticky="w")
  }

  if (panelNum == 1) {
    gp <- plots$x1y1
  } else {
    gp <- list(
      plots = plots,
      facet = list(
        is_facet_wrap = is_facet_wrap,
        is_facet_grid = is_facet_grid,
        byCOLS = byCOLS,
        byROWS = byROWS
      ),
      titles = list(
        title = title,
        colSubtitles = colSubtitles,
        rowSubtitles = rowSubtitles
      )
    )
    class(gp) <- c("l_ggplot", "l_compound", "loon")
  }

  # set args
  if(length(args) != 0){
    # args remove linkingKey and linkingGroup (if they have)
    new_args <- setNames(
      lapply(seq_len(length(args) + 1) - 1,
             function(j){
               if(j == 0) {
                 gp
               } else {
                 if(names(args)[j] == "linkingKey") NULL
                 else if(names(args)[j] == "linkingGroup") NULL
                 else args[[j]]
               }
             }
      ),
      c("target", names(args))
    )
    new_args[sapply(new_args, is.null)] <- NULL
    if(length(new_args) > 1){
      do.call(l_configure, new_args)
    }
  }

  return(gp)
}
#'@export
names.l_ggplot <- function(x) {attr(x, "names")}

#' @export
l_cget.l_ggplot <- function(target, state) {

  widgets <- target$plots
  plotNames <- names(widgets)
  plots <- lapply(plotNames,
                  function(plotName) {
                    widgets[[plotName]]
                  })
  setNames(lapply(plots, l_cget, state),
           plotNames)
}


#' @export
l_configure.l_ggplot <- function(target, ...) {

  args <- list(...)
  states <- names(args)
  widget <- target$plots

  if (is.null(states) || any("" %in% states))
    stop("configuration needs key=value pairs")

  plotNames <- names(widget)
  plots <- lapply(plotNames,
                  function(plotName) {
                    widget[[plotName]]

                  })
  states <- names(args)
  for (state in states) {

    switch(
      state,
      linkingGroup = lapply(plots, l_configure,
                            linkingGroup = args$linkingGroup, sync = "pull"),
      selected = stop("not implemented yet")
      # stop("state ", state, " not implemented")
    )
    lapply(plots,
           function(plot){
             plot[state] <- args[[state]]
           })
  }

  invisible(target)
}

# aliased in l_cget
#' @export
`[.l_ggplot` <- function(target, state) {
  l_cget(target, state)
}

# aliased in l_configure
#' @export
`[<-.l_ggplot` <- function(target, state, value) {
  args <- list(target, value)
  names(args) <- c("target", state)
  do.call("l_configure", args)
}

# 1. l_layer_lines does not work well on "dash" (l_layer_line is fine) *
# 2. arrow for every l_layer_line and l_layer_lines need to be added *
# 3. legend
# 4. transparent color (maybe impossible to do) *
# 5. specfic TODOs (before some loonLayer functions)
# 6. ggplot_build: need to rebuild for some specific data (eg: ts data) *
# 7. geom_histogram: transform to l_hist() or just leave it as l_plot() adding l_layer_rectangles() *
# 8. bar labels *

########################################### helper function ###########################################
hex6to12 <- function(col){
  if(is.null(col) ) {""} else{
    num <- length(col)
    sapply(1:num,
           function(i){
             if(is.na(col[i]) | col[i] == "NA") ""
             else{
               # ARGB is 8 digits, with last two representing transparency.
               # We have to erase last two digits (TK color codes do not include transparency information)
               splitCol <- unlist(strsplit(col[i], split = ""))
               if("#" %in% splitCol & length(splitCol) > 7 ) l_hexcolor(paste(splitCol[1:7], collapse = ""))
               else if("#" %in% splitCol & length(splitCol) < 7) ""
               else l_hexcolor(col[i])
             }
           }
    )
  }
}

pch_to_glyph <- function(pch, alpha) {
  len <- length(pch)

  switchPch <- function(pch){
    switch(
      as.character(pch),
      "16" = "circle" ,
      "1" = "ocircle",
      "21" = "ccircle",
      "15" = "square",
      "0" = "osquare",
      "22" = "csquare",
      "17" = "triangle",
      "2" = "otriangle",
      "24" = "ctriangle",
      "18" = "diamond",
      "5" = "odiamond",
      "23" = "cdiamond",
      {
        # warning("pch type ", glyph, " will be mapped to circle")
        "circle"
      }
    )
  }

  vapply(1:len,
         function(i) {
           if(is.na(alpha[i])){
             switchPch(pch[i])
           } else {
             if(alpha[i] < 0.5 ){
               switch(
                 as.character( pch[i] ),
                 "16" = "ocircle" ,
                 "1" = "ocircle",
                 "21" = "ocircle",
                 "15" = "osquare",
                 "0" = "osquare",
                 "22" = "osquare",
                 "17" = "otriangle",
                 "2" = "otriangle",
                 "24" = "otriangle",
                 "18" = "odiamond",
                 "5" = "odiamond",
                 "23" = "odiamond",
                 {
                   # warning("pch type ", glyph, " will be mapped to circle")
                   "ocircle"
                 }
               )
             } else {
               switchPch(pch[i])
             }
           }
         }, character(1))
}



as_loon_size <- function(s, type) {
  if(is.null(s) ) 1 else
    switch(type,
           "points" = ceiling( s^2 / 2),
           "lines" = 2 * s,
           "texts" = ceiling(s^2 / 1.5),
           {
             # unspecified type
             ""
           }
    )
}

as_loon_hvjust <- function(hjust, vjust) {
  if(length(hjust) != length(vjust) ) NULL
  else {
    len <- length(hjust)
    sapply(1:len,
           function(i){
             if(hjust[i] == 0.5 & vjust[i] == 0.5) "center"
             else if(hjust[i] == 0.5 & vjust[i] > 0.5) "n"
             else if(hjust[i] > 0.5 & vjust[i] > 0.5) "ne"
             else if(hjust[i] > 0.5 & vjust[i] == 0.5) "e"
             else if(hjust[i] > 0.5 & vjust[i] < 0.5) "se"
             else if(hjust[i] == 0.5 & vjust[i] < 0.5) "s"
             else if(hjust[i] < 0.5 & vjust[i] < 0.5) "sw"
             else if(hjust[i] < 0.5 & vjust[i] == 0.5) "w"
             else if(hjust[i] < 0.5 & vjust[i] > 0.5) "nw"
           })
  }
}

as_loon_dash <- function(linetype){

  lapply(linetype,
         function(l){
           if(l == 1 | is.na(l) ) ""
           else if(l == 2) rep(1, 4)
           else if(l == 3) rep(1,2)
           else rep(1, length(l))
         })

}

is.CoordPolar <- function(ggplotPanel_params){
  isyRange <- is.null(ggplotPanel_params$y.range)
  isxRange <- is.null(ggplotPanel_params$x.range)
  isthetaRange <- is.null(ggplotPanel_params$theta.range)
  isrRange <- is.null(ggplotPanel_params$r.range)
  isyRange & isxRange & !isthetaRange & !isrRange
}

abline2xy <- function(xrange, yrange, slope, intercept){
  x <- xrange
  if(slope > 0) {
    y <- c(intercept + slope * x[1], intercept + slope * x[2])
    if(y[1] < yrange[1] ) {
      x[1] <- (yrange[1] - intercept)/ slope
      y[1] <- yrange[1]
    }
    if(y[2] > yrange[2]){
      x[2] <- (yrange[2] - intercept)/ slope
      y[2] <- yrange[2]
    }
  } else {
    y <- c(intercept + slope * x[2], intercept + slope * x[1])
    if(y[1] < yrange[1] ) {
      x[2] <- (yrange[1] - intercept)/ slope
      y[1] <- yrange[1]
      y <- rev(y)
    }
    if(y[2] > yrange[2]){
      x[1] <- (yrange[2] - intercept)/ slope
      y[2] <- yrange[2]
      y <- rev(y)
    }
  }
  list(x = x, y = y)
}


loonLinkingKey <- function(data, args) {
  if (is.data.frame(data) & !"waiver" %in% class(data)) {
    # check linkingKey
    if (is.null(args[['linkingKey']])) {
      # default linkingKey
      row.names(data)
    } else {
      if (length(args[['linkingKey']]) != dim(data)[1]) {
        warning("the length of linkingKey does not match the number of observations")
      }
      # row_names <- row.names(dataFrame)
      args[['linkingKey']]
    }
  } else NULL
}

wrap_num <- function(ggLayout){
  dim2ggLayout <- dim(ggLayout)[2]
  dim2ggLayout - 5
}

importantLayers <- function(len_layers, ggplotObject){
  layerNames <- lapply(seq_len(len_layers),
                       function(j) {
                         className <- class(ggplotObject$layers[[j]]$geom)
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
                                    is.histogram_condition2 <- "StatBin" %in% class(ggplotObject$layers[[j]]$stat)
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

activeInfo <- function(importantLayers, activeGeomLayers, len_layers){

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

getSubtitle <- function(layoutByROWS, layoutByCOLS, ggLayout, ggLayout_start_pos, numOfSubtitles,
                        byROWS, byCOLS ,panelNum, is_facet_wrap, is_facet_grid, tkLabels){
  if(is_facet_wrap | !tkLabels) {
    colSubtitle <- if (numOfSubtitles > 0) {
      paste(sapply(ggLayout[panelNum, ggLayout_start_pos + c(1:numOfSubtitles)], as.character),
            collapse = "\n")
    } else NULL
    rowSubtitle <- NULL
  } else if(is_facet_grid) {
    if(byROWS & !byCOLS) {
      rowSubtitle <- paste(sapply(ggLayout[panelNum, layoutByROWS], as.character), collapse = "\n")
      colSubtitle <- NULL
    } else if(!byROWS & byCOLS){
      rowSubtitle <- NULL
      colSubtitle <- paste(sapply(ggLayout[panelNum, layoutByCOLS], as.character), collapse = "\n")
    } else if(byROWS & byCOLS){
      rowSubtitle <- paste(sapply(ggLayout[panelNum, layoutByROWS], as.character), collapse = "\n")
      colSubtitle <- paste(sapply(ggLayout[panelNum, layoutByCOLS], as.character), collapse = "\n")
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