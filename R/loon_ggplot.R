#' @title Create a loon plot from a ggplot2 object
#'
#' @description Interactive loon plots from ggplots
#'
#' @param ggplotObject a ggplot object
#' @param ggGuides if \code{TRUE}, loon plot will generate a ggplot guide layer. Default is FALSE.
#' @param active_geomLayers to determine which geom layer is active.
#' @param ... named arguments to modify loon plot states
#'
#' @return a loon widget
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
#'  suppressWarnings(g <- loon.ggplot(pp, active_geomLayers = 2))
#'
#' \dontrun{
#' ## the priority of points layer
#' df <- data.frame(
#'   x = rnorm(120, c(0, 2, 4)),
#'   y = rnorm(120, c(1, 2, 1)),
#'   z = letters[1:3]
#' )
#' df2 <- dplyr::select(df, -z)
#' pp <- ggplot(df, aes(x, y)) +
#'  geom_point(data = df2, colour = "grey70") +
#'  geom_point(aes(colour = z)) +
#'  facet_wrap(~z)
#'
#' ### check difference
#' suppressWarnings(g <- loon.ggplot(pp))
#' g <- loon.ggplot(pp, active_geomLayers = 2)
#' }


loon.ggplot <- function(ggplotObject, ggGuides = FALSE,
                        active_geomLayers = integer(0), ...){
  # check arguments
  args <- list(...)
  if (is.null(args[['linkingGroup']])) {
    args[['linkingGroup']] <- "none"
  }

  dataFrame <- ggplotObject$data
  linkingKey <- loonLinkingKey(dataFrame, args)

  # labels
  ggLabels <- list(
    title = if( is.null(ggplotObject$labels$title) ) NULL  else ggplotObject$labels$title,
    xlabel = if( is.null(ggplotObject$labels$x) ) ""  else ggplotObject$labels$x,
    ylabel = if( is.null(ggplotObject$labels$y) ) ""  else ggplotObject$labels$y
  )

  # ggplot_build
  buildggplotObject <-  ggBuild2Loon(ggplotObject, linkingKey)
  ggBuild <- buildggplotObject$ggBuild
  ggLayout <- buildggplotObject$ggLayout
  ggplotPanel_params <- buildggplotObject$ggplotPanel_params

  # theme
  theme <- ggplotObject$theme

  # number of panels
  panelNum <- dim(ggLayout)[1]

  tt <- tktoplevel()
  tktitle(tt) <- paste0("loon.ggplot", as.character(tktitle(tt)))

  # length layers
  len_layers <- length(ggplotObject$layers)

  # some default setting
  zoomX <- zoomY <- 5/6
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
                    wrap.num <- wrap_num(ggLayout)
                    subtitle <- if (wrap.num > 0) {
                      numOfSubtitles <- wrap.num
                      paste0(c(ggLabels$title, sapply(ggLayout[i, ggLayout_start_pos + c(1:numOfSubtitles)],
                                                      as.character)),
                             collapse = "\n")
                    } else ""

                    # is polar coord?
                    isCoordPolar <- is.CoordPolar(ggplotPanel_params[[i]])

                    if(isCoordPolar){
                      # theta can be only "x" or "y"
                      if(ggplotObject$coordinates$theta == "y")  swapAxes <- TRUE
                      showGuides <- FALSE
                      showScales <- FALSE
                    } else {
                      # if not polar coord
                      # swap or not
                      if( which( names(ggplotPanel_params[[i]]) %in% "y.range"  == T ) <
                          which( names(ggplotPanel_params[[i]]) %in% "x.range"  == T ) ) swapAxes <- TRUE
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

                    if (len_layers != 0) {
                      importantLayers <- importantLayers(len_layers, ggplotObject)

                      boxplotLayers <- importantLayers$boxplotLayers
                      curveLayers <- importantLayers$curveLayers

                      # set active geom layer and active model
                      activeInfo <- activeInfo(importantLayers, active_geomLayers, len_layers)
                      active_geomLayers <- activeInfo$active_geomLayers
                      active_model <- activeInfo$active_model

                      # boxplot has a hidden scatterplot model layer
                      boxplot_point_layers <- c(boxplotLayers, active_geomLayers)


                      if (is.data.frame(dataFrame) & !"waiver" %in% class(dataFrame)) {
                        mapping.x <- as.character(ggplotObject$mapping$x)
                        mapping.x <- mapping.x[-which("~" %in% mapping.x)]
                        mapping.y <- as.character(ggplotObject$mapping$y)
                        mapping.y <- mapping.y[-which("~" %in% mapping.y)]
                        column_names <- colnames(dataFrame)
                      } else {
                        if(length(active_geomLayers) != 0) {
                          dataFrame <- ggplotObject$layers[[active_geomLayers]]$data
                          linkingKey <- loonLinkingKey(dataFrame, args)
                          mapping.x <- as.character(ggplotObject$layers[[active_geomLayers]]$mapping$x)
                          mapping.x <- mapping.x[-which("~" %in% mapping.x)]
                          mapping.y <- as.character(ggplotObject$layers[[active_geomLayers]]$mapping$y)
                          mapping.y <- mapping.y[-which("~" %in% mapping.y)]
                          column_names <- colnames(dataFrame)
                        }
                      }

                      if (active_model == "l_hist" & length(active_geomLayers) != 0) {
                        loonPlot <- loonHistogram(ggBuild = ggBuild, ggLayout_start_pos = ggLayout_start_pos,
                                                  ggLayout = ggLayout, ggplotPanel_params = ggplotPanel_params,
                                                  ggplotObject = ggplotObject, ggLabels = ggLabels,
                                                  active_geomLayers = active_geomLayers, panelIndex = i, column_names = column_names,
                                                  dataFrame = dataFrame, mapping.x = mapping.x, mapping.y = mapping.y,
                                                  wrap.num = wrap.num, toplevel = tt, subtitle = subtitle, showGuides = showGuides,
                                                  showScales = showScales, swapAxes = swapAxes, linkingKey = linkingKey, args = args)


                      } else if(active_model == "l_plot" & length(boxplot_point_layers) != 0) {
                        loonPlot <- loonScatter(ggBuild = ggBuild, ggplotObject = ggplotObject,
                                                ggplotPanel_params = ggplotPanel_params, ggLabels = ggLabels,
                                                panelIndex = i, mapping.x = mapping.x, mapping.y = mapping.y,
                                                dataFrame = dataFrame, active_geomLayers = active_geomLayers,
                                                isCoordPolar = isCoordPolar, toplevel = tt, subtitle = subtitle,
                                                showGuides = showGuides, showScales = showScales,
                                                swapAxes = swapAxes, linkingKey = linkingKey, args = args)

                      } else {
                        loonPlot <- l_plot(parent = tt,
                                           title = subtitle,
                                           xlabel = ggLabels$xlabel,
                                           ylabel = ggLabels$ylabel,
                                           showGuides = showGuides,
                                           showScales = showScales,
                                           showLabels = TRUE,
                                           swapAxes = swapAxes)

                      }
                      # adding layers
                      loon_layers <- lapply(seq_len(len_layers),
                                            function(j){
                                              if(! j %in% active_geomLayers){
                                                loonLayer(widget = loonPlot,
                                                          layerGeom = ggplotObject$layers[[j]],
                                                          data =  ggBuild$data[[j]][ggBuild$data[[j]]$PANEL == i, ],
                                                          ggplotPanel_params = ggplotPanel_params[[i]],
                                                          ggplotObject = ggplotObject,
                                                          special = list(curve = list(which_curve = j,
                                                                                      curveLayers = curveLayers))
                                                )
                                              }
                                            })

                      # recover the points or histogram layer to the original position
                      if(length(active_geomLayers) != len_layers & length(active_geomLayers) != 0) {
                        otherLayerId <- (1:len_layers)[-active_geomLayers]
                        minOtherLayerId <- min(otherLayerId)
                        max_hist_points_layerId <- max(active_geomLayers)

                        if(max_hist_points_layerId > minOtherLayerId){
                          modelLayerup <- sapply(seq_len(length(which(otherLayerId < max_hist_points_layerId) == T)),
                                                 function(j){
                                                   l_layer_raise(loonPlot, "model")
                                                 })
                        }
                      }

                      # special case
                      if (length(boxplotLayers) != 0 & active_model == "l_plot" & length(active_geomLayers) == 0) {
                        # hidden points layer
                        l_layer_hide(loonPlot, "model")
                        # move the hidden layer on the top
                        modelLayerup <- sapply(seq_len(len_layers),
                                               function(j){
                                                 l_layer_raise(loonPlot, "model")
                                               })
                      }

                    } else loonPlot <- l_plot(parent = tt,
                                              title = subtitle,
                                              xlabel = ggLabels$xlabel,
                                              ylabel = ggLabels$ylabel,
                                              showGuides = showGuides,
                                              showScales = showScales,
                                              showLabels = TRUE,
                                              swapAxes = swapAxes)

                    # resize loon plot
                    tkconfigure(paste(loonPlot,'.canvas',sep=''),
                                width=850/max(ggLayout$COL),
                                height= 700/max(ggLayout$ROW))
                    # tk pack
                    tkgrid(loonPlot, row = ggLayout[i,]$ROW,
                           column=ggLayout[i,]$COL, sticky="nesw")
                    tkgrid.columnconfigure(tt, ggLayout[i,]$COL, weight=1)
                    tkgrid.rowconfigure(tt, ggLayout[i,]$ROW, weight=1)
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

  class(plots) <- c("l_ggplot", "l_compound", "loon")
  names(plots) <- sapply(seq_len(panelNum),
                         function(j){
                           paste0(c("x", "y"), ggLayout[j, c(ggLayout_ROW_pos, ggLayout_COL_pos)], collapse = "")
                         })

  # synchronize binding
  scalesSynchronize(plots, scales_free_x, scales_free_y)

  # set args
  if (length(args) != 0) {
    # args remove linkingKey and linkingGroup (if they have)
    new_args <- setNames(lapply(seq_len(length(args)),
                                function(j){
                                  if(names(args)[j] == "linkingKey") NULL
                                  else if(names(args)[j] == "linkingGroup") NULL
                                  else args[[j]]
                                }),
                         names(args))
    new_args[sapply(new_args, is.null)] <- NULL
    if(length(new_args) != 0){
      l_configure_l_ggplot(plots, new_args)
    }
  }

  plots
}
#'@export
names.l_ggplot <- function(x) {attr(x, "names")}

#' @export
l_cget.l_ggplot <- function(target, state) {

  plotNames <- names(target)
  plots <- lapply(plotNames,
                  function(plotName) {
                    target[[plotName]]

                  })
  values <- lapply(plots, l_cget, state)

  values

}


#' @export
l_configure.l_ggplot <- function(target, ...) {

  args <- list(...)
  states <- names(args)
  if (is.null(states) || any("" %in% states))
    stop("configuration needs key=value pairs")

  l_configure_l_ggplot(target, args)
}

# helper function for l_configure.l_ggplot
l_configure_l_ggplot <- function(target, args) {

  plotNames <- names(target)
  plots <- lapply(plotNames,
                  function(plotName) {
                    target[[plotName]]

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

  invisible()
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
    linkingKey <- if (is.null(args[['linkingKey']])) {
      # default linkingKey
      as.character(seq_len(dim(data)[1]) - 1)
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

activeInfo <- function(importantLayers, active_geomLayers, len_layers){

  pointLayers <- importantLayers$pointLayers
  histogramLayers <- importantLayers$histogramLayers
  boxplotLayers <- importantLayers$boxplotLayers

  point_hist_layers <- c(pointLayers, histogramLayers)
  if (length(active_geomLayers) == 0) {
    if(length(point_hist_layers) != 0) {
      active_geomLayers <- min(point_hist_layers)
      active_model <- if(active_geomLayers %in% pointLayers) "l_plot" else "l_hist"
    } else {
      active_model <- "l_plot"
    }
  } else {
    if(max(active_geomLayers) > len_layers)
      stop("the active_geomLayers is out of bound")
    canBeActive <- active_geomLayers %in% c(point_hist_layers, boxplotLayers)
    if(all(canBeActive)) {
      if(all(active_geomLayers %in% pointLayers)) { # all point layers?
        active_model <- "l_plot"
      } else if(all(active_geomLayers %in% histogramLayers)) { # all histogram layers?
        active_model <- "l_hist"
        if (length(active_geomLayers) > 1) {
          # multiple histograms?
          message("only one histogram layer is drawn as active (model) layer\n",
                  "the rest will be added as l_layer_rectangles")
          active_geomLayers <- active_geomLayers[1]
        }
      } else if(any(active_geomLayers %in% pointLayers) & any(active_geomLayers %in% histogramLayers)) {
        stop("histogram layer and point layer cannot be active at the same time")
      } else if(any(active_geomLayers %in% boxplotLayers) & any(active_geomLayers %in% histogramLayers)){
        stop("histogram layer and boxplot layer cannot be active at the same time")
      } else {
        # boxplot Layer?
        active_geomLayers <- integer(0)
        active_model <- "l_plot"
      }
    } else {
      stop(paste(c("This layer cannot be active"), collapse = " "))
    }
  }
  list(active_model = active_model,
       active_geomLayers = active_geomLayers)
}

