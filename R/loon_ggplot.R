#' @title Create a loon plot from a ggplot2 object
#'
#' @description Interactive loon plots from ggplots
#'
#' @param ggplotObject a ggplot object
#' @param ggGuides if \code{TRUE}, loon plot will generate a ggplot guide layer. Default is FALSE.
#' @param activeLayer to determine which layer is active. See details
#' @param ... named arguments to modify loon plot states
#'
#' @return a loon widget
#'
#' @details \code{activeLayer} is to help to set model. \code{ggplot} object can be transformed to model \code{l_plot()} or model \code{l_hist()}
#' (points and histograms cannot be active at the same time in \code{loon} so far). If point layers and histogram layers
#' both exist in \code{ggplot}, \code{activeLayer} can help you to set the model (l_plot() or l_hist(), one of these two); if multiple point
#' layers exist in \code{ggplot}, \code{activeLayer} can set which point layer is the model layer (selectable). Notice, more than
#' one point layers can be selectable at the same time, if so, the points configuration will be merged.
#'
#' \code{activeLayer} is a list and the order is very important: \code{list(l_plot = c(1,3,5),l_hist = c(1)} means if point layers exist,
#' the first, third and fifth point layer will be merged and \code{l_plot()} will be chosen as the model; if no point layers exist,
#' but histogram layers exist,  then \code{l_hist()} will be used to construct the \code{loon} plot; if neither of point layers and histogram
#' layers exists, \code{l_plot()} will be created as the model.
#'
#'
#'
#' @import ggplot2 loon tcltk methods
#' @importFrom stats quantile approxfun integrate setNames
#' @importFrom utils packageVersion menu data
#' @importFrom grDevices extendrange
#' @importFrom stringr str_detect
#'
#' @export
#'
#' @examples
#'
#'  # set linkingGroup
#'  p1 <- ggplot(mtcars, aes(mpg, wt, colour = as.factor(cyl))) + geom_point()+ facet_wrap(~gear)
#'  p1
#'  g1 <- loon.ggplot(p1, linkingGroup = "mtcars")
#'  p2 <- ggplot(mtcars, aes(x = mpg, fill = as.factor(cyl))) + geom_histogram()
#'  p2
#'  g2 <- loon.ggplot(p2, linkingGroup = "mtcars")
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
#'  suppressWarnings(g <- loon.ggplot(pp))
#'  g <- loon.ggplot(pp,
#'                   activeLayer = list(
#'                     l_hist = c(1),
#'                     l_plot = c(1))
#'                   )
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
#' g <- loon.ggplot(pp,
#'                  activeLayer = list(
#'                    l_plot = 2)
#'                  )
#' }


loon.ggplot <- function(ggplotObject, ggGuides = FALSE,
                        activeLayer = list(l_plot = c(1), l_hist = c(1)), ...){

  args <- list(...)
  dataFrame <- ggplotObject$data

  linkingKey <- loonLinkingKey(dataFrame, args)

  if (is.null(args[['linkingGroup']])) {
    args[['linkingGroup']] <- "none"
  }

  # lables
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

  p <- lapply(seq_len(panelNum),
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
                  layerNames <- lapply(seq_len(len_layers),
                                       function(j) {
                                         className <- class(ggplotObject$layers[[j]]$geom)
                                         className[-which(className %in% c("ggproto"  ,"gg" ,"Geom"))]
                                       })

                  # take the point layer as l_plot
                  pointsLayerId <- which(sapply(layerNames,
                                                function(layerName){
                                                  "GeomPoint" %in% layerName
                                                }) == TRUE
                  )

                  # take the histogram layer as l_hist
                  histogramLayerId <- which(sapply(seq_len(length(layerNames)),
                                                   function(j){
                                                     # it could be bar plot
                                                     is.histogram_condition1 <- all(c("GeomBar", "GeomRect") %in% layerNames[[j]])
                                                     # stat class of geom_bar is StatCount
                                                     is.histogram_condition2 <- "StatBin" %in% class(ggplotObject$layers[[j]]$stat)
                                                     is.histogram_condition1 & is.histogram_condition2
                                                   }) == TRUE
                  )

                  # boxlayer id
                  boxplotLayerId <- which(sapply(layerNames,
                                                 function(layerName){
                                                   "GeomBoxplot" %in% layerName
                                                 }) == TRUE
                  )

                  # curvelayer id
                  curveLayerId <- which(sapply(layerNames,
                                                 function(layerName){
                                                   "GeomCurve" %in% layerName
                                                 }) == TRUE
                  )

                  if(!is.list(activeLayer)) {
                    stop("activeLayer must be a list, with names l_plot or l_hist")
                  }
                  activeLayer_names <- names(activeLayer)
                  len.activeLayer_names  <- length(activeLayer_names)
                  # set layer as model layer
                  for(j in seq_len(len.activeLayer_names + 1)){
                    if(j == (len.activeLayer_names + 1)) {
                      activeLayer_name <- "l_plot"
                      break
                    }
                    if(activeLayer_names[j] == "l_plot" &
                       length(pointsLayerId) != 0) {
                      activeLayer_name <- "l_plot"
                      break
                    } else if(activeLayer_names[j] == "l_hist" &
                              length(histogramLayerId) != 0) {
                      activeLayer_name <- "l_hist"
                      break
                    }
                  }
                  # set match id
                  match_id <- switch(activeLayer_name,
                                     "l_plot" = {
                                       if(length(pointsLayerId) == 0) {
                                         integer(0)
                                       } else {
                                         l_plot_activeLayerId <- activeLayer[['l_plot']]
                                         if(max(l_plot_activeLayerId) > length(pointsLayerId)) {
                                           warning("the l_plot max layer id cannot be larger than the number of points layer id")
                                           l_plot_activeLayerId <- 1
                                         }
                                         pointsLayerId[l_plot_activeLayerId]
                                       }
                                     },
                                     "l_hist" = {
                                       if(length(histogramLayerId) == 0){
                                         integer(0)
                                       } else {
                                         l_hist_activeLayerId <- activeLayer[['l_hist']]
                                         if (length(histogramLayerId) > 1) {
                                           # multiple histograms?
                                           message("only one histogram layer is drawn as active (model) layer\n",
                                                   "the rest will be added as l_layer_rectangles")
                                         }
                                         if(max(l_hist_activeLayerId) > length(histogramLayerId)) {
                                           warning("the l_hist max layer id cannot be larger than the number of histogram layer id")
                                           l_plot_activeLayerId <- 1
                                         }
                                         histogramLayerId[l_hist_activeLayerId]
                                       }
                                     },
                                     {
                                       warning("The name of activeLayer can only be l_plot or l_hist, else none would be active")
                                       integer(0)
                                     }
                  )
                  # boxplot has a hidden scatterplot model layer
                  boxplot_points_layerId <- c(boxplotLayerId, match_id)

                  # TODO [2] should be replaced
                  if (is.data.frame(dataFrame) & !"waiver" %in% class(data)) {
                    mapping.x <- as.character(ggplotObject$mapping$x)[2]
                    mapping.y <- as.character(ggplotObject$mapping$y)[2]
                    column_names <- colnames(dataFrame)
                  } else {
                    if(length(match_id) != 0) {
                      dataFrame <- ggplotObject$layers[[match_id]]$data
                      linkingKey <- loonLinkingKey(dataFrame, args)
                      mapping.x <- as.character(ggplotObject$layers[[match_id]]$mapping$x)[2]
                      mapping.y <- as.character(ggplotObject$layers[[match_id]]$mapping$y)[2]
                      column_names <- colnames(dataFrame)
                    }
                  }

                  if (activeLayer_name == "l_hist" & length(match_id) != 0) {
                    # set binwidth
                    hist_data <- ggBuild$data[[match_id]]
                    binwidth_vec <- hist_data[hist_data$PANEL == i, ]$xmax - hist_data[hist_data$PANEL == i, ]$xmin
                    binwidth <- binwidth_vec[!is.na(binwidth_vec)][1]
                    hist_x <- as.numeric(with(dataFrame, eval(parse(text = mapping.x))))
                    # one facet
                    if (wrap.num == 0) {
                      isPanel_i.hist_x <- rep(TRUE, length(hist_x))
                    } else {
                      # multiple facets
                      panel_i.list <- lapply((1:wrap.num + ggLayout_start_pos),
                                             function(j) {
                                               c(names(ggLayout[i, ])[j], as.character(ggLayout[i, j]))
                                             })
                      isPanel_i.hist_x_TorF <- lapply(seq_len(length(panel_i.list)),
                                                      function(j){
                                                        unlist(dataFrame[, panel_i.list[[j]][1]]) == panel_i.list[[j]][2]
                                                      })
                      # one condition or multiple conditions; "if else" is not necessary, but for faster speed
                      isPanel_i.hist_x <- if (length(isPanel_i.hist_x_TorF) == 1) {
                        isPanel_i.hist_x_TorF[[1]]
                      } else {
                        sapply(seq_len(length(hist_x)),
                               function(j) {
                                 all(sapply(seq_len(length(isPanel_i.hist_x_TorF)),
                                            function(l){
                                              isPanel_i.hist_x_TorF[[l]][j]
                                            }))
                               })
                      }
                    }
                    hist_values <- hist_x[isPanel_i.hist_x]
                    # histogram start value, end value
                    start_value <- min(hist_data[hist_data$PANEL == i, ]$xmin, na.rm = TRUE)
                    end_value <- max(hist_data[hist_data$PANEL == i, ]$xmax, na.rm = TRUE)
                    # any x y limit?
                    x.limits <- ggBuild$layout$panel_scales_x[[match_id]]$limits
                    y.limits <- ggBuild$layout$panel_scales_y[[match_id]]$limits
                    in_x.limits <- in_y.limits <- rep(TRUE, length(hist_values))

                    if (!is.null(x.limits)) {
                      if(is.na(x.limits[1])) x.limits[1] <- ggplotPanel_params[[i]]$x.range[1]
                      if(is.na(x.limits[2])) x.limits[2] <- ggplotPanel_params[[i]]$x.range[2]
                      in_x.limits <- hist_values > x.limits[1] & hist_values < x.limits[2]
                    }

                    if (!is.null(y.limits)) {
                      if(is.na(y.limits[1])) y.limits[1] <- max(0, ggplotPanel_params[[i]]$y.range[1])
                      if(is.na(y.limits[2])) y.limits[2] <- ggplotPanel_params[[i]]$y.range[2]
                      bins <- 0
                      while ((start_value + bins * binwidth) <= end_value) {

                        bin_id <- if (bins == 0) {
                          which((start_value + bins * binwidth <= hist_values &
                                   start_value + (bins + 1) * binwidth >= hist_values) == TRUE)
                        } else {
                          which((start_value + bins * binwidth < hist_values &
                                   start_value + (bins + 1) * binwidth >= hist_values) == TRUE)
                        }
                        bin_height <- length(bin_id)
                        if (bin_height != 0) {
                          if(bin_height < y.limits[1] | bin_height > y.limits[2]) {
                            in_y.limits[bin_id] <- FALSE
                          }
                        }
                        bins <- bins + 1
                      }
                    }
                    in_limits <- in_x.limits & in_y.limits
                    # hist_values should be in the x y limits
                    hist_values <- hist_values[in_limits]

                    # reset the minimum "hist_values" to be the start value
                    hist_values[which(hist_values == min(hist_values))[1]] <- start_value

                    color <- hex6to12(hist_data$fill[1])
                    colorStackingOrder <- "selected"
                    # set stack color
                    if (!is.null(ggplotObject$labels$fill)) {
                      # fill color bin?
                      if (ggplotObject$labels$fill != "fill") {

                        color_var <- unlist(
                          dataFrame[, which(stringr::str_detect(ggplotObject$labels$fill, column_names) == TRUE)]
                        )
                        panel_i.color_var <- color_var[isPanel_i.hist_x][in_limits]
                        fill_color <- hex6to12(unique(hist_data$fill))
                        levels <- rev(levels(as.factor(color_var)))
                        if (length(fill_color) == length(levels)) {
                          color <- rep(NA, length(hist_values))
                          for(j in seq_len(length(levels))){
                            color[which(panel_i.color_var %in% levels[j])] <- fill_color[j]
                          }
                          colorStackingOrder <- c("selected", fill_color)
                        }
                      }
                    }

                    # show outline color or not
                    showOutlines <- if (any(!hex6to12(hist_data$colour) %in% "" )) TRUE else FALSE
                    # set linkingKey
                    linkingKey <- linkingKey[isPanel_i.hist_x][in_limits]
                    # set yshows
                    yshows <- "frequency"

                    if (!is.na(mapping.y)) {
                      if(any(str_detect(as.character(mapping.y), "density"))) yshows <- "density"
                    }
                    if (!is.null(ggplotObject$layers[[match_id]]$mapping$y)) {
                      if(any(str_detect(as.character(ggplotObject$layers[[match_id]]$mapping$y), "density"))) yshows <- "density"
                    }
                    # loon histogram
                    l_setColorList_ggplot2()
                    loonPlot <- l_hist(parent = tt,
                                       x = hist_values,
                                       color = color,
                                       title = subtitle,
                                       binwidth = binwidth + 1e-6, # need more thoughts
                                       xlabel = ggLabels$xlabel,
                                       ylabel = ggLabels$ylabel,
                                       showGuides = showGuides,
                                       showScales = showScales,
                                       showOutlines = showOutlines,
                                       swapAxes = swapAxes,
                                       colorStackingOrder = colorStackingOrder,
                                       yshows = yshows,
                                       linkingKey = linkingKey,
                                       showStackedColors = TRUE,
                                       linkingGroup = args$linkingGroup)

                  } else if(activeLayer_name == "l_plot" &
                            length(boxplot_points_layerId) != 0) {

                    if(length(match_id) !=0) {
                      # combine points data
                      combined.pointsData <- lapply(match_id,
                                                    function(k){
                                                      Layer.k <- ggBuild$data[[k]]
                                                      data <- Layer.k[Layer.k$PANEL == i, ]
                                                      x <- data$x
                                                      y <- data$y
                                                      label <- data$label
                                                      color <- sapply(1:dim(data)[1],
                                                                      function(j){
                                                                        if(data$shape[j] %in% 21:24 ){
                                                                          hex6to12(data$fill[j])
                                                                        }else {
                                                                          hex6to12(data$colour[j])
                                                                        }
                                                                      })
                                                      glyph <- pch_to_glyph(data$shape, data$alpha)
                                                      size <- as_loon_size( data$size , "points" )

                                                      if (is.null(label)) {
                                                        label <- paste0("item", seq_len(length(x)) - 1, "panel", i)
                                                        warning("item lable may not match")
                                                      }

                                                      data.frame(x = x, y = y, label = label, color = color, glyph = glyph, size = size)
                                                    })

                      combined.pointsData <- do.call(rbind, combined.pointsData)
                      combined.pointsData$color <- as.character( combined.pointsData$color)
                      combined.pointsData$glyph <- as.character( combined.pointsData$glyph)
                      # linkingKey
                      combined.pointsData$itemLabel <- combined.pointsData$linkingKey <- as.character(combined.pointsData$label)

                    } else {
                      combined.pointsData <- data.frame(x = with(dataFrame, eval(parse(text = mapping.x))),
                                                        y = with(dataFrame, eval(parse(text = mapping.y))))
                      # in case
                      combined.pointsData$x <- as.numeric(combined.pointsData$x)
                      combined.pointsData$y <- as.numeric(combined.pointsData$y)
                      # some default settings, need more thought
                      combined.pointsData$size <- 3
                      combined.pointsData$color <- "black"
                      combined.pointsData$glyph <- "circle"
                      # linkingKey
                      combined.pointsData$itemLabel <- combined.pointsData$linkingKey <- linkingKey
                    }

                    if(isCoordPolar) {
                      coordPolarxy <- Cartesianxy2Polarxy(NULL,
                                                          coordinates = ggplotObject$coordinates,
                                                          data = combined.pointsData,
                                                          ggplotPanel_params = ggplotPanel_params[[i]])
                      x <- coordPolarxy$x
                      y <- coordPolarxy$y
                    } else {
                      x <- combined.pointsData$x
                      y <- combined.pointsData$y
                    }

                    # loon scatter plot
                    loonPlot <- l_plot(parent = tt,
                                       x = x,
                                       y = y,
                                       title = subtitle,
                                       size = combined.pointsData$size,
                                       color = combined.pointsData$color,
                                       glyph = combined.pointsData$glyph,
                                       itemLabel = combined.pointsData$itemLabel,
                                       linkingKey = combined.pointsData$linkingKey,
                                       showGuides = showGuides,
                                       showScales = showScales,
                                       xlabel = ggLabels$xlabel,
                                       ylabel = ggLabels$ylabel,
                                       showLabels = TRUE,
                                       showItemLabels = TRUE,
                                       swapAxes = swapAxes,
                                       linkingGroup = args$linkingGroup)
                  } else {
                    # loon plot
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
                                          if(! j %in% match_id){
                                            loonLayer(widget = loonPlot,
                                                      layerGeom = ggplotObject$layers[[j]],
                                                      data =  ggBuild$data[[j]][ggBuild$data[[j]]$PANEL == i, ],
                                                      ggplotPanel_params = ggplotPanel_params[[i]],
                                                      ggplotObject = ggplotObject,
                                                      special = list(curve = list(which_curve = j,
                                                                                  curveLayerId = curveLayerId))
                                            )
                                          }
                                        })

                  # do we need to draw ggplot with curve layer
                  if (length(curveLayerId) > 0) {
                    if(any(vapply(curveLayerId,
                               function(j) {
                                 loon_layers[[j]]
                               }, logical(1)))) grid.draw(ggplotObject)
                  }

                  # recover the points or histogram layer to the original position
                  if(length(match_id) != len_layers & length(match_id) != 0) {
                    otherLayerId <- (1:len_layers)[-match_id]
                    minOtherLayerId <- min(otherLayerId)
                    max_hist_points_layerId <- max(match_id)

                    if(max_hist_points_layerId > minOtherLayerId){
                      modelLayerup <- sapply(seq_len(length(which(otherLayerId < max_hist_points_layerId) == T)),
                                             function(j){
                                               l_layer_raise(loonPlot, "model")
                                             })
                    }
                  }

                  # special case
                  if (length(boxplotLayerId) != 0 &
                      activeLayer_name == "l_plot" &
                      length(match_id) == 0) {
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

                tkgrid(loonPlot, row = ggLayout[i,]$ROW,
                       column=ggLayout[i,]$COL, sticky="nesw")
                tkgrid.columnconfigure(tt, ggLayout[i,]$COL, weight=1)
                tkgrid.rowconfigure(tt, ggLayout[i,]$ROW, weight=1)

                # draw specific guides
                if (isCoordPolar) {

                  if ("l_hist" %in% class(loonPlot)) {
                    warning("l_hist only works properly with Cartesian coordinates")
                  } else {
                    polarGuides <- polarGuides(loonPlot, ggplotPanel_params[[i]], swapAxes)
                    # lower to bottom
                    children <- l_layer_getChildren(loonPlot)
                    # the length of children is at least two
                    sapply(1:(length(children) - 1),
                           function(l){
                             l_layer_lower(loonPlot, polarGuides)
                           })
                    l_scaleto_world(loonPlot)
                  }

                } else {

                  if (ggGuides) {
                    CartesianGuides <- CartesianGuides(loonPlot, ggplotPanel_params[[i]], swapAxes)
                    # lower to bottom
                    children <- l_layer_getChildren(loonPlot)
                    # the length of children is at least two
                    sapply(seq_len(length(children) - 1),
                           function(l){
                             l_layer_lower(loonPlot, CartesianGuides)
                           })
                    l_scaleto_world(loonPlot)
                  } else {

                    l_configure(loonPlot,
                                panX=panX,
                                panY=panY,
                                deltaX= deltaX,
                                deltaY=deltaY,
                                zoomX = zoomX,
                                zoomY = zoomY)
                  }
                }

                loonPlot
              })
  class(p) <- c("l_ggplot", "loon")
  names(p) <- sapply(seq_len(panelNum),
                     function(j){
                       paste0(c("x", "y"), ggLayout[j, c(ggLayout_ROW_pos, ggLayout_COL_pos)], collapse = "")
                     })

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
      l_configure_l_ggplot(p, new_args)
    }
  }

  p

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

  vapply(1:len, function(i) {
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
