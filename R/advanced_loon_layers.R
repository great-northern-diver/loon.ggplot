########################################### advanced layers ###########################################

loonLayer.GeomViolin <- function(widget,
                                 layerGeom,
                                 data,
                                 ggplotPanel_params,
                                 ggplotObject,
                                 special,
                                 parent = "root",
                                 label = NULL,
                                 ...) {
  if(dim(data)[1] != 0) {

    uniGroup <- unique(data$group)
    seqLength <- 20

    if(parent == "root") {
      parent <- loon::l_layer_group(widget,
                                    label = label %||% "violins")
    }

    m <- length(uniGroup)
    lapply(1:m,
           function(i){

             method <- get_stat_param(layerGeom, "scale", ...)

             mappingLabel <- get_mappingLabel(layerGeom,
                                              name = paste(c("violin", method)),
                                              label = label,
                                              i = if(m == 1) NULL else i)

             violinGroup <- loon::l_layer_group(widget,
                                                label = mappingLabel,
                                                parent = parent)

             group_i_data <- data[data$group == uniGroup[i], ]
             group_i_revdata <- group_i_data
             n <- dim(group_i_data)[1]
             # set rev y
             group_i_revdata$y <- rev(group_i_revdata$y)
             group_i_revdata$violinwidth <- rev(group_i_revdata$violinwidth)
             # set x for both data set
             group_i_revdata$x <- group_i_revdata.x <- group_i_revdata$x + group_i_revdata$violinwidth/2
             group_i_data$x <- group_i_data.x <- group_i_data$x - group_i_data$violinwidth/2
             # extend group_i_data and group_i_revdata
             group_i_data <- group_i_data[c(seq_len(n), rep(n, seqLength)), ]
             group_i_revdata <- group_i_revdata[c(seq_len(n), rep(n, seqLength)), ]
             group_i_data$x <- c(group_i_data.x, seq(group_i_data.x[n], group_i_revdata.x[1], length.out = seqLength))
             group_i_revdata$x <- c(group_i_revdata.x, seq(group_i_revdata.x[n], group_i_data.x[1], length.out = seqLength))
             group_i_newdata <- rbind(group_i_data, group_i_revdata)

             method <- get_stat_param(layerGeom, "kernel", ...)

             loonLayer.GeomPolygon(widget,
                                   layerGeom,
                                   group_i_newdata,
                                   ggplotPanel_params,
                                   ggplotObject,
                                   special,
                                   parent = violinGroup,
                                   label = paste(c("density", method)))

             # newdata <- rbind(newdata, group_i_newdata)
             if (!is.null(layerGeom$geom_params$draw_quantiles)) {

               quantiles <- layerGeom$geom_params$draw_quantiles

               group_i_data <- data[data$group == uniGroup[i], ]
               xx <- group_i_data$violinwidth/2
               f <- approxfun(group_i_data$y, xx, yleft = 0, yright = 0)
               cumulativeArea <- sapply(group_i_data$y,
                                        function(j){
                                          integrate(f, min(group_i_data$y), j)$value
                                        })
               id <- sapply(quantiles,
                            function(j) {
                              abs_quantile_area <- abs(cumulativeArea - max(cumulativeArea) * j)
                              which(abs_quantile_area == min(abs_quantile_area))
                            })
               linesData <- group_i_data[id, ]

               linesData$x <- linesData$x - linesData$violinwidth/2
               linesData$xend <- linesData$x + linesData$violinwidth
               linesData$yend <- linesData$y

               loonLayer.GeomSegment(widget,
                                     layerGeom,
                                     linesData,
                                     ggplotPanel_params,
                                     ggplotObject,
                                     special,
                                     parent = violinGroup,
                                     label = "quantile")
             }
           })
  } else NULL
}

loonLayer.GeomRug <- function(widget,
                              layerGeom,
                              data,
                              ggplotPanel_params,
                              ggplotObject,
                              special,
                              parent = "root",
                              label = NULL,
                              ...){
  if(dim(data)[1] != 0) {
    sides <- layerGeom$geom_params$sides
    xranges <- ggplotPanel_params$x.range
    yranges <- ggplotPanel_params$y.range

    if(parent == "root") {
      parent <- loon::l_layer_group(widget,
                                    label = label %||% "rugs")
    }

    # 30 is an arbitrary choice
    topSides <- function(data, xranges, yranges, parent, label) {
      data$xend <- data$x
      data$y <- yranges[2]
      data$yend <- yranges[2] - diff(yranges)/30
      loonLayer.GeomSegment(widget,
                            layerGeom,
                            data,
                            ggplotPanel_params,
                            ggplotObject,
                            special,
                            parent = parent,
                            label = label)
    }
    bottomSides <- function(data, xranges, yranges, parent, label) {
      data$xend <- data$x
      data$y <- yranges[1]
      data$yend <- yranges[1] + diff(yranges)/30
      loonLayer.GeomSegment(widget,
                            layerGeom,
                            data,
                            ggplotPanel_params,
                            ggplotObject,
                            special,
                            parent = parent,
                            label = label)
    }
    leftSides <- function(data, xranges, yranges, parent, label){
      data$yend <- data$y
      data$x <- xranges[1]
      data$xend <- xranges[1] + diff(xranges)/30
      loonLayer.GeomSegment(widget,
                            layerGeom,
                            data,
                            ggplotPanel_params,
                            ggplotObject,
                            special,
                            parent = parent,
                            label = label)
    }
    rightSides <- function(data, xranges, yranges, parent, label){
      data$yend <- data$y
      data$x <- xranges[2]
      data$xend <- xranges[2] - diff(xranges)/30
      loonLayer.GeomSegment(widget,
                            layerGeom,
                            data,
                            ggplotPanel_params,
                            ggplotObject,
                            special,
                            parent = parent,
                            label = label)
    }
    allSides <- c("t", "r", "b", "l")
    splitSides <- strsplit(sides, "")[[1]]
    isTRBL <- allSides %in% splitSides

    mappingLabel <- get_mappingLabel(layerGeom,
                                     name = "rug",
                                     label = label,
                                     i = NULL)

    if(isTRBL[1]) topSides(data, xranges, yranges, parent, label = mappingLabel)
    if(isTRBL[2]) rightSides(data, xranges, yranges, parent, label = mappingLabel)
    if(isTRBL[3]) bottomSides(data, xranges, yranges, parent, label = mappingLabel)
    if(isTRBL[4]) leftSides(data, xranges, yranges, parent, label = mappingLabel)
  } else NULL
}



loonLayer.GeomBoxplot <- function(widget,
                                  layerGeom,
                                  data,
                                  ggplotPanel_params,
                                  ggplotObject,
                                  special,
                                  parent = "root",
                                  label = NULL,
                                  ...){

  if(dim(data)[1] != 0) {
    n <- dim(data)[1]

    if(parent == "root") {
      parent <- loon::l_layer_group(widget,
                                    label = label %||% "boxplots")
    }

    lapply(1:n,
           function(i){
             mappingLabel <- get_mappingLabel(layerGeom,
                                              name = "boxplot",
                                              label = label,
                                              i = if(n == 1) NULL else i)

             boxplotGroup <- loon::l_layer_group(widget,
                                                 label = mappingLabel,
                                                 parent = parent)
             # rectangulars
             rectData <- data[i, ]
             rectData$ymin <- data[i, ]$lower
             rectData$ymax <- data[i, ]$upper

             loonLayer.GeomRect(widget,
                                layerGeom,
                                rectData,
                                ggplotPanel_params,
                                ggplotObject,
                                special,
                                parent = boxplotGroup,
                                label = "box")

             # lines lower 25%
             linesData <- data[rep(i, 3), ]
             linesData$y <- linesData$ymin
             linesData$yend <- linesData$lower
             linesData$xend <- linesData$x

             # upper 75%
             linesData[2, ]$y <- linesData[1, ]$upper
             linesData[2, ]$yend <- linesData[1, ]$ymax

             # 50%
             linesData[3, ]$y <- linesData[3, ]$yend <- linesData[1, ]$middle
             linesData[3, ]$x <- linesData[1, ]$xmin
             linesData[3, ]$xend <- linesData[1, ]$xmax

             method <- get_stat_param(layerGeom, "coef", ...)

             loonLayer.GeomSegment(widget,
                                   layerGeom,
                                   linesData,
                                   ggplotPanel_params,
                                   ggplotObject,
                                   special,
                                   parent = boxplotGroup,
                                   label = paste(c("quantile", method)))

             # points layer
             if (length(data[i, ]$outliers[[1]]) != 0) {
               pointsData <- data[rep(i, length(data[i,]$outliers[[1]])), ]
               pointsData$y <- unlist(data[i, ]$outliers)
               if( !is.null(layerGeom$geom_params$outlier.colour) ) pointsData$colour <- layerGeom$geom_params$outlier.colour
               if( !is.null(layerGeom$geom_params$outlier.fill)) pointsData$fill <- layerGeom$geom_params$outlier.fill
               pointsData$shape <- layerGeom$geom_params$outlier.shape
               pointsData$size <- layerGeom$geom_params$outlier.size

               loonLayer.GeomPoint(widget,
                                   layerGeom,
                                   pointsData,
                                   ggplotPanel_params,
                                   ggplotObject,
                                   special,
                                   parent = boxplotGroup,
                                   label = "points")
             }
           })
  } else NULL
}



loonLayer.GeomLinerange <- function(widget,
                                    layerGeom,
                                    data,
                                    ggplotPanel_params,
                                    ggplotObject,
                                    special,
                                    parent = "root",
                                    label = NULL,
                                    ...) {
  if(dim(data)[1] != 0) {

    if (parent == "root") {
      parent <- loon::l_layer_group(widget,
                                    label = label %||% "lineranges")
    }

    n <- dim(data)[1]
    data$xend <- NA
    data$yend <- NA
    for (i in 1:n) {
      data[i, ]$xend <- data[i, ]$x
      data[i, ]$y <- data[i, ]$ymin
      data[i, ]$yend <- data[i, ]$ymax
    }

    mappingLabel <- get_mappingLabel(layerGeom,
                                     name = "linerange",
                                     label = label,
                                     i = NULL)

    loonLayer.GeomSegment(widget,
                          layerGeom,
                          data,
                          ggplotPanel_params,
                          ggplotObject,
                          special,
                          parent = parent,
                          label = mappingLabel)
  } else NULL
}



loonLayer.GeomPointrange <- function(widget,
                                     layerGeom,
                                     data,
                                     ggplotPanel_params,
                                     ggplotObject,
                                     special,
                                     parent = "root",
                                     label = NULL,
                                     ...){
  if(dim(data)[1] != 0) {

    if(parent == "root") {
      parent <- loon::l_layer_group(widget,
                                    label = label %||% "pointranges")
    }
    n <- dim(data)[1]

    lapply(1:n,
           function(i){
             mappingLabel <- get_mappingLabel(layerGeom,
                                              name = "pointrange",
                                              label = label,
                                              i = if(n == 1) NULL else i)

             pointrangeGroup <- loon::l_layer_group(widget,
                                                    label = mappingLabel,
                                                    parent = parent)

             loonLayer.GeomLinerange(widget,
                                     layerGeom,
                                     data[i, ],
                                     ggplotPanel_params,
                                     ggplotObject,
                                     special,
                                     parent = pointrangeGroup,
                                     label  = "range")

             loonLayer.GeomPoint(widget,
                                 layerGeom,
                                 data[i, ],
                                 ggplotPanel_params,
                                 ggplotObject,
                                 special,
                                 parent = pointrangeGroup,
                                 label = "point")
           })
  } else NULL
}



loonLayer.GeomCrossbar <- function(widget,
                                   layerGeom,
                                   data,
                                   ggplotPanel_params,
                                   ggplotObject,
                                   special,
                                   parent = "root",
                                   label = NULL,
                                   ...){
  if(dim(data)[1] != 0) {

    if(parent == "root") {
      parent <- loon::l_layer_group(widget,
                                    label = label %||% "crossbars")
    }
    n <- dim(data)[1]

    lapply(1:n,
           function(i){
             mappingLabel <- get_mappingLabel(layerGeom,
                                              name = "crossbar",
                                              label = label,
                                              i = if(n == 1) NULL else i)

             crossbarGroup <- loon::l_layer_group(widget,
                                                  label = mappingLabel,
                                                  parent = parent)

             loonLayer.GeomRect(widget,
                                layerGeom,
                                data[i, ],
                                ggplotPanel_params,
                                ggplotObject,
                                special,
                                parent = crossbarGroup,
                                label = "bar")

             nd <- data[i, ]
             nd$size <- 3 * nd$size
             nd$xend <- NA
             nd$yend <- NA
             nd$x <- nd$xmin
             nd$xend <- nd$xmax
             nd$yend <- nd$y

             loonLayer.GeomSegment(widget,
                                   layerGeom,
                                   nd,
                                   ggplotPanel_params,
                                   ggplotObject,
                                   special,
                                   parent = crossbarGroup,
                                   label = "cross")
           })
  } else NULL
}



loonLayer.GeomErrorbar <- function(widget,
                                   layerGeom,
                                   data,
                                   ggplotPanel_params,
                                   ggplotObject,
                                   special,
                                   parent = "root",
                                   label = NULL,
                                   ...) {
  if(dim(data)[1] != 0) {

    if(parent == "root") {
      parent <- loon::l_layer_group(widget,
                                    label = label %||% "errorbars")
    }
    n <- dim(data)[1]

    lapply(1:n,
           function(i){
             mappingLabel <- get_mappingLabel(layerGeom,
                                              name = "errorbar",
                                              label = label,
                                              i = if(n == 1) NULL else i)

             errorbarGroup <- loon::l_layer_group(widget,
                                                  label = mappingLabel,
                                                  parent = parent)

             loonLayer.GeomLinerange(widget,
                                     layerGeom,
                                     data[i, ],
                                     ggplotPanel_params,
                                     ggplotObject,
                                     special,
                                     parent = errorbarGroup,
                                     label = "range")

             newdata <- data[rep(i, 2), ]
             newdata$xend <- NA
             newdata$yend <- NA

             newdata$x <- newdata$xmin
             newdata$xend <- newdata$xmax

             newdata[1, ]$y <- newdata[1, ]$yend <- newdata[1, ]$ymin
             newdata[2, ]$y <- newdata[2, ]$yend <- newdata[2, ]$ymax

             loonLayer.GeomSegment(widget,
                                   layerGeom,
                                   newdata,
                                   ggplotPanel_params,
                                   ggplotObject,
                                   special,
                                   parent = errorbarGroup,
                                   label = "error")
           })
  } else NULL
}



loonLayer.GeomErrorbarh <- function(widget,
                                    layerGeom,
                                    data,
                                    ggplotPanel_params,
                                    ggplotObject,
                                    special,
                                    parent = "root",
                                    label = NULL,
                                    ...){
  if(dim(data)[1] != 0) {

    if(parent == "root") {
      parent <- loon::l_layer_group(widget,
                                    label = label %||%  "errorbarhs")
    }
    n <- dim(data)[1]

    lapply(1:n,
           function(i){
             mappingLabel <- get_mappingLabel(layerGeom,
                                              name = "errorbarh",
                                              label = label,
                                              i = if(n == 1) NULL else i)

             errorbarhGroup <- loon::l_layer_group(widget,
                                                   label = mappingLabel,
                                                   parent = parent)

             newdata <- data[rep(i, 3), ]
             newdata$xend <- NA
             newdata$yend <- NA

             newdata[1, ]$yend <- newdata[1, ]$y
             newdata[1, ]$x <- newdata[1, ]$xmin
             newdata[1, ]$xend <- newdata[1, ]$xmax

             newdata[2, ]$x <- newdata[2, ]$xend <- newdata[2, ]$xmax
             newdata[2, ]$y <- newdata[2, ]$ymin
             newdata[2, ]$yend <- newdata[2, ]$ymax

             newdata[3, ]$x <- newdata[3, ]$xend <- newdata[3, ]$xmin
             newdata[3, ]$y <- newdata[3, ]$ymin
             newdata[3, ]$yend <- newdata[3, ]$ymax

             loonLayer.GeomSegment(widget,
                                   layerGeom,
                                   newdata,
                                   ggplotPanel_params,
                                   ggplotObject,
                                   special,
                                   parent = errorbarhGroup,
                                   label = "barh")
           })
  } else NULL
}



# colorful line (line built with points)
loonLayer.GeomPath <- function(widget,
                               layerGeom,
                               data,
                               ggplotPanel_params,
                               ggplotObject,
                               special,
                               parent = "root",
                               label = NULL,
                               ...) {
  if(dim(data)[1] != 0) {
    isCoordPolar <- is.CoordPolar(ggplotPanel_params)
    # path group
    if (parent == "root") {
      parent <- loon::l_layer_group(widget,
                                    label = label %||% "paths")
    }

    coordinates <- ggplotObject$coordinates
    uniGroup <- unique(data$group)

    m <- length(uniGroup)

    lapply(1:m,
           function(i){
             groupData <- data[data$group == uniGroup[i], ]
             linesColor <- hex6to12(groupData$colour)
             len_uni_col <- length(unique(groupData$colour))

             linesWidth <- as_loon_size(groupData$size, "lines")
             linesDash <- as_loon_dash(groupData$linetype)
             # a single line with a single color
             if(len_uni_col == 1) {
               if(isCoordPolar){
                 coordPolarxy <- Cartesianxy2Polarxy.GeomPath(NULL, coordinates, groupData, ggplotPanel_params)
                 x <- coordPolarxy$x
                 y <- coordPolarxy$y
               } else {
                 x <- groupData$x
                 y <- groupData$y
               }

               method <- get_stat_param(layerGeom, "type", "level", "line.p", ...)

               mappingLabel <- get_mappingLabel(layerGeom,
                                                name = method %||% "path",
                                                label = label,
                                                i = if(m == 1) NULL else i)

               loon::l_layer_line(
                 widget,
                 x = x, y = y,
                 linewidth = linesWidth[1],
                 color = linesColor[1],
                 dash = linesDash[[1]],
                 parent = parent,
                 label = mappingLabel
               )
             } else {  # a line with different colors(gradual colors)
               n <- dim(groupData)[1]
               len <- ceiling( 1000/(n-1) )
               for( j in 1: (n - 1) ){
                 new <- groupData[rep(j,len), ]
                 new$x <- seq( groupData[j,]$x, groupData[j+1,]$x, length.out = len)
                 new$y <- seq( groupData[j,]$y, groupData[j+1,]$y, length.out = len)
                 if(j == 1) newdata <- new else newdata <- rbind(newdata, new)
               }

               method <- get_stat_param(layerGeom, ...)

               mappingLabel <- get_mappingLabel(layerGeom,
                                                name = method %||% "path",
                                                label = label,
                                                i = if(m == 1) NULL else i)

               loonLayer.GeomPoint(widget,
                                   layerGeom,
                                   newdata,
                                   ggplotPanel_params,
                                   ggplotObject,
                                   special,
                                   parent = parent,
                                   label = mappingLabel)
             }
           })
  } else NULL
}

loonLayer.GeomContour <- function(widget,
                                  layerGeom,
                                  data,
                                  ggplotPanel_params,
                                  ggplotObject,
                                  special,
                                  parent = "root",
                                  label = NULL,
                                  ...) {

  method <- get_stat_param(layerGeom, "contours", ...)

  if (parent == "root") {
    parent <- loon::l_layer_group(widget,
                                  label = method %||% "contours")
  }

  loonLayer.GeomPath(widget,
                     layerGeom,
                     data,
                     ggplotPanel_params,
                     ggplotObject,
                     special,
                     parent = parent,
                     label = "contour")
}

loonLayer.GeomQuantile <- function(widget,
                                   layerGeom,
                                   data,
                                   ggplotPanel_params,
                                   ggplotObject,
                                   special,
                                   parent = "root",
                                   label = NULL,
                                   ...) {

  method <- get_stat_param(layerGeom, "quantiles", ...)

  if (parent == "root") {
    parent <- loon::l_layer_group(widget,
                                  label = method %||% "quantile")
  }

  loonLayer.GeomPath(widget,
                     layerGeom,
                     data,
                     ggplotPanel_params,
                     ggplotObject,
                     special,
                     parent = parent,
                     label = "quantile",
                     "method")
}

loonLayer.GeomCurve <- function(widget,
                                layerGeom,
                                data,
                                ggplotPanel_params,
                                ggplotObject,
                                special,
                                parent = "root",
                                label = NULL,
                                ...){
  if(dim(data)[1] != 0) {
    # curve group
    if (parent == "root") {
      parent <- loon::l_layer_group(widget,
                                    label = label %||% "curves")
    }

    n <- dim(data)[1]
    size <- data$size
    colour <- data$colour
    linetype <- data$linetype

    which_curve <- special[["curve"]]$which_curve
    curveLayers <- special[["curve"]]$curveLayers

    # use "grid" to get the control coordinates
    answer <- utils::menu(c("y", "n"), title="Do you want to draw the ggplot?")

    if(answer == 1) {
      answer <- TRUE
      if (which_curve == curveLayers[1]) {
        grid.draw(ggplotObject)
        grid.force()
      }
      gridList <- grid.ls(print = FALSE)
      gridList.name <- gridList$name
      xspline.name <- gridList.name[which(stringr::str_detect(gridList.name, "curve") == TRUE)]
      # xspline.len <- length(xspline.name)

      lapply(1:n,
             function(i){
               if (n == 1) {
                 xy <- lapply(xsplinePoints(grid.get("xspline", grep=TRUE)),
                              function(coord){
                                as.numeric(coord)
                              })
               } else {
                 xy <- lapply(xsplinePoints(grid.get("xspline", grep=TRUE))[[i]],
                              function(coord){
                                as.numeric(coord)
                              })
               }
               x.start <- data[i, ]$x
               x.end <- data[i, ]$xend
               y.start <- data[i, ]$y
               y.end <- data[i, ]$yend
               # mapping to ggplot coordinates
               xy_x.diff <- xy$x[length(xy$x)] - xy$x[1]
               xy_y.diff <- xy$y[length(xy$y)] - xy$y[1]
               if(xy_x.diff == 0){
                 xy_x.diff <- 1e-16
               }
               if(xy_y.diff == 0){
                 xy_y.diff <- 1e-16
               }
               x <- (x.end - x.start) / (xy_x.diff) * (xy$x - xy$x[1]) + x.start
               y <- (y.end - y.start) / (xy_y.diff) * (xy$y - xy$y[1]) + y.start

               nd <- data.frame(x = x, y = y, group = rep(i, length(x)))
               nd$size <- size[i]
               nd$colour <- colour[i]
               nd$linetype <- linetype[i]

               method <- get_stat_param(layerGeom, ...)

               mappingLabel <- get_mappingLabel(layerGeom,
                                                name = method %||% "curve",
                                                label = label,
                                                i = if(n == 1) NULL else i)

               loonLayer.GeomPath(widget,
                                  layerGeom,
                                  nd,
                                  ggplotPanel_params,
                                  special,
                                  ggplotObject,
                                  parent,
                                  label = mappingLabel)
             })
      # grid remove
      grid.remove(xspline.name[1], redraw = FALSE)

    } else {
      answer <- FALSE
      message("Segment will be drawn instead of curve")

      method <- get_stat_param(layerGeom, ...)

      mappingLabel <- get_mappingLabel(layerGeom,
                                       name = method %||% "curve",
                                       label = label,
                                       i = NULL)

      loonLayer.GeomSegment(widget,
                            layerGeom,
                            data,
                            ggplotPanel_params,
                            special,
                            ggplotObject,
                            parent,
                            label = mappingLabel)
    }
    c("draw curve" = answer)
  } else NULL
}

loonLayer.GeomRibbon <- function(widget,
                                 layerGeom,
                                 data,
                                 ggplotPanel_params,
                                 ggplotObject,
                                 special,
                                 parent = "root",
                                 label = NULL,
                                 ...){

  if(dim(data)[1] != 0) {

    if(parent == "root") {
      parent <- loon::l_layer_group(widget,
                                    label = label %||% "ribbons")
    }

    uniGroup <- unique(data$group)
    m <- length(uniGroup)

    lapply(1:m,
           function(i){
             d <- data[data$group == uniGroup[i], ]
             n <- dim(d)[1]
             seqLength <- 20
             nd <- rbind(d, d[rep(n, seqLength), ], d)
             nd$x <- c(d$x, rep(d$x[n], seqLength), rev(d$x))
             nd$y <- c(d$ymin, seq(d$ymin[n], d$ymax[n], length.out = seqLength), rev(d$ymax))

             method <- get_stat_param(layerGeom, ...)

             mappingLabel <- get_mappingLabel(layerGeom,
                                              name = method %||% "ribbon",
                                              label = label,
                                              i = if(m == 1) NULL else i)

             loonLayer.GeomPolygon(widget,
                                   layerGeom,
                                   nd,
                                   ggplotPanel_params,
                                   ggplotObject,
                                   special,
                                   parent = parent,
                                   label = mappingLabel)
           })
  } else NULL
}

loonLayer.GeomDensity <- function(widget,
                                  layerGeom,
                                  data,
                                  ggplotPanel_params,
                                  ggplotObject,
                                  special,
                                  parent = "root",
                                  label = NULL,
                                  ...) {

  method <- get_stat_param(layerGeom, "kernel", ...)
  if(parent == "root") {
    parent <- loon::l_layer_group(widget,
                                  label = paste(c("density", method)))
  }

  loonLayer.GeomRibbon(widget,
                       layerGeom,
                       data,
                       ggplotPanel_params,
                       ggplotObject,
                       special,
                       parent = parent,
                       label = label,
                       "bw", "adjust", "n")
}


loonLayer.GeomSmooth <- function(widget,
                                 layerGeom,
                                 data,
                                 ggplotPanel_params,
                                 ggplotObject,
                                 special,
                                 parent = "root",
                                 label = NULL,
                                 ...) {
  if(dim(data)[1] != 0) {

    uniGroup <- unique(data$group)
    m <- length(uniGroup)
    parent <- loon::l_layer_group(widget,
                                  label = label %||% "smooths")

    # the only difference bewteen loonLayer.GeomRibbon is the polygon border colour. NA is set here
    seqLength <- 20

    lapply(1:m,
           function(i){
             mappingLabel <- get_mappingLabel(layerGeom,
                                              name = "smooth",
                                              label = label,
                                              i = if(m == 1) NULL else i)

             smoothGroup <- loon::l_layer_group(widget,
                                                label = mappingLabel,
                                                parent = parent)

             if(!is.null(data$se)) {

               group_i_data <- data[data$group == uniGroup[i], ]
               n <- dim(group_i_data)[1]
               # set rev data x
               group_i_revdata <- group_i_data
               group_i_revdata$x <- rev(group_i_revdata$x)
               # set y for both
               group_i_data$y <- group_i_data.y <- group_i_data$ymin
               group_i_revdata$y <- group_i_revdata.y <- rev(group_i_revdata$ymax)
               # extend data and rev data
               group_i_data <- group_i_data[c(seq_len(n), rep(n, seqLength)), ]
               group_i_revdata <- group_i_revdata[c(seq_len(n), rep(n, seqLength)), ]
               group_i_data$y <- c(group_i_data.y, seq(group_i_data.y[n], group_i_revdata.y[1], length.out = seqLength))
               group_i_revdata$y <- c(group_i_revdata.y, seq(group_i_revdata.y[n], group_i_data.y[1], length.out = seqLength))
               group_i_newdata <- rbind(group_i_data, group_i_revdata)
               # draw polygon but hidden
               group_i_newdata$colour <- NA
               se_polygon <- loonLayer.GeomPolygon(widget,
                                                   layerGeom,
                                                   group_i_newdata,
                                                   ggplotPanel_params,
                                                   ggplotObject,
                                                   special,
                                                   parent = smoothGroup,
                                                   label = "se polygon")
               loon::l_layer_hide(widget, se_polygon)
               # draw se path
               group_i_newdata$colour <- group_i_newdata$fill
               loonLayer.GeomPath(widget,
                                  layerGeom,
                                  group_i_newdata,
                                  ggplotPanel_params,
                                  ggplotObject,
                                  special,
                                  parent = smoothGroup,
                                  label = "se")
             }

             smooth_method <- get_stat_param(layerGeom, ...)

             loonLayer.GeomPath(widget,
                                layerGeom,
                                data[data$group == uniGroup[i], ],
                                ggplotPanel_params,
                                ggplotObject,
                                special,
                                parent = smoothGroup,
                                label = smooth_method %||% "line")
           })
  }
}

loonLayer.GeomStep <- function(widget,
                               layerGeom,
                               data,
                               ggplotPanel_params,
                               ggplotObject,
                               special,
                               parent = "root",
                               label = NULL,
                               ...) {
  if(dim(data)[1] != 0) {

    if(parent == "root") {
      parent <- loon::l_layer_group(widget,
                                    label = label %||% "steps")
    }

    direction <- layerGeom$geom_params$direction
    uniGroup <- unique(data$group)
    m <- length(uniGroup)

    lapply(1:m,
           function(i){
             d <- data[data$group == uniGroup[i], ]
             # n must be larger than 2 (line)
             n <- dim(d)[1]
             # stepNewAddedMatrix is the new matrix which would be added on d
             stepNewAddedMatrix <- d[-n, ]
             newOrder <- c(1:n, (1:(n-1) + 0.5))
             if(direction == "hv") {
               for(j in 1: (n-1)){
                 stepNewAddedMatrix[j, ]$x <- d[j+1, ]$x
                 stepNewAddedMatrix[j, ]$y <- d[j, ]$y
               }
             } else {
               for(j in 1: (n-1)){
                 stepNewAddedMatrix[j, ]$x <- d[j, ]$x
                 stepNewAddedMatrix[j, ]$y <- d[j+1, ]$y
               }
             }
             newdata <- rbind(d, stepNewAddedMatrix)[order(newOrder) ,]

             method <- get_stat_param(layerGeom, ...)

             mappingLabel <- get_mappingLabel(layerGeom,
                                              name = method %||% "step",
                                              label = label,
                                              i = if(m == 1) NULL else i)

             loonLayer.GeomPath(widget,
                                layerGeom,
                                newdata,
                                ggplotPanel_params,
                                ggplotObject,
                                special,
                                parent = parent,
                                label = mappingLabel)
           })
  } else NULL
}



loonLayer.GeomRaster <- function(widget,
                                 layerGeom,
                                 data,
                                 ggplotPanel_params,
                                 ggplotObject,
                                 special,
                                 parent = "root",
                                 label = NULL,
                                 ...) {

  if(dim(data)[1] != 0) {
    isCoordPolar <- is.CoordPolar(ggplotPanel_params)
    n <- dim(data)[1]
    fillColor <- hex6to12(data$fill)
    linesColor <- hex6to12(data$colour)
    linesWidth <- as_loon_size(data$size, "lines")
    xrange <- ggplotPanel_params$x.range
    yrange <- ggplotPanel_params$y.range

    coordinates <- ggplotObject$coordinates

    method <- get_stat_param(layerGeom, ...)

    mappingLabel <- get_mappingLabel(layerGeom,
                                     name = method %||% "raster",
                                     label = label,
                                     i = NULL)

    if(n == 1) {
      if(isCoordPolar){

        coordPolarxy <- Cartesianxy2Polarxy.GeomRect(NULL, coordinates, data, ggplotPanel_params)
        x <- coordPolarxy$x
        y <- coordPolarxy$y
        loon::l_layer_polygon(
          widget, x = x, y = y,
          color = fillColor,
          linecolor = linesColor,
          linewidth = linesWidth,
          parent = parent,
          label = mappingLabel
        )
      } else {

        x <- c(data$xmin, data$xmax)
        y <- c(data$ymin, data$ymax)
        loon::l_layer_rectangle(
          widget,
          x = x, y = y,
          color = fillColor,
          linecolor = linesColor,
          linewidth = linesWidth,
          parent = parent,
          label = mappingLabel
        )
      }
    } else {

      # any NA will not be drawn
      if(isCoordPolar) {

        x <- y <- list()
        lapply(1:n,
               function(i){
                 coordPolarxy <- Cartesianxy2Polarxy.GeomRect(NULL, coordinates, data[i, ], ggplotPanel_params)
                 x[[i]] <<- coordPolarxy$x
                 y[[i]] <<- coordPolarxy$y
               }
        )

        loon::l_layer_polygons(
          widget,
          x = x,
          y = y,
          color = fillColor,
          linecolor = linesColor,
          linewidth = linesWidth,
          parent = parent,
          label = mappingLabel
        )
      } else {

        x <- y <- list()
        lapply(1:n,
               function(i){
                 y <- if(is.na(data[i,]$ymin) & is.na(data[i,]$ymax)) rep(data[i,]$y, 2)
                 else if(is.na(data[i,]$ymin) & !is.na(data[i,]$ymax)) c(2 * data[i,]$y - data[i,]$ymax  , data[i,]$ymax)
                 else if(!is.na(data[i,]$ymin) & is.na(data[i,]$ymax)) c(data[i,]$ymin  , 2 * data[i,]$y - data[i,]$ymin)
                 else{
                   c(if(is.infinite(data[i,]$ymin)) yrange[1] else data[i,]$ymin,
                     if(is.infinite(data[i,]$ymax)) yrange[2] else data[i,]$ymax)
                 }
                 x <- if(is.na(data[i,]$xmin) & is.na(data[i,]$xmax)) rep(data[i,]$x, 2)
                 else if(is.na(data[i,]$xmin) & !is.na(data[i,]$xmax)) c(2 * data[i,]$x - data[i,]$xmax  , data[i,]$xmax)
                 else if(!is.na(data[i,]$xmin) & is.na(data[i,]$xmax)) c(data[i,]$xmin  , 2 * data[i,]$x - data[i,]$xmin)
                 else{
                   c(if(is.infinite(data[i,]$xmin)) xrange[1] else data[i,]$xmin,
                     if(is.infinite(data[i,]$xmax)) xrange[2] else data[i,]$xmax)
                 }

                 x[[i]] <<- x
                 y[[i]] <<- y
               }
        )

        loon::l_layer_rectangles(
          widget,
          x = x, y = y,
          color = fillColor,
          linecolor = linesColor,
          linewidth = linesWidth,
          parent = parent,
          label = mappingLabel
        )
      }
    }
  } else NULL
}

get_mappingLabel <- function(layerGeom, name, label = NULL, i = NULL) {

  if(is.null(label)) {
    if(is.null(layerGeom$mapping)) {
      paste(c(name, i), collapse = " ")
    } else {
      m <- length(layerGeom$mapping)
      names <- paste(
        sapply(1:m,
               function(j){
                 mapping <- as.character(layerGeom$mapping[[j]])
                 mapping <- mapping[-which("~" %in% mapping)]
               }
        ), collapse = ";")
      paste(c(names, i), collapse = " ")
    }
  } else paste(c(label, i), collapse = " ")
}

# TODO: is there a function in ggplot to grab stat_params method?
get_stat_param <- function(layerGeom, ...) {

  args <- list(...)
  if(length(args) == 0) {

    # default
    stat_param <- layerGeom$stat_params[["method"]]
    if(is.null(stat_param)) NULL else paste0("method=", stat_param)

  } else {

    n <- length(args)

    stat_params <- unlist(
      lapply(1:n,
             function(i){
               stat_param <- layerGeom$stat_params[[args[[i]]]]
               if(is.null(stat_param)) NULL else paste0(paste0(args[[i]], "="), paste(stat_param, collapse = ","))
             })
    )

    if(is.null(stat_params)) NULL else paste(stat_params, collapse = " ")
  }
}
