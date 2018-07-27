########################################### advanced layers ###########################################

loonLayer.GeomViolin <- function(widget,
                                 layerGeom,
                                 data,
                                 ggplotPanel_params,
                                 ggplotObject,
                                 special,
                                 parent = "root"){
  uniGroup <- unique(data$group)
  seqLength <- 20
  newdata <- data.frame()
  for (i in uniGroup) {
    group_i_data <- data[data$group == i, ]
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
    newdata <- rbind(newdata, group_i_newdata)
  }

  violinGroup <- l_layer_group(widget, "violin")

  loonLayer.GeomPolygon(widget,
                        layerGeom,
                        newdata,
                        ggplotPanel_params,
                        ggplotObject,
                        special,
                        parent = if(parent == "root") violinGroup else parent)

  if (!is.null(layerGeom$geom_params$draw_quantiles)) {
    quantiles <- layerGeom$geom_params$draw_quantiles
    len_quantiles <- length(quantiles)

    linesData <- do.call(rbind, lapply(uniGroup,
                                       function(i){
                                         group_i_data <- data[data$group==i, ]
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
                                         group_i_data[id, ]
                                       })
                         )
    linesData$x <- linesData$x - linesData$violinwidth/2
    linesData$xend <- linesData$x + linesData$violinwidth
    linesData$yend <- linesData$y
    loonLayer.GeomSegment(widget,
                          layerGeom,
                          linesData,
                          ggplotPanel_params,
                          ggplotObject,
                          special,
                          parent = if(parent == "root") violinGroup else parent)
  }
}



loonLayer.GeomRug <- function(widget,
                              layerGeom,
                              data,
                              ggplotPanel_params,
                              ggplotObject,
                              special,
                              parent = "root"){

  sides <- layerGeom$geom_params$sides
  xranges <- ggplotPanel_params$x.range
  yranges <- ggplotPanel_params$y.range

  rugGroup <- l_layer_group(widget, "rug")
  # 30 is an arbitrary choice
  topSides <- function(data, xranges, yranges){
    data$xend <- data$x
    data$y <- yranges[2]
    data$yend <- yranges[2] - diff(yranges)/30
    loonLayer.GeomSegment(widget,
                          layerGeom,
                          data,
                          ggplotPanel_params,
                          ggplotObject,
                          special,
                          parent = if(parent == "root") rugGroup else parent)
  }
  bottomSides <- function(data, xranges, yranges){
    data$xend <- data$x
    data$y <- yranges[1]
    data$yend <- yranges[1] + diff(yranges)/30
    loonLayer.GeomSegment(widget,
                          layerGeom,
                          data,
                          ggplotPanel_params,
                          ggplotObject,
                          special,
                          parent = if(parent == "root") rugGroup else parent)
  }
  leftSides <- function(data, xranges, yranges){
    data$yend <- data$y
    data$x <- xranges[1]
    data$xend <- xranges[1] + diff(xranges)/30
    loonLayer.GeomSegment(widget,
                          layerGeom,
                          data,
                          ggplotPanel_params,
                          ggplotObject,
                          special,
                          parent = if(parent == "root") rugGroup else parent)
  }
  rightSides <- function(data, xranges, yranges){
    data$yend <- data$y
    data$x <- xranges[2]
    data$xend <- xranges[2] - diff(xranges)/30
    loonLayer.GeomSegment(widget,
                          layerGeom,
                          data,
                          ggplotPanel_params,
                          ggplotObject,
                          special,
                          parent = if(parent == "root") rugGroup else parent)
  }
  allSides <- c("t", "r", "b", "l")
  splitSides <- strsplit(sides, "")[[1]]
  isTRBL <- allSides %in% splitSides
  if(isTRBL[1]) topSides(data, xranges, yranges)
  if(isTRBL[2]) rightSides(data, xranges, yranges)
  if(isTRBL[3]) bottomSides(data, xranges, yranges)
  if(isTRBL[4]) leftSides(data, xranges, yranges)
}



loonLayer.GeomBoxplot <- function(widget,
                                  layerGeom,
                                  data,
                                  ggplotPanel_params,
                                  ggplotObject,
                                  special,
                                  parent = "root"){

  n <- dim(data)[1]
  boxplotsGroup <- l_layer_group(widget, "boxplots")
  for (i in 1:n) {
    boxplotGroup <- l_layer_group(widget,
                                  label = paste0("boxplot", i, collapse = ""),
                                  parent = boxplotsGroup)
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
                       parent = if(parent == "root") boxplotGroup else parent)

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
    linesData[3, ]$size <- 1.5 * linesData[1, ]$size
    loonLayer.GeomSegment(widget,
                          layerGeom,
                          linesData,
                          ggplotPanel_params,
                          ggplotObject,
                          special,
                          parent = if(parent == "root") boxplotGroup else parent)

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
                          parent = if(parent == "root") boxplotGroup else parent)
    }
  }
}



loonLayer.GeomLinerange <- function(widget,
                                    layerGeom,
                                    data,
                                    ggplotPanel_params,
                                    ggplotObject,
                                    special,
                                    parent = "root"){

  if (parent == "root") {
    linerangeGroup <- l_layer_group(widget, "linerange")
    parent <- linerangeGroup
  }

  n <- dim(data)[1]
  data$xend <- NA
  data$yend <- NA
  for (i in 1:n){
    data[i, ]$xend <- data[i, ]$x
    data[i, ]$y <- data[i, ]$ymin
    data[i, ]$yend <- data[i, ]$ymax
  }
  loonLayer.GeomSegment(widget,
                        layerGeom,
                        data,
                        ggplotPanel_params,
                        ggplotObject,
                        special,
                        parent = parent)
}



loonLayer.GeomPointrange <- function(widget,
                                     layerGeom,
                                     data,
                                     ggplotPanel_params,
                                     ggplotObject,
                                     special,
                                     parent = "root"){

  pointrangeGroup <- l_layer_group(widget, "pointrange")

  loonLayer.GeomLinerange(widget,
                          layerGeom,
                          data,
                          ggplotPanel_params,
                          ggplotObject,
                          special,
                          parent = if(parent == "root") pointrangeGroup else parent)
  loonLayer.GeomPoint(widget,
                      layerGeom,
                      data,
                      ggplotPanel_params,
                      ggplotObject,
                      special,
                      parent = if(parent == "root") pointrangeGroup else parent)
}



loonLayer.GeomCrossbar <- function(widget,
                                   layerGeom,
                                   data,
                                   ggplotPanel_params,
                                   ggplotObject,
                                   special,
                                   parent = "root"){

  crossbarGroup <- l_layer_group(widget, "crossbar")

  loonLayer.GeomRect(widget,
                     layerGeom,
                     data,
                     ggplotPanel_params,
                     ggplotObject,
                     special,
                     parent = if(parent == "root")  crossbarGroup else parent)
  n <- dim(data)[1]
  data$size <- 3 * data$size
  data$xend <- NA
  data$yend <- NA
  for (i in 1:n){
    data[i, ]$x <- data[i, ]$xmin
    data[i, ]$xend <- data[i, ]$xmax
    data[i, ]$yend <- data[i, ]$y
  }
  loonLayer.GeomSegment(widget,
                        layerGeom,
                        data,
                        ggplotPanel_params,
                        ggplotObject,
                        special,
                        parent = if(parent == "root")  crossbarGroup else parent)
}



loonLayer.GeomErrorbar <- function(widget,
                                   layerGeom,
                                   data,
                                   ggplotPanel_params,
                                   ggplotObject,
                                   special,
                                   parent = "root"){

  errorbarGroup <- l_layer_group(widget, "errorbar")

  loonLayer.GeomLinerange(widget,
                          layerGeom,
                          data,
                          ggplotPanel_params,
                          ggplotObject,
                          special,
                          parent = if(parent == "root")  errorbarGroup else parent)
  n <- dim(data)[1]
  newdata <- data[rep(1:n, each = 2), ]
  newdata$xend <- NA
  newdata$yend <- NA
  for(i in 1: (2*n) ){
    newdata[i,]$x <- newdata[i,]$xmin
    newdata[i,]$xend <- newdata[i,]$xmax
    if(i %% 2 != 0){
      newdata[i,]$y <- newdata[i,]$yend <- newdata[i,]$ymin
    } else {
      newdata[i,]$y <- newdata[i,]$yend <- newdata[i,]$ymax
    }
  }
  loonLayer.GeomSegment(widget,
                        layerGeom,
                        newdata,
                        ggplotPanel_params,
                        ggplotObject,
                        special,
                        parent = if(parent == "root")  errorbarGroup else parent)
}



loonLayer.GeomErrorbarh <- function(widget,
                                    layerGeom,
                                    data,
                                    ggplotPanel_params,
                                    ggplotObject,
                                    special,
                                    parent = "root"){

  errorbarhGroup <- l_layer_group(widget, "errorbarh")

  n <- dim(data)[1]
  newdata <- data[rep(1:n, each = 3), ]
  newdata$xend <- NA
  newdata$yend <- NA
  for(i in 1: (3*n) ){
    if(i %% 3 == 0){
      newdata[i,]$x <- newdata[i,]$xend <- newdata[i,]$xmin
      newdata[i,]$y <- newdata[i,]$ymin
      newdata[i,]$yend <- newdata[i,]$ymax
    } else if(i %% 3 == 1){
      newdata[i,]$yend <- newdata[i,]$y
      newdata[i,]$x <- newdata[i,]$xmin
      newdata[i,]$xend <- newdata[i,]$xmax
    } else {
      newdata[i,]$x <- newdata[i,]$xend <- newdata[i,]$xmax
      newdata[i,]$y <- newdata[i,]$ymin
      newdata[i,]$yend <- newdata[i,]$ymax
    }
  }
  loonLayer.GeomSegment(widget,
                        layerGeom,
                        newdata,
                        ggplotPanel_params,
                        ggplotObject,
                        special,
                        parent = if(parent == "root")  errorbarhGroup else parent)
}



# colorful line (line built with points)
loonLayer.GeomPath <- function(widget,
                               layerGeom,
                               data,
                               ggplotPanel_params,
                               ggplotObject,
                               special,
                               parent = "root"){

  isCoordPolar <- is.CoordPolar(ggplotPanel_params)
  # path group
  if (parent == "root") {
    pathGroup <- l_layer_group(widget, "path")
    parent <- pathGroup
  }

  coordinates <- ggplotObject$coordinates
  uniGroup <- unique(data$group)
  lapply(1:length(uniGroup),
         function(i){
           groupData <- data[data$group == uniGroup[i], ]
           linesColor <- hex6to12(groupData$colour)
           len_uni_col <- length(unique(groupData$colour))

           linesWidth <- as_loon_size(groupData$size, "lines")
           linesDash <- as_loon_dash(groupData$linetype)
           # a single line with a single color
           if(len_uni_col == 1){
             if(isCoordPolar){
               coordPolarxy <- Cartesianxy2Polarxy.GeomPath(NULL, coordinates, groupData, ggplotPanel_params)
               x <- coordPolarxy$x
               y <- coordPolarxy$y
             } else {
               x <- groupData$x
               y <- groupData$y
             }
             l_layer_line(
               widget, x = x, y = y, linewidth = linesWidth[1],
               color = linesColor[1], dash = linesDash[[1]],
               parent = parent
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
             loonLayer.GeomPoint(widget,
                                 layerGeom,
                                 newdata,
                                 ggplotPanel_params,
                                 ggplotObject,
                                 special,
                                 parent = parent)
           }
         })
}

loonLayer.GeomCurve <- function(widget,
                                layerGeom,
                                data,
                                ggplotPanel_params,
                                ggplotObject,
                                special,
                                parent = "root"){
  # curve group
  if (parent == "root") {
    curveGroup <- l_layer_group(widget, "curve")
    parent <- curveGroup
  }

  # avoid duplicated rows
  data <- data[!duplicated(data), ]
  n <- dim(data)[1]
  isCoordPolar <- is.CoordPolar(ggplotPanel_params)
  size <- data$size
  colour <- data$colour
  linetype <- data$linetype

  coordinates <- ggplotObject$coordinates

  which_curve <- special[["curve"]]$which_curve
  curveLayerId <- special[["curve"]]$curveLayerId

  # use "grid" to get the control coordinates
  answer <- utils::menu(c("y", "n"), title="Do you want to draw the ggplot?")

  if(answer == 1) {
    answer <- TRUE
    if (which_curve == curveLayerId[1]) {
      grid.draw(ggplotObject)
      grid.force()
    }
    gridList <- grid.ls(print = FALSE)
    gridList.name <- gridList$name
    xspline.name <- gridList.name[which(str_detect(gridList.name, "curve") == TRUE)]
    xspline.len <- length(xspline.name)

    for(i in seq_len(n)){
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
      if (i == 1) {
        newdata <- data.frame(x = x, y = y, group = rep(i, length(x)))
        newdata$size <- size[i]
        newdata$colour <- colour[i]
        newdata$linetype <- linetype[i]
      } else {
        nd <- data.frame(x = x, y = y, group = rep(i, length(x)))
        nd$size <- size[i]
        nd$colour <- colour[i]
        nd$linetype <- linetype[i]
        newdata <- rbind(newdata, nd)
      }
    }
    # grid remove
    grid.remove(xspline.name[1], redraw = FALSE)

    loonLayer.GeomPath(widget,
                       layerGeom,
                       newdata,
                       ggplotPanel_params,
                       special,
                       ggplotObject,
                       parent)

  } else {
    answer <- FALSE
    message("Segment will be drawn instead of curve")
    loonLayer.GeomSegment(widget,
                          layerGeom,
                          data,
                          ggplotPanel_params,
                          special,
                          ggplotObject,
                          parent)
  }

  c("draw curve" = answer)
}

loonLayer.GeomRibbon <- function(widget,
                                 layerGeom,
                                 data,
                                 ggplotPanel_params,
                                 ggplotObject,
                                 special,
                                 parent = "root"){

  ribbonGroup <- l_layer_group(widget, "ribbon")

  uniGroup <- unique(data$group)
  newdata <- do.call(rbind, lapply(1:length(uniGroup),
                                   function(i){
                                     d <- data[data$group == uniGroup[i], ]
                                     n <- dim(d)[1]
                                     seqLength <- 20
                                     nd <- rbind(d, d[rep(n, seqLength), ], d)
                                     nd$x <- c(d$x, rep(d$x[n], seqLength), rev(d$x))
                                     nd$y <- c(d$ymin, seq(d$ymin[n], d$ymax[n], length.out = seqLength), rev(d$ymax))
                                     nd
                                   })
  )
  loonLayer.GeomPolygon(widget,
                        layerGeom,
                        newdata,
                        ggplotPanel_params,
                        ggplotObject,
                        special,
                        parent = if(parent == "root") ribbonGroup else parent)
}



loonLayer.GeomSmooth <- function(widget,
                                 layerGeom,
                                 data,
                                 ggplotPanel_params,
                                 ggplotObject,
                                 special,
                                 parent = "root"){

  smoothGroup <- l_layer_group(widget, "smooth")

  if(!is.null(data$se)){
    # the only difference bewteen loonLayer.GeomRibbon is the polygon border colour. NA is set here
    n <- dim(data)[1]
    uniGroup <- unique(data$group)
    seqLength <- 20
    newdata <- data.frame()
    for (i in uniGroup) {
      group_i_data <- data[data$group == i, ]
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
      newdata <- rbind(newdata, group_i_newdata)
    }
    newdata$colour <- NA
    loonLayer.GeomPolygon(widget,
                          layerGeom,
                          newdata,
                          ggplotPanel_params,
                          ggplotObject,
                          special,
                          parent = if(parent == "root") smoothGroup else parent)
  }
  loonLayer.GeomPath(widget,
                     layerGeom,
                     data,
                     ggplotPanel_params,
                     ggplotObject,
                     special,
                     parent = if(parent == "root") smoothGroup else parent)
}



loonLayer.GeomStep <- function(widget,
                               layerGeom,
                               data,
                               ggplotPanel_params,
                               ggplotObject,
                               special,
                               parent = "root"){

  stepGroup <- l_layer_group(widget, "step")

  direction <- layerGeom$geom_params$direction
  uniGroup <- unique(data$group)
  newdata <- do.call(rbind, lapply(1:length(uniGroup),
                                   function(i){
                                     d <- data[data$group == uniGroup[i], ]
                                     # n must be larger than 2 (line)
                                     n <- dim(d)[1]
                                     # stepNewAddedMatrix is the new matrix which would be added on d
                                     stepNewAddedMatrix <- d[-n, ]
                                     newOrder <- c(1:n, (1:(n-1) + 0.5))
                                     if(direction == "hv"){
                                       for(j in 1: (n-1)){
                                         stepNewAddedMatrix[j, ]$x <- d[j+1, ]$x
                                         stepNewAddedMatrix[j, ]$y <- d[j, ]$y
                                       }
                                     }else{
                                       for(j in 1: (n-1)){
                                         stepNewAddedMatrix[j, ]$x <- d[j, ]$x
                                         stepNewAddedMatrix[j, ]$y <- d[j+1, ]$y
                                       }
                                     }
                                     rbind(d, stepNewAddedMatrix)[order(newOrder) ,]
                                   })
  )

  loonLayer.GeomPath(widget,
                     layerGeom, newdata,
                     ggplotPanel_params,
                     ggplotObject,
                     special,
                     parent = if(parent == "root") stepGroup else parent)
}



loonLayer.GeomRaster <- function(widget,
                                 layerGeom,
                                 data,
                                 ggplotPanel_params,
                                 ggplotObject,
                                 special,
                                 parent = "root"){

  rasterGroup <- l_layer_group(widget, "raster")

  loonLayer.GeomRect(widget,
                     layerGeom,
                     data,
                     ggplotPanel_params,
                     ggplotObject,
                     special,
                     parent = if(parent == "root") rasterGroup else parent)
}
