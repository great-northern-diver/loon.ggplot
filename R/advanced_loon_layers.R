########################################### advanced layers ###########################################

# TODO quantiles
loonLayer.GeomViolin <- function(widget, layerGeom, data, ggplotPanel_params, theta, parent = "root"){
  n <- dim(data)[1]
  uniGroup <- unique(data$group)
  revdata <- data
  revdata$y <- unlist( lapply(uniGroup, function(i){
    rev(data$y[revdata$group == i])
  }))
  revdata$violinwidth <- unlist( lapply(uniGroup, function(i){
    rev(data$violinwidth[revdata$group == i])
  }))
  revdata$x <- revdata$x + revdata$violinwidth/2
  data$x <- data$x - data$violinwidth/2
  newdata <- rbind(data, revdata)

  violinGroup <- l_layer_group(widget, "violin")

  loonLayer.GeomPolygon(widget, layerGeom, newdata, ggplotPanel_params,
                        parent = if(parent == "root") violinGroup else parent)

  if (!is.null(layerGeom$geom_params$draw_quantiles)) {

    quantiles <- layerGeom$geom_params$draw_quantiles
    len_quantiles <- length(quantiles)
    groupDiff <- rep( sapply(uniGroup, function(i){
      max(data[data$group==i, ]$y) - min(data[data$group==i, ]$y)
    }), each = len_quantiles )
    linesData <- do.call(rbind, lapply(uniGroup, function(i){
      groupiData <- data[data$group==i, ]
      # cumulative area
      # cumulativeArea <- c(0, sapply(2:length(groupiData$y), function(i) {
      #   x <- groupiData$y[1:i]
      #   f1 <- approxfun(x, groupiData$x[1:i] + groupiData$violinwidth[1:i]/2)
      #   integrate(f1, min(x), max(x))$value
      # }))
      # quantileX <- quantile(cumulativeArea, probs = quantiles,
      #                       type = 1)
      # groupiData[which(cumulativeArea %in% quantileX), ]

      # groupiX <- groupiData$x + groupiData$violinwidth/2
      # newseq <- seq(min(groupiX), max(groupiX), length.out = length(groupiX))
      # quantileX <- quantile(newseq, probs = quantiles, type = 1)
      # groupiData[which(newseq %in% quantileX), ]
      quantileX <- quantile(groupiData$y, probs = quantiles, type = 1)
      groupiData[which(groupiData$y %in% quantileX), ]
    }))
    linesData$xend <- linesData$x + linesData$violinwidth
    linesData$yend <- linesData$y
    loonLayer.GeomSegment(widget, layerGeom, linesData, ggplotPanel_params, theta,
                          parent = if(parent == "root") violinGroup else parent)
  }
}



loonLayer.GeomRug <- function(widget, layerGeom, data, ggplotPanel_params, theta, parent = "root"){
  sides <- layerGeom$geom_params$sides
  xranges <- ggplotPanel_params$x.range
  yranges <- ggplotPanel_params$y.range

  rugGroup <- l_layer_group(widget, "rug")
  # 30 is an arbitrary choice
  topSides <- function(data, xranges, yranges){
    data$xend <- data$x
    data$y <- yranges[2]
    data$yend <- yranges[2] - diff(yranges)/30
    loonLayer.GeomSegment(widget, layerGeom, data, ggplotPanel_params, theta,
                          parent = if(parent == "root") rugGroup else parent)
  }
  bottomSides <- function(data, xranges, yranges){
    data$xend <- data$x
    data$y <- yranges[1]
    data$yend <- yranges[1] + diff(yranges)/30
    loonLayer.GeomSegment(widget, layerGeom, data, ggplotPanel_params, theta,
                          parent = if(parent == "root") rugGroup else parent)
  }
  leftSides <- function(data, xranges, yranges){
    data$yend <- data$y
    data$x <- xranges[1]
    data$xend <- xranges[1] + diff(xranges)/30
    loonLayer.GeomSegment(widget, layerGeom, data, ggplotPanel_params, theta,
                          parent = if(parent == "root") rugGroup else parent)
  }
  rightSides <- function(data, xranges, yranges){
    data$yend <- data$y
    data$x <- xranges[2]
    data$xend <- xranges[2] - diff(xranges)/30
    loonLayer.GeomSegment(widget, layerGeom, data, ggplotPanel_params, theta,
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



loonLayer.GeomBoxplot <- function(widget, layerGeom, data, ggplotPanel_params, theta, parent = "root"){
  n <- dim(data)[1]
  # rectangulars
  rectData <- data
  rectData$ymin <- data$lower
  rectData$ymax <- data$upper

  boxplotGroup <- l_layer_group(widget, "boxplot")

  loonLayer.GeomRect(widget, layerGeom, rectData, ggplotPanel_params, theta,
                     parent = if(parent == "root") boxplotGroup else parent)

  # lines
  linesData <- data[rep(1:n, 3), ]
  linesData$y <- linesData$ymin
  linesData$yend <- linesData$lower
  linesData$xend <- linesData$x
  for(i in 1:n){
    linesData[n + i, ]$y <- data[i, ]$upper
    linesData[n + i, ]$yend <- data[i, ]$ymax
    linesData[2*n + i, ]$y <- linesData[2*n + i, ]$yend <- data[i, ]$middle
    linesData[2*n + i, ]$x <- data[i, ]$xmin
    linesData[2*n + i, ]$xend <- data[i, ]$xmax
    linesData[2*n + i, ]$size <- 1.5 * data[i, ]$size
  }
  loonLayer.GeomSegment(widget, layerGeom, linesData, ggplotPanel_params, theta,
                        parent = if(parent == "root") boxplotGroup else parent)

  # points
  if(!is.null(data$outliers)){
    pointsData <- data[ rep(1:n, sapply(data$outliers, length)), ]
    pointsData$y <- unlist(data$outliers)
    if( !is.null(layerGeom$geom_params$outlier.colour) ) pointsData$colour <- layerGeom$geom_params$outlier.colour
    if( !is.null(layerGeom$geom_params$outlier.fill)) pointsData$fill <- layerGeom$geom_params$outlier.fill
    pointsData$shape <- layerGeom$geom_params$outlier.shape
    pointsData$size <- layerGeom$geom_params$outlier.size
    loonLayer.GeomPoint(widget, layerGeom, pointsData, ggplotPanel_params, theta,
                        parent = if(parent == "root") boxplotGroup else parent)
  }
}



loonLayer.GeomLinerange <- function(widget, layerGeom, data, ggplotPanel_params, theta, parent = "root"){

  linerangeGroup <- l_layer_group(widget, "linerange")

  n <- dim(data)[1]
  data$xend <- NA
  data$yend <- NA
  for (i in 1:n){
    data[i, ]$xend <- data[i, ]$x
    data[i, ]$y <- data[i, ]$ymin
    data[i, ]$yend <- data[i, ]$ymax
  }
  loonLayer.GeomSegment(widget, layerGeom, data, ggplotPanel_params, theta,
                        parent = if(parent == "root") linerangeGroup else parent)
}



loonLayer.GeomPointrange <- function(widget, layerGeom, data, ggplotPanel_params, theta, parent = "root"){

  pointrangeGroup <- l_layer_group(widget, "pointrange")

  loonLayer.GeomLinerange(widget, layerGeom, data, ggplotPanel_params, theta,
                          parent = if(parent == "root") pointrangeGroup else parent)
  loonLayer.GeomPoint(widget, layerGeom, data, ggplotPanel_params, theta,
                      parent = if(parent == "root") pointrangeGroup else parent)
}



loonLayer.GeomCrossbar <- function(widget, layerGeom, data, ggplotPanel_params, theta, parent = "root"){

  crossbarGroup <- l_layer_group(widget, "crossbar")

  loonLayer.GeomRect(widget, layerGeom, data, ggplotPanel_params, theta,
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
  loonLayer.GeomSegment(widget, layerGeom, data, ggplotPanel_params, theta,
                        parent = if(parent == "root")  crossbarGroup else parent)
}



loonLayer.GeomErrorbar <- function(widget, layerGeom, data, ggplotPanel_params, theta, parent = "root"){

  errorbarGroup <- l_layer_group(widget, "errorbar")

  loonLayer.GeomLinerange(widget, layerGeom, data, ggplotPanel_params, theta,
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
  loonLayer.GeomSegment(widget, layerGeom, newdata, ggplotPanel_params, theta,
                        parent = if(parent == "root")  errorbarGroup else parent)
}



loonLayer.GeomErrorbarh <- function(widget, layerGeom, data, ggplotPanel_params, theta, parent = "root"){

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
  loonLayer.GeomSegment(widget, layerGeom, newdata, ggplotPanel_params, theta,
                        parent = if(parent == "root")  errorbarhGroup else parent)
}



# colorful line (line built with points)
loonLayer.GeomPath <- function(widget, layerGeom, data, ggplotPanel_params, theta, parent = "root"){

  isCoordPolar <- is.CoordPolar(ggplotPanel_params)
  # lines group
  pathGroup <- l_layer_group(widget, "path")

  uniGroup <- unique(data$group)
  lapply(1:length(uniGroup), function(i){
    groupData <- data[data$group == uniGroup[i], ]
    linesColor <- hex6to12(groupData$colour)
    len_uni_col <- length(unique(groupData$colour))

    linesWidth <- as_loon_size(groupData$size, "lines")
    linesDash <- as_loon_dash(groupData$linetype)
    # a single line with a single color
    if(len_uni_col == 1){
      if(isCoordPolar){
        coordPolarxy <- cartesianxy2Polarxy.GeomPath(NULL, theta, groupData, ggplotPanel_params)
        x <- coordPolarxy$x
        y <- coordPolarxy$y
      } else {
        x <- groupData$x
        y <- groupData$y
      }
      l_layer_line(
        widget, x = x, y = y, linewidth = linesWidth[1],
        color = linesColor[1], dash = linesDash[[1]],
        parent = if(parent == "root") pathGroup else parent
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
      loonLayer.GeomPoint(widget, layerGeom, newdata, ggplotPanel_params, theta,
                          parent = if(parent == "root") pathGroup else parent)
    }
  })
}



loonLayer.GeomRibbon <- function(widget, layerGeom, data, ggplotPanel_params, theta, parent = "root"){

  ribbonGroup <- l_layer_group(widget, "ribbon")

  uniGroup <- unique(data$group)
  newdata <- do.call(rbind, lapply(1:length(uniGroup), function(i){
    d <- data[data$group == uniGroup[i], ]
    nd <- rbind(d, d)
    nd$x <- c(d$x, rev(d$x) )
    nd$y <- c(d$ymin, rev(d$ymax) )
    nd
  }))
  loonLayer.GeomPolygon(widget, layerGeom, newdata, ggplotPanel_params, theta,
                        parent = if(parent == "root") ribbonGroup else parent)
}



loonLayer.GeomSmooth <- function(widget, layerGeom, data, ggplotPanel_params, theta, parent = "root"){

  smoothGroup <- l_layer_group(widget, "smooth")

  if(!is.null(data$se)){
    # the only difference bewteen loonLayer.GeomRibbon is the polygon border colour. NA is set here
    newdata <- rbind(data, data)
    newdata$x <- c(data$x, rev(data$x) )
    newdata$y <- c(data$ymin, rev(data$ymax) )
    newdata$colour <- NA
    loonLayer.GeomPolygon(widget, layerGeom, newdata, ggplotPanel_params, theta,
                          parent = if(parent == "root") smoothGroup else parent)
  }
  loonLayer.GeomPath(widget, layerGeom, data, ggplotPanel_params, theta,
                     parent = if(parent == "root") smoothGroup else parent)
}



loonLayer.GeomStep <- function(widget, layerGeom, data, ggplotPanel_params, theta, parent = "root"){

  stepGroup <- l_layer_group(widget, "step")

  direction <- layerGeom$geom_params$direction
  uniGroup <- unique(data$group)
  newdata <- do.call(rbind, lapply(1:length(uniGroup), function(i){
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
  }))

  loonLayer.GeomPath(widget, layerGeom, newdata, ggplotPanel_params, theta,
                     parent = if(parent == "root") stepGroup else parent)
}



loonLayer.GeomRaster <- function(widget, layerGeom, data, ggplotPanel_params, theta, parent = "root"){

  rasterGroup <- l_layer_group(widget, "raster")

  loonLayer.GeomRect(widget, layerGeom, data, ggplotPanel_params, theta,
                     parent = if(parent == "root") rasterGroup else parent)
}
