########################################### basic layers ###########################################
loonLayer <- function(widget, layerGeom, data, ggplotPanel_params, coordinates, parent){
  UseMethod("loonLayer", layerGeom$geom)
}

loonLayer.GeomPoint <- function(widget, layerGeom, data, ggplotPanel_params, coordinates, parent = "root"){
  isCoordPolar <- is.CoordPolar(ggplotPanel_params)
  if(isCoordPolar){
    coordPolarxy <- Cartesianxy2Polarxy(layerGeom, coordinates, data, ggplotPanel_params)
    x <- coordPolarxy$x
    y <- coordPolarxy$y
  } else {
    x <- data$x
    y <- data$y
  }
  lineColor <- hex6to12(data$colour)
  pointsColor <- if(!is.null(data$shape) ){
    sapply(1:dim(data)[1], function(j){
      if(data$shape[j] %in% 21:24){
        hex6to12(data$fill[j])
      } else if (data$shape[j] %in% c(0, 1, 2, 5)) {
        ""
      }
      else {
        hex6to12(data$colour[j]) }
    } )
  } else hex6to12(data$colour)
  pointsSize <- as_loon_size( data$size, "points" )

  l_layer_points(widget, x = x, y = y, color = pointsColor,
                 size = pointsSize, linecolor = lineColor, parent = parent)
}



loonLayer.GeomRect <- function(widget, layerGeom, data, ggplotPanel_params, coordinates, parent = "root"){
  isCoordPolar <- is.CoordPolar(ggplotPanel_params)
  n <- dim(data)[1]
  fillColor <- hex6to12(data$fill)
  linesColor <- hex6to12(data$colour)
  linesWidth <- as_loon_size(data$size, "lines")
  xrange <- ggplotPanel_params$x.range
  yrange <- ggplotPanel_params$y.range
  if(n == 1){
    if(isCoordPolar){
      coordPolarxy <- Cartesianxy2Polarxy.GeomRect(NULL, coordinates, data, ggplotPanel_params)
      x <- coordPolarxy$x
      y <- coordPolarxy$y
      l_layer_polygon(
        widget, x = x, y = y,
        color = fillColor,
        linecolor = linesColor,
        linewidth = linesWidth,
        parent = parent
      )
    } else {
      x <- c(data$xmin, data$xmax)
      y <- c(data$ymin, data$ymax)
      l_layer_rectangle(
        widget, x = x, y = y,
        color = fillColor,
        linecolor = linesColor,
        linewidth = linesWidth,
        parent = parent
      )
    }
  }else{
    # any NA will not be drawn
    x <- list()
    y <- list()
    if(isCoordPolar){
      for(i in 1: n) {
        coordPolarxy <- Cartesianxy2Polarxy.GeomRect(NULL, coordinates, data[i, ], ggplotPanel_params)
        x[[i]] <- coordPolarxy$x
        y[[i]] <- coordPolarxy$y
      }
      l_layer_polygons(
        widget, x = x, y = y,
        color = fillColor,
        linecolor = linesColor,
        linewidth = linesWidth,
        parent = parent
      )
    } else {
      for(i in 1:n) {
        y[[i]] <- if(is.na(data[i,]$ymin) & is.na(data[i,]$ymax)) rep(data[i,]$y, 2)
        else if(is.na(data[i,]$ymin) & !is.na(data[i,]$ymax)) c(2 * data[i,]$y - data[i,]$ymax  , data[i,]$ymax)
        else if(!is.na(data[i,]$ymin) & is.na(data[i,]$ymax)) c(data[i,]$ymin  , 2 * data[i,]$y - data[i,]$ymin)
        else{
          c(if(is.infinite(data[i,]$ymin)) yrange[1] else data[i,]$ymin,
            if(is.infinite(data[i,]$ymax)) yrange[2] else data[i,]$ymax)
        }
        x[[i]] <- if(is.na(data[i,]$xmin) & is.na(data[i,]$xmax)) rep(data[i,]$x, 2)
        else if(is.na(data[i,]$xmin) & !is.na(data[i,]$xmax)) c(2 * data[i,]$x - data[i,]$xmax  , data[i,]$xmax)
        else if(!is.na(data[i,]$xmin) & is.na(data[i,]$xmax)) c(data[i,]$xmin  , 2 * data[i,]$x - data[i,]$xmin)
        else{
          c(if(is.infinite(data[i,]$xmin)) xrange[1] else data[i,]$xmin,
            if(is.infinite(data[i,]$xmax)) xrange[2] else data[i,]$xmax)
        }
      }
      l_layer_rectangles(
        widget, x = x, y = y,
        color = fillColor,
        linecolor = linesColor,
        linewidth = linesWidth,
        parent = parent
      )
    }
  }
}



loonLayer.GeomPolygon <- function(widget, layerGeom, data, ggplotPanel_params, coordinates, parent = "root"){
  isCoordPolar <- is.CoordPolar(ggplotPanel_params)
  uniGroup <- unique(data$group)
  fillColor <- hex6to12(data$fill)
  linesColor <- hex6to12(data$colour)
  linesWidth <- as_loon_size(data$size, "lines")

  if(length(uniGroup) == 1){
    if(isCoordPolar) {
      coordPolarxy <- Cartesianxy2Polarxy(layerGeom, coordinates, data, ggplotPanel_params)
      x <- coordPolarxy$x
      y <- coordPolarxy$y
    } else {
      x <- data$x
      y <- data$y
    }
    l_layer_polygon(
      widget, x = x, y = y,
      color = fillColor[1], linecolor = linesColor[1],
      linewidth = linesWidth[1],
      parent = parent
    )
  } else {
    x <- list()
    y <- list()
    fColor <- c()
    lColor <- c()
    lWidth <- c()
    for(i in 1:length(uniGroup)){
      if(isCoordPolar) {
        coordPolarxy <- Cartesianxy2Polarxy(layerGeom, coordinates, data[data$group == uniGroup[i], ], ggplotPanel_params)
        x[[i]] <- coordPolarxy$x
        y[[i]] <- coordPolarxy$y
      } else {
        x[[i]] <-  data$x[data$group == uniGroup[i]]
        y[[i]] <-  data$y[data$group == uniGroup[i]]
      }
      fColor[i] <- fillColor[data$group == uniGroup[i]][1]
      lColor[i] <- linesColor[data$group == uniGroup[i]][1]
      lWidth[i] <- linesWidth[data$group == uniGroup[i]][1]
    }
    l_layer_polygons(
      widget, x = x, y = y,
      color = fColor, linecolor = lColor,
      linewidth = lWidth,
      parent = parent
    )
  }
}




# TODO overlap
loonLayer.GeomText <- function(widget, layerGeom, data, ggplotPanel_params, coordinates, parent = "root"){
  isCoordPolar <- is.CoordPolar(ggplotPanel_params)
  textsSize <- as_loon_size(data$size, "texts")
  textsColor <- hex6to12(data$colour)
  textAnchor <- as_loon_hvjust(hjust = data$hjust, vjust = data$vjust)

  if(isCoordPolar){
    coordPolarxy <- Cartesianxy2Polarxy(layerGeom, coordinates, data, ggplotPanel_params)
    x <- coordPolarxy$x
    y <- coordPolarxy$y
  } else {
    x <- data$x
    y <- data$y
  }

  if(dim(data)[1] == 1){
    l_layer_text(
      widget, x = x, y = y, text = as.character( data$label ), size = textsSize,
      angle = data$angle, color =  textsColor, anchor = textAnchor, justify = "left",
      parent = parent)
  }else{
    l_layer_texts(
      widget, x = x, y = y, text = as.character( data$label ), size = textsSize,
      angle = data$angle, color = textsColor, anchor = textAnchor, justify = "left",
      parent = parent)
  }
}



# TODO draws a rectangle behind the text
loonLayer.GeomLabel <- function(widget, layerGeom, data, ggplotPanel_params, coordinates, parent = "root"){
  loonLayer.GeomText(widget, layerGeom, data, ggplotPanel_params, parent)
}



loonLayer.GeomVline <- function(widget, layerGeom, data, ggplotPanel_params, coordinates, parent = "root"){
  isCoordPolar <- is.CoordPolar(ggplotPanel_params)
  n <- dim(data)[1]
  linesWidth <- as_loon_size(data$size, "lines")
  linesColor <- hex6to12(data$colour)
  yrange <- ggplotPanel_params$y.range

  if(n == 1){
    linesDash <- as_loon_dash(data$linetype)
    if(isCoordPolar){
      coordPolarxy <- Cartesianxy2Polarxy(layerGeom, coordinates, data, ggplotPanel_params)
      x <- coordPolarxy$x
      y <- coordPolarxy$y
    } else {
      x <- rep(data$xintercept, 2)
      y <- yrange
    }

    l_layer_line(widget, x = x, y = y, linewidth = linesWidth,
                 color = linesColor, dash = linesDash[[1]],
                 parent = parent)
  }else{
    x <- list()
    y <- list()
    for(i in 1:n){
      if(isCoordPolar){
        coordPolarxy <- Cartesianxy2Polarxy(layerGeom, coordinates, data[i, ], ggplotPanel_params)
        x[[i]] <- coordPolarxy$x
        y[[i]] <- coordPolarxy$y
      } else {
        x[[i]] <- rep(data[i,]$xintercept, 2)
        y[[i]] <- yrange
      }
    }
    l_layer_lines(widget, x = x, y = y, linewidth = linesWidth,
                  color = linesColor, parent = parent)
  }
}



loonLayer.GeomHline <- function(widget, layerGeom, data, ggplotPanel_params, coordinates, parent = "root"){
  isCoordPolar <- is.CoordPolar(ggplotPanel_params)
  n <- dim(data)[1]
  linesWidth <- as_loon_size(data$size, "lines")
  linesColor <- hex6to12(data$colour)
  xrange <- ggplotPanel_params$x.range
  linesDash <- as_loon_dash(data$linetype)

  if(n == 1){
    linesDash <- as_loon_dash(data$linetype)
    if(isCoordPolar){
      coordPolarxy <- Cartesianxy2Polarxy(layerGeom, coordinates, data, ggplotPanel_params)
      x <- coordPolarxy$x
      y <- coordPolarxy$y
    } else {
      y <- rep(data$yintercept, 2)
      x <- xrange
    }
    l_layer_line(widget, x = x, y = y, linewidth = linesWidth,
                 color = linesColor, dash = linesDash[[1]],
                 parent = parent)
  }else{
    x <- list()
    y <- list()
    for(i in 1:n){
      if(isCoordPolar){
        coordPolarxy <- Cartesianxy2Polarxy(layerGeom, coordinates, data[i, ], ggplotPanel_params)
        x[[i]] <- coordPolarxy$x
        y[[i]] <- coordPolarxy$y
      } else {
        x[[i]] <- xrange
        y[[i]] <- rep(data[i,]$yintercept, 2)
      }
    }
    l_layer_lines(widget, x = x, y = y, linewidth = linesWidth,
                  color = linesColor, parent = parent)
  }
}



loonLayer.GeomAbline <- function(widget, layerGeom, data, ggplotPanel_params, coordinates,  parent = "root"){
  isCoordPolar <- is.CoordPolar(ggplotPanel_params)
  n <- dim(data)[1]
  linesWidth <- as_loon_size(data$size, "lines")
  linesColor <- hex6to12(data$colour)
  xrange <- ggplotPanel_params$x.range
  yrange <- ggplotPanel_params$y.range
  linesDash <- as_loon_dash(data$linetype)

  if(n == 1){
    if(isCoordPolar){
      coordPolarxy <- Cartesianxy2Polarxy(layerGeom, coordinates, data,
                                          ggplotPanel_params)
      x <- coordPolarxy$x
      y <- coordPolarxy$y
    } else {
      coordCartesianxy <- abline2xy(xrange, yrange, data$slope, data$intercept)
      x <- coordCartesianxy$x
      y <- coordCartesianxy$y
    }
    l_layer_line(widget, x = x, y = y, linewidth = linesWidth,
                 color = linesColor, dash = linesDash[[1]],
                 parent = parent)
  }else{
    x <- list()
    y <- list()
    if(isCoordPolar){
      for(i in 1:n){
        coordPolarxy <- Cartesianxy2Polarxy(layerGeom, coordinates, data[i,],
                                            ggplotPanel_params)
        x[[i]] <- coordPolarxy$x
        y[[i]] <- coordPolarxy$y
      }
    } else {
      for(i in 1:n){
        coordCartesianxy <- abline2xy(xrange, yrange, data[i, ]$slope, data[i, ]$intercept)
        x[[i]] <- coordCartesianxy$x
        y[[i]] <- coordCartesianxy$y
      }
    }
    l_layer_lines(widget, x = x, y = y, linewidth = linesWidth,
                  color = linesColor, parent = parent)
  }
}



# given start and end to draw a straight line
loonLayer.GeomSegment <- function(widget, layerGeom, data, ggplotPanel_params, coordinates, parent = "root"){
  isCoordPolar <- is.CoordPolar(ggplotPanel_params)
  n <- dim(data)[1]
  linesWidth <- as_loon_size(data$size, "lines")
  linesColor <- hex6to12(data$colour)
  linesDash <- as_loon_dash(data$linetype)

  if(n == 1){
    if(isCoordPolar) {
      coordPolarxy <- Cartesianxy2Polarxy.GeomSegment(NULL, coordinates, data, ggplotPanel_params)
      x <- coordPolarxy$x
      y <- coordPolarxy$y
    } else {
      x <- c(data$x, data$xend)
      y <- c(data$y, data$yend)
    }

    l_layer_line(widget, x = x, y = y, linewidth = linesWidth,
                 color = linesColor, dash = linesDash[[1]],
                 parent = parent)
  } else {
    x <- list()
    y <- list()
    for(i in 1:n){
      if(isCoordPolar){
        coordPolarxy <- Cartesianxy2Polarxy.GeomSegment(NULL, coordinates, data[i, ], ggplotPanel_params)
        x[[i]] <- coordPolarxy$x
        y[[i]] <- coordPolarxy$y
      } else {
        x[[i]] <- c(data[i,]$x, data[i,]$xend)
        y[[i]] <- c(data[i,]$y, data[i,]$yend)
      }
    }
    l_layer_lines(widget, x = x, y = y,
                  linewidth = linesWidth,
                  color = linesColor,
                  parent = parent)
  }
}



# TODO
loonLayer.GeomCurve <- function(widget, layerGeom, data, ggplotPanel_params, coordinates, parent = "root"){
  NULL
}



loonLayer.GeomHex <- function(widget, layerGeom, data, ggplotPanel_params, coordinates, parent = "root"){

  # ranges
  x.range <- ggplotPanel_params$x.range
  y.range <- ggplotPanel_params$y.range
  # can n be one?
  n <- dim(data)[1]
  if (n == 1) {
    warnings("one hexagon is not allowed")
  } else {
    unique_y <- unique(data$y)
    # hex width
    hexWidth <- min(unlist(lapply(unique_y, function(j) {
      id <- which(data$y == j)
      diff(data$x[id])
    })))
    # scale data
    scale.hex_radius <- hexWidth/ (diff(x.range) * sqrt(3))
    scale.x <- (data$x - x.range[1]) / diff(x.range)
    scale.y <- (data$y - y.range[1]) / diff(y.range)

    hex_x <- lapply(seq_len(n), function(i) {
      x.north <- x.south <- scale.x[i]
      x.northeast <- x.southeast <- scale.x[i] + sqrt(3) * scale.hex_radius / 2
      x.northwest <- x.southwest <- scale.x[i] - sqrt(3) * scale.hex_radius / 2
      c(x.north, x.northeast, x.southeast, x.south, x.southwest, x.northwest) * diff(x.range) + x.range[1]
    })

    hex_y <- lapply(seq_len(n), function(i) {
      y.north <- scale.y[i] + scale.hex_radius
      y.northeast <- y.northwest <- scale.y[i] + scale.hex_radius / 2
      y.southeast <- y.southwest <- scale.y[i] - scale.hex_radius / 2
      y.south <- scale.y[i] - scale.hex_radius
      c(y.north, y.northeast, y.southeast, y.south, y.southwest, y.northwest) * diff(y.range) + y.range[1]
    })
    fillColor <- hex6to12(data$fill)
    linesColor <- hex6to12(data$colour)
    linesWidth <- as_loon_size(data$size, "lines")
    l_layer_polygons(widget, hex_x, hex_y, color = fillColor, linecolor = linesColor,
                     linewidth = linesWidth, parent = parent)
  }
}



loonLayer.GeomDotplot <- function(widget, layerGeom, data, ggplotPanel_params, coordinates, parent = "root"){
  uniGroup <- unique(data$group)
  fillColor <- hex6to12(data$fill)
  lineColor <- hex6to12(data$colour)

  stackRatio <- layerGeom$geom_params$stackratio
  countId <- data$countidx
  n <- dim(data)[1]
  stackPos <- sapply(1:n, function(i) {
    if(i == 1) data$stackpos[i] else {
      data$stackpos[i - countId[i] + 1] + (countId[i] - 1) * stackRatio
    }
  })

  dotsGroup <- l_layer_group(widget, "dots")
  radius <- data$binwidth[1]/2
  if(layerGeom$geom_params$binaxis == "y"){
    lapply(1:n, function(i){
      xradius <- diff(ggplotPanel_params$x.range)/diff(ggplotPanel_params$y.range)*radius
      l_layer_oval(widget, parent = dotsGroup,
                   x = c(data$x[i] + stackPos[i] * 2 * xradius - xradius,
                         data$x[i] + stackPos[i] * 2 * xradius + xradius),
                   y = c(data$y[i] - radius, data$y[i] + radius),color =  fillColor[i],
                   linecolor = lineColor[i])
    })
  } else  {
    yradius <- diff(ggplotPanel_params$y.range)/diff(ggplotPanel_params$x.range)*radius
    lapply(1:n, function(i){
      l_layer_oval(widget, parent = dotsGroup,
                   x = c(data$x[i] - radius, data$x[i] + radius),
                   y = c(data$y[i] - yradius + stackPos[i] * 2 * yradius,
                         data$y[i] + yradius + stackPos[i] * 2 * yradius),color =  fillColor[i],
                   linecolor = lineColor[i])
    })
  }
}
