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
    sapply(1:dim(data)[1],
           function(j){
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

  # avoid duplicated rows
  data <- data[!duplicated(data), ]
  n <- dim(data)[1]
  isCoordPolar <- is.CoordPolar(ggplotPanel_params)
  linesWidth <- as_loon_size(data$size, "lines")
  linesColor <- hex6to12(data$colour)
  linesDash <- as_loon_dash(data$linetype)

  curvature <- layerGeom$geom_params$curvature
  angle <- layerGeom$geom_params$angle
  ncp <- layerGeom$geom_params$ncp


  if(n == 1){
    curvePoints <- calcControlPoints(data$x, data$y, data$xend, data$yend,
                                     curvature, angle, ncp)
    # mapping
    x <- (data$xend - data$x) / (curvePoints$x[length(curvePoints$x)] - curvePoints$x[1]) *
      (curvePoints$x - curvePoints$x[1]) + data$x
    y <- (data$yend - data$y) / (curvePoints$y[length(curvePoints$y)] - curvePoints$y[1]) *
      (curvePoints$y - curvePoints$y[1]) + data$y

    if(isCoordPolar) {
      coordPolarxy <- Cartesianxy2Polarxy.default(NULL, coordinates,
                                                  data = data.frame(x = x, y = y),
                                                  ggplotPanel_params)
      x <- coordPolarxy$x
      y <- coordPolarxy$y
    }

    l_layer_line(widget, x = x, y = y, linewidth = linesWidth,
                 color = linesColor, dash = linesDash[[1]],
                 parent = parent)
  } else {
    x <- list()
    y <- list()
    for(i in 1:n){
      curvePoints <- calcControlPoints(data[i, ]$x, data[i, ]$y, data[i, ]$xend, data[i, ]$yend,
                                       curvature, angle, ncp)
      # mapping
      x[[i]] <- (data[i, ]$xend - data[i, ]$x) / (curvePoints$x[length(curvePoints$x)] - curvePoints$x[1]) *
        (curvePoints$x - curvePoints$x[1]) + data[i, ]$x
      y[[i]] <- (data[i, ]$yend - data[i, ]$y) / (curvePoints$y[length(curvePoints$y)] - curvePoints$y[1]) *
        (curvePoints$y - curvePoints$y[1]) + data[i, ]$y

      if(isCoordPolar){
        coordPolarxy <- Cartesianxy2Polarxy.GeomSegment(NULL, coordinates,
                                                        data = data.frame(x = x[[i]], y = y[[i]]),
                                                        ggplotPanel_params)
        x[[i]] <- coordPolarxy$x
        y[[i]] <- coordPolarxy$y
      }
    }
    l_layer_lines(widget, x = x, y = y,
                  linewidth = linesWidth,
                  color = linesColor,
                  parent = parent)
  }

  # find origin of rotation; Rotate around that origin
  # from: https://stackoverflow.com/questions/49327247/add-labels-to-the-center-of-a-geom-curve-line-ggplot
  calcControlPoints <- function(x1, y1, x2, y2, curvature, angle, ncp) {
    # Negative curvature means curve to the left
    # Positive curvature means curve to the right
    # Special case curvature = 0 (straight line) has been handled
    xm <- (x1 + x2)/2
    ym <- (y1 + y2)/2
    dx <- x2 - x1
    dy <- y2 - y1
    if(dx == 0) dx <- 1e-8
    slope <- dy/dx

    # Calculate "corner" of region to produce control points in
    # (depends on 'angle', which MUST lie between 0 and 180)
    # Find by rotating start point by angle around mid point
    if (is.null(angle)) {
      # Calculate angle automatically
      angle <- ifelse(slope < 0,
                      2*atan(abs(slope)),
                      2*atan(1/slope))
    } else {
      angle <- angle/180*pi
    }
    sina <- sin(angle)
    cosa <- cos(angle)
    # FIXME:  special case of vertical or horizontal line ?
    cornerx <- xm + (x1 - xm)*cosa - (y1 - ym)*sina
    cornery <- ym + (y1 - ym)*cosa + (x1 - xm)*sina

    # Calculate angle to rotate region by to align it with x/y axes
    beta <- -atan((cornery - y1)/(cornerx - x1))
    sinb <- sin(beta)
    cosb <- cos(beta)
    # Rotate end point about start point to align region with x/y axes
    newx2 <- x1 + dx*cosb - dy*sinb
    newy2 <- y1 + dy*cosb + dx*sinb

    # Calculate x-scale factor to make region "square"
    # FIXME:  special case of vertical or horizontal line ?
    scalex <- (newy2 - y1)/(newx2 - x1)
    # Scale end points to make region "square"
    newx1 <- x1*scalex
    newx2 <- newx2*scalex

    # Calculate the origin in the "square" region
    # (for rotating start point to produce control points)
    # (depends on 'curvature')
    # 'origin' calculated from 'curvature'
    ratio <- 2*(sin(atan(curvature))^2)
    origin <- curvature - curvature/ratio
    # 'hand' also calculated from 'curvature'
    if (curvature > 0)
      hand <- "right"
    else
      hand <- "left"
    oxy <- calcOrigin(newx1, y1, newx2, newy2, origin, hand)
    ox <- oxy$x
    oy <- oxy$y

    # Calculate control points
    # Direction of rotation depends on 'hand'
    dir <- switch(hand,
                  left=-1,
                  right=1)
    # Angle of rotation depends on location of origin
    maxtheta <- pi + sign(origin*dir)*2*atan(abs(origin))
    theta <- seq(0, dir*maxtheta,
                 dir*maxtheta/(ncp + 1))[c(-1, -(ncp + 2))]
    costheta <- cos(theta)
    sintheta <- sin(theta)
    # May have BOTH multiple end points AND multiple
    # control points to generate (per set of end points)
    # Generate consecutive sets of control points by performing
    # matrix multiplication
    cpx <- ox + ((newx1 - ox) %*% t(costheta)) -
      ((y1 - oy) %*% t(sintheta))
    cpy <- oy + ((y1 - oy) %*% t(costheta)) +
      ((newx1 - ox) %*% t(sintheta))

    # Reverse transformations (scaling and rotation) to
    # produce control points in the original space
    cpx <- cpx/scalex
    sinnb <- sin(-beta)
    cosnb <- cos(-beta)
    finalcpx <- x1 + (cpx - x1)*cosnb - (cpy - y1)*sinnb
    finalcpy <- y1 + (cpy - y1)*cosnb + (cpx - x1)*sinnb

    list(x=as.numeric(t(finalcpx)), y=as.numeric(t(finalcpy)))
  }

  calcOrigin <- function(x1, y1, x2, y2, origin, hand) {
    # Positive origin means origin to the "right"
    # Negative origin means origin to the "left"
    xm <- (x1 + x2)/2
    ym <- (y1 + y2)/2
    dx <- x2 - x1
    dy <- y2 - y1
    slope <- dy/dx
    oslope <- -1/slope
    # The origin is a point somewhere along the line between
    # the end points, rotated by 90 (or -90) degrees
    # Two special cases:
    # If slope is non-finite then the end points lie on a vertical line, so
    # the origin lies along a horizontal line (oslope = 0)
    # If oslope is non-finite then the end points lie on a horizontal line,
    # so the origin lies along a vertical line (oslope = Inf)
    tmpox <- ifelse(!is.finite(slope),
                    xm,
                    ifelse(!is.finite(oslope),
                           xm + origin*(x2 - x1)/2,
                           xm + origin*(x2 - x1)/2))
    tmpoy <- ifelse(!is.finite(slope),
                    ym + origin*(y2 - y1)/2,
                    ifelse(!is.finite(oslope),
                           ym,
                           ym + origin*(y2 - y1)/2))
    # ALWAYS rotate by -90 about midpoint between end points
    # Actually no need for "hand" because "origin" also
    # encodes direction
    # sintheta <- switch(hand, left=-1, right=1)
    sintheta <- -1
    ox <- xm - (tmpoy - ym)*sintheta
    oy <- ym + (tmpox - xm)*sintheta

    list(x=ox, y=oy)
  }
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
    hexWidth <- min(unlist(lapply(unique_y,
                                  function(j) {
                                    id <- which(data$y == j)
                                    diff(data$x[id])
                                  })
    ))
    # scale data
    scale.hex_radius <- hexWidth/ (diff(x.range) * sqrt(3))
    scale.x <- (data$x - x.range[1]) / diff(x.range)
    scale.y <- (data$y - y.range[1]) / diff(y.range)

    hex_x <- lapply(seq_len(n),
                    function(i) {
                      x.north <- x.south <- scale.x[i]
                      x.northeast <- x.southeast <- scale.x[i] + sqrt(3) * scale.hex_radius / 2
                      x.northwest <- x.southwest <- scale.x[i] - sqrt(3) * scale.hex_radius / 2
                      c(x.north, x.northeast, x.southeast, x.south, x.southwest, x.northwest) * diff(x.range) + x.range[1]
                    })

    hex_y <- lapply(seq_len(n),
                    function(i) {
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
  stackPos <- sapply(1:n,
                     function(i) {
                       if(i == 1) data$stackpos[i] else {
                         data$stackpos[i - countId[i] + 1] + (countId[i] - 1) * stackRatio
                       }
                     })

  dotsGroup <- l_layer_group(widget, "dots")
  radius <- data$binwidth[1]/2
  if(layerGeom$geom_params$binaxis == "y"){
    lapply(1:n,
           function(i){
             xradius <- diff(ggplotPanel_params$x.range)/diff(ggplotPanel_params$y.range)*radius
             l_layer_oval(widget, parent = dotsGroup,
                          x = c(data$x[i] + stackPos[i] * 2 * xradius - xradius,
                                data$x[i] + stackPos[i] * 2 * xradius + xradius),
                          y = c(data$y[i] - radius, data$y[i] + radius),color =  fillColor[i],
                          linecolor = lineColor[i])
           })
  } else  {
    yradius <- diff(ggplotPanel_params$y.range)/diff(ggplotPanel_params$x.range)*radius
    lapply(1:n,
           function(i){
             l_layer_oval(widget, parent = dotsGroup,
                          x = c(data$x[i] - radius, data$x[i] + radius),
                          y = c(data$y[i] - yradius + stackPos[i] * 2 * yradius,
                                data$y[i] + yradius + stackPos[i] * 2 * yradius),color =  fillColor[i],
                          linecolor = lineColor[i])
           })
  }
}
