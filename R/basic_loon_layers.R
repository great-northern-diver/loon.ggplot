########################################### basic layers ###########################################
#' @title Transform geom layers to loon layers
#' @description Function \code{loonLayer} is used to create \code{loon} non-interactive layers.
#' For some \code{ggplot2} extension packages, one can edit this function to realize the transformation.
#' @param widget a \code{loon} widget
#' @param layerGeom a \code{ggplot} \code{Geom} layer object
#' @param data a data frame (i.e. \code{x}, \code{y}, etc) of this particular layer
#' @param ggplotPanelParams \code{ggplot} panel parameters
#' @param ggObj the \code{ggplot} object
#' @param parent a valid Tk parent widget path.
#' @param label label used in the layers inspector
#' @param ... not for users
#' @export
#'
#'
loonLayer <- function(widget,
                      layerGeom,
                      data,
                      ggplotPanelParams,
                      ggObj,
                      parent,
                      label,
                      ...) {
  UseMethod("loonLayer", layerGeom$geom)
}

#' @export
loonLayer.default <- function(widget,
                              layerGeom,
                              data,
                              ggplotPanelParams,
                              ggObj,
                              parent,
                              label,
                              ...) {
  warning("Unknown geom layers",
          call. = FALSE)
  return(NULL)
}

#' @export
loonLayer.GeomPoint <- function(widget,
                                layerGeom,
                                data,
                                ggplotPanelParams,
                                ggObj,
                                parent = "root",
                                label = NULL,
                                ...){

  if(dim(data)[1] == 0) return(NULL)

  isCoordPolar <- is.CoordPolar(ggObj$coordinates)
  coordinates <- ggObj$coordinates
  if(isCoordPolar) {
    coordPolarxy <- Cartesianxy2Polarxy(layerGeom, coordinates, data, ggplotPanelParams)
    x <- coordPolarxy$x
    y <- coordPolarxy$y
  } else {
    x <- data$x
    y <- data$y
  }

  lineColor <- data$colour
  pointsColor <- if(!is.null(data$shape)) {
    sapply(seq(dim(data)[1]),
           function(j){
             if(data$shape[j] %in% 21:24){
               data$fill[j]
             } else {
               data$colour[j]
             }
           })
  } else data$colour
  # l_layer_points cannot change the shape of the points so far
  # pointsGlyph <- pch_to_glyph(data$shape, data$alpha)
  pointsSize <- as_loon_size(data$size, "points",
                             stroke = data$stroke)

  # method <- get_stat_param(layerGeom, "distribution", ...)

  pl <- tryCatch(
    loon::l_layer_points(widget,
                         x = x, y = y,
                         color = hex6to12(pointsColor),
                         size = pointsSize,
                         linecolor = lineColor,
                         parent = parent,
                         label = "points"),
    error = function(e) NULL
  )
  return(pl)
}

#' @export
loonLayer.GeomRect <- function(widget,
                               layerGeom,
                               data,
                               ggplotPanelParams,
                               ggObj,
                               parent = "root",
                               label = NULL,
                               ...) {

  if(dim(data)[1] == 0) return(NULL)
  isCoordPolar <- is.CoordPolar(ggObj$coordinates)
  n <- dim(data)[1]
  fillColor <- data$fill
  linesColor <- data$colour
  linesWidth <- as_loon_size(data$size, "lines")
  xrange <- ggplotPanelParams$x.range
  yrange <- ggplotPanelParams$y.range

  coordinates <- ggObj$coordinates
  if(n == 1) {

    if(isCoordPolar){
      coordPolarxy <- Cartesianxy2Polarxy.GeomRect(NULL, coordinates, data, ggplotPanelParams)
      x <- coordPolarxy$x
      y <- coordPolarxy$y
      rl <- tryCatch(
        loon::l_layer_polygon(
          widget, x = x, y = y,
          color = hex6to12(fillColor),
          linecolor = hex6to12(linesColor),
          linewidth = linesWidth,
          parent = parent,
          label = label %||% "rectangle"
        ),
        error = function(e) NULL
      )
    } else {
      x <- c(data$xmin, data$xmax)
      y <- c(data$ymin, data$ymax)
      rl <- tryCatch(
        loon::l_layer_rectangle(
          widget,
          x = x, y = y,
          color = hex6to12(fillColor),
          linecolor = hex6to12(linesColor),
          linewidth = linesWidth,
          parent = parent,
          label = label %||% "rectangle"
        ),
        error = function(e) NULL
      )
    }
  } else {

    method <- get_stat_param(layerGeom, "bins", ...)

    if(parent == "root") {
      parent <- loon::l_layer_group(widget,
                                    label = label %||% paste(c("rectangles", method)))
    }

    # any NA will not be drawn
    if(isCoordPolar) {

      x <- c()
      y <- c()
      group <- c()
      lapply(1:n,
             function(i){
               coordPolarxy <- Cartesianxy2Polarxy.GeomRect(NULL, coordinates, data[i, ], ggplotPanelParams)
               xx <- coordPolarxy$x
               yy <- coordPolarxy$y
               x <<- c(x, xx)
               y <<- c(y, yy)
               group <<- c(group, rep(i, length(xx)))
             }
      )

      mappingLabel <- get_mappingLabel(layerGeom,
                                       name = "rectangle",
                                       label = label)

      tryCatch(
        loon::l_layer_polygons(
          widget,
          x = x,
          y = y,
          color = hex6to12(fillColor),
          linecolor = hex6to12(linesColor),
          linewidth = linesWidth,
          group = group,
          parent = parent,
          label = mappingLabel
        ),
        error = function(e) NULL
      )

    } else {

      mappingLabel <- get_mappingLabel(layerGeom,
                                       name = "rectangle",
                                       label = label)
      x <- c()
      y <- c()
      colorId <- 1:n
      lapply(1:n,
             function(i){

               yy <- if(is.na(data[i,]$ymin) & is.na(data[i,]$ymax)) rep(data[i,]$y, 2)
               else if(is.na(data[i,]$ymin) & !is.na(data[i,]$ymax)) c(2 * data[i,]$y - data[i,]$ymax  , data[i,]$ymax)
               else if(!is.na(data[i,]$ymin) & is.na(data[i,]$ymax)) c(data[i,]$ymin  , 2 * data[i,]$y - data[i,]$ymin)
               else{
                 c(if(is.infinite(data[i,]$ymin)) yrange[1] else data[i,]$ymin,
                   if(is.infinite(data[i,]$ymax)) yrange[2] else data[i,]$ymax)
               }
               xx <- if(is.na(data[i,]$xmin) & is.na(data[i,]$xmax)) rep(data[i,]$x, 2)
               else if(is.na(data[i,]$xmin) & !is.na(data[i,]$xmax)) c(2 * data[i,]$x - data[i,]$xmax  , data[i,]$xmax)
               else if(!is.na(data[i,]$xmin) & is.na(data[i,]$xmax)) c(data[i,]$xmin  , 2 * data[i,]$x - data[i,]$xmin)
               else{
                 c(if(is.infinite(data[i,]$xmin)) xrange[1] else data[i,]$xmin,
                   if(is.infinite(data[i,]$xmax)) xrange[2] else data[i,]$xmax)
               }

               if(any(is.na(xx)) | any(is.na(yy))) {
                 NULL # no need to draw
                 colorId <<- colorId[!colorId == i]
               } else {
                 x <<- c(x, xx)
                 y <<- c(y, yy)
               }
             }
      )

      if(length(colorId) == 0) return(NULL)

      rl <- tryCatch(
        loon::l_layer_rectangles(
          widget,
          x = x,
          y = y,
          color = hex6to12(fillColor[colorId]),
          linecolor = hex6to12(linesColor[colorId]),
          linewidth = linesWidth[colorId],
          group = rep(colorId, each = 2),
          parent = parent,
          label = mappingLabel
        ),
        error = function(e) NULL
      )
    }

    rl <- parent
  }

  return(rl)
}

#' @export
loonLayer.GeomPolygon <- function(widget,
                                  layerGeom,
                                  data,
                                  ggplotPanelParams,
                                  ggObj,
                                  parent = "root",
                                  label = NULL,
                                  ...){

  if(dim(data)[1] == 0) return(NULL)

  isCoordPolar <- is.CoordPolar(ggObj$coordinates)
  # for map data

  rearrangePolygonData <- function(data) {
    na_x <- is.na(data$x)
    na_y <- is.na(data$y)
    if (all(na_x != na_y)) {
      data <- data[!union(na_x, na_y), ]
      return(data)
    }

    pos <- c(which(na_x), dim(data)[1])
    npolygons <- length(pos)

    if(npolygons > 1) {
      group <- c()

      for(i in 1:npolygons) {
        if(i == 1)
          group <- c(group, rep(i, pos[i]))
        else
          group <- c(group, rep(i, pos[i] - pos[i-1]))
      }

      data$group <- group
      data <- data[!na_x, ]
    }

    return(data)
  }

  data <- rearrangePolygonData(data)
  uniGroup <- unique(data$group)
  fillColor <- data$fill
  linesColor <- data$colour
  linesWidth <- as_loon_size(data$size, "lines")

  coordinates <- ggObj$coordinates
  if(length(uniGroup) == 1) {
    if(isCoordPolar) {
      coordPolarxy <- Cartesianxy2Polarxy(layerGeom, coordinates, data, ggplotPanelParams)
      x <- coordPolarxy$x
      y <- coordPolarxy$y
    } else {
      x <- data$x
      y <- data$y
    }

    pl <- tryCatch(
      loon::l_layer_polygon(
        widget,
        x = x,
        y = y,
        color = hex6to12(fillColor[1]),
        linecolor = hex6to12(linesColor[1]),
        linewidth = linesWidth[1],
        parent = parent,
        label = label %||% "polygon"
      ),
      error = function(e) NULL
    )
  } else {

    if(parent == "root") {
      parent <- loon::l_layer_group(widget,
                                    label = label %||% "polygons")
    }

    mappingLabel <- get_mappingLabel(layerGeom,
                                     name = "polygon",
                                     label = label)

    colorId <- vapply(uniGroup,
                       function(x) {
                         which(data$group == x)[1]
                       }, numeric(1L))

    if(isCoordPolar) {

      m <- length(uniGroup)
      x <- c()
      y <- c()
      group <- c()

      lapply(1:m,
             function(i){

               coordPolarxy <- Cartesianxy2Polarxy(layerGeom, coordinates, data[data$group == uniGroup[i], ], ggplotPanelParams)
               xx <- coordPolarxy$x
               yy <- coordPolarxy$y
               x <<- c(x, xx)
               y <<- c(y, yy)
               group <<- c(group, rep(i, length(xx)))
             })

      tryCatch(
        loon::l_layer_polygons(
          widget,
          x = x,
          y = y,
          color = hex6to12(fillColor[colorId]),
          linecolor = hex6to12(linesColor[colorId]),
          linewidth = linesWidth[colorId],
          group = group,
          parent = parent,
          label = mappingLabel
        ),
        error = function(e) NULL
      )
    } else {

      tryCatch(
        loon::l_layer_polygons(
          widget,
          x = data$x,
          y = data$y,
          color = hex6to12(fillColor[colorId]),
          linecolor = hex6to12(linesColor[colorId]),
          linewidth = linesWidth[colorId],
          parent = parent,
          group = data$group,
          label = mappingLabel
        ),
        error = function(e) NULL
      )
    }

    pl <- parent
  }

  return(pl)
}



#' @export
# TODO overlap
loonLayer.GeomText <- function(widget,
                               layerGeom,
                               data,
                               ggplotPanelParams,
                               ggObj,
                               parent = "root",
                               label = NULL,
                               ...){

  if(dim(data)[1] == 0) return(NULL)

  isCoordPolar <- is.CoordPolar(ggObj$coordinates)
  textsSize <- as_loon_size(data$size, "texts")
  textsColor <- data$colour
  textAnchor <- as_loon_hvjust(hjust = data$hjust, vjust = data$vjust)

  coordinates <- ggObj$coordinates
  if(isCoordPolar){
    coordPolarxy <- Cartesianxy2Polarxy(layerGeom, coordinates, data, ggplotPanelParams)
    x <- coordPolarxy$x
    y <- coordPolarxy$y
  } else {
    x <- data$x
    y <- data$y
  }

  if(dim(data)[1] == 1) {

    tl <- tryCatch(
      loon::l_layer_text(
        widget,
        x = x, y = y,
        text = as.character(data$label),
        size = textsSize,
        angle = data$angle,
        color =  hex6to12(textsColor),
        anchor = textAnchor,
        justify = "left",
        parent = parent,
        label = label %||% "text"
      ),
      error = function(e) NULL
    )
  } else {
    tl <- tryCatch(
      loon::l_layer_texts(
        widget,
        x = x, y = y,
        text = as.character(data$label),
        size = textsSize,
        angle = data$angle,
        color = hex6to12(textsColor),
        anchor = textAnchor,
        justify = "left",
        parent = parent,
        label = label %||% "texts"),
      error = function(e) NULL
    )
  }

  return(tl)
}


#' @export
# TODO draws a rectangle behind the text
loonLayer.GeomLabel <- function(widget,
                                layerGeom,
                                data,
                                ggplotPanelParams,
                                ggObj,
                                parent = "root",
                                label = NULL,
                                ...){

  loonLayer.GeomText(widget,
                     layerGeom,
                     data,
                     ggplotPanelParams,
                     ggObj,
                     parent,
                     label,
                     ...)
}


#' @export
loonLayer.GeomVline <- function(widget,
                                layerGeom,
                                data,
                                ggplotPanelParams,
                                ggObj,
                                parent = "root",
                                label = NULL,
                                ...) {
  if(dim(data)[1] == 0) return(NULL)

  isCoordPolar <- is.CoordPolar(ggObj$coordinates)
  n <- dim(data)[1L]
  linesWidth <- as_loon_size(data$size, "lines")
  linesColor <- data$colour
  linesDash <- as_loon_dash(data$linetype)
  yrange <- ggplotPanelParams$y.range

  coordinates <- ggObj$coordinates

  if(n == 1) {
    if(isCoordPolar){
      coordPolarxy <- Cartesianxy2Polarxy(layerGeom, coordinates, data, ggplotPanelParams)
      x <- coordPolarxy$x
      y <- coordPolarxy$y
    } else {
      x <- rep(data$xintercept, 2)
      y <- yrange
    }

    ll <- tryCatch(
      loon::l_layer_line(widget,
                         x = x, y = y,
                         linewidth = linesWidth,
                         color = hex6to12(linesColor),
                         dash = linesDash[[1]],
                         parent = parent,
                         label = label %||% "vline"),
      error = function(e) NULL
    )
  } else {

    if(parent == "root") {
      parent <-  loon::l_layer_group(widget,
                                     label = label %||% "vlines")
    }

    mappingLabel <- get_mappingLabel(layerGeom,
                                     name = "vline",
                                     label = label)
    x <- c()
    y <- c()
    group <- c()

    lapply(1:n,
           function(i){
             if(isCoordPolar){
               coordPolarxy <- Cartesianxy2Polarxy(layerGeom, coordinates, data[i, ], ggplotPanelParams)
               xx <- coordPolarxy$x
               yy <- coordPolarxy$y
             } else {
               xx <- rep(data[i,]$xintercept, 2)
               yy <- yrange
             }

             x <<- c(x, xx)
             y <<- c(y, yy)
             group <<- c(group, rep(i, length(xx)))
           })
    tryCatch(
      loon::l_layer_lines(widget,
                          x = x,
                          y = y,
                          linewidth = linesWidth,
                          color = hex6to12(linesColor),
                          # dash = unlist(linesDash), l_layer_lines does not take dash
                          group = group,
                          parent = parent,
                          label = mappingLabel),
      error = function(e) NULL
    )

    ll <- parent
  }

  return(ll)
}


#' @export
loonLayer.GeomHline <- function(widget,
                                layerGeom,
                                data,
                                ggplotPanelParams,
                                ggObj,
                                parent = "root",
                                label = NULL,
                                ...){
  if(dim(data)[1] == 0) return(NULL)

  isCoordPolar <- is.CoordPolar(ggObj$coordinates)
  n <- dim(data)[1]
  linesWidth <- as_loon_size(data$size, "lines")
  linesColor <- data$colour
  xrange <- ggplotPanelParams$x.range
  linesDash <- as_loon_dash(data$linetype)

  coordinates <- ggObj$coordinates
  if(n == 1) {
    if(isCoordPolar){
      coordPolarxy <- Cartesianxy2Polarxy(layerGeom, coordinates, data, ggplotPanelParams)
      x <- coordPolarxy$x
      y <- coordPolarxy$y
    } else {
      y <- rep(data$yintercept, 2)
      x <- xrange
    }
    ll <- tryCatch(
      loon::l_layer_line(widget,
                         x = x, y = y,
                         linewidth = linesWidth,
                         color = hex6to12(linesColor),
                         dash = linesDash[[1]],
                         parent = parent,
                         label = label %||% "hline"),
      error = function(e) NULL
    )
  } else {

    if(parent == "root") {
      parent <- loon::l_layer_group(widget,
                                    label = label %||% "hlines")
    }

    mappingLabel <- get_mappingLabel(layerGeom,
                                     name = "hline",
                                     label = label)

    x <- c()
    y <- c()
    group <- c()
    lapply(1:n,
           function(i){
             if(isCoordPolar){
               coordPolarxy <- Cartesianxy2Polarxy(layerGeom, coordinates, data[i, ], ggplotPanelParams)
               xx <- coordPolarxy$x
               yy <- coordPolarxy$y
             } else {
               xx <- xrange
               yy <- rep(data[i,]$yintercept, 2)
             }

             x <<- c(x, xx)
             y <<- c(y, yy)
             group <<- c(group, rep(i, length(xx)))
           })

    tryCatch(
      loon::l_layer_lines(widget,
                          x = x,
                          y = y,
                          linewidth = linesWidth,
                          color = hex6to12(linesColor),
                          # dash = unlist(linesDash), l_layer_lines does not take dash
                          group = group,
                          parent = parent,
                          label = mappingLabel),
      error = function(e) NULL
    )

    ll <- parent
  }

  return(ll)
}

#' @export
loonLayer.GeomAbline <- function(widget,
                                 layerGeom,
                                 data,
                                 ggplotPanelParams,
                                 ggObj,
                                 parent = "root",
                                 label = NULL,
                                 ...){
  if(dim(data)[1] == 0) return(NULL)
  isCoordPolar <- is.CoordPolar(ggObj$coordinates)
  n <- dim(data)[1]
  linesWidth <- as_loon_size(data$size, "lines")
  linesColor <- (data$colour)
  xrange <- ggplotPanelParams$x.range
  yrange <- ggplotPanelParams$y.range
  linesDash <- as_loon_dash(data$linetype)

  coordinates <- ggObj$coordinates
  if(n == 1) {
    if(isCoordPolar){
      coordPolarxy <- Cartesianxy2Polarxy(layerGeom, coordinates, data,
                                          ggplotPanelParams)
      x <- coordPolarxy$x
      y <- coordPolarxy$y
    } else {
      coordCartesianxy <- abline2xy(xrange, yrange, data$slope, data$intercept)
      x <- coordCartesianxy$x
      y <- coordCartesianxy$y
    }
    ll <- tryCatch(
      loon::l_layer_line(widget, x = x, y = y,
                         linewidth = linesWidth,
                         color = hex6to12(linesColor),
                         dash = linesDash[[1]],
                         parent = parent,
                         label = label %||% "abline"),
      error = function(e) NULL
    )
  } else {

    if(parent == "root") {
      parent <-  loon::l_layer_group(widget,
                                     label = label %||% "ablines")
    }

    mappingLabel <- get_mappingLabel(layerGeom,
                                     name = "abline",
                                     label = label)

    x <- c()
    y <- c()
    group <- c()
    lapply(1:n,
           function(i){
             if(isCoordPolar) {
               coordPolarxy <- Cartesianxy2Polarxy(layerGeom, coordinates, data[i,],
                                                   ggplotPanelParams)
               xx <- coordPolarxy$x
               yy <- coordPolarxy$y
             } else {
               coordCartesianxy <- abline2xy(xrange, yrange, data[i, ]$slope, data[i, ]$intercept)
               xx <- coordCartesianxy$x
               yy <- coordCartesianxy$y
             }

             x <<- c(x, xx)
             y <<- c(y, yy)
             group <<- c(group, rep(i, length(xx)))
           })
    tryCatch(
      loon::l_layer_lines(widget,
                          x = x,
                          y = y,
                          linewidth = linesWidth,
                          color = hex6to12(linesColor),
                          # dash = unlist(linesDash), l_layer_lines does not take dash
                          group = group,
                          parent = parent,
                          label =mappingLabel),
      error = function(e) NULL
    )

    ll <- parent
  }

  return(ll)
}


#' @export
# given start and end to draw a straight line
loonLayer.GeomSegment <- function(widget,
                                  layerGeom,
                                  data,
                                  ggplotPanelParams,
                                  ggObj,
                                  parent = "root",
                                  label = NULL,
                                  ...){
  if(dim(data)[1] == 0) return(NULL)
  isCoordPolar <- is.CoordPolar(ggObj$coordinates)
  n <- dim(data)[1]
  linesWidth <- as_loon_size(data$size, "lines")
  linesColor <- (data$colour)
  linesDash <- as_loon_dash(data$linetype)

  coordinates <- ggObj$coordinates
  if(n == 1) {
    if(isCoordPolar) {
      coordPolarxy <- Cartesianxy2Polarxy.GeomSegment(NULL, coordinates, data, ggplotPanelParams)
      x <- coordPolarxy$x
      y <- coordPolarxy$y
    } else {
      x <- c(data$x, data$xend)
      y <- c(data$y, data$yend)
    }

    ll <- tryCatch(
      loon::l_layer_line(widget, x = x, y = y,
                         linewidth = linesWidth,
                         color = hex6to12(linesColor),
                         dash = linesDash[[1]],
                         parent = parent,
                         label = label %||% "segment"),
      error = function(e) NULL
    )
  } else {

    if(parent == "root") {
      parent <-  loon::l_layer_group(widget,
                                     label = label %||% "segments")
    }

    mappingLabel <- get_mappingLabel(layerGeom,
                                     name = "segment",
                                     label = label)

    x <- c()
    y <- c()
    group <- c()
    lapply(1:n,
           function(i){
             if(isCoordPolar){
               coordPolarxy <- Cartesianxy2Polarxy.GeomSegment(NULL, coordinates, data[i, ], ggplotPanelParams)
               xx <- coordPolarxy$x
               yy <- coordPolarxy$y
             } else {
               xx <- c(data[i,]$x, data[i,]$xend)
               yy <- c(data[i,]$y, data[i,]$yend)
             }

             x <<- c(x, xx)
             y <<- c(y, yy)
             group <<- c(group, rep(i, length(xx)))
           })
    tryCatch(
      loon::l_layer_lines(widget,
                          x = x,
                          y = y,
                          linewidth = linesWidth,
                          color = hex6to12(linesColor),
                          # dash = unlist(linesDash), l_layer_lines does not take dash
                          group = group,
                          parent = parent,
                          label = mappingLabel),
      error = function(e) NULL
    )

    ll <- parent
  }

  return(ll)
}

#' @export
loonLayer.GeomHex <- function(widget,
                              layerGeom,
                              data,
                              ggplotPanelParams,
                              ggObj,
                              parent = "root",
                              label = NULL,
                              ...){
  if(dim(data)[1] == 0) return(NULL)

  # ranges
  x.range <- ggplotPanelParams$x.range
  y.range <- ggplotPanelParams$y.range
  # can n be one?
  n <- dim(data)[1]
  if (n == 1) {
    warning("one hexagon is not allowed yet", call. = FALSE)
    return(NULL)
  }
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
  fillColor <- (data$fill)
  linesColor <- (data$colour)
  linesWidth <- as_loon_size(data$size, "lines")

  method <- get_stat_param(layerGeom, "bins", ...)

  mappingLabel <- get_mappingLabel(layerGeom,
                                   name = paste(c("hex", method)),
                                   label = label,
                                   i = NULL)

  lp <- tryCatch(
    loon::l_layer_polygons(widget,
                           hex_x, hex_y,
                           color = hex6to12(fillColor),
                           linecolor = hex6to12(linesColor),
                           linewidth = linesWidth,
                           parent = parent,
                           label = mappingLabel),
    error = function(e) NULL
  )

  return(lp)
}


#' @export
loonLayer.GeomDotplot <- function(widget,
                                  layerGeom,
                                  data,
                                  ggplotPanelParams,
                                  ggObj,
                                  parent = "root",
                                  label = NULL,
                                  ...) {
  if(dim(data)[1] == 0) return(NULL)
  # uniGroup <- unique(data$group)
  fillColor <- (data$fill)
  lineColor <- (data$colour)

  stackRatio <- layerGeom$geom_params$stackratio
  countId <- data$countidx
  n <- dim(data)[1]
  stackPos <- sapply(seq(n),
                     function(i) {
                       if(i == 1) data$stackpos[i] else {
                         data$stackpos[i - countId[i] + 1] + (countId[i] - 1) * stackRatio
                       }
                     })

  radius <- data$binwidth[1]/2

  if(parent == "root") {
    parent <- loon::l_layer_group(widget,
                                  label = label %||% "dots")
  }

  if(layerGeom$geom_params$binaxis == "y") {
    lapply(1:n,
           function(i){
             xradius <- diff(ggplotPanelParams$x.range)/diff(ggplotPanelParams$y.range)*radius

             mappingLabel <- get_mappingLabel(layerGeom,
                                              name = "dot",
                                              label = label,
                                              i = if(n == 1) NULL else i)
             tryCatch(
               loon::l_layer_oval(widget,
                                  x = c(data$x[i] + stackPos[i] * 2 * xradius - xradius,
                                        data$x[i] + stackPos[i] * 2 * xradius + xradius),
                                  y = c(data$y[i] - radius, data$y[i] + radius),
                                  color =  hex6to12(fillColor[i]),
                                  linecolor = hex6to12(lineColor[i]),
                                  parent = parent,
                                  label = mappingLabel),
               error = function(e) NULL
             )
           })
  } else  {
    yradius <- diff(ggplotPanelParams$y.range)/diff(ggplotPanelParams$x.range)*radius
    lapply(1:n,
           function(i){

             mappingLabel <- get_mappingLabel(layerGeom,
                                              name = "dot",
                                              label = label,
                                              i = if(n == 1) NULL else i)
             tryCatch(
               loon::l_layer_oval(widget,
                                  x = c(data$x[i] - radius, data$x[i] + radius),
                                  y = c(data$y[i] - yradius + stackPos[i] * 2 * yradius,
                                        data$y[i] + yradius + stackPos[i] * 2 * yradius),
                                  color =  hex6to12(fillColor[i]),
                                  linecolor = hex6to12(lineColor[i]),
                                  parent = parent,
                                  label = mappingLabel),
               error = function(e) NULL
             )
           })
  }

  return(parent)
}

#' @export
# colorful line (line built with points)
loonLayer.GeomPath <- function(widget,
                               layerGeom,
                               data,
                               ggplotPanelParams,
                               ggObj,
                               parent = "root",
                               label = NULL,
                               ...) {
  if(dim(data)[1] == 0) return(NULL)

  isCoordPolar <- is.CoordPolar(ggObj$coordinates)
  # path group
  if (parent == "root") {
    parent <- loon::l_layer_group(widget,
                                  label = label %||% "paths")
  }

  coordinates <- ggObj$coordinates
  uniGroup <- unique(data$group)

  m <- length(uniGroup)

  lapply(1:m,
         function(i){
           groupData <- data[data$group == uniGroup[i], ]
           linesColor <- groupData$colour
           len_uni_col <- len_unique(groupData$colour)

           linesWidth <- as_loon_size(groupData$size, "lines")
           linesDash <- as_loon_dash(groupData$linetype)
           # a single line with a single color
           if(len_uni_col == 1) {
             if(isCoordPolar){
               coordPolarxy <- Cartesianxy2Polarxy.GeomPath(NULL, coordinates, groupData, ggplotPanelParams)
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

             tryCatch(
               loon::l_layer_line(
                 widget,
                 x = x, y = y,
                 linewidth = linesWidth[1],
                 color = hex6to12(linesColor[1]),
                 dash = linesDash[[1]],
                 parent = parent,
                 label = mappingLabel
               ),
               error = function(e) NULL
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
                                 ggplotPanelParams,
                                 ggObj,
                                 parent = parent,
                                 label = mappingLabel)
           }
         })
  return(parent)
}

#' @export
loonLayer.GeomRaster <- function(widget,
                                 layerGeom,
                                 data,
                                 ggplotPanelParams,
                                 ggObj,
                                 parent = "root",
                                 label = NULL,
                                 ...) {

  if(dim(data)[1] == 0) return(NULL)

  isCoordPolar <- is.CoordPolar(ggObj$coordinates)
  n <- dim(data)[1]
  fillColor <- data$fill
  linesColor <- data$colour
  linesWidth <- as_loon_size(data$size, "lines")
  xrange <- ggplotPanelParams$x.range
  yrange <- ggplotPanelParams$y.range

  coordinates <- ggObj$coordinates

  method <- get_stat_param(layerGeom, ...)

  mappingLabel <- get_mappingLabel(layerGeom,
                                   name = method %||% "raster",
                                   label = label,
                                   i = NULL)

  if(n == 1) {
    if(isCoordPolar){

      coordPolarxy <- Cartesianxy2Polarxy.GeomRect(NULL, coordinates, data, ggplotPanelParams)
      x <- coordPolarxy$x
      y <- coordPolarxy$y
      l <- tryCatch(
        loon::l_layer_polygon(
          widget, x = x, y = y,
          color = hex6to12(fillColor),
          linecolor = hex6to12(linesColor),
          linewidth = linesWidth,
          parent = parent,
          label = mappingLabel
        ),
        error = function(e) NULL
      )

    } else {

      x <- c(data$xmin, data$xmax)
      y <- c(data$ymin, data$ymax)
      l <- tryCatch(
        loon::l_layer_rectangle(
          widget,
          x = x, y = y,
          color = hex6to12(fillColor),
          linecolor = hex6to12(linesColor),
          linewidth = linesWidth,
          parent = parent,
          label = mappingLabel
        ),
        error = function(e) NULL
      )
    }
  } else {

    # any NA will not be drawn
    if(isCoordPolar) {

      x <- y <- list()
      lapply(1:n,
             function(i){
               coordPolarxy <- Cartesianxy2Polarxy.GeomRect(NULL, coordinates, data[i, ], ggplotPanelParams)
               x[[i]] <<- coordPolarxy$x
               y[[i]] <<- coordPolarxy$y
             }
      )

      l <- tryCatch(
        loon::l_layer_polygons(
          widget,
          x = x,
          y = y,
          color = fillColor,
          linecolor = linesColor,
          linewidth = linesWidth,
          parent = parent,
          label = mappingLabel
        ),
        error = function(e) NULL
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

      l <- tryCatch(
        loon::l_layer_rectangles(
          widget,
          x = x, y = y,
          color = fillColor,
          linecolor = linesColor,
          linewidth = linesWidth,
          parent = parent,
          label = mappingLabel
        ),
        error = function(e) NULL
      )
    }
  }

  return(l)
}

#' @export
loonLayer.GeomImageGlyph <- function(widget,
                                     layerGeom,
                                     data,
                                     ggplotPanelParams,
                                     ggObj,
                                     parent = "root",
                                     label = NULL,
                                     ...) {

  method <- get_stat_param(layerGeom, "image glyph", ...)

  if (parent == "root") {
    parent <- loon::l_layer_group(widget,
                                  label = method %||% "image glyph")
  }

  isCoordPolar <- is.CoordPolar(ggObj$coordinates)
  coordinates <- ggObj$coordinates
  if(isCoordPolar){
    coordPolarxy <- Cartesianxy2Polarxy(layerGeom, coordinates, data, ggplotPanelParams)
    xpos <- coordPolarxy$x
    ypos <- coordPolarxy$y
  } else {
    # position
    xpos <- data$x
    ypos <- data$y
  }

  images <- data$images
  imagewidth <- data$imagewidth
  imageheight <- data$imageheight

  size <-  glyph_as_layer_size(data$size, "image",
                               min = min(xpos, ypos, na.rm = TRUE),
                               max = max(xpos, ypos, na.rm = TRUE))
  n <- length(images)

  lapply(seq(n),
         function(i) {
           tryCatch(
             loon::l_layer_rasterImage(
               widget,
               image = images[[i]],
               xleft = xpos[i] - imagewidth[i]/2 * size[i],
               xright = xpos[i] + imagewidth[i]/2 * size[i],
               ybottom = ypos[i] - imageheight[i]/2 * size[i],
               ytop = ypos[i] + imageheight[i]/2 * size[i],
               label = get_mappingLabel(layerGeom,
                                        name = paste0("image", i),
                                        label = label),
               parent = parent
             ),
             error = function(e) NULL
           )
         })

  return(parent)
}

#' @export
loonLayer.GeomSerialAxesGlyph <- function(widget,
                                          layerGeom,
                                          data,
                                          ggplotPanelParams,
                                          ggObj,
                                          parent = "root",
                                          label = NULL,
                                          ...) {

  method <- get_stat_param(layerGeom, "serialaxes glyph", ...)

  if (parent == "root") {
    parent <- loon::l_layer_group(widget,
                                  label = method %||% "serialaxes glyph")
  }

  isCoordPolar <- is.CoordPolar(ggObj$coordinates)
  coordinates <- ggObj$coordinates
  if(isCoordPolar){
    coordPolarxy <- Cartesianxy2Polarxy(layerGeom, coordinates, data, ggplotPanelParams)
    xpos <- coordPolarxy$x
    ypos <- coordPolarxy$y
  } else {
    # position
    xpos <- data$x
    ypos <- data$y
  }

  axes.layout <- one_dim_state(data$axes.layout)
  scaling <- one_dim_state(data$scaling)
  # In the new ggmulti version, the andrews (logical) can be accessed
  andrews <- one_dim_state(data$andrews) %||% FALSE
  show.enclosing <-  one_dim_state(data$show.enclosing)
  show.axes <- one_dim_state(data$show.axes)
  axescolour <- one_dim_state(data$axescolour)

  serialAxesData <- data[, grepl("serialaxes.data", colnames(data))]
  scaledData <- get_scaledData(data = serialAxesData,
                               sequence = NULL,
                               scaling = scaling)

  p <- ncol(scaledData)

  if(andrews) {
    fourierTrans <- ggmulti::andrews(p = p, k = 200)
    scaledData <- as.matrix(scaledData) %*% fourierTrans$matrix

    dataRange <- range(scaledData)
    d <- if(diff(dataRange) == 0) 1 else diff(dataRange)

    scaledData <- (scaledData - min(scaledData))/d
  }

  dimension <- dim(scaledData)[2]

  show.area <- !any(is.na(data$fill))
  switch(
    axes.layout,
    "parallel" = {
      scale.x <- glyph_as_layer_size(data$size, "serialaxes",
                                     min = min(xpos, na.rm = TRUE),
                                     max = max(xpos, na.rm = TRUE),
                                     coord = "x", axesLayout = "parallel")
      scale.y <- glyph_as_layer_size(data$size, "serialaxes",
                                     min = min(ypos, na.rm = TRUE),
                                     max = max(ypos, na.rm = TRUE),
                                     coord = "y", axesLayout = "parallel")

      xaxis <- t(sapply(scale.x, function(x) seq(-0.5 * x, 0.5 * x, length.out = dimension)))
      yaxis <- (scaledData - 0.5) * scale.y
    },
    "radial" = {
      scale.x <- glyph_as_layer_size(data$size, "serialaxes",
                                     min = min(xpos, na.rm = TRUE),
                                     max = max(xpos, na.rm = TRUE),
                                     coord = "x", axesLayout = "radial")
      scale.y <- glyph_as_layer_size(data$size, "serialaxes",
                                     min = min(ypos, na.rm = TRUE),
                                     max = max(ypos, na.rm = TRUE),
                                     coord = "y", axesLayout = "radial")

      angle <- seq(0, 2*base::pi, length.out = dimension + 1)[seq(dimension)]

      xaxis <- t(sapply(seq(length(scale.x)),
                        function(i) scale.x[i] * scaledData[i, ] * cos(angle)))
      yaxis <- t(sapply(seq(length(scale.y)),
                        function(i) scale.y[i] * scaledData[i, ] * sin(angle)))
    }
  )

  aesthetic <- get_aesthetic(axes.layout = axes.layout,
                             andrews = andrews,
                             xpos = xpos, ypos = ypos,
                             scale.x = scale.x, scale.y = scale.y,
                             xaxis = xaxis, yaxis = yaxis,
                             dimension = dimension,
                             p = p, show.area = show.area,
                             show.enclosing = show.enclosing)

  if(show.enclosing) {
    tryCatch(
      loon::l_layer_lines(
        widget,
        x = unlist(aesthetic$enclosingX),
        y = unlist(aesthetic$enclosingY),
        group = aesthetic$enclosingId,
        color = axescolour,
        label = get_mappingLabel(layerGeom,
                                 name = "enclosing",
                                 label = label),
        parent = parent
      ),
      error = function(e) NULL
    )
  }

  if(show.axes) {
    tryCatch(
      loon::l_layer_lines(
        widget,
        x = unlist(aesthetic$axesX),
        y = unlist(aesthetic$axesY),
        group = aesthetic$axesId,
        color = axescolour,
        label = get_mappingLabel(layerGeom,
                                 name = "axes",
                                 label = label),
        parent = parent
      ),
      error = function(e) NULL
    )
  }

  if(show.area) {
    tryCatch(
      loon::l_layer_polygons(
        widget,
        x = aesthetic$serialCoordX,
        y = aesthetic$serialCoordY,
        color = data$fill,
        linecolor = data$colour,
        label = get_mappingLabel(layerGeom,
                                 name = "serialaxes lines",
                                 label = label),
        parent = parent),
      error = function(e) NULL
    )

  } else {
    tryCatch(
      loon::l_layer_lines(
        widget,
        x = aesthetic$serialCoordX,
        y = aesthetic$serialCoordY,
        color = data$colour,
        label = get_mappingLabel(layerGeom,
                                 name = "serialaxes lines",
                                 label = label),
        parent = parent),
      error = function(e) NULL
    )
  }

  return(parent)
}

#' @export
loonLayer.GeomPolygonGlyph <- function(widget,
                                       layerGeom,
                                       data,
                                       ggplotPanelParams,
                                       ggObj,
                                       parent = "root",
                                       label = NULL,
                                       ...) {

  method <- get_stat_param(layerGeom, "polygon glyph", ...)

  if (parent == "root") {
    parent <- loon::l_layer_group(widget,
                                  label = method %||% "polygon glyph")
  }

  p <- length(data$x)
  show.area <- rep(TRUE, p)
  if(is.null(data$fill)) show.area <- rep(FALSE, p)
  show.area[is.na(data$fill)] <- FALSE

  isCoordPolar <- is.CoordPolar(ggObj$coordinates)
  coordinates <- ggObj$coordinates
  if(isCoordPolar){
    coordPolarxy <- Cartesianxy2Polarxy(layerGeom, coordinates, data, ggplotPanelParams)
    xpos <- coordPolarxy$x
    ypos <- coordPolarxy$y
  } else {
    # position
    xpos <- data$x
    ypos <- data$y
  }


  for(i in seq(p)) {

    polyx <- xpos[i] + data$polygon_x[[i]] *
      glyph_as_layer_size(data$size[i], "polygon",
                          min = min(xpos, na.rm = TRUE),
                          max = max(xpos, na.rm = TRUE))
    polyy <- ypos[i] + data$polygon_y[[i]] *
      glyph_as_layer_size(data$size[i], "polygon",
                          min = min(ypos, na.rm = TRUE),
                          max = max(ypos, na.rm = TRUE))

    if(show.area[i]) {

      # polygon
      tryCatch(
        loon::l_layer_polygon(
          widget,
          x = polyx,
          y = polyy,
          color = data$fill[i],
          linecolor = data$colour[i],
          linewidth = data$linewidth[i],
          label = get_mappingLabel(layerGeom,
                                   name = paste0("polygon", i),
                                   label = label),
          parent = parent
        ),
        error = function(e) NULL
      )
    } else {
      tryCatch(
        loon::l_layer_line(
          widget,
          x = c(polyx, polyx[1L]),
          y = c(polyy, polyy[1L]),
          color = data$colour[i],
          linewidth = data$linewidth[i],
          label = get_mappingLabel(layerGeom,
                                   name = paste0("polygon", i),
                                   label = label),
          parent = parent
        ),
        error = function(e) NULL
      )
    }
  }

  return(parent)
}

# TODO: the numbers in this function deserve more research, either practically or theoretically
glyph_as_layer_size <- function(x, type = "", min, max, ...) {

  if(min == max) {
    mag <- 1
  } else {
    mag <- 10^(floor(log(max - min, 10)))
  }

  switch(type,
         "image" = 1/10 * x * mag,
         "polygon" = 1/30 * x * mag,
         "serialaxes" = {
           args <- list(...)

           fun <- function(size, coord, axesLayout) {
             if (is.numeric(size)) {
               # trial and error to choose scale for size
               if (axesLayout == "radial") {
                 size <- sqrt(size) / 5
               } else if (axesLayout == "parallel"){
                 if (coord == "x") {
                   size <- sqrt(size) / 3.2
                 } else if (coord == "y"){
                   size <- sqrt(size) / 6.4
                 } else size <- NA
               } else size <- NA
               size[size == 0] <- 0.01
             }
             size * mag
           }
           fun(x, args$coord, args$axesLayout)
         },
         {
           x * mag
         })
}
