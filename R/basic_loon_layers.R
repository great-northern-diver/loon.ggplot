########################################### basic layers ###########################################
loonLayer <- function(widget,
                      layerGeom,
                      data,
                      ggplotPanel_params,
                      ggObj,
                      special,
                      parent,
                      label,
                      ...) {
  UseMethod("loonLayer", layerGeom$geom)
}

loonLayer.GeomPoint <- function(widget,
                                layerGeom,
                                data,
                                ggplotPanel_params,
                                ggObj,
                                special,
                                parent = "root",
                                label = NULL,
                                ...){

  if(dim(data)[1] != 0) {
    isCoordPolar <- is.CoordPolar(ggplotPanel_params)
    coordinates <- ggObj$coordinates
    if(isCoordPolar){
      coordPolarxy <- Cartesianxy2Polarxy(layerGeom, coordinates, data, ggplotPanel_params)
      x <- coordPolarxy$x
      y <- coordPolarxy$y
    } else {
      x <- data$x
      y <- data$y
    }

    lineColor <- hex6to12(data$colour)
    pointsColor <- if(!is.null(data$shape)) {
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

    # method <- get_stat_param(layerGeom, "distribution", ...)

    loon::l_layer_points(widget,
                         x = x, y = y,
                         color = pointsColor,
                         size = pointsSize,
                         linecolor = lineColor,
                         parent = parent,
                         label = "points")
  } else NULL
}

loonLayer.GeomRect <- function(widget,
                               layerGeom,
                               data,
                               ggplotPanel_params,
                               ggObj,
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

    coordinates <- ggObj$coordinates
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
          label = label %||% "rectangle"
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
          label = label %||% "rectangle"
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

        lapply(1:n,
               function(i){
                 coordPolarxy <- Cartesianxy2Polarxy.GeomRect(NULL, coordinates, data[i, ], ggplotPanel_params)
                 x <- coordPolarxy$x
                 y <- coordPolarxy$y

                 mappingLabel <- get_mappingLabel(layerGeom,
                                                  name = "rectangle",
                                                  label = label,
                                                  i = if(n == 1) NULL else i)

                 loon::l_layer_polygon(
                   widget,
                   x = x,
                   y = y,
                   color = fillColor[i],
                   linecolor = linesColor[i],
                   linewidth = linesWidth[i],
                   parent = parent,
                   label = mappingLabel
                 )
               }
        )

      } else {

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

                 if(any(is.na(x)) | any(is.na(y))) {
                   NULL # no need to draw
                 } else {
                   mappingLabel <- get_mappingLabel(layerGeom,
                                                    name = "rectangle",
                                                    label = label,
                                                    i = if(n == 1) NULL else i)

                   loon::l_layer_rectangle(
                     widget,
                     x = x, y = y,
                     color = fillColor[i],
                     linecolor = linesColor[i],
                     linewidth = linesWidth[i],
                     parent = parent,
                     label = mappingLabel
                   )
                 }
               }
        )
      }
    }
  } else NULL
}

loonLayer.GeomPolygon <- function(widget,
                                  layerGeom,
                                  data,
                                  ggplotPanel_params,
                                  ggObj,
                                  special,
                                  parent = "root",
                                  label = NULL,
                                  ...){
  if(dim(data)[1] != 0) {
    isCoordPolar <- is.CoordPolar(ggplotPanel_params)
    uniGroup <- unique(data$group)
    fillColor <- hex6to12(data$fill)
    linesColor <- hex6to12(data$colour)
    linesWidth <- as_loon_size(data$size, "lines")

    coordinates <- ggObj$coordinates
    if(length(uniGroup) == 1) {
      if(isCoordPolar) {
        coordPolarxy <- Cartesianxy2Polarxy(layerGeom, coordinates, data, ggplotPanel_params)
        x <- coordPolarxy$x
        y <- coordPolarxy$y
      } else {
        x <- data$x
        y <- data$y
      }

      loon::l_layer_polygon(
        widget,
        x = x, y = y,
        color = fillColor[1],
        linecolor = linesColor[1],
        linewidth = linesWidth[1],
        parent = parent,
        label = label %||% "polygon"
      )
    } else {

      if(parent == "root") {
        parent <- loon::l_layer_group(widget,
                                      label = label %||% "polygons")
      }

      m <- length(uniGroup)
      lapply(1:m,
             function(i){
               if(isCoordPolar) {
                 coordPolarxy <- Cartesianxy2Polarxy(layerGeom, coordinates, data[data$group == uniGroup[i], ], ggplotPanel_params)
                 x <- coordPolarxy$x
                 y <- coordPolarxy$y
               } else {
                 x <-  data$x[data$group == uniGroup[i]]
                 y <-  data$y[data$group == uniGroup[i]]
               }
               fColor <- fillColor[data$group == uniGroup[i]][1]
               lColor <- linesColor[data$group == uniGroup[i]][1]
               lWidth <- linesWidth[data$group == uniGroup[i]][1]

               mappingLabel <- get_mappingLabel(layerGeom,
                                                name = "polygon",
                                                label = label,
                                                i = if(m == 1) NULL else i)

               loon::l_layer_polygon(
                 widget,
                 x = x,
                 y = y,
                 color = fColor,
                 linecolor = lColor,
                 linewidth = lWidth,
                 parent = parent,
                 label = mappingLabel
               )
             })
    }
  } else NULL
}




# TODO overlap
loonLayer.GeomText <- function(widget,
                               layerGeom,
                               data,
                               ggplotPanel_params,
                               ggObj,
                               special,
                               parent = "root",
                               label = NULL,
                               ...){
  if(dim(data)[1] != 0) {
    isCoordPolar <- is.CoordPolar(ggplotPanel_params)
    textsSize <- as_loon_size(data$size, "texts")
    textsColor <- hex6to12(data$colour)
    textAnchor <- as_loon_hvjust(hjust = data$hjust, vjust = data$vjust)

    coordinates <- ggObj$coordinates
    if(isCoordPolar){
      coordPolarxy <- Cartesianxy2Polarxy(layerGeom, coordinates, data, ggplotPanel_params)
      x <- coordPolarxy$x
      y <- coordPolarxy$y
    } else {
      x <- data$x
      y <- data$y
    }

    if(dim(data)[1] == 1) {

      loon::l_layer_text(
        widget,
        x = x, y = y,
        text = as.character(data$label),
        size = textsSize,
        angle = data$angle,
        color =  textsColor,
        anchor = textAnchor,
        justify = "left",
        parent = parent,
        label = label %||% "text"
      )
    } else {
      loon::l_layer_texts(
        widget,
        x = x, y = y,
        text = as.character(data$label),
        size = textsSize,
        angle = data$angle,
        color = textsColor,
        anchor = textAnchor,
        justify = "left",
        parent = parent,
        label = label %||% "texts")
    }
  } else NULL
}



# TODO draws a rectangle behind the text
loonLayer.GeomLabel <- function(widget,
                                layerGeom,
                                data,
                                ggplotPanel_params,
                                ggObj,
                                special,
                                parent = "root",
                                label = NULL,
                                ...){

  loonLayer.GeomText(widget,
                     layerGeom,
                     data,
                     ggplotPanel_params,
                     ggObj,
                     special,
                     parent,
                     label,
                     ...)
}



loonLayer.GeomVline <- function(widget,
                                layerGeom,
                                data,
                                ggplotPanel_params,
                                ggObj,
                                special,
                                parent = "root",
                                label = NULL,
                                ...) {
  if(dim(data)[1] != 0) {
    isCoordPolar <- is.CoordPolar(ggplotPanel_params)
    n <- dim(data)[1]
    linesWidth <- as_loon_size(data$size, "lines")
    linesColor <- hex6to12(data$colour)
    linesDash <- as_loon_dash(data$linetype)
    yrange <- ggplotPanel_params$y.range

    coordinates <- ggObj$coordinates
    if(n == 1) {

      if(isCoordPolar){
        coordPolarxy <- Cartesianxy2Polarxy(layerGeom, coordinates, data, ggplotPanel_params)
        x <- coordPolarxy$x
        y <- coordPolarxy$y
      } else {
        x <- rep(data$xintercept, 2)
        y <- yrange
      }

      loon::l_layer_line(widget,
                         x = x, y = y,
                         linewidth = linesWidth,
                         color = linesColor,
                         dash = linesDash[[1]],
                         parent = parent,
                         label = label %||% "vline")
    } else {

      if(parent == "root") {
        parent <-  loon::l_layer_group(widget,
                                       label = label %||% "vlines")
      }

      lapply(1:n,
             function(i){
               if(isCoordPolar){
                 coordPolarxy <- Cartesianxy2Polarxy(layerGeom, coordinates, data[i, ], ggplotPanel_params)
                 x <- coordPolarxy$x
                 y <- coordPolarxy$y
               } else {
                 x <- rep(data[i,]$xintercept, 2)
                 y <- yrange
               }

               mappingLabel <- get_mappingLabel(layerGeom,
                                                name = "vline",
                                                label = label,
                                                i = if(n == 1) NULL else i)

               loon::l_layer_line(widget,
                                  x = x,
                                  y = y,
                                  linewidth = linesWidth[i],
                                  color = linesColor[i],
                                  dash = linesDash[[i]],
                                  parent = parent,
                                  label = mappingLabel)
             })
    }
  } else NULL
}



loonLayer.GeomHline <- function(widget,
                                layerGeom,
                                data,
                                ggplotPanel_params,
                                ggObj,
                                special,
                                parent = "root",
                                label = NULL,
                                ...){
  if(dim(data)[1] != 0) {
    isCoordPolar <- is.CoordPolar(ggplotPanel_params)
    n <- dim(data)[1]
    linesWidth <- as_loon_size(data$size, "lines")
    linesColor <- hex6to12(data$colour)
    xrange <- ggplotPanel_params$x.range
    linesDash <- as_loon_dash(data$linetype)

    coordinates <- ggObj$coordinates
    if(n == 1) {
      if(isCoordPolar){
        coordPolarxy <- Cartesianxy2Polarxy(layerGeom, coordinates, data, ggplotPanel_params)
        x <- coordPolarxy$x
        y <- coordPolarxy$y
      } else {
        y <- rep(data$yintercept, 2)
        x <- xrange
      }
      loon::l_layer_line(widget,
                         x = x, y = y,
                         linewidth = linesWidth,
                         color = linesColor,
                         dash = linesDash[[1]],
                         parent = parent,
                         label = label %||% "hline")
    } else {

      if(parent == "root") {
        parent <- loon::l_layer_group(widget,
                                      label = label %||% "hlines")
      }
      lapply(1:n,
             function(i){
               if(isCoordPolar){
                 coordPolarxy <- Cartesianxy2Polarxy(layerGeom, coordinates, data[i, ], ggplotPanel_params)
                 x <- coordPolarxy$x
                 y <- coordPolarxy$y
               } else {
                 x <- xrange
                 y <- rep(data[i,]$yintercept, 2)
               }

               mappingLabel <- get_mappingLabel(layerGeom,
                                                name = "hline",
                                                label = label,
                                                i = if(n == 1) NULL else i)

               loon::l_layer_line(widget,
                                  x = x,
                                  y = y,
                                  linewidth = linesWidth[i],
                                  color = linesColor[i],
                                  dash = linesDash[[i]],
                                  parent = parent,
                                  label = mappingLabel)
             })
    }
  } else NULL

}

loonLayer.GeomAbline <- function(widget,
                                 layerGeom,
                                 data,
                                 ggplotPanel_params,
                                 ggObj,
                                 special,
                                 parent = "root",
                                 label = NULL,
                                 ...){
  if(dim(data)[1] != 0) {
    isCoordPolar <- is.CoordPolar(ggplotPanel_params)
    n <- dim(data)[1]
    linesWidth <- as_loon_size(data$size, "lines")
    linesColor <- hex6to12(data$colour)
    xrange <- ggplotPanel_params$x.range
    yrange <- ggplotPanel_params$y.range
    linesDash <- as_loon_dash(data$linetype)

    coordinates <- ggObj$coordinates
    if(n == 1) {
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
      loon::l_layer_line(widget, x = x, y = y,
                         linewidth = linesWidth,
                         color = linesColor,
                         dash = linesDash[[1]],
                         parent = parent,
                         label = label %||% "abline")
    } else {

      if(parent == "root") {
        parent <-  loon::l_layer_group(widget,
                                       label = label %||% "ablines")
      }

      lapply(1:n,
             function(i){
               if(isCoordPolar) {
                 coordPolarxy <- Cartesianxy2Polarxy(layerGeom, coordinates, data[i,],
                                                     ggplotPanel_params)
                 x <- coordPolarxy$x
                 y <- coordPolarxy$y
               } else {
                 coordCartesianxy <- abline2xy(xrange, yrange, data[i, ]$slope, data[i, ]$intercept)
                 x <- coordCartesianxy$x
                 y <- coordCartesianxy$y
               }

               mappingLabel <- get_mappingLabel(layerGeom,
                                                name = "abline",
                                                label = label,
                                                i = if(n == 1) NULL else i)

               loon::l_layer_line(widget,
                                  x = x, y = y,
                                  linewidth = linesWidth[i],
                                  color = linesColor[i],
                                  dash = linesDash[[i]],
                                  parent = parent,
                                  label =mappingLabel)
             })
    }
  } else NULL
}



# given start and end to draw a straight line
loonLayer.GeomSegment <- function(widget,
                                  layerGeom,
                                  data,
                                  ggplotPanel_params,
                                  ggObj,
                                  special,
                                  parent = "root",
                                  label = NULL,
                                  ...){
  if(dim(data)[1] != 0) {
    isCoordPolar <- is.CoordPolar(ggplotPanel_params)
    n <- dim(data)[1]
    linesWidth <- as_loon_size(data$size, "lines")
    linesColor <- hex6to12(data$colour)
    linesDash <- as_loon_dash(data$linetype)

    coordinates <- ggObj$coordinates
    if(n == 1) {
      if(isCoordPolar) {
        coordPolarxy <- Cartesianxy2Polarxy.GeomSegment(NULL, coordinates, data, ggplotPanel_params)
        x <- coordPolarxy$x
        y <- coordPolarxy$y
      } else {
        x <- c(data$x, data$xend)
        y <- c(data$y, data$yend)
      }

      loon::l_layer_line(widget, x = x, y = y,
                         linewidth = linesWidth,
                         color = linesColor,
                         dash = linesDash[[1]],
                         parent = parent,
                         label = label %||% "segment")
    } else {

      if(parent == "root") {
        parent <-  loon::l_layer_group(widget,
                                       label = label %||% "segments")
      }

      lapply(1:n,
             function(i){
               if(isCoordPolar){
                 coordPolarxy <- Cartesianxy2Polarxy.GeomSegment(NULL, coordinates, data[i, ], ggplotPanel_params)
                 x <- coordPolarxy$x
                 y <- coordPolarxy$y
               } else {
                 x <- c(data[i,]$x, data[i,]$xend)
                 y <- c(data[i,]$y, data[i,]$yend)
               }

               mappingLabel <- get_mappingLabel(layerGeom,
                                                name = "segment",
                                                label = label,
                                                i = if(n == 1) NULL else i)

               loon::l_layer_line(widget,
                                  x = x, y = y,
                                  linewidth = linesWidth[i],
                                  color = linesColor[i],
                                  dash = linesDash[[i]],
                                  parent = parent,
                                  label = mappingLabel)
             })
    }
  } else NULL
}

loonLayer.GeomHex <- function(widget,
                              layerGeom,
                              data,
                              ggplotPanel_params,
                              ggObj,
                              special,
                              parent = "root",
                              label = NULL,
                              ...){
  if(dim(data)[1] != 0) {
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

      method <- get_stat_param(layerGeom, "bins", ...)

      mappingLabel <- get_mappingLabel(layerGeom,
                                       name = paste(c("hex", method)),
                                       label = label,
                                       i = NULL)

      loon::l_layer_polygons(widget,
                             hex_x, hex_y,
                             color = fillColor,
                             linecolor = linesColor,
                             linewidth = linesWidth,
                             parent = parent,
                             label = mappingLabel)
    }
  } else NULL
}



loonLayer.GeomDotplot <- function(widget,
                                  layerGeom,
                                  data,
                                  ggplotPanel_params,
                                  ggObj,
                                  special,
                                  parent = "root",
                                  label = NULL,
                                  ...) {
  if(dim(data)[1] != 0) {
    # uniGroup <- unique(data$group)
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

    radius <- data$binwidth[1]/2

    if(parent == "root") {
      parent <- loon::l_layer_group(widget,
                                    label = label %||% "dots")
    }

    if(layerGeom$geom_params$binaxis == "y"){
      lapply(1:n,
             function(i){
               xradius <- diff(ggplotPanel_params$x.range)/diff(ggplotPanel_params$y.range)*radius

               mappingLabel <- get_mappingLabel(layerGeom,
                                                name = "dot",
                                                label = label,
                                                i = if(n == 1) NULL else i)

               loon::l_layer_oval(widget,
                                  x = c(data$x[i] + stackPos[i] * 2 * xradius - xradius,
                                        data$x[i] + stackPos[i] * 2 * xradius + xradius),
                                  y = c(data$y[i] - radius, data$y[i] + radius),
                                  color =  fillColor[i],
                                  linecolor = lineColor[i],
                                  parent = parent,
                                  label = mappingLabel)
             })
    } else  {
      yradius <- diff(ggplotPanel_params$y.range)/diff(ggplotPanel_params$x.range)*radius
      lapply(1:n,
             function(i){

               mappingLabel <- get_mappingLabel(layerGeom,
                                                name = "dot",
                                                label = label,
                                                i = if(n == 1) NULL else i)

               loon::l_layer_oval(widget,
                                  x = c(data$x[i] - radius, data$x[i] + radius),
                                  y = c(data$y[i] - yradius + stackPos[i] * 2 * yradius,
                                        data$y[i] + yradius + stackPos[i] * 2 * yradius),
                                  color =  fillColor[i],
                                  linecolor = lineColor[i],
                                  parent = parent,
                                  label = mappingLabel)
             })
    }
  } else NULL
}
