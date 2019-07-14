geom_textGlyph <- function(mapping = NULL, data = NULL, stat = 'identity',
                           text,
                           position = 'identity', na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE, ...) {

  if(missing(text)) stop('no texts exist')

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTextGlyph,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      text = text,
      ...
    )
  )
}


GeomTextGlyph <- ggplot2::ggproto('GeomTextGlyph', Geom,
                                  required_aes = c('x', 'y'),
                                  default_aes = ggplot2::aes(colour = "black", size = 4,
                                                             angle = 0, hjust = 0.5, stroke = 0.5, shape = 19, fill = NA,
                                                             vjust = 0.5, alpha = NA, family = "",
                                                             fontface = 1, lineheight = 1.2),
                                  draw_key = ggplot2::draw_key_point,
                                  draw_panel = function(data, panel_params, coord, text, na.rm) {

                                    data <- coord$transform(data, panel_params)
                                    if (is.character(data$vjust)) {
                                      data$vjust <- ggplot2:::compute_just(data$vjust, data$y)
                                    }
                                    if (is.character(data$hjust)) {
                                      data$hjust <- ggplot2:::compute_just(data$hjust, data$x)
                                    }

                                    ggplot2:::ggname(
                                      "geom_textGlyph",
                                      grid::textGrob(
                                        text,
                                        data$x,
                                        data$y,
                                        default.units = "native",
                                        hjust = data$hjust, vjust = data$vjust,
                                        rot = data$angle,
                                        gp = grid::gpar(
                                          col = alpha(data$colour, data$alpha),
                                          fontsize = data$size * .pt,
                                          fontfamily = data$family,
                                          fontface = data$fontface,
                                          lineheight = data$lineheight
                                        )
                                      )
                                    )
                                  }
)


geom_pointrangeGlyph <- function(mapping = NULL, data = NULL, stat = 'identity',
                                 ymin, ymax, linewidth = 1, showArea = TRUE,
                                 position = 'identity', na.rm = FALSE, show.legend = NA,
                                 inherit.aes = TRUE, ...) {
  if(missing(ymin)) stop('no ymin exist')
  if(missing(ymax)) stop('no ymax exist')
  if(!is.numeric(linewidth)) stop('numerical linewidth is required')

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPointrangeGlyph,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      ymin = ymin,
      ymax = ymax,
      linewidth = linewidth,
      showArea = showArea,
      na.rm = na.rm,
      ...
    )
  )
}

GeomPointrangeGlyph <- ggproto('GeomPointrangeGlyph', Geom,
                               required_aes = c('x', 'y'),
                               default_aes = aes(colour = "black", linetype = 1, size = 4,
                                                 fill = NA, alpha = NA, stroke = 1),
                               draw_key = ggplot2::draw_key_point,
                               draw_panel = function(data, panel_params, coord, ymin, ymax, linewidth, showArea, na.rm) {

                                 data$ymin <- ymin
                                 data$ymax <- ymax

                                 if(showArea) data$shape <- 1 else data$shape <- 19

                                 grob <- if(is.null(data$y)) {

                                   ggplot2::GeomLinerange$draw_panel(transform(data, size = as_r_line_size(linewidth)),
                                                                     panel_params, coord)
                                 } else {

                                   grid::gTree(
                                     children = grid::gList(
                                       ggplot2::GeomLinerange$draw_panel(transform(data, size = as_r_line_size(linewidth)),
                                                                         panel_params, coord),
                                       ggplot2::GeomPoint$draw_panel(transform(data, size = as_r_point_size(size)),
                                                                     panel_params, coord)
                                     )
                                   )
                                 }

                                 ggplot2:::ggname("geom_pointrangeGlyph", grob)
                               }
)

geom_polygonGlyph <- function(mapping = NULL, data = NULL, stat = 'identity',
                              polygon_x, polygon_y, showArea = TRUE, linewidth = 1,
                              position = 'identity', na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE, ...) {
  if(missing(polygon_x)) stop('no polygon_x exist')
  if(missing(polygon_y)) stop('no polygon_y exist')

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPolygonGlyph,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      polygon_x = polygon_x,
      polygon_y = polygon_y,
      showArea = showArea,
      linewidth = linewidth,
      na.rm = na.rm,
      ...
    )
  )
}

GeomPolygonGlyph <- ggproto('GeomPolygonGlyph', Geom,
                            required_aes = c('x', 'y'),
                            default_aes = aes(colour = 'black',
                                              fill = 'black', size = 4,
                                              linetype = 1, alpha = 1,
                                              shape = 19, stroke = 0.5),
                            draw_key = ggplot2::draw_key_point,
                            draw_panel = function(data, panel_params, coord,
                                                  polygon_x, polygon_y, showArea, linewidth, na.rm) {


                              data <- coord$transform(data, panel_params)
                              poly_x <- lapply(1:length(polygon_x),
                                               function(i) {
                                                 if(showArea) {
                                                   grid::unit(data$x[i], 'native') +
                                                     grid::unit(polygon_x[[i]] * loon:::as_r_polygonGlyph_size(data$size[i]), "mm")
                                                 } else {
                                                   grid::unit(data$x[i], 'native') +
                                                     grid::unit(c(polygon_x[[i]], polygon_x[[i]][1]) * loon:::as_r_polygonGlyph_size(data$size[i]), "mm")
                                                 }

                                               })
                              poly_y <- lapply(1:length(polygon_y),
                                               function(i) {
                                                 if(showArea) {

                                                   grid::unit(data$y[i], 'native') +
                                                     grid::unit(-polygon_y[[i]] * loon:::as_r_polygonGlyph_size(data$size[i]), "mm")

                                                 } else {

                                                   grid::unit(data$y[i], 'native') +
                                                     grid::unit(c(polygon_y[[i]], polygon_y[[i]][1]) * loon:::as_r_polygonGlyph_size(data$size[i]), "mm")
                                                 }
                                               })

                              grob <- if(showArea){
                                grid::polygonGrob(
                                  x = do.call(grid::unit.c, poly_x),
                                  y = do.call(grid::unit.c, poly_y),
                                  id = rep(seq(length(poly_x)), lengths(poly_x)),
                                  gp = grid::gpar(
                                    fill = data$fill,
                                    col =  data$colour,
                                    lwd = as_r_line_size(linewidth)
                                  )
                                )
                              } else {
                                grid::polylineGrob(
                                  x = do.call(grid::unit.c, poly_x),
                                  y = do.call(grid::unit.c, poly_y),
                                  id = rep(seq(length(poly_x)), lengths(poly_x)),
                                  gp = gpar(
                                    fill = data$fill,
                                    col =  data$colour,
                                    lwd = as_r_line_size(linewidth)
                                  )
                                )
                              }

                              ggplot2:::ggname("geom_polygonGlyph", grob)
                            }
)


geom_imageGlyph <- function(mapping = NULL, data = NULL, stat = 'identity',
                            images, width = 4, height = 3,
                            position = 'identity', na.rm = FALSE, show.legend = NA,
                            inherit.aes = TRUE, ...) {

  if(missing(images)) stop('no images exist')

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomImageGlyph,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      images = images,
      width = width,
      height = height,
      na.rm = na.rm,
      ...
    )
  )
}

GeomImageGlyph <- ggproto('GeomImageGlyph', Geom,
                          required_aes = c('x', 'y'),
                          default_aes = aes(colour = NA,
                                            fill = 'black', size = 4,
                                            linetype = 1, alpha = 1,
                                            shape = 19, stroke = 0.5),
                          draw_key = ggplot2::draw_key_point,
                          setup_data = function(data, params) {

                            browser()
                            lapply(names(params),
                                   function(name) {

                                     switch(name,
                                            "serialAxesData" = {data <<- cbind(data, params[[name]])},
                                            "sequence" = {data$sequence <<- if(is.null(params[[name]])) NULL else paste(params[[name]], collapse = "\\")},
                                            {
                                              data[, name] <<- params[[name]]
                                            }
                                     )



                                   })
                            data
                          },
                          draw_panel = function(data, panel_params, coord, images, width, height, na.rm) {

                            data <- coord$transform(data, panel_params)
                            width_p <- grid::unit(as_r_image_size(width) * data$size, "cm")
                            height_p <- grid::unit(as_r_image_size(height) * data$size, "cm")

                            ggplot2:::ggname("geom_imageGlyph",
                                             grid::gTree(
                                               children = do.call(grid::gList,
                                                                  lapply(1:length(images),
                                                                         function(i) {
                                                                           grid::gList(
                                                                             grid::rectGrob(x = grid::unit(data$x[i], "native"),
                                                                                            y = grid::unit(data$y[i], "native"),
                                                                                            just = "centre",
                                                                                            width = width_p[i] + unit(2, "mm"),
                                                                                            height = height_p[i] + unit(2, "mm"),
                                                                                            gp = grid::gpar(
                                                                                              fill = data$fill[i],
                                                                                              col = data$colour[i]
                                                                                            )
                                                                             ),
                                                                             grid::rasterGrob(image = if(is.list(images)) images[[i]] else images,
                                                                                              x = grid::unit(data$x[i], "native"),
                                                                                              y = grid::unit(data$y[i], "native"),
                                                                                              just = "centre",
                                                                                              width = width_p[i],
                                                                                              height = height_p[i])
                                                                           )
                                                                         })
                                               )
                                             )
                            )
                          }
)

geom_serialAxesGlyph <- function(mapping = NULL, data = NULL, stat = 'identity',
                                 serialAxesData, sequence = NULL, linewidth = 1,
                                 scaling = c('variable', 'data', 'observation', 'none'),
                                 axesLayout = c("parallel", "radial"),
                                 showAxes = FALSE, showArea = FALSE,  showEnclosing = FALSE,
                                 axesColor = "black", bboxColor = 'black',
                                 position = 'identity', na.rm = FALSE, show.legend = NA,
                                 inherit.aes = TRUE, ...) {

  if(missing(serialAxesData)) stop('no serial axes data exists')
  scaling <- match.arg(scaling)
  axesLayout <- match.arg(axesLayout)

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSerialAxesGlyph,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      serialAxesData = serialAxesData,
      sequence = sequence,
      scaling = scaling,
      axesLayout = axesLayout,
      showAxes = showAxes,
      showArea = showArea,
      showEnclosing = showEnclosing,
      axesColor = axesColor,
      bboxColor = bboxColor,
      linewidth = linewidth,
      na.rm = na.rm,
      ...
    )
  )
}

GeomSerialAxesGlyph <- ggproto('GeomSerialAxesGlyph', Geom,
                               required_aes = c('x', 'y'),
                               default_aes = aes(colour = 'black',
                                                 size = 4, shape = 19, fill = NA, stroke = 0.5,
                                                 linetype = 1, alpha = 1),
                               draw_key = ggplot2::draw_key_point,
                               setup_data = function(data, params) {

                                 lapply(names(params),
                                        function(name) {

                                          switch(name,
                                                 "serialAxesData" = {data <<- cbind(data, params[[name]])},
                                                 "sequence" = {data$sequence <<- if(is.null(params[[name]])) NULL else paste(params[[name]], collapse = "\\")},
                                                 {
                                                   data[, name] <<- params[[name]]
                                                 }
                                          )



                                        })
                                 data
                               },
                               draw_panel = function(data, panel_params, coord,
                                                     serialAxesData, sequence, scaling, axesLayout, showAxes, showArea,
                                                     showEnclosing,  axesColor, bboxColor, linewidth, na.rm) {

                                 data <- coord$transform(data, panel_params)


                                 # each element color
                                 color <- data$colour
                                 fill <- data$fill
                                 # size
                                 size <- data$size
                                 # parallel or radial
                                 scaledData <- get_scaledData(serialAxesData, sequence, scaling)
                                 dimension <- dim(scaledData)[2]

                                 # position
                                 xpos <- data$x
                                 ypos <- data$y

                                 if(axesLayout == "parallel") {

                                   scaleX <- loon:::as_r_serialaxesGlyph_size(data$size, "x", "parallel")
                                   scaleY <- loon:::as_r_serialaxesGlyph_size(data$size, "y", "parallel")

                                   xaxis <- t(sapply(scaleX, function(x) seq(-0.5 * x, 0.5 * x, length.out = dimension)))
                                   yaxis <- (scaledData - 0.5) * scaleY

                                   gridAesthetic <- get_gridAesthetic(axesLayout = 'parallel',
                                                                      xpos = xpos, ypos = ypos,
                                                                      scaleX = scaleX, scaleY = scaleY,
                                                                      xaxis = xaxis, yaxis = yaxis,
                                                                      dimension = dimension, showArea = showArea)
                                 } else if(axesLayout == 'radial') {
                                   scaleX <- loon:::as_r_serialaxesGlyph_size(data$size, "x", "radial")
                                   scaleY <- loon:::as_r_serialaxesGlyph_size(data$size, "y", "radial")

                                   angle <- seq(0, 2*pi, length.out = dimension + 1)[1:dimension]

                                   radialxaxis <- t(sapply(1:length(scaleX), function(i) scaleX[i] * scaledData[i, ] * cos(angle)))
                                   radialyaxis <- t(sapply(1:length(scaleY), function(i) scaleY[i] * scaledData[i, ] * sin(angle)))

                                   gridAesthetic <- get_gridAesthetic(axesLayout = 'radial',
                                                                      xpos = xpos, ypos = ypos,
                                                                      scaleX = scaleX, scaleY = scaleY,
                                                                      xaxis = radialxaxis, yaxis = radialyaxis,
                                                                      dimension = dimension, showArea = showArea)
                                 } else stop("unknown axes layout")

                                 ggplot2:::ggname("geom_serialAxesGlyph",
                                                  grid::gTree(
                                                    children = grid::gList(
                                                      if(showEnclosing) {
                                                        grid::polylineGrob(
                                                          x = gridAesthetic$enclosingX,
                                                          y = gridAesthetic$enclosingY,
                                                          id = gridAesthetic$enclosingId,
                                                          gp = grid::gpar(col = bboxColor)
                                                        )
                                                      },
                                                      if(showAxes) {
                                                        grid::polylineGrob(
                                                          x = gridAesthetic$axesX,
                                                          y = gridAesthetic$axesY,
                                                          id = gridAesthetic$axesId,
                                                          gp = grid::gpar(col = axesColor)
                                                        )
                                                      },
                                                      if(showArea) {
                                                        grid::polygonGrob(
                                                          x = gridAesthetic$serialCoordX,
                                                          y = gridAesthetic$serialCoordY,
                                                          id = gridAesthetic$serialCoordId,
                                                          gp = grid::gpar(fill = data$colour,
                                                                          col = NA))
                                                      } else {
                                                        grid::polylineGrob(
                                                          x = gridAesthetic$serialCoordX,
                                                          y = gridAesthetic$serialCoordY,
                                                          id = gridAesthetic$serialCoordId,
                                                          gp = grid::gpar(col = data$colour,
                                                                          lwd = linewidth)
                                                        )
                                                      }
                                                    )
                                                  )
                                 )
                               }
)

as_r_image_size <- function(x) x/10

get_scaledData <- function(serialAxesData, sequence, scaling) {

  dat <- sapply(serialAxesData,
                function(x) {
                  if(is.numeric(x)) x
                  else if(is.character(x)) {
                    warning("character column exists and will be converted to numeric",
                            call. = FALSE)
                    as.numeric(as.factor(x))
                  } else if (is.factor(x)) {
                    warning("factor column exists and will be converted to numeric",
                            call. = FALSE)
                    as.numeric(x)
                  } else if(is.logical(x)) {
                    warning("logical column exists and will be converted to numeric",
                            call. = FALSE)
                    as.numeric(x)
                  } else stop("unknown data structure")
                })
  if(!is.null(sequence)) {
    if(!all(sequence %in% colnames(dat))) stop("unknown variable names in sequence")
    dat <-  dat[, sequence]
  }

  switch(scaling,
         "variable" = {
           minV <- apply(dat, 2, "min")
           maxV <- apply(dat, 2, "max")
           t(
             (t(dat) - minV) / (maxV  - minV)
           )
         },
         "observation" = {
           minO <- apply(dat, 1, "min")
           maxO <- apply(dat, 1, "max")
           (dat - minO) / (maxO - minO)
         },
         "data" = {
           minD <- min(dat)
           maxD <- max(dat)
           (dat - minD)/ (maxD - minD)
         },
         "none" = NULL)

}

get_gridAesthetic <- function(axesLayout, xpos, ypos, scaleX, scaleY, xaxis, yaxis,
                              dimension, showEnclosing, showAxes, showArea) {

  enclosingX <- enclosingY <- enclosingId <- list()
  axesX <- axesY <- axesId <- list()
  serialCoordX <- serialCoordY <- list()

  N <- length(xpos)
  hide <- if(axesLayout == "parallel") {
    lapply(1:N,
           function(i){
             # enclosing
             enclosingX[[i]] <<- grid::unit(xpos[i], 'native') + grid::unit((c(0, 0, 1, 0, 0, 1, 1, 1) - 0.5) * scaleX[i], "mm")
             enclosingY[[i]] <<- grid::unit(ypos[i], 'native') + grid::unit((c(0, 0, 0, 1, 1, 0, 1, 1) - 0.5) * scaleY[i], "mm")
             enclosingId[[i]] <<- rep(((i - 1)*dimension + 1):(dimension * i), 2)
             # axes
             axesX[[i]] <<- grid::unit(xpos[i], 'native') + rep(grid::unit(xaxis[i, ], "mm"), each = 2)
             axesY[[i]] <<- grid::unit(ypos[i], 'native') + rep(grid::unit(c(- 0.5 * scaleY[i], 0.5 * scaleY[i]), "mm"), dimension)
             axesId[[i]] <<- rep(((i - 1)*dimension + 1):(dimension * i), each = 2)
             # serialCoord
             if(showArea) {
               serialCoordX[[i]] <<- grid::unit(xpos[i], 'native') + grid::unit(c(xaxis[i, ], rev(xaxis[i, ])), "mm")
               serialCoordY[[i]] <<- grid::unit(ypos[i], 'native') + grid::unit(c(yaxis[i, ], rep(-0.5 * scaleY[i], dimension)), "mm")
             } else {
               serialCoordX[[i]] <<- grid::unit(xpos[i], 'native') + grid::unit(xaxis[i, ], "mm")
               serialCoordY[[i]] <<- grid::unit(ypos[i], 'native') + grid::unit(yaxis[i, ], "mm")
             }
           })

    serialCoordId <- if(showArea) rep(1:N, each = 2*dimension) else rep(1:N, each = dimension)

  } else if (axesLayout == "radial") {

    len_radial <- 101
    angle <- seq(0, 2*pi, length.out = dimension + 1)[1:dimension]

    lapply(1:N,
           function(i){
             # enclosing
             enclosingX[[i]] <<- grid::unit(xpos[i], 'native') + grid::unit(scaleX[i] * cos(seq(0, 2*pi, length=len_radial)), "mm")
             enclosingY[[i]] <<- grid::unit(ypos[i], 'native') + grid::unit(scaleY[i] * sin(seq(0, 2*pi, length=len_radial)), "mm")
             # axes
             axesX[[i]] <<- grid::unit(xpos[i], 'native') + grid::unit(c(rep(0, dimension), scaleX[i] * cos(angle)), "mm")
             axesY[[i]] <<- grid::unit(ypos[i], 'native') + grid::unit(c(rep(0, dimension), scaleY[i] * sin(angle)), "mm")
             axesId[[i]] <<- rep(((i - 1)*dimension + 1):(dimension * i), 2)
             # serialCoord
             serialCoordX[[i]] <<- grid::unit(xpos[i], 'native') + grid::unit(c(xaxis[i, ], rev(xaxis[i, 1])), "mm")
             serialCoordY[[i]] <<- grid::unit(ypos[i], 'native') + grid::unit(c(yaxis[i, ], rev(yaxis[i, 1])), "mm")
           })

    enclosingId <- rep(1:N, each = len_radial)
    serialCoordId <- rep(1:N, each = (dimension + 1))

  } else stop('unknown axes layout')

  list(
    enclosingX = do.call(grid::unit.c, enclosingX),
    enclosingY = do.call(grid::unit.c, enclosingY),
    enclosingId = unlist(enclosingId),
    axesX = do.call(grid::unit.c, axesX),
    axesY = do.call(grid::unit.c, axesY),
    axesId = unlist(axesId),
    serialCoordX = do.call(grid::unit.c, serialCoordX),
    serialCoordY = do.call(grid::unit.c, serialCoordY),
    serialCoordId = serialCoordId
  )
}
