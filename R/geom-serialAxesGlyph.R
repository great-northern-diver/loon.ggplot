#' @export
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

GeomSerialAxesGlyph <- ggplot2::ggproto('GeomSerialAxesGlyph', Geom,
                                        required_aes = c('x', 'y'),
                                        default_aes = aes(colour = 'black',
                                                          size = 4, shape = 19, fill = NA, stroke = 0.5,
                                                          linetype = 1, alpha = 1),
                                        draw_key = ggplot2::draw_key_point,
                                        setup_data = function(data, params) {

                                          n <- dim(data)[1]

                                          stopifnot(exprs = {
                                            n == dim(params$serialAxesData)[1]
                                          })

                                          tryCatch(
                                            {
                                              lapply(names(params),
                                                     function(name) {

                                                       switch(name,
                                                              "serialAxesData" = {
                                                                serialAxesData <- params[[name]]
                                                                colnames(serialAxesData) <- paste0("V", 1: dim(serialAxesData)[2])
                                                                data <<- cbind(data, serialAxesData)
                                                              },
                                                              "sequence" = {data$sequence <<- if(is.null(params[[name]])) NULL else paste(params[[name]], collapse = "\\")},
                                                              {
                                                                data[, name] <<- params[[name]]
                                                              }
                                                       )



                                                     })
                                            },
                                            error = function(e) warning("welcome to report issues in https://github.com/great-northern-diver/loon.ggplot")
                                          )

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
                                          scaledData <- loon:::get_scaledData(data = serialAxesData,
                                                                              sequence = sequence,
                                                                              scaling = scaling,
                                                                              displayOrder = 1:length(color))

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

                                            angle <- seq(0, 2*base::pi, length.out = dimension + 1)[1:dimension]

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
                                                                                   col = NA,
                                                                                   alpha = data$alpha))
                                                               } else {
                                                                 grid::polylineGrob(
                                                                   x = gridAesthetic$serialCoordX,
                                                                   y = gridAesthetic$serialCoordY,
                                                                   id = gridAesthetic$serialCoordId,
                                                                   gp = grid::gpar(col = data$colour,
                                                                                   lwd = linewidth,
                                                                                   alpha = data$alpha)
                                                                 )
                                                               }
                                                             )
                                                           )
                                          )
                                        }
)

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
             enclosingId[[i]] <<- rep(((i - 1)*4 + 1):(4 * i), 2)
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
    angle <- seq(0, 2*base::pi, length.out = dimension + 1)[1:dimension]

    lapply(1:N,
           function(i){
             # enclosing
             enclosingX[[i]] <<- grid::unit(xpos[i], 'native') + grid::unit(scaleX[i] * cos(seq(0, 2*base::pi, length=len_radial)), "mm")
             enclosingY[[i]] <<- grid::unit(ypos[i], 'native') + grid::unit(scaleY[i] * sin(seq(0, 2*base::pi, length=len_radial)), "mm")
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
