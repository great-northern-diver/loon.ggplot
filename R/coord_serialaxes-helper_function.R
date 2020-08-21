###################################### set grouped data ######################################

groupedData_setup <- function(layer, groupedData, axesLayout, ...) {
  UseMethod("groupedData_setup", layer$geom)
}

groupedData_setup.GeomDensity <- function(layer, groupedData, axesLayout, name, x, y, len.xaxis, i, right) {

  if(!"fill" %in% name) return(groupedData)

  xmin <- trans_x_coord(x, y = 0,
                        len.xaxis, i,
                        axesLayout = axesLayout,
                        right = right)

  switch(axesLayout,
         "parallel" = {

           groupedData$xmin <- xmin
         },
         "radial" = {
           xx <- groupedData$x
           yy <- groupedData$y

           ymin <- trans_y_coord(x, y = 0,
                                 len.xaxis, i,
                                 axesLayout = axesLayout,
                                 right = right)

           groupedData <- rbind(groupedData, groupedData)
           groupedData$x <- c(xx, rev(xmin))
           groupedData$y <- c(yy, rev(ymin))
         })

  groupedData
}

groupedData_setup.GeomFreqpoly <- function(layer, groupedData, axesLayout, name, x, y, len.xaxis, i, right) {
  groupedData_setup.GeomDensity(layer, groupedData, axesLayout, name, x, y, len.xaxis, i, right)
}

groupedData_setup.GeomBar <- function(layer, groupedData, axesLayout,
                                      xmax, xmin, ymax, ymin,
                                      group, len.xaxis, i, right) {

  switch(axesLayout,
         "parallel" = {

           delta <- 1/(len.xaxis - 1)
           location <- (i - 1) * delta

           if(nrow(groupedData) == 0) {
             groupedData <- data.frame(
               xmax = ymax * sign(right - 0.5) + location,
               xmin = ymin * sign(right - 0.5) + location,
               ymax = xmax,
               ymin = xmin,
               group = group
             )
           } else {
             groupedData$xmax <- ymax * sign(right - 0.5) + location
             groupedData$xmin <- ymin * sign(right - 0.5) + location
             groupedData$ymax <- xmax
             groupedData$ymin <- xmin
             groupedData$group <- group
           }
         },
         "radial" = {
           angle <- seq(0, 2 * base::pi, length.out = len.xaxis + 1)[1:len.xaxis][i]
           rotateM <- matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)), nrow = 2, byrow = TRUE)

           # xmin xmax ymin ymax
           # each row is repeated to four
           groupedData <- rbind(
             groupedData,
             groupedData,
             groupedData,
             groupedData
           )

           rotatedCoords <- as.data.frame(
             t(
               rotateM %*% matrix(c(xmin, xmax, xmax, xmin,
                                    -ymin * sign(right - 0.5), -ymin * sign(right - 0.5),
                                    -ymax * sign(right - 0.5), -ymax * sign(right - 0.5)),
                                  nrow = 2,
                                  byrow = TRUE)
             )
           )

           x <- rotatedCoords[[1]] * loon_default_setting("radius") + 0.5
           y <- rotatedCoords[[2]] * loon_default_setting("radius") + 0.5
           group <- rep(seq(length(xmin)), 4)

           if(nrow(groupedData) == 0) {
             groupedData <- data.frame(
               x = x,
               y = y,
               group = group
             )
           } else {
             groupedData$x <- x
             groupedData$y <- y
             groupedData$group <- group
           }
         })

  groupedData
}

###################################### plot_fun ######################################

plot_fun <- function(layer, axesLayout, ...) {
  UseMethod("plot_fun", layer$geom)
}

plot_fun.GeomDensity <- function(layer, axesLayout, name) {

  if("fill" %in% name) {
    switch(
      axesLayout,
      "parallel" = ggplot2::geom_ribbon,
      "radial" = ggplot2::geom_polygon
    )
  } else {
    ggplot2::geom_path
  }
}

plot_fun.GeomFreqpoly <- function(layer, axesLayout, name) {
  plot_fun.GeomDensity(layer, axesLayout, name)
}

plot_fun.GeomBar <- function(layer, axesLayout) {
  switch(
    axesLayout,
    "parallel" = ggplot2::geom_rect,
    "radial" = ggplot2::geom_polygon
  )
}

###################################### layered mapping aes ######################################

layeredMapping_setup <- function(layer, axesLayout, mapping, ...) {
  UseMethod("layeredMapping_setup", layer$geom)
}

layeredMapping_setup.GeomDensity <- function(layer, axesLayout, mapping, name) {

  # useless; just to pass the CMD check
  # or consider using the `globalVariables`
  x <- numeric(0)
  xmin <- numeric(0)
  y <- numeric(0)
  group <- numeric(0)

  if("fill" %in% name) {
    switch(axesLayout,
           "parallel" = mbind(
             mapping,
             ggplot2::aes(xmax = x,
                          xmin = xmin,
                          y = y,
                          group = group)
           ),
           "radial" = mbind(
             mapping,
             ggplot2::aes(x = x,
                          y = y,
                          group = group)
           )
    )
  } else {
    mbind(
      mapping,
      ggplot2::aes(x = x,
                   y = y,
                   group = group)
    )
  }
}

layeredMapping_setup.GeomFreqpoly <- function(layer, axesLayout, mapping, name) {
  layeredMapping_setup.GeomDensity(layer, axesLayout, mapping, name)
}

layeredMapping_setup.GeomBar <- function(layer, axesLayout, mapping) {

  # useless; just to pass the CMD check
  # or consider using the `globalVariables`
  xmax <- numeric(0)
  xmin <- numeric(0)
  ymax <- numeric(0)
  ymin <- numeric(0)
  x <- numeric(0)
  y <- numeric(0)
  group <- numeric(0)

  switch(axesLayout,
         "parallel" = mbind(
           mapping,
           ggplot2::aes(xmax = xmax,
                        xmin = xmin,
                        ymax = ymax,
                        ymin = ymin,
                        group = group)
         ),
         "radial" = mbind(
           mapping,
           ggplot2::aes(x = x,
                        y = y,
                        group = group)
         )
  )
}
