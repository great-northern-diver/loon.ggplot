#' @title Layers on serial axes coordinate
#' @description Project the regular \code{geom} layers onto the serial axes coordinate.
#' @param layer a layer object
#' @param plot a \code{ggplot} object
#' @param object some parameters used to modify this serial axes \code{ggplot} object (i.e. \code{axesLayout}, \code{axesLabels}, ...)
#' @export
add_serialaxes_layers <- function(layer, plot, object) {

  data <- {if(is.waive(layer$data)) NULL else layer$data} %||% plot$data

  if(is.null(data)) {
    warning("Data is not found, neither in the function `ggplot()` nor in the layer.",
            call. = FALSE)
    return(plot)
  }

  UseMethod("add_serialaxes_layers", layer$geom)
}

#' @export
add_serialaxes_layers.default <- function(layer, plot, object) {
  warning("The layer ", class(layer$geom)[1], " is not implemented in serialaxes coordinate yet. ",
          "It will be omitted.",
          call. = FALSE)
  return(plot)
}

#' @export
add_serialaxes_layers.GeomPath <- function(layer, plot, object) {

  data <- {if(is.waive(layer$data)) NULL else layer$data} %||% plot$data

  layer_mapping <- layer$mapping

  mapping <- if(layer$inherit.aes) {
    mbind(plot$mapping, layer_mapping)
  } else {
    layer_mapping
  }

  if("fill" %in% names(mapping)) {
    warning("The 'fill' aesthetics will be ignored. Maybe you want to set 'colour' in the `aes()` or use the `geom_ribbon()` instead?",
            call. = FALSE)
  }

  switch(
    object$axesLayout,

    "parallel" = {

      ggParallelSerialAxes(plot,
                           data = data,
                           mapping = mapping,
                           axesLabels = object$axesLabels,
                           displayOrder = object$displayOrder,
                           scaling = object$scaling,
                           showArea = FALSE,
                           color = layer$aes_params$colour,
                           lineWidth = layer$aes_params$size,
                           alpha = layer$aes_params$alpha
      )
    },
    "radial" = {

      ggRadialSerialAxes(plot,
                         data = data,
                         mapping = mapping,
                         axesLabels = object$axesLabels,
                         displayOrder = object$displayOrder,
                         scaling = object$scaling,
                         color = layer$aes_params$colour,
                         lineWidth = layer$aes_params$size,
                         showArea = FALSE,
                         alpha = layer$aes_params$alpha)

    }
  )
}

#' @export
add_serialaxes_layers.GeomRibbon <- function(layer, plot, object) {

  data <- {if(is.waive(layer$data)) NULL else layer$data} %||% plot$data

  layer_mapping <- layer$mapping

  mapping <- if(layer$inherit.aes) {
    mbind(plot$mapping, layer_mapping)
  } else {
    layer_mapping
  }

  switch(
    object$axesLayout,

    "parallel" = {

      ggParallelSerialAxes(plot,
                           data = data,
                           mapping = mapping,
                           axesLabels = object$axesLabels,
                           displayOrder = object$displayOrder,
                           scaling = object$scaling,
                           showArea = TRUE,
                           color = layer$aes_params$fill,
                           alpha = layer$aes_params$alpha
      )
    },
    "radial" = {

      ggRadialSerialAxes(plot,
                         data = data,
                         mapping = mapping,
                         axesLabels = object$axesLabels,
                         displayOrder = object$displayOrder,
                         scaling = object$scaling,
                         color = layer$aes_params$fill,
                         showArea = TRUE,
                         alpha = layer$aes_params$alpha)
    }
  )
}

#' @export
add_serialaxes_layers.GeomDensity <- function(layer, plot, object) {

  data <- {if(is.waive(layer$data)) NULL else layer$data} %||% plot$data

  layer_mapping <- layer$mapping

  mapping <- if(layer$inherit.aes) {
    mbind(plot$mapping, layer_mapping)
  } else {
    layer_mapping
  }

  mappingNames <- vapply(mapping,
                         function(m) {
                           rlang::quo_name(m)
                         }, character(1L))

  p <- ncol(data)

  scaledData <- get_scaledData(data = data,
                               sequence = object$axesLabels,
                               scaling = object$scaling,
                               as.data.frame = TRUE)

  # to make sure no data column names conflict with it
  colnames <- colnames(data)
  ithName <- not_in_column_names(colnames)

  axesLabels <- object$axesLabels %||% colnames
  len.xaxis <- length(axesLabels)

  groupid <- 0

  groupedData <- lapply(seq(p),
                        function(i) {
                          # Create a fake ggplot
                          # to calculate the stats of such layer
                          ithColumn <- scaledData[, i]
                          newData <- cbind(ithColumn, data)
                          newColnames <- c(ithName, colnames)
                          newData <- stats::setNames(newData, newColnames)


                          newPlot <- ggplot2::ggplot(data = newData)
                          layer$mapping <- mbind(mapping, aes(x = !!rlang::sym(ithName)))
                          newPlot$layers <- list(layer)
                          layeredData <- suppressMessages(ggplot2::layer_data(newPlot, i = 1L))

                          # recover the mapping
                          layer$mapping <- layer_mapping

                          x <- layeredData$x
                          y <- layeredData$y

                          group <- layeredData$group
                          groupNum <- length(unique(group))
                          # default density n should be 512
                          groupValue <- nrow(layeredData)/groupNum

                          groupedData <- mapping_data(data, mapping, groupValue)

                          if(nrow(groupedData) == 0) {
                            groupedData <- data.frame(
                              x = trans_x_coord(x, y, len.xaxis, i,
                                                axesLayout = object$axesLayout,
                                                right = object$right),
                              y = trans_y_coord(x, y, len.xaxis, i,
                                                axesLayout = object$axesLayout,
                                                right = object$right),
                              group = group + groupid
                            )
                          } else {
                            # flip x and y
                            groupedData$x <- trans_x_coord(x, y, len.xaxis, i,
                                                           axesLayout = object$axesLayout,
                                                           right = object$right)
                            groupedData$y <- trans_y_coord(x, y, len.xaxis, i,
                                                           axesLayout = object$axesLayout,
                                                           right = object$right)
                            groupedData$group <- group + groupid
                          }

                          # re-group
                          groupid <<- groupid + groupNum

                          groupedData_setup(layer = layer,
                                            groupedData = groupedData,
                                            name = names(mappingNames),
                                            axesLayout = object$axesLayout,
                                            x = x, y = y,
                                            len.xaxis = len.xaxis,
                                            i = i,
                                            right = object$right)
                        })


  layeredMapping <- layeredMapping_setup(layer = layer,
                                         name = names(mappingNames),
                                         axesLayout = object$axesLayout,
                                         mapping = mapping)

  groupedData <- do.call(rbind, groupedData)

  args <- remove_null(data = groupedData,
                      mapping = layeredMapping,
                      alpha = layer$aes_params$alpha,
                      size = layer$aes_params$size,
                      colour = layer$aes_params$colour,
                      fill = layer$aes_params$fill,
                      inherit.aes = FALSE)

  plotFun <- plot_fun(layer = layer,
                      name = names(mappingNames),
                      axesLayout = object$axesLayout)

  plot <- plot + do.call(plotFun, args)

  plot
}

#' @export
add_serialaxes_layers.GeomFreqpoly <- function(layer, plot, object) {
  add_serialaxes_layers.GeomDensity(layer, plot, object)
}

#' @export
add_serialaxes_layers.GeomBar <- function(layer, plot, object) {

  data <- {if(is.waive(layer$data)) NULL else layer$data} %||% plot$data

  layer_mapping <- layer$mapping

  mapping <- if(layer$inherit.aes) {
    mbind(plot$mapping, layer_mapping)
  } else {
    layer_mapping
  }

  p <- ncol(data)

  scaledData <- get_scaledData(data = data,
                               sequence = object$axesLabels,
                               scaling = object$scaling,
                               as.data.frame = TRUE)

  # to make sure no data column names conflict with it
  colnames <- colnames(data)
  ithName <- not_in_column_names(colnames)

  axesLabels <- object$axesLabels %||% colnames
  len.xaxis <- length(axesLabels)
  delta <- 1/(len.xaxis - 1)

  groupid <- 0

  groupedData <- lapply(seq(p),
                        function(i) {
                          # Create a fake ggplot
                          # to calculate the stats of such layer
                          ithColumn <- scaledData[, i]
                          newData <- cbind(ithColumn, data)
                          newColnames <- c(ithName, colnames)
                          newData <- stats::setNames(newData, newColnames)

                          newPlot <- ggplot2::ggplot(data = newData)
                          layer$mapping <- mbind(mapping, aes(x = !!rlang::sym(ithName)))
                          newPlot$layers <- list(layer)
                          layeredData <- suppressMessages(ggplot2::layer_data(newPlot, i = 1L))

                          # recover the mapping
                          layer$mapping <- layer_mapping

                          x <- scale01(c(layeredData$xmin, layeredData$xmax))
                          y <- scale01(c(layeredData$ymin, layeredData$ymax))
                          n <- nrow(layeredData)

                          xmin <- x[seq(n)]
                          xmax <- x[-seq(n)]
                          ymin <- y[seq(n)]* (delta - 0.05)
                          ymax <- y[-seq(n)]* (delta - 0.05)

                          group <- layeredData$group
                          groupNum <- length(unique(group))
                          groupValue <- nrow(layeredData)/groupNum

                          groupedData <- groupedData_setup(layer,
                                                           groupedData = mapping_data(data, mapping, groupValue),
                                                           axesLayout = object$axesLayout,
                                                           xmax = xmax,
                                                           xmin = xmin,
                                                           ymax = ymax,
                                                           ymin = ymin,
                                                           group = group,
                                                           len.xaxis = len.xaxis,
                                                           i = i,
                                                           right = object$right)

                          # reset group
                          group <- groupedData$group
                          groupedData$group <- group + groupid
                          groupid <<- groupid + length(unique(group))
                          groupedData
                        })

  groupedData <- do.call(rbind, groupedData)

  args <- remove_null(data = groupedData,
                      mapping = layeredMapping_setup(layer, object$axesLayout, mapping),
                      alpha = layer$aes_params$alpha,
                      size = layer$aes_params$size,
                      colour = layer$aes_params$colour,
                      fill = layer$aes_params$fill,
                      inherit.aes = FALSE)

  plotFun <- plot_fun(layer = layer,
                      axesLayout = object$axesLayout)

  plot <- plot + do.call(plotFun, args)

  plot
}

################################# helper function ###############################

not_in_column_names <- function(colnames) {
  name <- "densityX"
  while(name %in% colnames) {
    name <- paste0(name, "X")
  }
  name
}

mapping_data <- function(data, mapping, groupValue) {

  l <- length(mapping)
  if(l == 0) return(data.frame())

  mappingNames <- vapply(mapping,
                         function(m) {
                           rlang::quo_name(m)
                         }, character(1L))

  colNames <- colnames(data)
  mappingNamesInDataNames <- sapply(mappingNames,
                                    function(mN) {
                                      ## the first one always
                                      colNames[sapply(colNames, function(colName) {grepl(colName, mN)})][1]
                                    })

  notDuplicated <- which(!duplicated(mappingNamesInDataNames))

  stats::setNames(
    as.data.frame(
      lapply(notDuplicated,
             function(i) {
               # m <- mapping[[i]]
               # rep(unique(rlang::eval_tidy(rlang::quo(!!m),  data)),
               #     each = groupValue)
               ## we want the original data, not the manipulated data
               m <- mappingNamesInDataNames[i]
               rep(unique(data[, m]), each = groupValue)
             }),
      stringsAsFactors = FALSE
    ),
    mappingNamesInDataNames[notDuplicated]
  )
}

scale01 <- function(x, na.rm = TRUE) {
  minx <- min(x, na.rm = na.rm)
  maxx <- max(x, na.rm = na.rm)
  if(minx == maxx)
    rep(0, length(x))
  else
    (x - minx)/(maxx - minx)
}

trans_x_coord <- function(x, y, len.xaxis, i, axesLayout = "parallel", right = TRUE) {

  delta <- 1/(len.xaxis - 1)

  rotate_x <- function(x, y, angle, delta = 1, right = TRUE) {
    cos(angle) * x + sign(right - 0.5) * sin(angle) * y * delta
  }

  switch(axesLayout,
         "parallel" = {

           scale01(y) * (delta - 0.05) * sign(right - 0.5) + delta * (i - 1)
         },
         "radial" = {

           angle <- seq(0, 2 * base::pi, length.out = len.xaxis + 1)[1:len.xaxis][i]
           # recenter to (0.5, 0.5)
           rotate_x(x = scale01(x),
                    y = scale01(y),
                    angle = angle,
                    delta = delta,
                    right = right) *
             loon_default_setting("radius") +
             0.5
         })
}

trans_y_coord <- function(x, y, len.xaxis, i, axesLayout = "parallel", right = TRUE) {

  delta <- 1/(len.xaxis - 1)

  rotate_y <- function(x, y, angle, delta = 1, right = TRUE) {
    sin(angle) * x - sign(right - 0.5) * cos(angle) * y * delta
  }

  switch(axesLayout,
         "parallel" = {
           scale01(x)
         },
         "radial" = {

           angle <- seq(0, 2 * base::pi, length.out = len.xaxis + 1)[1:len.xaxis][i]
           # recenter to (0.5, 0.5)
           rotate_y(x = scale01(x),
                    y = scale01(y),
                    angle = angle,
                    delta = delta,
                    right = right) *
             loon_default_setting("radius") +
             0.5
         })
}

