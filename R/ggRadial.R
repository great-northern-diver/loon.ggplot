############## radial ######################
ggRadialAes <- function(ggObj,
                        axesLabels = NULL,
                        title = "",
                        showLabels = TRUE,
                        showAxesLabels = TRUE,
                        showGuides = TRUE,
                        showAxes = TRUE) {

  stopifnot(
    exprs = {
      is.logical(showLabels)
      is.logical(showAxesLabels)
      is.logical(showGuides)
      is.logical(showAxes)
      ggplot2::is.ggplot(ggObj)
    }
  )

  axesLabels <- axesLabels %||% colnames(ggObj$data)

  if(is.null(axesLabels))
    return(ggObj)

  len.xaxis <- length(axesLabels)
  # radial axes
  xlim <- ylim <- c(-0.2, 1.2)
  angle <- seq(0, 2 * base::pi, length.out = len.xaxis + 1)[1:len.xaxis]

  xpos <- 0.5
  ypos <- 0.5
  radius <- loon_default_setting("radius")

  boundary_lineWidth <- loon_default_setting("boundaryLineWidth")

  panel_bg_fill <- ifelse(showGuides,
                          loon::l_getOption("canvas_bg_guides"),
                          loon::l_getOption("background"))
  line_color <- ifelse(showGuides,
                       loon::l_getOption("background"),
                       loon::l_getOption("foreground"))
  ggObj <- ggObj +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      plot.margin = grid::unit(c(5,12,5,12), "mm"),
      panel.background = ggplot2::element_rect(fill = panel_bg_fill),
      plot.background = ggplot2::element_rect(fill = panel_bg_fill),
      legend.background = ggplot2::element_rect(fill = panel_bg_fill)
    ) +
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim)

  if(showLabels && title != "") {
    ggObj <- ggObj +
      ggplot2::ggtitle(title)
  }


  # This is a hack! Fix me
  # I am sorry for this
  # It is because in radial axes, we create three layers
  # which is used to draw
  # the guides, axes and labels
  # the 4th layer is the layer to be displayed on top
  # An alternative to do so is to use `coord_polar`
  # First, to create the preimage of the coords
  # Then, call `coord_polar` to transform such preimage to polar image

  # guides
  guideX <- xpos + radius * cos(seq(0, 2 * base::pi, length=101))
  guideY <- ypos + radius * sin(seq(0, 2 * base::pi, length=101))

  guideArgs <- list(
    data = data.frame(
      x = guideX,
      y = guideY
    ),
    mapping = ggplot2::aes(x = x, y = y),
    color = loon::l_getOption("guidelines"),
    size = boundary_lineWidth,
    alpha = 0.5,
    inherit.aes = FALSE
  )

  guideLayer <- if (showGuides) {
    do.call(ggplot2::geom_path, guideArgs)
  } else {
    do.call(ggplot2::geom_blank, guideArgs)
  }

  # axes
  axesX <- xpos + c(rep(0, len.xaxis) ,radius * cos(angle))
  axesY <- ypos + c(rep(0, len.xaxis) ,radius * sin(angle))
  group <- rep(seq(len.xaxis), 2)

  axesArgs <- list(
    data = data.frame(
      x = axesX,
      y = axesY,
      group = group
    ),
    mapping = ggplot2::aes(x = x, y = y, group = group),
    color = line_color,
    size = boundary_lineWidth,
    alpha = 0.5,
    inherit.aes = FALSE
  )

  axesLayer <- if(showAxes) {
    do.call(ggplot2::geom_path, axesArgs)
  } else {
    do.call(ggplot2::geom_blank, axesArgs)
  }

  labelX <- (radius + 0.1) * cos(angle) + xpos
  labelY <- (radius + 0.1) * sin(angle) + ypos
  label <- axesLabels

  labelArgs <- list(
    data = data.frame(
      x = labelX,
      y = labelY,
      label = label
    ),
    mapping = ggplot2::aes(x = x, y = y, label = label),
    inherit.aes = FALSE
  )

  textLayer <- if(showAxesLabels) {
    do.call(ggplot2::geom_text, labelArgs)
  } else {
    do.call(ggplot2::geom_blank, labelArgs)
  }

  layers <- ggObj$layers

  layers <- c(
    layers,
    list(guideLayer,
         axesLayer,
         textLayer)
  )

  ggObj$layers <- layers

  return(ggObj)
}

ggRadialSerialAxes <- function(ggObj,
                               data = NULL,
                               mapping = NULL,
                               axesLabels = NULL,
                               displayOrder = NULL,
                               scaling = c("variable", "observation", "data", "none"),
                               color = NULL,
                               lineWidth = 0.5,
                               alpha = NULL,
                               showArea = FALSE,
                               ymin = NULL) {

  stopifnot(
    exprs = {
      is.logical(showArea)
      ggplot2::is.ggplot(ggObj)
    }
  )

  data <- data %||% ggObj$data
  axesLabels <- axesLabels %||% colnames(data)

  if(is.null(data) || is.null(axesLabels))
    return(ggObj)

  len.xaxis <- length(axesLabels)
  # radial axes
  xlim <- ylim <- c(-0.2, 1.2)
  angle <- seq(0, 2 * base::pi, length.out = len.xaxis + 1)[1:len.xaxis]

  xpos <- 0.5
  ypos <- 0.5
  radius <- loon_default_setting("radius")

  # set scaledData
  scaling <- match.arg(scaling)
  displayOrder <- displayOrder %||% (1:dim(data)[1])

  if(length(displayOrder) == 0) return(ggObj)

  mapping <- mapping %||% ggObj$mapping
  lineWidth <- set_lineSize(data, mapping, lineWidth)

  grouped_data <- set_data_group(
    data = get_scaledData(data = data,
                          sequence = axesLabels,
                          scaling = scaling,
                          displayOrder = displayOrder),
    mapping = mapping,
    showArea = showArea,
    ymin = ymin,
    color = color,
    lineWidth = lineWidth,
    axesLayout = "radial",
    originalData = data)

  x <- grouped_data$x
  y <- grouped_data$y
  group <- grouped_data$group

  if(showArea) {

    args <- remove_null(data = grouped_data,
                        mapping = mbind(
                          mapping,
                          ggplot2::aes(x = x,
                                       y = y,
                                       group = group)
                        ),
                        alpha = alpha,
                        inherit.aes = FALSE,
                        fill = color
    )

    ggObj <- ggObj +
      do.call(
        what = ggplot2::geom_polygon,
        args
      )

  } else {

    args <- remove_null(data = grouped_data,
                        mapping = mbind(
                          mapping,
                          ggplot2::aes(x = x,
                                       y = y,
                                       group = group)
                        ),
                        alpha = alpha,
                        colour = color, # NULL or some specific value
                        size = lineWidth, # NULL or some specific value
                        inherit.aes = FALSE)

    ggObj <- ggObj +
      do.call(
        what = ggplot2::geom_path,
        args
      )
  }

  return(ggObj)
}
