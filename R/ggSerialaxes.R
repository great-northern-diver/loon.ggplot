#' @title ggplot with serialaxes
#'
#' @description The ggplot serialaxes graphics displays multivariate data either as
#' a stacked star glyph plot, or as a parallel coordinate plot.
#'
#' @param ggObj A `ggplot` object
#' @param data A data frame for serialaxes. If `NULL`, data must be set in `ggObj`
#' @param axesLabels A vector with variable names that defines the axes sequence.
#' @param showAxes Logical value to indicate whether axes should be shown or not
#' @param showAxesLabels Logical value to indicate whether axes labels should be shown or not
#' @param scaling one of 'variable', 'data', 'observation' or 'none' to
#' specify how the data is scaled. See Details for more information
#' @param layout either "radial" or "parallel"
#' @param displayOrder The display order of the observations.
#' @param title title of the display
#' @param showLabels Logical value to indicate whether label (mainly **title**) should be shown or not
#' @param color Line color
#' @param size Line width
#' @param showGuides Logical value to indicate whether guides should be shown or not
#' @param showArea Logical value to indicate whether to display lines or area
#'
#' @return a ggplot object
#'
#' @export
#' @examples
#' # Blank plot
#' p <- ggplot(data = mtcars, mapping = aes(colour = as.factor(cyl)))
#' # Add serial axes (returns a ggplot object)
#' g <- ggSerialAxes(p)
#' # modify categorical variable color and legend background
#' g +
#'   theme(legend.key = element_rect(fill = "lightblue", color = "black")) +
#'   scale_colour_manual(values = c("4" = "red", "6" = "blue", "8" = "green"))
#'
#' # An eulerian path of iris variables
#' # ordSeq <- PairViz::eulerian(4)
#' ordSeq <- c(1, 2, 3, 1, 4, 2, 3, 4)
#' g <- ggSerialAxes(
#'        ggObj = ggplot(data = iris, mapping = aes(colour = Species)),
#'        axesLabels = colnames(iris)[ordSeq],
#'        layout = "radial"
#' )

ggSerialAxes <- function(ggObj,
                         data = NULL, axesLabels = NULL,
                         showAxes = TRUE, showAxesLabels = TRUE,
                         scaling = c("variable", "observation", "data", "none"),
                         layout = c("parallel", "radial"), displayOrder = NULL,
                         title = "", showLabels = TRUE,
                         color = NULL, size = NULL,
                         showGuides = TRUE, showArea = FALSE) {

  # check arguments
  if(!ggplot2::is.ggplot(ggObj)) {
    stop(paste(deparse(substitute(ggObj)), "is not a ggplot object"))
  }

  scaling <- match.arg(scaling)
  layout <- match.arg(layout)

  data <- data %||% ggObj$data %||% stop("No data found", call. = FALSE)

  mapping <- ggObj$mapping
  lineWidth <- set_lineSize(data,
                            mapping = mapping,
                            size = size)
  ggObj <- switch(
    layout,
    "parallel" = {
      ggObj %>%
        ggParallelAes(
          axesLabels = axesLabels,
          title = title,
          showLabels = showLabels,
          showAxesLabels = showAxesLabels,
          showGuides = showGuides,
          showAxes = showAxes) %>%
        ggParallelSerialAxes(
          data = data,
          mapping = mapping,
          axesLabels = axesLabels,
          displayOrder = displayOrder,
          scaling = scaling,
          color = color,
          lineWidth = lineWidth,
          showArea = showArea)
    },
    "radial" = {
      ggObj %>%
        ggRadialAes(
          axesLabels = axesLabels,
          title = title,
          showLabels = showLabels,
          showAxesLabels = showAxesLabels,
          showGuides = showGuides,
          showAxes = showAxes) %>%
        ggRadialSerialAxes(
          data = data,
          mapping = mapping,
          axesLabels = axesLabels,
          displayOrder = displayOrder,
          scaling = scaling,
          color = color,
          lineWidth = lineWidth,
          showArea = showArea)
    },
    {NULL}
  )

  return(ggObj)
}

############## parallel ######################
ggParallelAes <- function(ggObj,
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
  xlim <- ylim <- c(0, 1)
  xaxis <- seq(0, 1, length.out =  len.xaxis)

  panel_bg_fill <- ifelse(showGuides,
                          loon::l_getOption("canvas_bg_guides"),
                          loon::l_getOption("background"))
  line_color <- ifelse(showGuides,
                       loon::l_getOption("background"),
                       loon::l_getOption("foreground"))

  boundary_lineWidth <- 1.3

  ggObj <- ggObj +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(color = loon::l_getOption("foreground")),
      axis.text.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.grid.major.x = if(showAxes) {
        ggplot2::element_line(color = line_color, size = boundary_lineWidth)
      } else ggplot2::element_blank(),
      panel.grid.major.y = if(showGuides) ggplot2::element_line() else ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = panel_bg_fill),
      plot.margin = grid::unit(c(5,12,5,12), "mm"),
      plot.background = ggplot2::element_rect(fill = panel_bg_fill),
      legend.background = element_rect(fill = panel_bg_fill)
    ) +
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim) +
    ggplot2::scale_y_continuous(
      labels = NULL,
      expand = c(0,0)
    ) +
    ggplot2::scale_x_continuous(
      labels = if(showAxesLabels) axesLabels else NULL,
      breaks = if(showAxesLabels) seq(0, 1, length.out = len.xaxis) else ggplot2::waiver(),
      expand = c(0,0)
    )

  if(showLabels && title != "") {
    ggObj <- ggObj +
      ggplot2::ggtitle(title)
  }

  return(ggObj)
}

ggParallelSerialAxes <- function(ggObj,
                                 data = NULL,
                                 mapping = ggplot2::aes(),
                                 axesLabels = NULL,
                                 displayOrder = NULL,
                                 scaling = c("variable", "observation", "data", "none"),
                                 color = NULL,
                                 lineWidth = 0.5,
                                 showArea = FALSE) {

  stopifnot(
    exprs = {
      is.logical(showArea)
      ggplot2::is.ggplot(ggObj)
    }
  )

  axesLabels <- axesLabels %||% colnames(ggObj$data)
  data <- data %||% ggObj$data

  if(is.null(data) || is.null(axesLabels))
    return(ggObj)

  len.xaxis <- length(axesLabels)
  xaxis <- seq(0, 1, length.out =  len.xaxis)

  # set scaledData
  scaling <- match.arg(scaling)
  displayOrder <- displayOrder %||% (1:dim(data)[1])

  if(length(displayOrder) == 0) return(ggObj)

  grouped_data <- get_scaledData(data = data,
                                 sequence = axesLabels,
                                 scaling = scaling,
                                 displayOrder = displayOrder) %>%
    set_data_group(
      mapping = mapping,
      showArea = showArea,
      color = color,
      lineWidth = lineWidth,
      axesLayout = "parallel",
      originalData = data)

  x <- grouped_data$x
  y <- grouped_data$y
  group <- grouped_data$group

  args <- if(showArea) {
    list(
      data = grouped_data,
      mapping = ggplot2::aes(x = x, y = y, group = group),
      fill = grouped_data$fill,
      inherit.aes = TRUE
    )
  } else {
    list(
      data = grouped_data,
      mapping = ggplot2::aes(x = x, y = y, group = group),
      color = grouped_data$color,
      size = grouped_data$size,
      inherit.aes = TRUE
    )
  }

  args <- Filter(Negate(is.null), args)

  ggObj <- ggObj +
    do.call(
      what = if(showArea) ggplot2::geom_polygon else ggplot2::geom_path,
      args
    )
  return(ggObj)
}

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
  radius <- default_radius()

  boundary_lineWidth <- 1.3

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

  if (showGuides) {

    x <- xpos + radius * cos(seq(0, 2 * base::pi, length=101))
    y <- ypos + radius * sin(seq(0, 2 * base::pi, length=101))

    ggObj <- ggObj +
      ggplot2::geom_path(
        data = data.frame(
          x = x,
          y = y
        ),
        mapping = ggplot2::aes(x = x, y = y),
        color = loon::l_getOption("guidelines"),
        size = boundary_lineWidth,
        inherit.aes = FALSE
      )
  }

  if(showAxes) {

    x <- xpos + c(rep(0, len.xaxis) ,radius * cos(angle))
    y <- ypos + c(rep(0, len.xaxis) ,radius * sin(angle))
    group <- rep(1:len.xaxis, 2)

    ggObj <- ggObj +
      ggplot2::geom_path(
        data = data.frame(
          x = x,
          y = y,
          group = group
        ),
        mapping = ggplot2::aes(x = x, y = y, group = group),
        color = line_color,
        size = boundary_lineWidth,
        inherit.aes = FALSE
      )
  }

  if(showLabels && title != "") {
    ggObj <- ggObj +
      ggplot2::ggtitle(title)
  }

  if(showAxesLabels) {

    x <- (radius + 0.1) * cos(angle) + xpos
    y <- (radius + 0.1) * sin(angle) + ypos
    label <- axesLabels

    ggObj <- ggObj +
      ggplot2::geom_text(
        data = data.frame(
          x = x,
          y = y,
          label = label
        ),
        mapping = ggplot2::aes(x = x, y = y, label = label),
        inherit.aes = FALSE
      )
  }

  return(ggObj)
}

ggRadialSerialAxes <- function(ggObj,
                               data = NULL,
                               mapping = ggplot2::aes(),
                               axesLabels = NULL,
                               displayOrder = NULL,
                               scaling = c("variable", "observation", "data", "none"),
                               color = NULL,
                               lineWidth = 1,
                               showArea = FALSE) {

  stopifnot(
    exprs = {
      is.logical(showArea)
      ggplot2::is.ggplot(ggObj)
    }
  )

  axesLabels <- axesLabels %||% colnames(ggObj$data)
  data <- data %||% ggObj$data

  if(is.null(data) || is.null(axesLabels))
    return(ggObj)

  len.xaxis <- length(axesLabels)
  # radial axes
  xlim <- ylim <- c(-0.2, 1.2)
  angle <- seq(0, 2 * base::pi, length.out = len.xaxis + 1)[1:len.xaxis]

  xpos <- 0.5
  ypos <- 0.5
  radius <- default_radius()

  # set scaledData
  scaling <- match.arg(scaling)
  displayOrder <- displayOrder %||% (1:dim(data)[1])

  if(length(displayOrder) == 0) return(ggObj)

  grouped_data <- get_scaledData(data = data,
                                 sequence = axesLabels,
                                 scaling = scaling,
                                 displayOrder = displayOrder) %>%
    set_data_group(
      mapping = mapping,
      showArea = showArea,
      color = color,
      lineWidth = lineWidth,
      axesLayout = "radial",
      originalData = data)

  x <- grouped_data$x
  y <- grouped_data$y
  group <- grouped_data$group

  args <- if(showArea) {
    list(
      data = grouped_data,
      mapping = ggplot2::aes(x = x, y = y, group = group),
      fill = grouped_data$fill,
      inherit.aes = TRUE
    )
  } else {
    list(
      data = grouped_data,
      mapping = ggplot2::aes(x = x, y = y, group = group),
      color = grouped_data$color,
      size = grouped_data$size,
      inherit.aes = TRUE
    )
  }

  args <- Filter(Negate(is.null), args)

  ggObj <- ggObj +
    do.call(
      what = if(showArea) ggplot2::geom_polygon else ggplot2::geom_path,
      args
    )
  return(ggObj)
}

set_data_group <- function(data = NULL,
                           mapping = ggplot2::aes(),
                           showArea = FALSE,
                           color = NULL,
                           lineWidth = 0.5,
                           axesLayout = "parallel",
                           originalData = NULL) {

  if(is.null(data)) stop("No data found", call. = FALSE)

  dimD <- dim(data)
  n <- dimD[1]
  p <- dimD[2]

  stopifnot(
    exprs = {
      length(color) == 0 || length(color) == 1 || length(color) == n
      length(lineWidth) == 1 || length(lineWidth) == n
    }
  )

  if(length(color) == 0) color <- rep(NA, n)
  if(length(color) == 1) color <- rep(color, n)
  if(length(lineWidth) == 1) lineWidth <- rep(lineWidth, n)

  grouped_data <- switch(
    axesLayout,
    "parallel" = {
      xaxis <- seq(0, 1, length.out =  p)
      do.call(
        rbind,
        lapply(1:n,
               function(i) {
                 if(showArea) {
                   data.frame(
                     x = c(xaxis, rev(xaxis)),
                     y = as.numeric(c(data[i, ], rep(0, p))),
                     group = rep(i, 2 * p),
                     fill = rep(color[i], 2 * p)
                   )
                 } else {
                   data.frame(
                     x = xaxis,
                     y = as.numeric(data[i, ]),
                     group = rep(i, p),
                     size = rep(lineWidth[i], p),
                     color = rep(color[i], p)
                   )
                 }
               })
      )
    },
    "radial" = {
      radius <- default_radius()
      xpos <- 0.5
      ypos <- 0.5
      angle <- seq(0, 2 * base::pi, length.out = p + 1)[1:p]
      do.call(
        rbind,
        lapply(1:n,
               function(i) {

                 radialxais <- radius * data[i,] * cos(angle)
                 radialyais <- radius * data[i,] * sin(angle)

                 if(showArea) {
                   data.frame(
                     x = xpos + c(radialxais, radialxais[1]),
                     y = ypos + c(radialyais, radialyais[1]),
                     group = rep(i, p + 1),
                     fill = rep(color[i], p + 1)
                   )
                 } else {
                   data.frame(
                     x = xpos + c(radialxais, radialxais[1]),
                     y = ypos + c(radialyais, radialyais[1]),
                     group = rep(i, p + 1),
                     color = rep(color[i], p + 1),
                     size = rep(lineWidth[i], p + 1)
                   )
                 }
               })
      )
    }
  )

  # remove NA color, fill
  if(any(is.na(grouped_data$color))) grouped_data$color <- NULL
  if(any(is.na(grouped_data$fill))) grouped_data$fill <- NULL

  quo_color <- mapping$colour
  column_names <- colnames(originalData) %||% colnames(data)

  if(!rlang::is_empty(quo_color) && !is.null(column_names) && !is.null(originalData)) {
    quo_text <- rlang::expr_text(quo_color)
    color_name <- column_names[sapply(column_names, function(name) grepl(name, quo_text))]

    if(length(color_name) == 0) stop(paste(quo_text, "is not found in data"), call. = FALSE)

    grouped_data <- data.frame(
      grouped_data,
      setNames(
        lapply(color_name,
               function(name)
                 rep(originalData[, name], each =  dim(grouped_data)[1]/n)
        ),
        color_name
      )
    )
  }

  return(grouped_data)
}
