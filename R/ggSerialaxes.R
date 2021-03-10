#' @title ggplot serialaxes (deprecated)
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
#' @details
#' So far, `ggSerialAxes` is deprecated and will not be maintained any more. Create serial axes
#' in `ggplot`, see \code{help("coord_serialaxes", ggmulti)} for more details.
#'
#' @export
#' @examples
#' # Blank plot
#' p <- ggplot(data = mtcars, mapping = aes(colour = factor(cyl)))
#' # Add serial axes (returns a ggplot object)
#' g <- ggSerialAxes(p)
#' g
#' # An eulerian path of iris variables
#' # ordSeq <- PairViz::eulerian(4)
#' ordSeq <- c(1, 2, 3, 1, 4, 2, 3, 4)
#' ggSerialAxes(
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
    stop(paste(deparse(substitute(ggObj)), "is not a ggplot object"), call. = FALSE)
  }

  scaling <- match.arg(scaling)
  layout <- match.arg(layout)

  data <- data %||% ggObj$data %||% stop("No data found", call. = FALSE)

  ggObj <- switch(
    layout,
    "parallel" = {
      ggObj <-  ggParallelAes(
        ggObj,
        axesLabels = axesLabels,
        title = title,
        showLabels = showLabels,
        showAxesLabels = showAxesLabels,
        showGuides = showGuides,
        showAxes = showAxes)
      ggParallelSerialAxes(
        ggObj,
        data = data,
        axesLabels = axesLabels,
        displayOrder = displayOrder,
        scaling = scaling,
        color = color,
        lineWidth = size,
        showArea = showArea)
    },
    "radial" = {
      ggObj <- ggRadialAes(
        ggObj,
        axesLabels = axesLabels,
        title = title,
        showLabels = showLabels,
        showAxesLabels = showAxesLabels,
        showGuides = showGuides,
        showAxes = showAxes)
      ggRadialSerialAxes(
        ggObj,
        data = data,
        axesLabels = axesLabels,
        displayOrder = displayOrder,
        scaling = scaling,
        color = color,
        lineWidth = size,
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

  boundary_lineWidth <- loon_default_setting("boundaryLineWidth")

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
      plot.title = ggplot2::element_text(hjust = 0.5),
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

  mapping <- ggObj$mapping
  lineWidth <- set_lineSize(data, mapping, lineWidth)

  grouped_data <- set_data_group(
    suppressWarnings(
      get_scaledData(data = data,
                     sequence = axesLabels,
                     scaling = scaling,
                     displayOrder = displayOrder)
    ),
    mapping = mapping,
    showArea = showArea,
    color = color,
    lineWidth = lineWidth,
    axesLayout = "parallel",
    originalData = data)

  x <- grouped_data$x
  y <- grouped_data$y
  group <- grouped_data$group

  if(showArea) {

    fill <- grouped_data$color

    args <- remove_null(
      data = grouped_data,
      mapping = ggplot2::aes(x = x, y = y, group = group, fill = fill),
      inherit.aes = FALSE
    )

    uni_fill <- as.character(unique(fill))

    ggObj <- ggObj +
      do.call(
        what = ggplot2::geom_polygon,
        args
      ) +
      ggplot2::scale_fill_manual(values = stats::setNames(valid_color(uni_fill),
                                                          nm = uni_fill),
                                 labels = stats::setNames(selection_color_labels(uni_fill),
                                                          nm = uni_fill))

    if(length(uni_fill) == 1)
      ggObj <- ggObj + ggplot2::guides(fill = FALSE)

  } else {

    color <- grouped_data$color
    size <- grouped_data$size

    args <- remove_null(data = grouped_data,
                        mapping = ggplot2::aes(x = x,
                                               y = y,
                                               group = group,
                                               color = color,
                                               size = size),
                        inherit.aes = FALSE)

    uni_color <- as.character(unique(color))
    uni_size <- unique(size)

    ggObj <- ggObj +
      do.call(
        what = ggplot2::geom_path,
        args
      ) +
      ggplot2::scale_color_manual(values = stats::setNames(valid_color(uni_color),
                                                           nm = uni_color),
                                  labels = stats::setNames(selection_color_labels(uni_color),
                                                           nm = uni_color)) +
      ggplot2::scale_size(range = range(size))

    if(length(uni_color) == 1)
      ggObj <- ggObj + ggplot2::guides(color = FALSE)

    if(length(uni_size) == 1)
      ggObj <- ggObj + ggplot2::guides(size = FALSE)

  }

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
      plot.title = ggplot2::element_text(hjust = 0.5),
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

  mapping <- ggObj$mapping
  lineWidth <- set_lineSize(data, mapping, lineWidth)

  grouped_data <- set_data_group(
    suppressWarnings(
      get_scaledData(data = data,
                     sequence = axesLabels,
                     scaling = scaling,
                     displayOrder = displayOrder)),
    mapping = mapping,
    showArea = showArea,
    color = color,
    lineWidth = lineWidth,
    axesLayout = "radial",
    originalData = data)

  x <- grouped_data$x
  y <- grouped_data$y
  group <- grouped_data$group

  if(showArea) {

    fill <- grouped_data$color

    args <- remove_null(
      data = grouped_data,
      mapping = ggplot2::aes(x = x, y = y, group = group, fill = fill),
      inherit.aes = FALSE
    )

    uni_fill <- as.character(unique(fill))

    ggObj <- ggObj +
      do.call(
        what = ggplot2::geom_polygon,
        args
      ) +
      ggplot2::scale_fill_manual(values = stats::setNames(valid_color(uni_fill),
                                                          nm = uni_fill),
                                 labels = stats::setNames(selection_color_labels(uni_fill),
                                                          nm = uni_fill))

    if(length(uni_fill) == 1)
      ggObj <- ggObj + ggplot2::guides(fill = FALSE)

  } else {

    color <- grouped_data$color
    size <- grouped_data$size

    args <- remove_null(data = grouped_data,
                        mapping = ggplot2::aes(x = x, y = y, group = group, color = color, size = size),
                        inherit.aes = FALSE)

    uni_color <- as.character(unique(color))
    uni_size <- unique(size)

    ggObj <- ggObj +
      do.call(
        what = ggplot2::geom_path,
        args
      ) +
      ggplot2::scale_color_manual(values = stats::setNames(valid_color(uni_color),
                                                           nm = uni_color),
                                  labels = stats::setNames(selection_color_labels(uni_color),
                                                           nm = uni_color)) +
      ggplot2::scale_size(range = range(size))

    if(length(uni_color) == 1)
      ggObj <- ggObj + ggplot2::guides(color = FALSE)

    if(length(uni_size) == 1)
      ggObj <- ggObj + ggplot2::guides(size = FALSE)
  }

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
                     color = rep(color[i], 2 * p),
                     stringsAsFactors = FALSE
                   )
                 } else {
                   data.frame(
                     x = xaxis,
                     y = as.numeric(data[i, ]),
                     group = rep(i, p),
                     size = rep(lineWidth[i], p),
                     color = rep(color[i], p),
                     stringsAsFactors = FALSE
                   )
                 }
               })
      )
    },
    "radial" = {
      radius <- loon_default_setting("radius")
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
                     color = rep(color[i], p + 1),
                     stringsAsFactors = FALSE
                   )
                 } else {
                   data.frame(
                     x = xpos + c(radialxais, radialxais[1]),
                     y = ypos + c(radialyais, radialyais[1]),
                     group = rep(i, p + 1),
                     color = rep(color[i], p + 1),
                     size = rep(lineWidth[i], p + 1),
                     stringsAsFactors = FALSE
                   )
                 }
               })
      )
    }
  )

  # remove NA color
  if(any(is.na(grouped_data$color))) grouped_data$color <- NULL
  quo_color <- mapping$colour

  if(!rlang::is_empty(quo_color) && !is.null(originalData)) {
    grouped_data <- cbind(
      grouped_data,
      color = rep(
        rlang::eval_tidy(rlang::quo(!!quo_color),  originalData),
        each = switch(axesLayout, "parallel" = p, "radial" = p + 1)
      )
    )
  }

  if(!is.null(grouped_data$color)) {
    if(is.numeric(grouped_data$color)) {
      warning("Color can only be discrete", call. = FALSE)
      grouped_data$color <- as.character(grouped_data$color)
    }
  }

  return(grouped_data)
}

valid_color <- function(x) {
  if(any(!is.color(x)))
    gg_color_hue(length(x))
  else
    x
}
