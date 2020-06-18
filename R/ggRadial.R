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

  grouped_data <- suppressWarnings(
    get_scaledData(data = data,
                   sequence = axesLabels,
                   scaling = scaling,
                   displayOrder = displayOrder) )%>%
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

  if(showArea) {

    fill <- grouped_data$color

    args <- remove_null(
      data = grouped_data,
      mapping = ggplot2::aes(x = x, y = y, group = group, fill = fill),
      inherit.aes = FALSE
    )

    uni_fill <- unique(fill) %>%
      as.character()

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

    uni_color <- unique(color) %>%
      as.character()
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
