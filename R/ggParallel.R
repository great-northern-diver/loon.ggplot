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
      legend.background = element_rect(fill = panel_bg_fill)
    ) +
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
                                 mapping = NULL,
                                 axesLabels = NULL,
                                 displayOrder = NULL,
                                 scaling = c("variable", "observation", "data", "none"),
                                 color = NULL,
                                 lineWidth = NULL,
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
  xaxis <- seq(0, 1, length.out =  len.xaxis)

  # set scaledData
  scaling <- match.arg(scaling)
  displayOrder <- displayOrder %||% (1:dim(data)[1])

  if(length(displayOrder) == 0) return(ggObj)

  mapping <- mapping %||% ggObj$mapping

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
    axesLayout = "parallel",
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
                        inherit.aes = FALSE,
                        alpha = alpha,
                        fill = color # NULL or some specific value
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
                        colour = color, # NULL or some specific value
                        size = lineWidth, # NULL or some specific value
                        alpha = alpha,
                        inherit.aes = FALSE)

    ggObj <- ggObj +
      do.call(
        what = ggplot2::geom_path,
        args
      )
  }

  return(ggObj)
}
