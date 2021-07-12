#' @rdname loon2ggplot
#' @export
loon2ggplot.l_serialaxes <- function(target, asAes = TRUE, selectedOnTop = TRUE, ...) {

  widget <- target
  remove(target)
  serialaxes.data <- char2num.data.frame(widget['data'])
  colNames <- colnames(serialaxes.data)

  # active or not
  displayOrder <- if(selectedOnTop) {
    get_model_display_order(widget)
  } else {
    seq(widget['n'])
  }

  ndimNames <- loon::l_nDimStateNames(widget)
  # N dim names
  data <- as.data.frame(
    remove_null(
      stats::setNames(
        lapply(ndimNames,
               function(s) {
                 state <- widget[s]
                 if(length(state) == 0) return(NULL)
                 state
               }),
        ndimNames
      ), as_list = FALSE)
  )

  active <- data$active[displayOrder]
  active_displayOrder <- displayOrder[active]

  if(widget['showArea']) {
    message("`showArea` is not implemented yet.")
  }

  stat <- "serialaxes"
  if(widget['andrews'])
    stat <- "dotProduct"

  axes.layout <- widget['axesLayout']
  axes.sequence <- widget['sequence']
  if(axes.layout == "radial" && stat == "serialaxes") {
    axes.sequence <- c(axes.sequence, axes.sequence[1L])
  }

  if(asAes) {

    color <- l_colorName(
      get_display_color(
        data$color[active_displayOrder],
        data$selected[active_displayOrder]
      ), error = FALSE
    )
    size <- as_ggplot_size(data$linewidth[active_displayOrder], "lines")

    ggObj <- ggplot2::ggplot(data = data) +
      ggmulti::geom_serialaxes(
        data = serialaxes.data[active_displayOrder, ],
        mapping = ggplot2::aes(
          color = color,
          size = size
        ),
        stat = stat,
        axes.sequence = axes.sequence,
        scaling = widget['scaling']
      ) +
      ggplot2::scale_x_continuous(labels = axes.sequence)

    uni_color <- unique(color)
    if(length(uni_color) > 0) {

      ggObj <- ggObj +
        ggplot2::scale_color_manual(values = uni_color,
                                    labels = uni_color,
                                    breaks = uni_color)
    }

    if(length(uni_color) <= 1) {
      ggObj <- ggObj + ggplot2::guides(color = FALSE)
    }

    uni_size <- unique(size)
    if(length(uni_size) > 0) {
      ggObj <- ggObj +
        ggplot2::scale_size(range = range(size[!is.na(size)]))
    }

    if(length(uni_size) <= 1)
      ggObj <- ggObj + ggplot2::guides(size = FALSE)

  } else {

    ggObj <- ggplot2::ggplot(data = data) +
      ggmulti::geom_serialaxes(
        data = serialaxes.data[active_displayOrder, ],
        color = get_display_color(
          loon::as_hex6color(data$color[active_displayOrder]),
          data$selected[active_displayOrder]
        ),
        size = as_ggplot_size(data$linewidth[active_displayOrder], "lines"),
        stat = stat,
        scaling = widget['scaling'],
        axes.sequence = axes.sequence
      ) +
      ggplot2::scale_x_continuous(labels = axes.sequence)
  }

  if(axes.layout == "radial") {

    if(utils::packageVersion("ggmulti") >= "1.0.2") {
      coord_radial <- ggmulti::coord_radial
    } else {
      coord_radial <- ggmulti::coord_radar
    }

    ggObj <- ggObj +
      coord_radial(direction = -1, # anticlock
                   start = 11) # at 11

  }

  # set labels (it is equivalent to set the title)
  ggObj <- ggObj +
    ggplot2::ggtitle(label = if(widget['showLabels']) widget['title'] else "")

  # set themes
  suppressMessages(
    set_serialaxes_themes(
      ggObj = ggObj,
      sequence = widget['sequence'],
      showGuides = widget['showGuides'],
      showAxesLabels = widget['showAxesLabels'],
      showAxes = widget['showAxes']
    )
  )
}

set_serialaxes_themes <- function(ggObj, sequence = NULL,
                                  showGuides = TRUE, showAxesLabels = TRUE,
                                  showAxes = TRUE) {
  if(missing(ggObj))
    stop("ggObj is missing", call. = FALSE)

  if(is.null(sequence))
    return(ggObj)

  len.xaxis <- length(sequence)
  xaxis <- seq(0, 1, length.out = len.xaxis)

  panel_bg_fill <- ifelse(showGuides,
                          loon::l_getOption("canvas_bg_guides"),
                          loon::l_getOption("background"))
  line_color <- ifelse(showGuides,
                       loon::l_getOption("background"),
                       loon::l_getOption("foreground"))

  boundary_lineWidth <- loon_default_setting("boundaryLineWidth")

  ggObj +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text.x = if(showAxesLabels)
        ggplot2::element_text(color = loon::l_getOption("foreground"))
      else ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      plot.title = element_text(hjust = 0.5),
      panel.border = ggplot2::element_blank(),
      panel.grid.major.x = if(showAxes) {
        ggplot2::element_line(color = line_color, size = boundary_lineWidth)
      } else ggplot2::element_blank(),
      panel.grid.major.y = if(showGuides) ggplot2::element_line() else ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = panel_bg_fill),
      plot.margin = grid::unit(c(5,12,5,12), "mm"),
      plot.background = ggplot2::element_rect(fill = panel_bg_fill),
      legend.background = element_rect(fill = panel_bg_fill)
    )
}
