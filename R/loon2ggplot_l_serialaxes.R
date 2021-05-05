#' @rdname loon2ggplot
#' @export
loon2ggplot.l_serialaxes <- function(target, asAes = TRUE, ...) {

  widget <- target
  remove(target)
  data <- char2num.data.frame(widget['data'])
  colNames <- colnames(data)

  # active or not
  displayOrder <- get_model_display_order(widget)
  active <- widget['active'][displayOrder]
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

    color <- get_display_color(
      as_hex6color(widget['color'][active_displayOrder]),
      widget['selected'][active_displayOrder]
    )
    size <- as_r_line_size(widget['linewidth'][active_displayOrder])

    ggObj <- ggplot2::ggplot(data[active_displayOrder, ]) +
      ggplot2::geom_path(
        mapping = ggplot2::aes(
          color = color,
          size = size
        ),
        stat = stat
      ) +
      ggmulti::coord_serialaxes(direction = -1, # anticlock
                                start = 11, # at 11
                                axes.layout = axes.layout,
                                scaling = widget['scaling'],
                                axes.sequence = axes.sequence) +
      ggplot2::ggtitle(widget['title'])

    uni_color <- unique(color)
    if(length(uni_color) > 0) {

      ggObj <- ggObj +
        ggplot2::scale_color_manual(values = uni_color,
                                    labels = selection_color_labels(uni_color),
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
    ggObj <- ggplot2::ggplot(data[active_displayOrder, ]) +
      ggplot2::geom_path(
        color = get_display_color(
          as_hex6color(widget['color'][active_displayOrder]),
          widget['selected'][active_displayOrder]
        ),
        size = as_r_line_size(widget['linewidth'][active_displayOrder]),
        stat = stat
      ) +
      ggmulti::coord_serialaxes(direction = -1, # anticlock
                                start = 11, # at 11
                                axes.layout = axes.layout,
                                scaling = widget['scaling'],
                                axes.sequence = axes.sequence) +
      ggplot2::ggtitle(widget['title'])
  }

  # set themes
  suppressMessages(
    set_serialaxes_themes(
      ggObj = ggObj,
      sequence = widget['sequence'],
      showLabels =  widget['showLabels'],
      showAxesLabels = widget['showAxesLabels'],
      showGuides = widget['showGuides'],
      showAxes = widget['showAxes']
    )
  )
}

set_serialaxes_themes <- function(ggObj, sequence = NULL,
                                  showLabels = TRUE, showAxesLabels = TRUE,
                                  showGuides = TRUE, showAxes = TRUE) {
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
    )
}
