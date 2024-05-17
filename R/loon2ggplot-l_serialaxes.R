#' @rdname loon2ggplot
#' @export
loon2ggplot.l_serialaxes <- function(target, asAes = TRUE, selectedOnTop = TRUE,
                                     showNearestColor = FALSE, ...) {

  widget <- target

  ggObj <- ggSerialaxes(widget, asAes = asAes,
                        selectedOnTop = selectedOnTop,
                        showNearestColor = showNearestColor,
                        ...)

  if(widget['axesLayout'] == "radial") {

    coord_radial <- function (theta = "x", start = 0, direction = 1, clip = "on") {
      theta <- match.arg(theta, c("x", "y"))
      r <- if (theta == "x")
        "y"
      else "x"
      ggplot2::ggproto(NULL, ggplot2::CoordPolar, theta = theta,
                       r = r, start = start, direction = sign(direction), clip = clip,
                       is_linear = function(coord) TRUE)
    }

    ggObj <- ggObj +
      coord_radial(direction = -1, # anticlock
                   start = 11) # at 11

  } else {
    ggObj <- ggObj +
      ggplot2::coord_cartesian(expand = FALSE)
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

ggSerialaxes <- function(widget, asAes = TRUE, selectedOnTop = TRUE,
                         showNearestColor = FALSE, ...) {

  args <- list(...)
  ggObj <- args$ggObj
  facets <- args$facets
  facetsLabels <- args$facetsLabels
  levels <- args$levels

  if(is.null(facets)) {

    n <- widget['n']
    if(n == 0) return(ggplot2::ggplot())

    data <- serialaxes_states(widget = widget,
                              selectedOnTop = selectedOnTop,
                              showNearestColor = showNearestColor)
    axes.sequence <- widget['sequence']

  } else {

    n <- sum(vapply(facets, function(facet) facet['n'], numeric(1L)))
    if(n == 0) return(ggObj)

    facetNum <- nrow(facetsLabels)
    facetsVar <- rownames(facetsLabels)

    axes.sequence <- widget['sequence']
    if (length(axes.sequence) == 0) {
      for(facet in facets) {
        axes.sequence <- facet['sequence']
        if (length(axes.sequence) > 0) break
      }
    }

    data <- do.call(rbind,
                    lapply(seq_along(facets),
                           function(i) {
                             facet <- facets[[i]]
                             dat <- serialaxes_states(widget = facet,
                                                      selectedOnTop = selectedOnTop,
                                                      showNearestColor = showNearestColor,
                                                      axes.sequence = axes.sequence)

                             do.call(cbind,
                                     c(list(dat),
                                       stats::setNames(as.list(facetsLabels[, i]),
                                                       facetsVar),
                                       facetGroup = i))
                           })
    )
  }

  if(widget['showArea']) {
    message("`showArea` is not implemented yet.")
  }

  stat <- "serialaxes"
  if(widget['andrews'])
    stat <- "dotProduct"

  axes.layout <- widget['axesLayout']
  if(axes.layout == "radial" && stat == "serialaxes") {
    axes.sequence <- c(axes.sequence, axes.sequence[1L])
  }

  scaling <- widget['scaling']
  color <- na.omit(data$color)
  size <- na.omit(data$size)

  if(asAes) {

    ggObj <- ggplot2::ggplot(data = data) +
      ggmulti::geom_serialaxes(
        mapping = ggplot2::aes(
          color = color,
          size = size
        ),
        axes.sequence = axes.sequence, # used as mapping
        stat = stat,
        scaling = scaling
      ) +
      ggplot2::scale_x_continuous(labels = axes.sequence,
                                  breaks = seq_along(axes.sequence))

    # modify color guides
    uni_color <- unique(color)
    if(length(uni_color) > 0) {

      ggObj <- ggObj +
        ggplot2::scale_color_manual(values = uni_color,
                                    labels = uni_color,
                                    breaks = uni_color)
    }
    if(length(uni_color) <= 1) {
      ggObj <- ggObj + ggplot2::guides(color = "none")
    }

    # modify size guides
    uni_size <- unique(size)
    if(length(uni_size) > 0) {
      ggObj <- ggObj + ggplot2::scale_size(range = range(size[!is.na(size)]))
    }

    if(length(uni_size) <= 1) {
      ggObj <- ggObj + ggplot2::guides(size = "none")
    }
  } else {


    if(len_unique(color) == 1L) color <- color[1L]
    if(len_unique(size) == 1L) size <- size[1L]

    ggObj <- ggplot2::ggplot(data = data) +
      ggmulti::geom_serialaxes(
        color = color,
        size = size,
        stat = stat,
        scaling = scaling,
        axes.sequence = axes.sequence # used as mapping
      ) +
      ggplot2::scale_x_continuous(labels = axes.sequence)
  }

  return(ggObj)
}

serialaxes_states <- function(widget, selectedOnTop = TRUE, showNearestColor = FALSE,
                              axes.sequence = c()) {

  n <- widget['n']
  if (n == 0 || !any(widget['active'])) {
    serialaxes.data <- as.data.frame(stats::setNames(lapply(axes.sequence,
                                                            function(x) NA),
                                                     axes.sequence))

    ndimNames <- loon::l_nDimStateNames(widget)
    states <- as.data.frame(stats::setNames(lapply(ndimNames,
                                                   function(x) NA),
                                            ndimNames))
    states$data <- NULL
    return(
      cbind(states, serialaxes.data)
    )
  }

  # active or not
  displayOrder <- if(selectedOnTop) {
    get_model_display_order(widget)
  } else {
    seq(n)
  }

  # We do not call `get_layer_states(widget, native_unit = FALSE)`
  # Because, `loon::l_nDimStateNames` will return all n dimensional states,
  # but the `get_layer_states` only return the n dimensional aesthetics attributes
  # e.g, `itemLabels` will not be returned.
  # `data` is used in `ggplot()`
  # N dim names
  ndimNames <- loon::l_nDimStateNames(widget)

  states <- as.data.frame(
    remove_null(
      stats::setNames(
        lapply(ndimNames,
               function(s) {
                 if(s == "data") return(NULL)
                 state <- widget[s]
                 state
               }),
        ndimNames
      ), as_list = FALSE)
  )
  serialaxes.data <- char2num.data.frame(widget['data'])
  # merge states and serialaxes.data
  data <- cbind(states, serialaxes.data)

  # modify color
  color <- l_colorName(
    get_display_color(
      data$color,
      data$selected
    ), error = FALSE,
    precise = !showNearestColor
  )
  data$color <- color
  # modify size
  size <- as_ggplot_size(data$linewidth, "lines")
  data$size <- size
  # reorder
  data <- data[displayOrder, ]
  # only visualize active points
  data <- data[data$active, ]

  return(data)
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
      plot.background = ggplot2::element_rect(fill = panel_bg_fill),
      legend.background = element_rect(fill = panel_bg_fill)
    )
}
