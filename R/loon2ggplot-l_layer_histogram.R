#' @rdname loon2ggplot
#' @export
loon2ggplot.l_layer_histogram <- function(target, asAes = TRUE, selectedOnTop = TRUE,
                                          showNearestColor = FALSE, ...) {

  widget <- loon::l_create_handle(attr(target, "widget"))
  args <- list(...)
  facets <- args$facets

  if(is.null(facets)) {
    ggObj <- ggHistogram(widget, asAes = asAes, selectedOnTop = selectedOnTop,
                         showNearestColor = showNearestColor, ...)
  } else {
    ggObj <- ggHistogramFacets(widget, asAes = asAes, selectedOnTop = selectedOnTop,
                               showNearestColor = showNearestColor, ...)
  }
  return(ggObj)
}

ggHistogramFacets <- function(widget, asAes = TRUE, selectedOnTop = TRUE,
                              showNearestColor = FALSE, ...) {

  args <- list(...)
  facets <- args$facets
  facetsLabels <- args$facetsLabels
  levels <- args$levels
  ggObj <- args$ggObj

  facetNum <- nrow(facetsLabels)
  facetsVar <- rownames(facetsLabels)

  if(!asAes) {
    stop("Once `asAes` is set as FALSE, the ggplot histogram is constructed via `geom_rect()`.",
         "Therefore, facets are not allowed.", call. = FALSE)
  }

  color <- c()
  values <- c()
  states <- list()
  for(i in seq_along(facets)) {
    facet <- facets[[i]]
    data <- histogramAsAesTRUE(facet, showNearestColor)
    color <- c(color, data$color)
    values <- c(values, data$values)
    histData <- data.frame(
      x = data$x, fill = data$fill
    )
    states <- c(states,
                list(do.call(cbind,
                             c(list(histData),
                               stats::setNames(as.list(facetsLabels[, i]),
                                               facetsVar),
                               facetGroup = i)))
    )
  }
  states <- do.call(rbind, states)
  for (i in seq_along(facetsVar)) {
    states[[facetsVar[i]]] <- factor(states[[facetsVar[i]]], levels = levels[[i]])
  }

  color <- na.omit(color)
  if(len_unique(color) == 0L) {
    color <- NA
  } else if(len_unique(color) == 1L) {
    color <- color[1L]
  }

  values <- unique(na.omit(values))

  ggObj <- ggObj +
    ggplot2::geom_histogram(
      data = states,
      mapping = if(widget['yshows'] == "frequency") {
        ggplot2::aes(x = x, fill = fill)
      } else {
        ggplot2::aes(x = x, fill = fill,
                     y = ..density..)
        # the layout would be different from the loon one
      },
      color = color,
      boundary = widget['origin'],
      binwidth = widget['binwidth'],
      inherit.aes = FALSE,
      closed = "left"
    ) +
    ggplot2::scale_fill_manual(
      values = values,
      breaks = values,
      labels = values)

  fill <- states$fill
  uniFill <- unique(fill[!is.na(fill)])
  if(length(uniFill) <= 1)
    ggObj <- ggObj + ggplot2::guides(color = "none", fill = "none")

  return(ggObj)
}

ggHistogram <- function(widget, asAes = TRUE, selectedOnTop = TRUE,
                        showNearestColor = FALSE, ...) {

  args <- list(...)
  ggObj <- args$ggObj
  n <- widget['n']
  if(n == 0) return(ggObj)

  if(asAes) {

    data <- histogramAsAesTRUE(widget, showNearestColor)

    fill <- data$fill
    color <- data$color
    if(len_unique(color) == 1L) color <- color[1L]

    x <- data$x
    values <- data$values

    ggObj <- ggObj +
      ggplot2::geom_histogram(
        data = data.frame(x = x, fill = fill),
        mapping = if(widget['yshows'] == "frequency") {
          ggplot2::aes(x = x, fill = fill)
        } else {
          ggplot2::aes(x = x, fill = fill,
                       y = ..density..)
          # the layout would be different from the loon one
        },
        color = color,
        boundary = widget['origin'],
        binwidth = widget['binwidth'],
        inherit.aes = FALSE,
        closed = "left"
      ) +
      ggplot2::scale_fill_manual(
        values = values,
        breaks = values,
        labels = values)

    uniFill <- unique(fill[!is.na(fill)])
    if(length(uniFill) <= 1)
      ggObj <- ggObj + ggplot2::guides(color = "none", fill = "none")

  } else {

    data <- histogramAsAesFALSE(widget)

    xmin <- data$x
    ymin <- data$y
    xmax <- data$x + data$width
    ymax <- data$y + data$height

    fill <- data$fill
    if(len_unique(fill) == 1L) fill <- fill[1L]

    color <- data$color
    if(len_unique(color) == 1L) color <- color[1L]

    if(!selectedOnTop)
      warning("For histogram, if the `asAes` is set as FALSE, the plot will be created by adding layer `geom_rect()`. ",
              "It is pointless to set the `selectedOnTop` as FALSE",
              call. = FALSE)

    ggObj <- ggObj +
      ggplot2::geom_rect(
        data = data.frame(xmin = xmin,
                          ymin = ymin,
                          xmax = xmax,
                          ymax = ymax),
        mapping = ggplot2::aes(xmin = xmin,
                               ymin = ymin,
                               xmax = xmax,
                               ymax = ymax),
        fill = fill,
        color = color,
        inherit.aes = FALSE
      )
  }

  return(ggObj)
}

histogramAsAesTRUE <- function(widget, showNearestColor = FALSE) {

  if (widget['n'] == 0 || !any(widget['active'])) {
    return(
      list(
        x = NA,
        fill = NA,
        color = NA,
        values = NA
      )
    )
  }
  states <- get_layer_states(widget, native_unit = FALSE)
  active <- states$active
  colorOutline <- if(widget['showOutlines']) as_hex6color(widget["colorOutline"]) else NA
  activeSelected <- states$selected[active]
  activeColor <- if(widget['showStackedColors']) {
    states$color[active]
  } else {rep(as_hex6color(widget['colorFill']), widget['n'])[active]}

  selectcolor <- loon::l_getOption("select-color")
  activeColor[activeSelected] <- selectcolor
  activeX <- if(widget["swapAxes"]) states$y[active] else states$x[active]

  uniqueCol <- rev(levels(factor(activeColor)))
  colorStackingOrder <- widget['colorStackingOrder']

  if(any(activeSelected)) {

    if(length(colorStackingOrder) == 1 && colorStackingOrder == "selected") {
      # selected color should be stacked at the bottom
      uniqueCol <- uniqueCol[uniqueCol != selectcolor]
      values <- c(uniqueCol, selectcolor)
    } else {
      colorStackingOrder[colorStackingOrder == "selected"] <- selectcolor
      values <- rev(as_hex6color(colorStackingOrder))
    }

  } else {

    if(length(colorStackingOrder) == 1 && colorStackingOrder == "selected") {
      values <- uniqueCol
    } else {
      colorStackingOrder <- colorStackingOrder[colorStackingOrder != "selected"]
      values <- rev(as_hex6color(colorStackingOrder))
    }
  }

  # preserve the order
  values <- l_colorName(values, error = FALSE, precise = !showNearestColor)
  activeColor <- l_colorName(activeColor, error = FALSE, precise = !showNearestColor)
  activeColor <- factor(activeColor, levels = values)

  return(
    list(
      x = activeX,
      fill = activeColor,
      color = colorOutline,
      values = values
    )
  )
}

histogramAsAesFALSE <- function(widget) {

  yshows <- widget['yshows']
  bins <- l_getBinData(widget)

  showStackedColors <- widget['showStackedColors']
  showOutlines <- widget['showOutlines']
  colorOutline <- if(showOutlines) as_hex6color(widget["colorOutline"]) else NA

  colorStackingOrder <- widget['colorStackingOrder']
  if(length(colorStackingOrder) == 1) {
    if(colorStackingOrder == "selected") {
      colorStackingOrder <- c("selected",
                              levels(as.factor(widget['color'])))
    }
  }

  b_bins <- lapply(bins,
                   function(bin) {

                     name <- names(bin$count[-1])

                     if (showStackedColors) {
                       first <- intersect(colorStackingOrder, name)
                       rest <- setdiff(name, c("all", first))
                       bnames <- c(first, rest)
                       count <- bin$count
                       default_fill <- NULL
                     } else {
                       count <- list()
                       if("selected" %in% name) {
                         count$selected <- bin$count$selected
                         count$unselected <- bin$count$all - bin$count$selected
                       } else count$unselected <- bin$count$all
                       bnames <- names(count)
                       default_fill <- as_hex6color(widget['colorFill'])
                     }

                     bin_data(count = count,
                              fills = bnames,
                              x = bin$x0,
                              width = bin$x1 - bin$x0,
                              color = colorOutline,
                              default_fill = default_fill)
                   })

  data <- do.call(rbind, b_bins)
  return(data2List(data))
}

data2List <- function(data) {
  stats::setNames(
    lapply(1:dim(data)[2],
           function(i) unlist(data[,i])),
    colnames(data)
  )

}

bin_data <- function(count,
                     fills,
                     x,
                     width,
                     color,
                     default_fill = NULL) {
  y0 <- 0
  bin_dataFrame <- do.call(rbind,
                           lapply(1:length(fills),
                                  function(i) {
                                    fill <- fills[i]
                                    height <- count[[fill]]

                                    fill <- if (fill == "selected") {
                                      loon::l_getOption("select-color")
                                    } else {
                                      default_fill %||% as_hex6color(fill)
                                    }
                                    y <- y0
                                    y0 <<- y0 + height

                                    return(
                                      list(
                                        x = x,
                                        y = y,
                                        width = width,
                                        height = height,
                                        fill = fill,
                                        color = color
                                      )
                                    )
                                  })
  )

  return(bin_dataFrame)
}
