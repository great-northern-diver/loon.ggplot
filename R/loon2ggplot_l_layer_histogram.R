#' @rdname loon2ggplot
#' @export
loon2ggplot.l_layer_histogram <- function(target, asAes = TRUE, selectedOnTop = TRUE, ...) {

  widget <- loon::l_create_handle(attr(target, "widget"))
  ggObj <- list(...)$ggObj

  if(asAes) {

    data <- histogramAsAesTRUE(widget)

    fill <- data$fill
    color <- data$colour
    x <- data$x
    values <- data$values

    ggObj <- ggObj +
      ggplot2::geom_histogram(
        data = data.frame(x = x, fill = fill),
        mapping = if(widget['yshows'] == "frequency") {
          ggplot2::aes(x = x, fill = fill)
        } else {
          ggplot2::aes(x = x, fill = fill,
                       y = ..density..) # the layout would be different from the loon one
        },
        colour = color,
        boundary = widget['origin'],
        binwidth = widget['binwidth'],
        inherit.aes = FALSE
      ) +
      ggplot2::scale_fill_manual(
        values = values,
        breaks = values,
        labels = values)

    uniFill <- unique(fill[!is.na(fill)])

    if(length(uniFill) <= 1) {
      ggObj <- ggObj + ggplot2::guides(color = FALSE, fill = FALSE)
    }

  } else {

    data <- histogramAsAesFALSE(widget)

    xmin <- data$x
    ymin <- data$y
    xmax <- data$x + data$width
    ymax <- data$y + data$height
    fill <- data$fill
    colour <- data$colour

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
        colour = colour,
        inherit.aes = FALSE
      )
  }
  return(ggObj)
}

histogramAsAesTRUE <- function(widget) {

  colorOutline <- if(widget['showOutlines']) as_hex6color(widget["colorOutline"]) else NA
  states <- get_layer_states(widget, native_unit = FALSE)

  active <- states$active
  activeSelected <- states$selected[active]
  activeColor <- if(widget['showStackedColors']) {
    states$color[active]
  } else {rep(as_hex6color(widget['colorFill']), widget['n'])[active]}

  selectcolor <- loon::l_getOption("select-color")
  activeColor[activeSelected] <- selectcolor
  activeX <- states$x[active]

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
  values <- l_colorName(values, error = FALSE)
  activeColor <- l_colorName(activeColor, error = FALSE)
  activeColor <- factor(activeColor, levels = values)

  return(
    list(
      x = activeX,
      fill = activeColor,
      colour = colorOutline,
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
                              colour = colorOutline,
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
                     colour,
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
                                        colour = colour
                                      )
                                    )
                                  })
  )

  return(bin_dataFrame)
}
