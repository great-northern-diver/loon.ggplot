#' @rdname loon2ggplot
#' @export
loon2ggplot.l_layer_histogram <- function(target, ...) {

  widget <- loon::l_create_handle(attr(target, "widget"))
  ggObj <- list(...)$ggObj

  data <- transform_hist_x(widget)

  ggObj <- ggObj +
    ggplot2::geom_rect(
      data = data.frame(xmin = data$x,
                        ymin = data$y,
                        xmax = data$x + data$width,
                        ymax = data$y + data$height),
      mapping = ggplot2::aes(xmin = xmin,
                             ymin = ymin,
                             xmax = xmax,
                             ymax = ymax),
      fill = data$fill,
      colour = data$colour,
      inherit.aes = FALSE
    )
  return(ggObj)
}


transform_hist_x <- function(widget) {

  yshows <- widget['yshows']
  bins <- loon:::getBinData(widget)

  showStackedColors <- widget['showStackedColors']
  showOutlines <- widget['showOutlines']
  colorOutline <- if(showOutlines) loon:::as_hex6color(widget["colorOutline"]) else NA

  colorStackingOrder <- widget['colorStackingOrder']
  if(length(colorStackingOrder) == 1) {
    if(colorStackingOrder == "selected") {
      colorStackingOrder <- c("selected",
                              levels(as.factor(widget['color'])))
    }
  }

  b_bins <- lapply(bins,
                   function(bin) {

                     nam <- names(bin$count[-1])

                     if (showStackedColors) {
                       first <- intersect(colorStackingOrder, nam)
                       rest <- setdiff(nam, c("all", first))
                       bnames <- c(first, rest)
                       count <- bin$count
                       default_fill <- NULL
                     } else {
                       count <- list()
                       if("selected" %in% nam) {
                         count$selected <- bin$count$selected
                         count$unselected <- bin$count$all - bin$count$selected
                       } else count$unselected <- bin$count$all
                       bnames <- names(count)
                       default_fill <- loon:::as_hex6color(widget['colorFill'])
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
                                      default_fill %||% loon:::as_hex6color(fill)
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
