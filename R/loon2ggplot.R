#' @title \code{loon} to \code{ggplot}
#'
#' @description Create a \code{ggplot} object from a \code{loon} widget
#'
#' @param target a\code{loon} or a vector that specifies the
#' widget, layer, glyph, navigator or context completely.
#' The widget is specified by the widget path name (e.g. '.l0.plot'),
#' the remaining objects by their ids.
#' @param ... arguments used inside \code{loon2ggplot()}, not used by this method
#'
#' @return a ggplot object
#'
#' @export
#' @examples
#' l <- l_plot(iris, color = iris$Species)
#' p <- loon2ggplot(l)
#' p # a ggplot object
#' str(p)
#' # add themes
#' p + geom_smooth() + theme_linedraw()
#'
loon2ggplot <- function(target, ...) {

  if(ggplot2::is.ggplot(target) || is.ggmatrix(target)) {
    error_info <- deparse(substitute(target))
    stop(
      paste0(
        "Target should be a loon object. ",
        "Maybe you want to call `ggplot2loon(",
        error_info,
        ")`?\n",
        "Or, just call `loon.ggplot(`",
        error_info,
        ")` for simplification."
      ),
      call. = FALSE
    )
  }
  UseMethod('loon2ggplot', target)
}

#' @export
#' @rdname loon2ggplot
loon2ggplot.default <- function(target, ...) {
  # stop('loon2ggplot default no valid inheritance')
  # TODO
  ggObj <- list(...)$ggObj
  ggObj
}

#' @rdname loon2ggplot
#' @export
loon2ggplot.l_plot <- function(target, ...) {

  loon::l_isLoonWidget(target) || stop(target, " does not exist")
  rl <- loon::l_create_handle(c(target, 'root'))
  cartesian_gg(target = target,
               ggObj = loon2ggplot(rl))
}

#' @rdname loon2ggplot
#' @export
loon2ggplot.l_hist <- function(target, ...) {

  loon::l_isLoonWidget(target) || stop("widget does not seem to exist")
  rl <- loon::l_create_handle(c(target, 'root'))
  cartesian_gg(target = target,
               ggObj = loon2ggplot(rl))
}

#' @export
loon2ggplot.l_graph <- function(target, ...) {

  loon::l_isLoonWidget(target) || stop("widget does not seem to exist")
  rl <- loon::l_create_handle(c(target, 'root'))
  cartesian_gg(target = target,
               ggObj = loon2ggplot(rl))
}

#' @rdname loon2ggplot
#' @export
loon2ggplot.l_plot3D <- function(target, ...) {

  loon::l_isLoonWidget(target) || stop("widget does not seem to exist")
  rl <- loon::l_create_handle(c(target, 'root'))

  axes_coords <- target["axesCoords"]

  adjust_brightness <- function(z_coord, r, g, b) {
    change <- as.integer(100 + 80 * z_coord)
    if (change < 0) {
      grDevices::rgb(0,0,0)
    } else if (change <= 100) {
      grDevices::rgb((r/256) * change/100, (g/256) * change/100, (b/256) * change/100)
    } else {
      grDevices::rgb(r,g,b, maxColorValue=255)
    }
  }

  x_color <- adjust_brightness(axes_coords[[3]][1], 255, 0, 0)
  y_color <- adjust_brightness(axes_coords[[3]][2], 0, 0, 255)
  z_color <- adjust_brightness(axes_coords[[3]][3], 0, 255, 0)

  x <- c(0.5, 0.5 + 0.08*axes_coords[[1]][1])
  y <- c(0.5, 0.5 + 0.08*axes_coords[[2]][1])

  cartesian_gg(target = target,
               ggObj = loon2ggplot(rl)) +
    ggplot2::geom_line(
      data = data.frame(
        x = x,
        y = y
      ),
      mapping = ggplot2::aes(x = x, y = y),
      colour = x_color,
      size = 1,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_line(
      data = data.frame(
        x = c(0.5, 0.5 + 0.08*axes_coords[[1]][2]),
        y = c(0.5, 0.5 + 0.08*axes_coords[[2]][2])
      ),
      mapping = ggplot2::aes(x = x, y = y),
      colour = y_color,
      size = 1,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_line(
      data = data.frame(
        x = c(0.5, 0.5 + 0.08*axes_coords[[1]][3]),
        y = c(0.5, 0.5 + 0.08*axes_coords[[2]][3])
      ),
      mapping = ggplot2::aes(x = x, y = y),
      colour = z_color,
      size = 1,
      inherit.aes = FALSE
    )
}

cartesian_gg <- function(target, ggObj) {

  # keep name consistency
  widget <- target

  panX <- widget['panX']
  deltaX <- widget['deltaX']
  zoomX <- widget['zoomX']
  xlim <- c(panX, panX + deltaX/zoomX)

  panY <- widget['panY']
  deltaY <- widget['deltaY']
  zoomY <- widget['zoomY']
  ylim <- c(panY, panY + deltaY/zoomY)

  swapAxes <- widget['swapAxes']
  showScales <- widget['showScales']

  showLabels <- widget['showLabels']
  showGuides <- widget['showGuides']

  title <- loon_title(widget)
  xlabel <- widget['xlabel']
  ylabel <- widget['ylabel']

  minimumMargins <- widget['minimumMargins']
  margins <- c(0, 0, 0, 0)
  if (showLabels) {
    labelMargins <- widget['labelMargins']
    if(xlabel == "") labelMargins[1] <- minimumMargins[1]
    if(ylabel == "") labelMargins[2] <- minimumMargins[2]
    if(title == "") labelMargins[3] <- minimumMargins[3]
    margins <- margins + labelMargins
  }
  if (showScales) {margins <- margins + widget['scalesMargins'] }
  if(showLabels | showScales) {
    margins <- apply(cbind(margins, minimumMargins), 1, max)
  }
  # loon pixel margin to grid margin
  margins <- pixels_2_lines(margins)

  xlabelFont <- get_font_info_from_tk(loon::l_getOption("font-xlabel"))
  ylabelFont <- get_font_info_from_tk(loon::l_getOption("font-ylabel"))
  titleFont <- get_font_info_from_tk(loon::l_getOption("font-title"))
  scalesFont <- get_font_info_from_tk(loon::l_getOption("font-scales"))

  guidesBackGround <- if(showGuides) as_hex6color(widget['guidesBackground']) else as_hex6color(widget['background'])

  ggObj <- ggObj +
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim) +
    ggplot2::xlab(xlabel) +
    ggplot2::ylab(ylabel) +
    ggplot2::ggtitle(label = if(title == "") NULL else title) +
    ggplot2::theme(
      axis.ticks = if(showScales) ggplot2::element_line() else ggplot2::element_blank(),
      axis.text.x = if(showScales) ggplot2::element_text(size = scalesFont$size, family = scalesFont$family, face = scalesFont$face) else ggplot2::element_blank(),
      axis.text.y = if(showScales) ggplot2::element_text(size = scalesFont$size, family = scalesFont$family, face = scalesFont$face) else ggplot2::element_blank(),
      axis.title = if(showLabels) ggplot2::element_text(size = titleFont$size, family = titleFont$family, face = titleFont$face) else ggplot2::element_blank(),
      axis.title.x = if(showLabels) ggplot2::element_text(size = xlabelFont$size, family = xlabelFont$family, face = xlabelFont$face) else ggplot2::element_blank(),
      axis.title.y = if(showLabels) ggplot2::element_text(size = ylabelFont$size, family = ylabelFont$family, face = ylabelFont$face) else ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = as_hex6color(widget['background'])),
      plot.title = ggplot2::element_text(hjust = 0.5),
      panel.background = ggplot2::element_rect(fill = guidesBackGround),
      panel.grid.major = ggplot2::element_line(size = 1,
                                               linetype = 'solid',
                                               colour = as_hex6color(widget['guidelines'])),
      panel.grid.minor = ggplot2::element_line(size = 0.5,
                                               linetype = 'solid',
                                               colour = as_hex6color(widget['guidelines'])),
      panel.border = if(sum(margins) > 0)
        ggplot2::element_rect(colour = as_hex6color(widget['foreground']),
                              fill = NA,
                              size = 1) else ggplot2::element_blank(),
      plot.margin = grid::unit(margins, "lines")
    )
  if(swapAxes) ggObj <- ggObj + coord_flip()
  return(ggObj)
}

#' @export
loon2ggplot.l_layer_group <- function(target, ...) {

  widget <- l_create_handle(attr(target, "widget"))
  ggObj <- ggplot2::ggplot()

  children <- l_layer_getUngroupedChildren(widget = widget, target = widget)
  l_children_layers <- lapply(rev(children),
                              function(layerid) {

                                layer <- loon::l_create_handle(c(widget, layerid))

                                if(layerid == 'model') {

                                  states <- get_layer_states(widget, native_unit = FALSE)

                                  if (length(states$x) == length(states$y)) {
                                    # scatter plot
                                    swapAxes <- widget["swapAxes"]
                                    if(swapAxes) {
                                      y <- states$x
                                      x <- states$y
                                    } else {
                                      x <- states$x
                                      y <- states$y
                                    }
                                    ggObj <<- ggplot2::ggplot(data = data.frame(x = x, y = y),
                                                              mapping = ggplot2::aes(x = x, y = y))
                                  } else if(length(states$y) == 1 && is.na(states$y)) {
                                    # histogram
                                    ggObj <<- ggplot2::ggplot(data = data.frame(x = states$x),
                                                              mapping = ggplot2::aes(x = x))
                                  } else NULL
                                }

                                layer
                              })

  l_visible_children_layer <- Filter(
    function(layerid) {
      loon::l_layer_isVisible(widget, layerid)
    },
    l_children_layers)
  lapply(l_visible_children_layer,
         function(layer) {

           ggObj <<- loon2ggplot(layer, ggObj = ggObj)
         })

  ggObj
}

# primitive ggplot layers
#' @export
loon2ggplot.l_layer_polygon <- function(target, ...) {

  widget <- l_create_handle(attr(target, "widget"))
  swapAxes <- widget["swapAxes"]
  states <- get_layer_states(target, native_unit = FALSE)

  ggObj <- list(...)$ggObj

  if(length(states$x) > 0  & length(states$y) > 0) {

    x <- if(swapAxes) states$y else states$x
    y <- if(swapAxes) states$x else states$y

    ggObj <- ggObj +
      ggplot2::geom_polygon(
        data = data.frame(
          x = x, y = y
        ),
        mapping = ggplot2::aes(x = x, y = y),
        fill = states$color,
        colour = states$linecolor,
        size =  as_r_line_size(states$linewidth),
        inherit.aes = FALSE
      )
  }

  ggObj
}

#' @export
loon2ggplot.l_layer_line <- function(target, ...) {

  widget <- l_create_handle(attr(target, "widget"))
  swapAxes <- widget["swapAxes"]
  states <- get_layer_states(target, native_unit = FALSE)

  ggObj <- list(...)$ggObj

  if(length(states$x) > 0  & length(states$y) > 0) {

    x <- if(swapAxes) states$y else states$x
    y <- if(swapAxes) states$x else states$y

    ggObj <- ggObj +
      ggplot2::geom_line(
        data = data.frame(
          x = x,
          y = y
        ),
        mapping = ggplot2::aes(x = x, y = y),
        colour = states$color,
        size = as_r_line_size(states$linewidth),
        inherit.aes = FALSE
      )
  }

  ggObj
}

#' @export
loon2ggplot.l_layer_rectangle <- function(target, ...) {

  widget <- l_create_handle(attr(target, "widget"))
  swapAxes <- widget["swapAxes"]
  states <- get_layer_states(target, native_unit = FALSE)

  ggObj <- list(...)$ggObj

  if(length(states$x) > 0  & length(states$y) > 0) {

    x <- if(swapAxes) states$y else states$x
    y <- if(swapAxes) states$x else states$y

    ggObj <- ggObj +
      ggplot2::geom_rect(
        data = data.frame(
          x = x,
          y = y
        ),
        mapping = ggplot2::aes(xmin = x[1], xmax = x[2], ymin = y[1], ymax = y[2]),
        colour = states$linecolor,
        fill = states$color,
        size =  as_r_line_size(states$linewidth),
        inherit.aes = FALSE
      )

  }

  ggObj
}
#' @export
loon2ggplot.l_layer_oval <- function(target, ...) {

  widget <- l_create_handle(attr(target, "widget"))
  swapAxes <- widget["swapAxes"]
  states <- get_layer_states(target, native_unit = FALSE)

  ggObj <- list(...)$ggObj

  if(length(states$x) > 0  & length(states$y) > 0){

    xcoords <- if(swapAxes) states$y else states$x
    ycoords <- if(swapAxes) states$x else states$y

    angle <- seq(0, 2*base::pi, length=101)

    xCenter <- mean(xcoords)
    yCenter <- mean(ycoords)

    xRadius <- diff(range(xcoords))/2
    yRadius <- diff(range(ycoords))/2

    x <- mean(xcoords) + xRadius * cos(angle)
    y <- mean(ycoords) + yRadius * sin(angle)

    ggObj <- ggObj +
      ggplot2::geom_polygon(
        data = data.frame(
          x = x,
          y = y
        ),
        mapping = ggplot2::aes(x = x, y = y),
        fill = states$color,
        colour = states$linecolor,
        size = as_r_line_size(states$linewidth),
        inherit.aes = FALSE
      )
  }

  ggObj
}
#' @export
loon2ggplot.l_layer_text <- function(target, ...) {

  widget <- l_create_handle(attr(target, "widget"))
  swapAxes <- widget["swapAxes"]
  states <- get_layer_states(target, native_unit = FALSE)
  textCoords <- get_textCoords(angle = states$angle, anchor = states$anchor, just = states$just)

  ggObj <- list(...)$ggObj

  if(length(states$x) > 0  & length(states$y) > 0){

    x <- if(swapAxes) states$y else states$x
    y <- if(swapAxes) states$x else states$y
    label <- states$text

    ggObj <- ggObj +
      ggplot2::geom_text(
        data = data.frame(
          x = x + textCoords[1],
          y = y + textCoords[2],
          label =label
        ),
        mapping = ggplot2::aes(x = x, y = y, label = label),
        angle = states$angle,
        colour = states$color,
        hjust = get_hjust(states$justify),
        size = as_r_text_size(states$size),
        inherit.aes = FALSE
      )
  }

  ggObj
}
#' @export
loon2ggplot.l_layer_texts <- function(target, ...) {

  widget <- l_create_handle(attr(target, "widget"))
  swapAxes <- widget["swapAxes"]
  states <- get_layer_states(target, native_unit = FALSE)

  active <- states$active
  x <- if(swapAxes) states$y[active] else states$x[active]
  y <- if(swapAxes) states$x[active] else states$y[active]

  ggObj <- list(...)$ggObj

  if(length(x) > 0  & length(y) > 0){

    label  <- states$text[active]
    size  <- as_r_text_size(states$size[active])
    angle  <- states$angle[active]
    anchor  <- states$anchor[active]
    justify  <- states$justify[active]
    color <- states$color[active]

    data <- lapply(1:length(label),
                   function(i){
                     textCoords <- get_textCoords(angle = angle[i],
                                                  anchor = anchor[i],
                                                  just = justify[i])

                     c(label = label[i],
                       x = x[i] + textCoords[1],
                       y = y[i] + textCoords[2])
                   })

    df <- as.data.frame(
      do.call(rbind, data),
      stringsAsFactors = FALSE
    )
    df$x <- as.numeric(df$x)
    df$y <- as.numeric(df$y)

    ggObj <- ggObj +
      ggplot2::geom_text(
        data = df,
        mapping = ggplot2::aes(x = x, y = y, label = label),
        angle = angle,
        colour = color,
        hjust = get_hjust(justify),
        size = as_r_text_size(size),
        inherit.aes = FALSE
      )
  }

  ggObj
}
#' @export
loon2ggplot.l_layer_points <- function(target, ...) {
  widget <- l_create_handle(attr(target, "widget"))
  swapAxes <- widget["swapAxes"]
  states <- get_layer_states(target, native_unit = FALSE)

  active <- states$active
  x <- if(swapAxes) states$y[active] else states$x[active]
  y <- if(swapAxes) states$x[active] else states$y[active]

  ggObj <- list(...)$ggObj

  if(length(x) > 0  && length(y)  > 0) {
    size  <- as_r_point_size(states$size[active])
    color <- states$color[active]

    ggObj <- ggObj +
      ggplot2::geom_point(
        data = data.frame(x = x, y = y),
        mapping = ggplot2::aes(x = x, y = y),
        colour = color,
        size = size,
        pch = 16,
        inherit.aes = FALSE
      )
  }

  ggObj
}
#' @export
loon2ggplot.l_layer_polygons <- function(target, ...) {

  widget <- l_create_handle(attr(target, "widget"))
  swapAxes <- widget["swapAxes"]
  states <- get_layer_states(target, native_unit = FALSE)

  active <- states$active
  x <- if(swapAxes) states$y[active] else states$x[active]
  y <- if(swapAxes) states$x[active] else states$y[active]

  ggObj <- list(...)$ggObj

  if(length(x) > 0  & length(y) > 0){

    linewidth  <- as_r_line_size(states$linewidth[active])
    linecolor <- states$linecolor[active]
    fill <- states$color[active]

    len_x <- lengths(x)

    group <- as.factor(rep(1:length(len_x), times = len_x))
    df <- data.frame(
      x = as.numeric(unlist(x)),
      y = as.numeric(unlist(y)),
      group = group
    )

    ggObj <- ggObj +
      ggplot2::geom_polygon(
        data =df,
        mapping = ggplot2::aes(x = x, y = y, group = group),
        fill = rep(fill, times = len_x),
        colour = rep(linecolor, times = len_x),
        size = rep(linewidth, times = len_x),
        inherit.aes = FALSE
      )
  }

  ggObj
}
#' @export
loon2ggplot.l_layer_rectangles <- function(target, ...) {

  widget <- l_create_handle(attr(target, "widget"))
  swapAxes <- widget["swapAxes"]
  states <- get_layer_states(target, native_unit = FALSE)

  active <- states$active
  x <- if(swapAxes) states$y[active] else states$x[active]
  y <- if(swapAxes) states$x[active] else states$y[active]

  ggObj <- list(...)$ggObj

  if(length(x) > 0  & length(y) > 0) {

    xmin <- as.numeric(sapply(x, function(xx) xx[1]))
    xmax <- as.numeric(sapply(x, function(xx) xx[2]))
    ymin <- as.numeric(sapply(y, function(yy) yy[1]))
    ymax <- as.numeric(sapply(y, function(yy) yy[2]))

    df <- data.frame(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax
    )

    ggObj <- ggObj +
      ggplot2::geom_rect(
        data =df,
        mapping = ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
        fill = states$color[active],
        colour = states$linecolor[active],
        size = as_r_line_size(states$linewidth[active]),
        inherit.aes = FALSE
      )
  }

  ggObj
}
#' @export
loon2ggplot.l_layer_lines <- function(target, ...) {

  widget <- l_create_handle(attr(target, "widget"))
  swapAxes <- widget["swapAxes"]
  states <- get_layer_states(target, native_unit = FALSE)

  active <- states$active
  x <- if(swapAxes) states$y[active] else states$x[active]
  y <- if(swapAxes) states$x[active] else states$y[active]

  ggObj <- list(...)$ggObj

  if(length(x) > 0  & length(y) > 0){

    linewidth  <- as_r_line_size(states$linewidth[active])
    linecolor <- states$color[active]

    len_x <- lengths(x)

    group <- as.factor(rep(1:length(len_x), times = len_x))

    df <- data.frame(
      x = as.numeric(unlist(x)),
      y = as.numeric(unlist(y)),
      group = group
    )

    ggObj <- ggObj +
      ggplot2::geom_line(
        data =df,
        mapping = ggplot2::aes(x = x, y = y, group = group),
        colour = rep(linecolor, times = len_x),
        size = rep(linewidth, times = len_x),
        inherit.aes = FALSE
      )
  }

  ggObj
}
