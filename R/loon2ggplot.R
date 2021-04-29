#' @title Turn a \code{loon} widget to a \code{ggplot} object
#'
#' @description Create a \code{ggplot} object from a \code{loon} widget
#'
#' @param target a \code{loon} or a vector that specifies the
#' widget, layer, glyph, navigator or context completely.
#' The widget is specified by the widget path name (e.g. '.l0.plot'),
#' the remaining objects by their ids.
#' @param asAes logical; set aesthetics attributes, i.e. `color`, `fill` as
#' variables (default \code{TRUE}) or general visual properties (\code{FALSE}).
#' See details
#' @param ... arguments used inside \code{loon2ggplot()}, not used by this method
#'
#' @return a \code{ggplot} object
#'
#' @details
#' in \code{ggplot}, generally speaking, there are two ways to set the
#' aesthetics attributes, either take it as variables
#' (set in function \code{aes()}) or visual properties. The main benefits to
#' consider it as variables are that 1. legend could be shown; 2. convenient for
#' further analysis. See examples.
#'
#' @export
#' @examples
#' if(interactive()) {
#' lp <- l_plot(iris,
#'              color = iris$Species,
#'              glyph = "circle")
#' gp <- loon2ggplot(lp)
#' gp # a ggplot object
#'
#' # add smooth layer, grouped by color
#' gp +
#'   geom_smooth(aes(color = color)) +
#'   # give meaningful legend label names
#'   scale_color_manual(
#'     # make sure the order is correct
#'     values = unique(hex12tohex6(lp['color'])),
#'     labels = c("setosa", "versicolor", "virginica")
#'   )
#'
#' # histogram
#' lh <- l_hist(mtcars$mpg,
#'              color = factor(mtcars$gear))
#'
#' gh0 <- loon2ggplot(lh)
#' # facet by `fill`
#' gh0 + facet_wrap(~fill)
#'
#' # set `asAes = FALSE`
#' gh1 <- loon2ggplot(lh, asAes = FALSE)
#' # Expect the legend, they both are identical
#' gh1
#'
#' \dontrun{
#' # ERROR
#' # The bins are constructed by `ggplot2::geom_rect()`
#' # Very limited "fancy" operations can be made
#' gh1 + facet_wrap(~fill)}
#' }
#'
loon2ggplot <- function(target, asAes = TRUE, ...) {

  if(ggplot2::is.ggplot(target) || is.ggmatrix(target)) {
    error_info <- deparse(substitute(target))
    stop(
      "Target should be a loon object. Maybe you want to call `ggplot2loon(",
      error_info,
      ")`? Or, just call `loon.ggplot(",
      error_info,
      ")` for simplification.", call. = FALSE
    )
  }
  UseMethod('loon2ggplot', target)
}

#' @export
#' @rdname loon2ggplot
loon2ggplot.default <- function(target, asAes = TRUE, ...) {
  # TODO
  ggObj <- list(...)$ggObj
  ggObj
}

#' @rdname loon2ggplot
#' @export
loon2ggplot.l_plot <- function(target, asAes = TRUE, ...) {

  loon::l_isLoonWidget(target) || stop("target does not exist", call. = FALSE)
  rl <- loon::l_create_handle(c(target, 'root'))
  cartesian_gg(target = target,
               ggObj = loon2ggplot(rl, asAes = asAes))
}

#' @rdname loon2ggplot
#' @export
loon2ggplot.l_hist <- function(target, asAes = TRUE, ...) {

  loon::l_isLoonWidget(target) || stop("target does not seem to exist", call. = FALSE)
  rl <- loon::l_create_handle(c(target, 'root'))

  if(target['yshows'] == "density" && length(unique(target['color'])) > 1) {
    setLimits <- FALSE
    message("In ggplot histogram, if `y` shows density, ",
            "the area of each category (grouped by color) is 1; ",
            "however, for an `l_hist` widget, ",
            "the whole area is one and the area of each category is proportional to its counts.")
  } else
    setLimits <- TRUE

  cartesian_gg(target = target,
               ggObj = loon2ggplot(rl, asAes = asAes),
               setLimits = setLimits)
}

#' @export
loon2ggplot.l_graph <- function(target, asAes = TRUE, ...) {

  loon::l_isLoonWidget(target) || stop("target does not seem to exist", call. = FALSE)
  rl <- loon::l_create_handle(c(target, 'root'))
  cartesian_gg(target = target,
               ggObj = loon2ggplot(rl, asAes = asAes))
}

#' @rdname loon2ggplot
#' @export
loon2ggplot.l_plot3D <- function(target,  asAes = TRUE, ...) {

  loon::l_isLoonWidget(target) || stop("target does not seem to exist", call. = FALSE)
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
               ggObj = loon2ggplot(rl, asAes = asAes)) +
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

cartesian_gg <- function(target, ggObj, setLimits = TRUE) {

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
      panel.border = if(sum(margins, na.rm = TRUE) > 0)
        ggplot2::element_rect(colour = as_hex6color(widget['foreground']),
                              fill = NA,
                              size = 1) else ggplot2::element_blank(),
      plot.margin = grid::unit(margins, "lines")
    )

  if(setLimits) {
    ggObj <- ggObj +
      ggplot2::coord_cartesian(xlim = xlim, ylim = ylim)

    if(swapAxes) ggObj <- ggObj + coord_flip(xlim = xlim, ylim = ylim)

  } else {
    if(swapAxes) ggObj <- ggObj + coord_flip()
  }

  return(ggObj)
}

#' @export
loon2ggplot.l_layer_group <- function(target, asAes = TRUE, ...) {

  widget <- l_create_handle(attr(target, "widget"))
  ggObj <- ggplot2::ggplot()

  children <- l_layer_getUngroupedChildren(widget = widget, target = widget)
  l_children_layers <- lapply(
    rev(children),
    function(layerid) {

      layer <- loon::l_create_handle(c(widget, layerid))

      if(layerid == 'model') {

        states <- get_layer_states(widget, native_unit = FALSE)

        if(length(states$x) > 0) {

          if(inherits(widget, "l_hist")) {
            # histogram
            ggObj <<- ggplot2::ggplot(data = data.frame(x = states$x,
                                                        color = hex2colorName(states$color),
                                                        selected = states$selected,
                                                        active = states$active),
                                      mapping = ggplot2::aes(x = x))

          } else {

            # scatter plot
            swapAxes <- widget["swapAxes"]
            if(swapAxes) {
              y <- states$x
              x <- states$y
            } else {
              x <- states$x
              y <- states$y
            }

            ggObj <<- ggplot2::ggplot(data = data.frame(x = x,
                                                        y = y,
                                                        glyph = states$glyph,
                                                        size = states$size,
                                                        color = hex2colorName(states$color),
                                                        selected = states$selected,
                                                        active = states$active),
                                      mapping = ggplot2::aes(x = x,
                                                             y = y))
          }
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

           ggObj <<- loon2ggplot(layer, asAes = asAes, ggObj = ggObj)
         })

  ggObj
}

# primitive ggplot layers
#' @export
loon2ggplot.l_layer_polygon <- function(target, asAes = TRUE, ...) {

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
loon2ggplot.l_layer_line <- function(target, asAes = TRUE, ...) {

  widget <- l_create_handle(attr(target, "widget"))
  swapAxes <- widget["swapAxes"]
  states <- get_layer_states(target, native_unit = FALSE)

  ggObj <- list(...)$ggObj

  if(length(states$x) > 0  & length(states$y) > 0) {

    x <- if(swapAxes) states$y else states$x
    y <- if(swapAxes) states$x else states$y

    ggObj <- ggObj +
      ggplot2::geom_path(
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
loon2ggplot.l_layer_rectangle <- function(target, asAes = TRUE, ...) {

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
loon2ggplot.l_layer_oval <- function(target, asAes = TRUE, ...) {

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
loon2ggplot.l_layer_text <- function(target, asAes = TRUE, ...) {

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
loon2ggplot.l_layer_texts <- function(target, asAes = TRUE, ...) {

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
loon2ggplot.l_layer_points <- function(target, asAes = TRUE, ...) {
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
loon2ggplot.l_layer_polygons <- function(target, asAes = TRUE, ...) {

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
loon2ggplot.l_layer_rectangles <- function(target, asAes = TRUE, ...) {

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
loon2ggplot.l_layer_lines <- function(target, asAes = TRUE, ...) {

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
      ggplot2::geom_path(
        data =df,
        mapping = ggplot2::aes(x = x, y = y, group = group),
        colour = rep(linecolor, times = len_x),
        size = rep(linewidth, times = len_x),
        inherit.aes = FALSE
      )
  }

  ggObj
}
