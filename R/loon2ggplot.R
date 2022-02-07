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
#' @param selectedOnTop logical and default is \code{TRUE}; whether to display the
#' selected points on top. See details.
#' @param showNearestColor logical and default is \code{FALSE}; if \code{TRUE},
#' the legend of color and fill (hex code) would be converted to the \code{R} built-in
#' color names. For some hex codes, there are no precise matching.
#' Consequently, these colors will be converted to the \code{R} built-in color names
#' which are the "nearest" of these hex codes.
#' @param ... arguments used inside \code{loon2ggplot()}, not used by this method
#'
#' @return a \code{ggplot} object (or a \code{patchwork} object, a extension of \code{ggplot2})
#'
#' @details
#' In \code{ggplot2}, typically, there are two ways to set the
#' aesthetic attributes, either take them as variables \code{asAes = TRUE}
#' (set in the function \code{aes()}) or constants \code{asAes = FALSE}.
#' The main benefits to consider them as variables are that 1. legend could be displayed;
#' 2. convenient for further analysis.
#'
#' In \code{loon}, when points were \code{selected} (highlighted),
#' the order would be changed so that
#' the highlighted points would be displayed at the front.
#' To turn the \code{loon} plot static, if \code{selectedOnTop = TRUE},
#' the points would be partitioned into two
#' groups -- one group representing the un-highlighted points,
#' and the other group representing the highlighted points.
#' The un-highlighted group would be drawn first,
#' then the selected group;
#' if \code{selectedOnTop = FALSE}, no partition would be applied so that
#' the displayed order remained. However, the highlighted points could be
#' displayed at the back. See examples.
#'
#' @export
#' @examples
#' if(interactive()) {
#' ######## Basic ########
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
#' ######## Argument `asAes` ########
#' gh1 <- loon2ggplot(lh, asAes = FALSE)
#' gh1
#' \dontrun{
#' # The bins are constructed by `ggplot2::geom_rect()`
#' # Very limited manipulations can be made
#' # ERROR
#' gh1 + facet_wrap(~fill)
#' }
#'
#' ######## Argument `selectedOnTop` ########
#' p <- l_plot(iris, color = iris$Species)
#' p['selected'][iris$Petal.Length > 5] <- TRUE
#' g <- loon.ggplot(p)
#' # It looks correct.
#' g
#' # facet by "Species"
#' \dontrun{
#' g + facet_wrap(iris$Species)
#' }
#' # Something is wrong here. There is a pink point (at least one)
#' # in species "versicolor"! It is because after points are
#' # highlighted, the displayed order has been changed.
#' # Set `selectedOnTop` as FALSE, as in
#' loon.ggplot(p, selectedOnTop = FALSE) +
#'   facet_wrap(iris$Species)
#'
#' \donttest{
#' ######## l_patchwork --> ggplot ########
#' library(patchwork)
#' p1 <- ggplot(mtcars) + geom_point(aes(mpg, disp))
#' p2 <- ggplot(mtcars) +
#'    geom_boxplot(aes(gear, disp, group = gear))
#' p3 <- ggplot(mtcars) + geom_smooth(aes(disp, qsec))
#' design <- c(
#'   area(1,1),
#'   area(1,2),
#'   area(2,1,2,2)
#' )
#' pp <- p1 + p2 + p3 + plot_layout(design = design)
#'
#' # turn a patchwork obj to a loon (l_compound)
#' lp <- ggplot2loon(pp)
#' # turn a loon (l_compound) back to a patchwork
#' plp <- loon2ggplot(lp)
#' plp # almost identical to pp
#'
#'
#' ######## zneplots --> ggplot ########
#' library(zenplots)
#' stopifnot(packageVersion("zenplots") > "1.0.4")
#' zen <- zenplots::zenplot(iris, plot1d = "density", pkg = "loon")
#' ggzen <- loon.ggplot(zen)
#' ggzen +
#'   patchwork::plot_annotation(title = "This is a ggplot")
#' }
#' }
loon2ggplot <- function(target, asAes = TRUE, selectedOnTop = TRUE,
                        showNearestColor = FALSE, ...) {

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
loon2ggplot.default <- function(target, asAes = TRUE, selectedOnTop = TRUE,
                                showNearestColor = FALSE, ...) {
  # TODO
  ggObj <- list(...)$ggObj
  ggObj
}

#' @rdname loon2ggplot
#' @export
loon2ggplot.l_plot <- function(target, asAes = TRUE, selectedOnTop = TRUE,
                               showNearestColor = FALSE, ...) {

  loon::l_isLoonWidget(target) || stop("target does not exist", call. = FALSE)
  rl <- loon::l_create_handle(c(target, 'root'))
  cartesian_gg(target = target,
               ggObj = loon2ggplot(rl,
                                   asAes = asAes,
                                   selectedOnTop = selectedOnTop,
                                   showNearestColor = showNearestColor,
                                   ...))
}

#' @rdname loon2ggplot
#' @export
loon2ggplot.l_hist <- function(target, asAes = TRUE, selectedOnTop = TRUE,
                               showNearestColor = FALSE, ...) {

  loon::l_isLoonWidget(target) || stop("target does not seem to exist", call. = FALSE)
  rl <- loon::l_create_handle(c(target, 'root'))

  setLimits <- TRUE
  if(target['yshows'] == "density") {
    if(len_unique(target['color']) > 1 || length(unique(target['selected'])) > 1) {
      setLimits <- FALSE
      message("In `ggplot` histogram, if `y` shows density, ",
              "the area of each category (grouped by color) is 1; ",
              "however, for an `l_hist` widget, ",
              "the whole area is one and the area of each category is proportional to its counts.")
    }
  }

  cartesian_gg(target = target,
               ggObj = loon2ggplot(rl, asAes = asAes,
                                   selectedOnTop = selectedOnTop,
                                   showNearestColor = showNearestColor, ...),
               setLimits = setLimits)
}

#' @export
loon2ggplot.l_graph <- function(target, asAes = TRUE, selectedOnTop = TRUE,
                                showNearestColor = FALSE, ...) {

  loon::l_isLoonWidget(target) || stop("target does not seem to exist", call. = FALSE)
  rl <- loon::l_create_handle(c(target, 'root'))
  cartesian_gg(target = target,
               ggObj = loon2ggplot(rl, asAes = asAes, selectedOnTop = selectedOnTop,
                                   showNearestColor = showNearestColor, ...))
}

#' @rdname loon2ggplot
#' @export
loon2ggplot.l_plot3D <- function(target,  asAes = TRUE, selectedOnTop = TRUE,
                                 showNearestColor = FALSE, ...) {

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
               ggObj = loon2ggplot(rl, asAes = asAes, selectedOnTop = selectedOnTop,
                                   showNearestColor = showNearestColor, ...)) +
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
  margins <- round(margins/100, 2)

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
      panel.border = if(sum(margins, na.rm = TRUE) > 0) {
        ggplot2::element_rect(colour = as_hex6color(widget['foreground']),
                              fill = NA,
                              size = 0.5)
      } else {
        ggplot2::element_blank()
      },
      plot.margin = grid::unit(margins, "lines")
    )

  if(setLimits) {

    if(swapAxes) {
      ggObj <- ggObj +
        ggplot2::coord_flip(xlim = xlim, ylim = ylim,
                            expand = FALSE)
    } else {
      ggObj <- ggObj +
        ggplot2::coord_cartesian(xlim = xlim, ylim = ylim,
                                 expand = FALSE)
    }

  } else {
    if(swapAxes) ggObj <- ggObj +
        ggplot2::coord_flip()
  }

  return(ggObj)
}

#' @export
loon2ggplot.l_layer_group <- function(target, asAes = TRUE, selectedOnTop = TRUE,
                                      showNearestColor = FALSE, ...) {

  widget <- l_create_handle(attr(target, "widget"))
  ggObj <- ggplot2::ggplot()
  visibleChildrenLayers <- l_get_visible_children(widget)

  lapply(visibleChildrenLayers,
         function(layer) {

           ggObj <<- loon2ggplot(layer, asAes = asAes,
                                 selectedOnTop = selectedOnTop,
                                 showNearestColor = showNearestColor,
                                 ggObj = ggObj, ...)
         })

  ggObj
}

# primitive ggplot layers
#' @export
loon2ggplot.l_layer_polygon <- function(target, asAes = TRUE, selectedOnTop = TRUE,
                                        showNearestColor = FALSE, ...) {

  widget <- l_create_handle(attr(target, "widget"))
  swapAxes <- widget["swapAxes"]
  states <- get_layer_states(target, native_unit = FALSE)

  args <- list(...)
  ggObj <- args$ggObj
  facets <- args$facets

  if(length(states$x) == 0  || length(states$y) == 0)
    return(ggObj)

  if(is.null(facets)) {
    data <- flipxy(states, swapAxes = swapAxes)
  } else {
    data <- facets_layer_data(target, facets,
                              facetsLabels = args$facetsLabels,
                              levels = args$levels,
                              swapAxes = swapAxes)
  }

  ggObj +
    ggplot2::geom_polygon(
      data = data,
      mapping = ggplot2::aes(x = x, y = y),
      fill = states$color,
      colour = states$linecolor,
      size =  as_ggplot_size(states$linewidth, "lines")
    )
}

#' @export
loon2ggplot.l_layer_line <- function(target, asAes = TRUE, selectedOnTop = TRUE,
                                     showNearestColor = FALSE, ...) {

  widget <- l_create_handle(attr(target, "widget"))
  swapAxes <- widget["swapAxes"]
  states <- get_layer_states(target, native_unit = FALSE)

  args <- list(...)
  ggObj <- args$ggObj
  facets <- args$facets

  if(length(states$x) == 0  || length(states$y) == 0)
    return(ggObj)

  if(is.null(facets)) {
    data <- flipxy(states, swapAxes = swapAxes)
  } else {
    data <- facets_layer_data(target, facets,
                              facetsLabels = args$facetsLabels,
                              levels = args$levels,
                              swapAxes = swapAxes)
  }

  ggObj +
    ggplot2::geom_path(
      data = data,
      mapping = ggplot2::aes(x = x, y = y),
      colour = states$color,
      size = as_ggplot_size(states$linewidth, "lines")
    )
}

#' @export
loon2ggplot.l_layer_rectangle <- function(target, asAes = TRUE, selectedOnTop = TRUE,
                                          showNearestColor = FALSE, ...) {

  widget <- l_create_handle(attr(target, "widget"))
  swapAxes <- widget["swapAxes"]
  states <- get_layer_states(target, native_unit = FALSE)

  args <- list(...)
  ggObj <- args$ggObj
  facets <- args$facets

  if(length(states$x) == 0  || length(states$y) == 0)
    return(ggObj)

  if(is.null(facets)) {

    data <- flipxy(states, swapAxes = swapAxes)

  } else {

    data <- facets_layer_data(target, facets,
                              facetsLabels = args$facetsLabels,
                              levels = args$levels,
                              swapAxes = swapAxes)

  }

  ggObj +
    ggplot2::geom_rect(
      data = data,
      mapping = ggplot2::aes(xmin = x[1], xmax = x[2],
                             ymin = y[1], ymax = y[2]),
      colour = states$linecolor,
      fill = states$color,
      size =  as_ggplot_size(states$linewidth, "lines")
    )
}
#' @export
loon2ggplot.l_layer_oval <- function(target, asAes = TRUE, selectedOnTop = TRUE,
                                     showNearestColor = FALSE, ...) {

  widget <- l_create_handle(attr(target, "widget"))
  swapAxes <- widget["swapAxes"]
  states <- get_layer_states(target, native_unit = FALSE)

  args <- list(...)
  ggObj <- args$ggObj
  facets <- args$facets

  if(length(states$x) == 0  || length(states$y) == 0)
    return(ggObj)

  if(!is.null(facets)) {
    # data <- facets_layer_data(target, facets, facetsLabels, levels, swapAxes)
    warning("The settings of facets for `loon2ggplot.l_layer_oval()` are still in development.",
            call. = FALSE)
  }


  data <- flipxy(states, swapAxes = swapAxes)
  xcoords <- data$x
  ycoords <- data$y

  angle <- seq(0, 2*base::pi, length=101)

  xCenter <- mean(xcoords)
  yCenter <- mean(ycoords)

  xRadius <- diff(range(xcoords))/2
  yRadius <- diff(range(ycoords))/2

  x <- mean(xcoords) + xRadius * cos(angle)
  y <- mean(ycoords) + yRadius * sin(angle)

  ggObj +
    ggplot2::geom_polygon(
      data = data.frame(
        x = x,
        y = y
      ),
      mapping = ggplot2::aes(x = x, y = y),
      fill = states$color,
      colour = states$linecolor,
      size = as_ggplot_size(states$linewidth, "lines")
    )
}
#' @export
loon2ggplot.l_layer_text <- function(target, asAes = TRUE, selectedOnTop = TRUE,
                                     showNearestColor = FALSE, ...) {

  widget <- l_create_handle(attr(target, "widget"))
  swapAxes <- widget["swapAxes"]
  states <- get_layer_states(target, native_unit = FALSE)
  textCoords <- get_textCoords(angle = states$angle,
                               anchor = states$anchor,
                               just = states$just)

  args <- list(...)
  ggObj <- args$ggObj
  facets <- args$facets

  if(length(states$x) == 0  || length(states$y) == 0)
    return(ggObj)

  if(is.null(facets)) {

    data <- flipxy(states, swapAxes = swapAxes)
    data$label <- states$label %||% states$text

  } else {

    data <- facets_layer_data(target, facets,
                              facetsLabels = args$facetsLabels,
                              levels = args$levels,
                              swapAxes = swapAxes,
                              label = TRUE)
  }

  data$x <- data$x + textCoords[1]
  data$y <- data$y + textCoords[2]

  ggObj +
    ggplot2::geom_text(
      data = data,
      mapping = ggplot2::aes(x = x, y = y,
                             label = label),
      angle = states$angle,
      colour = states$color,
      hjust = get_hjust(states$justify),
      size = as_ggplot_size(states$size)
    )
}
#' @export
loon2ggplot.l_layer_texts <- function(target, asAes = TRUE, selectedOnTop = TRUE,
                                      showNearestColor = FALSE, ...) {

  widget <- l_create_handle(attr(target, "widget"))
  swapAxes <- widget["swapAxes"]
  states <- get_layer_states(target, native_unit = FALSE)

  args <- list(...)
  ggObj <- args$ggObj
  facets <- args$facets

  if(length(states$x) == 0  || length(states$y) == 0)
    return(ggObj)

  if(!is.null(facets)) {
    warning("The settings of facets for `loon2ggplot.l_layer_texts()` are still in development.",
            call. = FALSE)
  }

  active <- states$active
  x <- if(swapAxes) states$y[active] else states$x[active]
  y <- if(swapAxes) states$x[active] else states$y[active]

  label  <- states$text[active]
  size  <- states$size[active]
  angle  <- states$angle[active]
  anchor  <- states$anchor[active]
  justify  <- states$justify[active]
  color <- states$color[active]

  data <- lapply(seq(length(label)),
                 function(i){
                   textCoords <- get_textCoords(angle = angle[i],
                                                anchor = anchor[i],
                                                just = justify[i])

                   c(label = label[i],
                     x = as.numeric(x[i] + textCoords[1]),
                     y = as.numeric(y[i] + textCoords[2]))
                 })

  df <- as.data.frame(
    do.call(rbind, data),
    stringsAsFactors = FALSE
  )

  if(len_unique(angle) == 1L) angle <- angle[1L]
  if(len_unique(size) == 1L) size <- size[1L]
  if(len_unique(justify) == 1L) justify <- justify[1L]
  if(len_unique(color) == 1L) color <- color[1L]

  ggObj +
    ggplot2::geom_text(
      data = df,
      mapping = ggplot2::aes(x = x, y = y, label = label),
      angle = angle,
      colour = color,
      hjust = get_hjust(justify),
      size = as_ggplot_size(size)
    )
}
#' @export
loon2ggplot.l_layer_points <- function(target, asAes = TRUE, selectedOnTop = TRUE,
                                       showNearestColor = FALSE, ...) {
  widget <- l_create_handle(attr(target, "widget"))
  swapAxes <- widget["swapAxes"]
  states <- get_layer_states(target, native_unit = FALSE)

  args <- list(...)
  ggObj <- args$ggObj
  facets <- args$facets

  if(length(states$x) == 0  || length(states$y) == 0)
    return(ggObj)

  if(!is.null(facets)) {
    warning("The settings of facets for `loon2ggplot.l_layer_points()` are still in development.",
            call. = FALSE)
  }

  active <- states$active
  x <- if(swapAxes) states$y[active] else states$x[active]
  y <- if(swapAxes) states$x[active] else states$y[active]
  color <- states$color[active]
  if(len_unique(color) == 1L) color <- color[1L]
  size <- as_ggplot_size(states$size[active])
  if(len_unique(size) == 1L) size <- size[1L]

  ggObj +
    ggplot2::geom_point(
      data = data.frame(x = x, y = y),
      mapping = ggplot2::aes(x = x, y = y),
      colour =  color,
      size = size,
      pch = 19
    )
}
#' @export
loon2ggplot.l_layer_polygons <- function(target, asAes = TRUE, selectedOnTop = TRUE,
                                         showNearestColor = FALSE, ...) {

  widget <- l_create_handle(attr(target, "widget"))
  swapAxes <- widget["swapAxes"]
  states <- get_layer_states(target, native_unit = FALSE)

  args <- list(...)
  ggObj <- args$ggObj
  facets <- args$facets

  if(length(states$x) == 0  || length(states$y) == 0)
    return(ggObj)

  if(!is.null(facets)) {
    warning("The settings of facets for `loon2ggplot.l_layer_polygons()` are still in development.",
            call. = FALSE)
  }

  active <- states$active
  x <- if(swapAxes) states$y[active] else states$x[active]
  y <- if(swapAxes) states$x[active] else states$y[active]
  len_x <- lengths(x)

  linewidth  <- as_ggplot_size(states$linewidth[active], "lines")
  linewidth <- if(len_unique(linewidth) == 1L) {
    linewidth[1L]
  } else {
    rep(linewidth, times = len_x)
  }

  linecolor <- states$linecolor[active]
  linecolor <- if(len_unique(linecolor) == 1L) {
    linecolor[1L]
  } else {
    rep(linecolor, times = len_x)
  }

  fill <- states$color[active]
  fill <- if(len_unique(fill) == 1L) {
    fill[1L]
  } else {
    rep(fill, times = len_x)
  }

  group <- as.factor(rep(1:length(len_x), times = len_x))
  df <- data.frame(
    x = as.numeric(unlist(x)),
    y = as.numeric(unlist(y)),
    group = group
  )

  ggObj +
    ggplot2::geom_polygon(
      data =df,
      mapping = ggplot2::aes(x = x, y = y, group = group),
      fill = fill,
      colour = linecolor,
      size = linewidth
    )
}
#' @export
loon2ggplot.l_layer_rectangles <- function(target, asAes = TRUE, selectedOnTop = TRUE,
                                           showNearestColor = FALSE, ...) {

  widget <- l_create_handle(attr(target, "widget"))
  swapAxes <- widget["swapAxes"]
  states <- get_layer_states(target, native_unit = FALSE)

  args <- list(...)
  ggObj <- args$ggObj
  facets <- args$facets

  if(length(states$x) == 0  || length(states$y) == 0)
    return(ggObj)

  if(!is.null(facets)) {
    warning("The settings of facets for `loon2ggplot.l_layer_rectangles()` are still in development.",
            call. = FALSE)
  }

  active <- states$active
  x <- if(swapAxes) states$y[active] else states$x[active]
  y <- if(swapAxes) states$x[active] else states$y[active]

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

  fill <- states$color[active]
  if(len_unique(fill) == 1L) fill <- fill[1L]

  color <- states$linecolor[active]
  if(len_unique(color) == 1L) color <- color[1L]

  size <- as_ggplot_size(states$linewidth[active], "lines")
  if(len_unique(size) == 1L) size <- size[1L]

  ggObj +
    ggplot2::geom_rect(
      data =df,
      mapping = ggplot2::aes(xmin = xmin, xmax = xmax,
                             ymin = ymin, ymax = ymax),
      fill = fill,
      colour = color,
      size = size
    )
}
#' @export
loon2ggplot.l_layer_lines <- function(target, asAes = TRUE, selectedOnTop = TRUE,
                                      showNearestColor = FALSE, ...) {

  widget <- l_create_handle(attr(target, "widget"))
  swapAxes <- widget["swapAxes"]
  states <- get_layer_states(target, native_unit = FALSE)

  args <- list(...)
  ggObj <- args$ggObj
  facets <- args$facets

  if(length(states$x) == 0  || length(states$y) == 0)
    return(ggObj)

  if(!is.null(facets)) {
    warning("The settings of facets for `loon2ggplot.l_layer_lines()` are still in development.",
            call. = FALSE)
  }

  active <- states$active
  x <- if(swapAxes) states$y[active] else states$x[active]
  y <- if(swapAxes) states$x[active] else states$y[active]
  len_x <- lengths(x)
  linewidth  <- as_ggplot_size(states$linewidth[active], "lines")
  linewidth <- if(len_unique(linewidth) == 1L) {
    linewidth[1L]
  } else {
    rep(linewidth, times = len_x)
  }

  linecolor <- states$color[active]
  linecolor <- if(len_unique(linecolor) == 1L) {
    linecolor[1L]
  } else {
    rep(linecolor, times = len_x)
  }

  group <- as.factor(rep(seq(length(len_x)),
                         times = len_x))

  df <- data.frame(
    x = as.numeric(unlist(x)),
    y = as.numeric(unlist(y)),
    group = group
  )

  ggObj +
    ggplot2::geom_path(
      data =df,
      mapping = ggplot2::aes(x = x, y = y, group = group),
      colour = linecolor,
      size = linewidth
    )
}

l_get_visible_children <- function(widget) {

  children <- l_layer_getUngroupedChildren(widget = widget, target = widget)

  childrenLayers <- lapply(
    rev(children),
    function(layerid) {
      loon::l_create_handle(c(widget, layerid))
    })

  visibleChildrenLayers <- Filter(
    function(layerid) {
      loon::l_layer_isVisible(widget, layerid)
    },
    childrenLayers)

  return(visibleChildrenLayers)
}

flipxy <- function(states, swapAxes = FALSE) {

  if(swapAxes) {
    data <- data.frame(
      x = states$y,
      y = states$x
    )
  } else {
    data <- data.frame(
      x = states$x,
      y = states$y
    )
  }

  data
}

facets_layer_data <- function(target, facets, facetsLabels, levels, swapAxes, label = FALSE) {

  facetsVar <- rownames(facetsLabels)
  data <- do.call(rbind,
                  lapply(seq_along(facets),
                         function(i) {

                           facet <- facets[[i]]
                           visibleChildrenLayers <- l_get_visible_children(facet)
                           targeti <- Filter(function(x) x == target, visibleChildrenLayers)[[1L]]
                           states <- get_layer_states(targeti, native_unit = FALSE)

                           data <- flipxy(states, swapAxes = swapAxes)
                           if(label) {
                             data$label <- states$label %||% states$text
                           }

                           do.call(cbind,
                                   c(list(data),
                                     stats::setNames(as.list(facetsLabels[, i]),
                                                     facetsVar),
                                     facetGroup = i))
                         })
  )

  for (i in seq_along(facetsVar)) {
    data[[facetsVar[i]]] <- factor(data[[facetsVar[i]]],
                                   levels = levels[[i]])
  }

  return(data)
}
