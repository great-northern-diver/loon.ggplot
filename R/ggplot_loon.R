#' @export
ggplot2.loon <- function(target, ...) {
  UseMethod('ggplot2.loon', target)
}

#' @export
ggplot2.loon.default <- function(target, ...) {
  # stop('ggplot2.loon default no valid inheritance')
  # TODO
  ggObj <- list(...)$ggObj
  ggObj
}

#' @export
ggplot2.loon.l_plot <- function(target, ...) {

  loon::l_isLoonWidget(target) || stop("widget does not seem to exist")
  rl <- loon::l_create_handle(c(target, 'root'))
  cartesian_gg(target = target,
               ggObj = ggplot2.loon(rl))
}

#' @export
ggplot2.loon.l_hist <- function(target, ...) {

  loon::l_isLoonWidget(target) || stop("widget does not seem to exist")
  rl <- loon::l_create_handle(c(target, 'root'))
  cartesian_gg(target = target,
               ggObj = ggplot2.loon(rl))
}

#' @export
ggplot2.loon.l_graph <- function(target, ...) {

  loon::l_isLoonWidget(target) || stop("widget does not seem to exist")
  rl <- loon::l_create_handle(c(target, 'root'))
  cartesian_gg(target = target,
               ggObj = ggplot2.loon(rl))
}

#' @export
ggplot2.loon.l_plot3D <- function(target, ...) {

  loon::l_isLoonWidget(target) || stop("widget does not seem to exist")
  rl <- loon::l_create_handle(c(target, 'root'))

  axes_coords <- target["axesCoords"]

  adjust_brightness <- function(z_coord, r, g, b) {
    change <- as.integer(100 + 80 * z_coord)
    if (change < 0) {
      rgb(0,0,0)
    } else if (change <= 100) {
      rgb((r/256) * change/100, (g/256) * change/100, (b/256) * change/100)
    } else {
      rgb(r,g,b, maxColorValue=255)
    }
  }

  x_color <- adjust_brightness(axes_coords[[3]][1], 255, 0, 0)
  y_color <- adjust_brightness(axes_coords[[3]][2], 0, 0, 255)
  z_color <- adjust_brightness(axes_coords[[3]][3], 0, 255, 0)

  cartesian_gg(target = target,
               ggObj = ggplot2.loon(rl)) +
    ggplot2::geom_line(
      data = data.frame(
        x = c(0.5, 0.5 + 0.08*axes_coords[[1]][1]),
        y = c(0.5, 0.5 + 0.08*axes_coords[[2]][1])
      ),
      mapping = aes(x = x, y = y),
      colour = x_color,
      size = 1
    ) +
    ggplot2::geom_line(
      data = data.frame(
        x = c(0.5, 0.5 + 0.08*axes_coords[[1]][2]),
        y = c(0.5, 0.5 + 0.08*axes_coords[[2]][2])
      ),
      mapping = aes(x = x, y = y),
      colour = y_color,
      size = 1
    ) +
    ggplot2::geom_line(
      data = data.frame(
        x = c(0.5, 0.5 + 0.08*axes_coords[[1]][3]),
        y = c(0.5, 0.5 + 0.08*axes_coords[[2]][3])
      ),
      mapping = aes(x = x, y = y),
      colour = z_color,
      size = 1
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

  title <- widget['title']
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

  xlabelFont <- loon:::get_font_info_from_tk(loon::l_getOption("font-xlabel"))
  ylabelFont <- loon:::get_font_info_from_tk(loon::l_getOption("font-ylabel"))
  titleFont <- loon:::get_font_info_from_tk(loon::l_getOption("font-title"))
  scalesFont <- loon:::get_font_info_from_tk(loon::l_getOption("font-scales"))

  guidesBackGround <- if(showGuides) loon:::as_hex6color(widget['guidesBackground']) else loon:::as_hex6color(widget['background'])

  ggObj <- ggObj +
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim) +
    ggplot2::xlab(xlabel) +
    ggplot2::ylab(ylabel) +
    ggplot2::theme(
      axis.ticks = if(showScales) element_line() else element_blank(),
      axis.title = if(showLabels) element_text(size = titleFont$size, family = titleFont$family, face = titleFont$face) else element_blank(),
      axis.text.x = if(showScales) element_text(size = scalesFont$size, family = scalesFont$family, face = scalesFont$face) else element_blank(),
      axis.text.y = if(showScales) element_text(size = scalesFont$size, family = scalesFont$family, face = scalesFont$face) else element_blank(),
      axis.title.x = if(showLabels) element_text(size = xlabelFont$size, family = xlabelFont$family, face = xlabelFont$face) else element_blank(),
      axis.title.y = if(showLabels) element_text(size = ylabelFont$size, family = ylabelFont$family, face = ylabelFont$face) else element_blank(),
      plot.background = element_rect(fill = loon:::as_hex6color(widget['background'])),
      panel.background = element_rect(fill = guidesBackGround),
      panel.grid.major = element_line(size = 1,
                                      linetype = 'solid',
                                      colour = loon:::as_hex6color(widget['guidelines'])),
      panel.grid.minor = element_line(size = 0.5,
                                      linetype = 'solid',
                                      colour = loon:::as_hex6color(widget['guidelines'])),
      panel.border = if(sum(margins) > 0)
        element_rect(colour = loon:::as_hex6color(widget['foreground']),
                     fill = NA,
                     size = 1) else element_blank()
    )
  if(swapAxes) ggObj <- ggObj + coord_flip()
  return(ggObj)
}
#' @export
ggplot2.loon.l_layer_group <- function(target, ...) {

  widget <- l_create_handle(attr(target, "widget"))
  ggObj <- ggplot2::ggplot()

  children <- l_layer_getUngroupedChildren(widget = widget, target = widget)
  l_children_layers <- lapply(rev(children),
                              function(layerid) {

                                layer <- loon::l_create_handle(c(widget, layerid))

                                if(layerid == 'model') {

                                  states <- loon:::get_layer_states(widget, native_unit = FALSE)

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
                                                              mapping = aes(x = x, y = y))
                                  } else if(length(states$y) == 1 && is.na(states$y)) {
                                    # histogram
                                    ggObj <<- ggplot2::ggplot(data = data.frame(x = states$x))
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

           ggObj <<- ggplot2.loon(layer, ggObj = ggObj)
         })

  ggObj
}

# primitive ggplot layers
#' @export
ggplot2.loon.l_layer_polygon <- function(target, ...) {

  widget <- l_create_handle(attr(target, "widget"))
  swapAxes <- widget["swapAxes"]
  states <- loon:::get_layer_states(target, native_unit = FALSE)

  ggObj <- list(...)$ggObj

  if(length(states$x) > 0  & length(states$y) > 0){
    ggObj <- ggObj + ggplot2::geom_polygon(
      data = data.frame(
        x = if(swapAxes) states$y else states$x,
        y = if(swapAxes) states$x else states$y
      ),
      mapping = aes(x = x, y = y),
      fill = states$color,
      colour = states$linecolor,
      size =  as_r_line_size(states$linewidth)
    )
  }

  ggObj
}
#' @export
ggplot2.loon.l_layer_line <- function(target, ...) {

  widget <- l_create_handle(attr(target, "widget"))
  swapAxes <- widget["swapAxes"]
  states <- loon:::get_layer_states(target, native_unit = FALSE)

  ggObj <- list(...)$ggObj

  if(length(states$x) > 0  & length(states$y) > 0) {

    ggObj <- ggObj + ggplot2::geom_line(
      data = data.frame(
        x = if(swapAxes) states$y else states$x,
        y = if(swapAxes) states$x else states$y
      ),
      mapping = aes(x = x, y = y),
      colour = states$color,
      size = as_r_line_size(states$linewidth)
    )
  }

  ggObj
}
#' @export
ggplot2.loon.l_layer_rectangle <- function(target, ...) {

  widget <- l_create_handle(attr(target, "widget"))
  swapAxes <- widget["swapAxes"]
  states <- loon:::get_layer_states(target, native_unit = FALSE)

  ggObj <- list(...)$ggObj

  if(length(states$x) > 0  & length(states$y) > 0) {

    ggObj <- ggObj + ggplot2::geom_rect(
      data = data.frame(
        x = if(swapAxes) states$y else states$x,
        y = if(swapAxes) states$x else states$y
      ),
      mapping = aes(xmin = x[1], xmax = x[2], ymin = y[1], ymax = y[2]),
      colour = states$linecolor,
      fill = states$color,
      size =  as_r_line_size(states$linewidth)
    )

  }

  ggObj
}
#' @export
ggplot2.loon.l_layer_oval <- function(target, ...) {

  widget <- l_create_handle(attr(target, "widget"))
  swapAxes <- widget["swapAxes"]
  states <- loon:::get_layer_states(target, native_unit = FALSE)

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

    ggObj <- ggObj + ggplot2::geom_polygon(
      data = data.frame(
        x = x,
        y = y
      ),
      mapping = aes(x = x, y = y),
      fill = states$color,
      colour = states$linecolor,
      size = as_r_line_size(states$linewidth)
    )
  }

  ggObj
}
#' @export
ggplot2.loon.l_layer_text <- function(target, ...) {

  widget <- l_create_handle(attr(target, "widget"))
  swapAxes <- widget["swapAxes"]
  states <- loon:::get_layer_states(target, native_unit = FALSE)
  textCoords <- get_textCoords(angle = states$angle, anchor = states$anchor, just = states$just)

  ggObj <- list(...)$ggObj

  if(length(states$x) > 0  & length(states$y) > 0){

    x <- if(swapAxes) states$y else states$x
    y <- if(swapAxes) states$x else states$y

    ggObj <- ggObj + ggplot2::geom_text(
      data = data.frame(
        x = x + textCoords[1],
        y = y + textCoords[2],
        label = states$text
      ),
      mapping = aes(x = x, y = y, label = label),
      angle = states$angle,
      colour = states$color,
      hjust = get_hjust(states$justify),
      size = as_r_text_size(states$size)
    )
  }

  ggObj
}
#' @export
ggplot2.loon.l_layer_texts <- function(target, ...) {

  widget <- l_create_handle(attr(target, "widget"))
  swapAxes <- widget["swapAxes"]
  states <- loon:::get_layer_states(target, native_unit = FALSE)

  active <- states$active
  x <- if(swapAxes) states$y[active] else states$x[active]
  y <- if(swapAxes) states$x[active] else states$y[active]

  ggObj <- list(...)$ggObj

  if(length(x) > 0  & length(y) > 0){

    text  <- states$text[active]
    size  <- as_r_text_size(states$size[active])
    angle  <- states$angle[active]
    anchor  <- states$anchor[active]
    justify  <- states$justify[active]
    color <- states$color[active]

    data <- lapply(1:length(text),
                   function(i){
                     textCoords <- get_textCoords(angle = angle[i],
                                                  anchor = anchor[i],
                                                  just = justify[i])

                     c(label = text[i],
                       x = x[i] + textCoords[1],
                       y = y[i] + textCoords[2])
                   })

    df <- as.data.frame(
      do.call(rbind, data),
      stringsAsFactors = FALSE
    )
    df$x <- as.numeric(df$x)
    df$y <- as.numeric(df$y)

    ggObj <- ggObj + ggplot2::geom_text(
      data = df,
      mapping = aes(x = x, y = y, label = label),
      angle = angle,
      colour = color,
      hjust = get_hjust(justify),
      size = as_r_text_size(size)
    )
  }

  ggObj
}
#' @export
ggplot2.loon.l_layer_points <- function(target, ...) {
  widget <- l_create_handle(attr(target, "widget"))
  swapAxes <- widget["swapAxes"]
  states <- loon:::get_layer_states(target, native_unit = FALSE)

  active <- states$active
  x <- if(swapAxes) states$y[active] else states$x[active]
  y <- if(swapAxes) states$x[active] else states$y[active]

  ggObj <- list(...)$ggObj

  if(length(x) > 0  && length(y)  > 0) {
    size  <- as_r_point_size(states$size[active])
    color <- states$color[active]

    ggObj <- ggObj + ggplot2::geom_point(
      data = data.frame(x = x, y = y),
      mapping = aes(x = x, y = y),
      colour = color,
      size = size,
      pch = 16
    )
  }

  ggObj
}
#' @export
ggplot2.loon.l_layer_polygons <- function(target, ...) {

  widget <- l_create_handle(attr(target, "widget"))
  swapAxes <- widget["swapAxes"]
  states <- loon:::get_layer_states(target, native_unit = FALSE)

  active <- states$active
  x <- if(swapAxes) states$y[active] else states$x[active]
  y <- if(swapAxes) states$x[active] else states$y[active]

  ggObj <- list(...)$ggObj

  if(length(x) > 0  & length(y) > 0){

    linewidth  <- as_r_line_size(states$linewidth[active])
    linecolor <- states$linecolor[active]
    fill <- states$color[active]

    len_x <- lengths(x)

    df <- data.frame(
      x = as.numeric(unlist(x)),
      y = as.numeric(unlist(y)),
      id = as.factor(rep(1:length(len_x), times = len_x))
    )

    ggObj <- ggObj + ggplot2::geom_polygon(
      data =df,
      mapping = aes(x = x, y = y, group = id),
      fill = rep(fill, times = len_x),
      colour = rep(linecolor, times = len_x),
      size = rep(linewidth, times = len_x)
    )
  }

  ggObj
}
#' @export
ggplot2.loon.l_layer_rectangles <- function(target, ...) {

  widget <- l_create_handle(attr(target, "widget"))
  swapAxes <- widget["swapAxes"]
  states <- loon:::get_layer_states(target, native_unit = FALSE)

  active <- states$active
  x <- if(swapAxes) states$y[active] else states$x[active]
  y <- if(swapAxes) states$x[active] else states$y[active]

  ggObj <- list(...)$ggObj

  if(length(x) > 0  & length(y) > 0){

    df <- data.frame(
      xmin = as.numeric(sapply(x, function(xx) xx[1])),
      xmax = as.numeric(sapply(x, function(xx) xx[2])),
      ymin = as.numeric(sapply(y, function(yy) yy[1])),
      ymax = as.numeric(sapply(y, function(yy) yy[2]))
    )

    ggObj <- ggObj + ggplot2::geom_rect(
      data =df,
      mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = states$color[active],
      colour = states$linecolor[active],
      size = as_r_line_size(states$linewidth[active])
    )
  }

  ggObj
}
#' @export
ggplot2.loon.l_layer_lines <- function(target, ...) {

  widget <- l_create_handle(attr(target, "widget"))
  swapAxes <- widget["swapAxes"]
  states <- loon:::get_layer_states(target, native_unit = FALSE)

  active <- states$active
  x <- if(swapAxes) states$y[active] else states$x[active]
  y <- if(swapAxes) states$x[active] else states$y[active]

  ggObj <- list(...)$ggObj

  if(length(x) > 0  & length(y) > 0){

    linewidth  <- as_r_line_size(states$linewidth[active])
    linecolor <- states$color[active]

    len_x <- lengths(x)

    df <- data.frame(
      x = as.numeric(unlist(x)),
      y = as.numeric(unlist(y)),
      id = as.factor(rep(1:length(len_x), times = len_x))
    )

    ggObj <- ggObj + ggplot2::geom_line(
      data =df,
      mapping = aes(x = x, y = y, group = id),
      colour = rep(linecolor, times = len_x),
      size = rep(linewidth, times = len_x)
    )
  }

  ggObj
}

get_textCoords <- function(angle, anchor, just) {

  angle <- angle * pi / 180

  switch(anchor,
         "center" = {
           hjust <- 0
           vjust <- 0
         },
         "n" = {
           hjust <- 1/2 * sin(angle)
           vjust <- -1/2 * cos(angle)
         },
         "e" = {
           hjust <- -1/2 * cos(angle)
           vjust <- -1/2 * sin(angle)
         },
         "s" = {
           hjust <- - 1/2 * sin(angle)
           vjust <- 1/2 * cos(angle)
         },
         "w" = {
           hjust <- 1/2 * cos(angle)
           vjust <- 1/2 * sin(angle)
         },
         "sw" = {
           hjust <- - 1/2 * sin(angle) +
             1/2 * cos(angle)
           vjust <- 1/2 * cos(angle) +
             1/2 * sin(angle)
         },
         "nw" = {
           hjust <- 1/2 * sin(angle) +
             1/2 * cos(angle)
           vjust <- -1/2 * cos(angle) +
             1/2 * sin(angle)
         },
         "ne" =  {
           hjust <-  1/2 * sin(angle)  +
             (-1/2) * cos(angle)
           vjust <- -1/2 * cos(angle)  +
             (-1/2) * sin(angle)
         },
         "se" = {
           hjust <- - 1/2 * sin(angle)  +
             (-1/2) * cos(angle)
           vjust <- 1/2 * cos(angle)  +
             (-1/2) * sin(angle)
         }
  )
  # just can only be "left", "right" and "center"
  if(just == "left") {
    hjust <- hjust - 1/2 * cos(angle)
    vjust <- vjust - 1/2 * sin(angle)
  } else if(just == "right") {
    hjust <- hjust + 1/2 * cos(angle)
    vjust <- vjust + 1/2 * sin(angle)
  }

  c(hjust, vjust)
}

get_hjust <- function(just) {
  sapply(just,
         function(j){
           switch(j,
                  "right" = 0,
                  "left" = 1,
                  "center" = 0.5)
         })
}

l_layer_getUngroupedChildren <- function(widget, target) {

  loon::l_isLoonWidget(widget) || stop("widget does not seem to exist")
  children <- loon::l_layer_getChildren(target)
  layer <- lapply(children,
                  function(child) {
                    target <- loon::l_create_handle(c(widget, child))
                    if(is(target, "l_layer_group")) {
                      # do recursive
                      l_layer_getUngroupedChildren(widget, target)
                    } else {
                      target
                    }
                  }
  )

  unlist(layer, recursive = TRUE)
}

as_r_text_size <- function(size) {
  size/1.76
}

as_r_point_size <- function(size) {
  2*log(size)
}

as_r_line_size <- function(size) {
  size/.pt
}

pixels_2_lines <- function(x) {
  x / 30
}


get_layer_states <- function(target, omit = NULL, native_unit = TRUE) {

  if (!is(target, "loon")) {
    target <- l_create_handle(target)
  }

  if (is(target, "l_layer")) {
    layer <- target
    widget <- l_create_handle(attr(target, "widget"))
    obj_states <- target
  } else {
    widget <- target
    layer <- l_create_handle(c(as.vector(widget), "model"))
    obj_states <- widget
  }

  states_info <- l_info_states(obj_states)
  state_names <- setdiff(names(states_info), c(omit, cartesian_model_widget_states))

  states <- setNames(lapply(state_names,
                            function(state) l_cget(target, state)),
                     state_names)

  # Add Coordinates
  if (!is(layer, "l_layer_group")) {
    states <- c(xy_coords_layer(layer, native_unit = native_unit), states)
  }


  # Deal with color
  is_color <- vapply(states_info[state_names],
                     function(s) s$type %in% c("color", "colorOrTransparent"),
                     logical(1))
  if (any(is_color)) {
    for (state_name in state_names[is_color]) {
      states[[state_name]] <- as_hex6color(states[[state_name]])
    }

  }

  states
}

