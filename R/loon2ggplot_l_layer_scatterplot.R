#' @export
#' @rdname loon2ggplot
loon2ggplot.l_layer_scatterplot <- function(target, ...) {

  widget <- loon::l_create_handle(attr(target, "widget"))
  states <- get_layer_states(widget)
  swapAxes <- widget["swapAxes"]

  ggObj <- list(...)$ggObj

  if (!any(states$active)) return(ggObj)

  # No active points in scatterplot
  display_order <- get_model_display_order(widget)

  active <- states$active[display_order]
  selected <- states$selected[display_order][active]

  s_a <- list(
    x = if(swapAxes) states$y[display_order][active] else states$x[display_order][active],
    y = if(swapAxes) states$x[display_order][active] else states$y[display_order][active],
    glyph = states$glyph[display_order][active],
    color = get_display_color(states$color[display_order][active], selected),
    size = states$size[display_order][active],
    index = display_order[active]
  )

  x <- as.numeric(s_a$x)
  y <- as.numeric(s_a$y)
  pch <- glyph_to_pch(s_a$glyph)
  color <- fill <- s_a$color
  size <- s_a$size

  # This is a hack!
  # since legend of "fill" in ggplot has bugs
  pch <- hack_pch(pch)

  if (!any(is.na(pch)) && !any(pch %in% 21:24)) {

    size <- as_r_point_size(size)
    # No NAs and no points with borders
    ggObj <- ggObj +
      ggplot2::geom_point(
        data = data.frame(x = x,
                          y = y,
                          color = color,
                          pch = factor(pch),
                          size = size),
        mapping = ggplot2::aes(x = x, y = y, color = color, size = size,
                               pch = pch),
        inherit.aes = FALSE
      )

  } else if (!any(is.na(pch)) && all(pch %in% 21:24)) {

    size <- as_r_point_size(size)
    # No NAs and ALL points with borders
    ggObj <- ggObj +
      ggplot2::geom_point(
        data = data.frame(x = x,
                          y = y,
                          pch = factor(pch),
                          fill = color,
                          size = size),
        mapping = ggplot2::aes(x = x, y = y, fill = fill, size = size,
                               pch = pch),
        color = loon::l_getOption("foreground"),
        inherit.aes = FALSE
      )

  } else {
    # possibly some NAs (means some points are text, polygons, images, etc.)
    # and/or a mix of regular and closed points.
    type <- sapply(s_a$glyph, function(glyph) loon::l_glyph_getType(widget, glyph))

    uni_type <- unique(type)
    hide <- lapply(uni_type,
                   function(t) {
                     id <- which(type == t)
                     aesthetic <- list(
                       x = as.numeric(s_a$x[id]),
                       y = as.numeric(s_a$y[id]),
                       glyph = s_a$glyph[id],
                       color = s_a$color[id],
                       size = s_a$size[id],
                       index = s_a$index[id]
                     )
                     switch(t,
                            "polygon" = {
                              gh <- loon::l_create_handle(c(widget, aesthetic$glyph[1]))

                              ggObj <<- ggObj +
                                geom_polygonGlyph(
                                  data = data.frame(x = aesthetic$x,
                                                    y = aesthetic$y,
                                                    fill = aesthetic$color,
                                                    color = aesthetic$color,
                                                    size = aesthetic$size),
                                  mapping = ggplot2::aes(x = x,
                                                         y = y,
                                                         fill = fill,
                                                         color = color,
                                                         size = size),
                                  polygon_x = gh['x'][aesthetic$index],
                                  polygon_y = gh['y'][aesthetic$index],
                                  showArea = gh['showArea'][aesthetic$index],
                                  linewidth = gh['linewidth'][aesthetic$index],
                                  inherit.aes = FALSE
                                )
                            },
                            "serialaxes" = {
                              gh <- loon::l_create_handle(c(widget, aesthetic$glyph[1]))
                              # loon data will be converted into character by default

                              ggObj <<- ggObj +
                                geom_serialAxesGlyph(
                                  data = data.frame(x = aesthetic$x,
                                                    y = aesthetic$y,
                                                    color = aesthetic$color,
                                                    size = aesthetic$size),
                                  mapping = ggplot2::aes(x = x, y = y, color = color, size = size),
                                  serialAxesData = char2num.data.frame(gh['data'][aesthetic$index, ]),
                                  sequence = gh['sequence'],
                                  scaling = gh['scaling'],
                                  axesLayout = gh['axesLayout'],
                                  showAxes = gh['showAxes'],
                                  showArea = gh['showArea'],
                                  showEnclosing = gh['showEnclosing'],
                                  axesColor = as_hex6color(gh['axesColor']),
                                  bboxColor = as_hex6color(gh['bboxColor']),
                                  linewidth = gh['linewidth'][aesthetic$index],
                                  inherit.aes = FALSE
                                )
                            },
                            "text" = {
                              gh <- loon::l_create_handle(c(widget, aesthetic$glyph[1]))

                              text_size <- as_r_text_size(aesthetic$size)
                              size <<- c(size, text_size)

                              ggObj <<- ggObj +
                                geom_textGlyph(
                                  data = data.frame(x = aesthetic$x,
                                                    y = aesthetic$y,
                                                    color = aesthetic$color,
                                                    size = text_size),
                                  mapping = ggplot2::aes(x = x, y = y, color = color, size = size),
                                  text = gh["text"][aesthetic$index],
                                  inherit.aes = FALSE
                                )
                            },
                            "primitive_glyph" = {

                              pch <- glyph_to_pch(aesthetic$glyph)
                              bounded_id <- pch %in% 21:24

                              x <- aesthetic$x
                              y <- aesthetic$y

                              if(sum(bounded_id) != 0) {

                                point_size <- as_r_point_size(aesthetic$size[bounded_id])
                                size <<- c(size, point_size)

                                ggObj <- ggObj +
                                  ggplot2::geom_point(
                                    data = data.frame(x = x[bounded_id],
                                                      y = y[bounded_id],
                                                      fill = aesthetic$color[bounded_id],
                                                      pch = factor(pch[bounded_id]),
                                                      size = point_size),
                                    mapping = ggplot2::aes(x = x,
                                                           y = y,
                                                           fill = fill,
                                                           pch = pch,
                                                           size = size),
                                    colour = loon::l_getOption("foreground"),
                                    inherit.aes = FALSE
                                  )
                              }

                              if(sum(!bounded_id) != 0) {

                                point_size <- as_r_point_size(aesthetic$size[!bounded_id])
                                size <<- c(size, point_size)

                                ggObj <- ggObj +
                                  ggplot2::geom_point(
                                    data = data.frame(x = x[!bounded_id],
                                                      y = y[!bounded_id],
                                                      color = aesthetic$color[!bounded_id],
                                                      pch = factor(pch[!bounded_id]),
                                                      size = point_size),
                                    mapping = ggplot2::aes(x = x,
                                                           y = y,
                                                           color = color,
                                                           pch = pch,
                                                           size = size),
                                    inherit.aes = FALSE
                                  )
                              }

                              ggObj <<- ggObj
                            },
                            "pointrange" = {
                              gh <- loon::l_create_handle(c(widget, aesthetic$glyph[1]))

                              point_size <- as_r_point_size(aesthetic$size)
                              size <<- c(size, point_size)

                              ggObj <<- ggObj +
                                geom_pointrangeGlyph(
                                  data = data.frame(x = aesthetic$x,
                                                    y = aesthetic$y,
                                                    color = aesthetic$color,
                                                    size = point_size),
                                  mapping = ggplot2::aes(x = x, y = y, color = color, size = size),
                                  showArea = gh["showArea"],
                                  ymin = gh["ymin"][aesthetic$index],
                                  ymax = gh["ymax"][aesthetic$index],
                                  linewidth = gh["linewidth"][aesthetic$index],
                                  inherit.aes = FALSE
                                )
                            },
                            "image" = {
                              gh <- loon::l_create_handle(c(widget, aesthetic$glyph[1]))
                              tcl_img <- gh['images'][aesthetic$index]
                              size <- aesthetic$size
                              width_p <- height_p <- c()
                              images <- lapply(1:length(tcl_img),
                                               function(i) {

                                                 height <- as.numeric(tcltk::tcl("image", "height", tcl_img[i]))
                                                 width <- as.numeric(tcltk::tcl("image", "width", tcl_img[i]))

                                                 area <- as.numeric(tcltk::tcl("::loon::map_image_size", size[i]))

                                                 scale <- sqrt(area/(width*height))

                                                 image_w <- floor(scale*width)
                                                 image_h <- floor(scale*height)

                                                 width_p[i] <<- image_w
                                                 height_p[i] <<- image_h

                                                 scaled_img <- as.character(tcltk::tkimage.create("photo"))
                                                 tcltk::tcl(tcltk::tcl("set", "::loon::Options(image_scale)"),
                                                            tcl_img[i],
                                                            image_w,
                                                            image_h,
                                                            scaled_img)
                                                 # get the scaled_image
                                                 image <- tcl_img_2_r_raster(scaled_img)
                                                 tcl("image", "delete", scaled_img)
                                                 image
                                               })

                              ggObj <<- ggObj +
                                geom_imageGlyph(
                                  data = data.frame(x = aesthetic$x,
                                                    y = aesthetic$y,
                                                    fill = aesthetic$color,
                                                    color = aesthetic$color,
                                                    size = aesthetic$size),
                                  mapping = ggplot2::aes(x = x,
                                                         y = y,
                                                         fill = fill,
                                                         color = color,
                                                         size = size),
                                  images = images,
                                  width = width_p/15,
                                  height = height_p/15,
                                  inherit.aes = FALSE
                                )
                            }
                     )
                   })
  }

  uni_color <- unique(color[!is.na(color)])
  uni_pch <- unique(pch[!is.na(pch)])
  uni_size <- unique(size[!is.na(size)])

  ggObj <- ggObj +
    ggplot2::scale_color_manual(values = stats::setNames(uni_color, nm = uni_color),
                                labels = stats::setNames(selection_color_labels(uni_color), nm = uni_color)) +
    ggplot2::scale_fill_manual(values = stats::setNames(uni_color, nm = uni_color),
                               labels = stats::setNames(selection_color_labels(uni_color), nm = uni_color)) +
    ggplot2::scale_shape_manual(values = stats::setNames(uni_pch, nm = uni_pch)) +
    ggplot2::scale_size(range = range(size[!is.na(size)]))

  if(length(uni_color) == 1)
    ggObj <- ggObj + ggplot2::guides(color = FALSE, fill = FALSE)
  if(length(uni_size) == 1)
    ggObj <- ggObj + ggplot2::guides(size = FALSE)
  if(length(uni_pch) == 1)
    ggObj <- ggObj + ggplot2::guides(pch = FALSE)

  return(ggObj)
}

selection_color_labels <- function(x, name = "select") {

  select_color <- loon::l_getOption("select-color")
  if(select_color %in% x) {
    x[x == select_color] <- name
  }

  x
}

hack_pch <- function(x) {
  vapply(x,
         function(k) {
           xx <- if(is.na(k)) {
             k
           } else {
             if(k == 21) {
               19
             } else if(k == 22) {
               15
             } else if(k == 23) {
               18
             } else if(k == 24) {
               17
             } else if(k == 25) {
               6
             } else k
           }
           return(xx)
         }, numeric(1))
}
