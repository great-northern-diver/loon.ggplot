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
  if (!any(is.na(pch)) && !any(pch %in% 21:24)) {

    # No NAs and no points with borders
    ggObj <- ggObj +
      ggplot2::geom_point(
        data = data.frame(x = x,
                          y = y),
        mapping = ggplot2::aes(x = x, y = y),
        pch = pch,
        color = s_a$color,
        size = as_r_point_size(s_a$size),
        inherit.aes = FALSE
      )
  } else if (!any(is.na(pch)) && all(pch %in% 21:24)) {

    # No NAs and ALL points with borders
    ggObj <- ggObj +
      ggplot2::geom_point(
        data = data.frame(x = x,
                          y = y),
        mapping = ggplot2::aes(x = x, y = y),
        pch = pch,
        fill = s_a$color,
        color = loon::l_getOption("foreground"),
        size = as_r_point_size(s_a$size),
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

                              x <- aesthetic$x
                              y <- aesthetic$y

                              ggObj <<- ggObj +
                                geom_polygonGlyph(
                                  data = data.frame(x = x, y = y),
                                  mapping = ggplot2::aes(x = x, y = y),
                                  polygon_x = gh['x'][aesthetic$index],
                                  polygon_y = gh['y'][aesthetic$index],
                                  showArea = gh['showArea'][aesthetic$index],
                                  linewidth = gh['linewidth'][aesthetic$index],
                                  colour = aesthetic$color,
                                  fill = aesthetic$color,
                                  size = aesthetic$size,
                                  inherit.aes = FALSE
                                )
                            },
                            "serialaxes" = {
                              gh <- loon::l_create_handle(c(widget, aesthetic$glyph[1]))
                              # loon data will be converted into character by default
                              x <- aesthetic$x
                              y <- aesthetic$y

                              ggObj <<- ggObj +
                                geom_serialAxesGlyph(
                                  data = data.frame(x = x, y = y),
                                  mapping = ggplot2::aes(x = x, y = y),
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
                                  colour = aesthetic$color,
                                  inherit.aes = FALSE
                                )
                            },
                            "text" = {
                              gh <- loon::l_create_handle(c(widget, aesthetic$glyph[1]))
                              x <- aesthetic$x
                              y <- aesthetic$y

                              ggObj <<- ggObj +
                                geom_textGlyph(
                                  data = data.frame(x = x, y = y),
                                  mapping = ggplot2::aes(x = x, y = y),
                                  colour = aesthetic$color,
                                  text = gh["text"][aesthetic$index],
                                  inherit.aes = FALSE
                                )
                            },
                            "primitive_glyph" = {

                              pch <- glyph_to_pch(aesthetic$glyph)
                              # set fill
                              fill <- aesthetic$color
                              fill[pch %in% 16:18] <- NA
                              # set color
                              color <- aesthetic$color
                              color[pch %in% 21:24] <- loon::l_getOption("foreground")

                              x <- aesthetic$x
                              y <- aesthetic$y

                              ggObj <<- ggObj +
                                ggplot2::geom_point(
                                  data = data.frame(x = x, y = y),
                                  mapping = ggplot2::aes(x = x, y = y),
                                  pch = pch,
                                  size = as_r_point_size(aesthetic$size),
                                  colour = color,
                                  fill = fill,
                                  inherit.aes = FALSE
                                )
                            },
                            "pointrange" = {
                              gh <- loon::l_create_handle(c(widget, aesthetic$glyph[1]))

                              x <- aesthetic$x
                              y <- aesthetic$y

                              ggObj <<- ggObj +
                                geom_pointrangeGlyph(
                                  data = data.frame(x = x, y = y),
                                  mapping = ggplot2::aes(x = x, y = y),
                                  showArea = gh["showArea"],
                                  size = as_r_point_size(aesthetic$size),
                                  ymin = gh["ymin"][aesthetic$index],
                                  ymax = gh["ymax"][aesthetic$index],
                                  linewidth = gh["linewidth"][aesthetic$index],
                                  colour = aesthetic$color,
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

                              x <- aesthetic$x
                              y <- aesthetic$y

                              ggObj <<- ggObj +
                                geom_imageGlyph(
                                  data = data.frame(x = x, y = y),
                                  mapping = ggplot2::aes(x = x, y = y),
                                  images = images,
                                  width = width_p/15,
                                  height = height_p/15,
                                  fill = aesthetic$color,
                                  size = aesthetic$size,
                                  inherit.aes = FALSE
                                )
                            }
                     )
                   })
  }

  return(ggObj)
}
