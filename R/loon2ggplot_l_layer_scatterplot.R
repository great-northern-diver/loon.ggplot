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

  pch <- glyph_to_pch(s_a$glyph)

  if (!any(is.na(pch)) && !any(pch %in% 21:24)) {

    # No NAs and no points with borders
    ggObj <- ggObj +
      ggplot2::geom_point(
        data = data.frame(x = as.numeric(s_a$x),
                          y = as.numeric(s_a$y)),
        mapping = ggplot2::aes(x = x, y = y),
        color = s_a$color,
        shape = pch,
        size = as_r_point_size(s_a$size),
        inherit.aes = FALSE
      )

  } else if (!any(is.na(pch)) && all(pch %in% 21:24)) {

    # No NAs and ALL points with borders
    ggObj <- ggObj +
      ggplot2::geom_point(
        data = data.frame(x = as.numeric(s_a$x),
                          y = as.numeric(s_a$y)),
        mapping = ggplot2::aes(x = x, y = y),
        fill = s_a$color,
        size = as_r_point_size(s_a$size),
        color = loon::l_getOption("foreground"),
        shape = pch,
        inherit.aes = FALSE
      )

  } else {
    # possibly some NAs (means some points are text, polygons, images, etc.)
    # and/or a mix of regular and closed points.
    type <- sapply(s_a$glyph, function(glyph) loon::l_glyph_getType(widget, glyph))

    uni_type <- unique(type)
    # side effect to speed the loop
    # actually, for loop would not be much slower here (loop on type)
    # feel free to change to make the code more readable
    lapply(uni_type,
           function(utype) {

             id <- which(type == utype)

             aesthetic <- list(
               x = as.numeric(s_a$x[id]),
               y = as.numeric(s_a$y[id]),
               glyph = s_a$glyph[id],
               color = s_a$color[id],
               size = s_a$size[id],
               index = s_a$index[id]
             )

             switch(utype,
                    "polygon" = {
                      gh <- loon::l_create_handle(c(widget, aesthetic$glyph[1]))

                      ggObj <<- ggObj +
                        ggmulti::geom_polygon_glyph(
                          data = data.frame(x = aesthetic$x,
                                            y = aesthetic$y),
                          mapping = ggplot2::aes(x = x,
                                                 y = y),
                          fill = ifelse(gh['showArea'][aesthetic$index], aesthetic$color, NA),
                          color = aesthetic$color,
                          size = as_ggplot_size(aesthetic$size),
                          polygon_x = gh['x'][aesthetic$index],
                          polygon_y = lapply(gh['y'][aesthetic$index], function(y) -y),
                          linewidth = gh['linewidth'][aesthetic$index],
                          inherit.aes = FALSE
                        )
                    },
                    "serialaxes" = {
                      gh <- loon::l_create_handle(c(widget, aesthetic$glyph[1]))
                      # loon data will be converted into character by default

                      ggObj <<- ggObj +
                        ggmulti::geom_serialaxes_glyph(
                          data = data.frame(x = aesthetic$x,
                                            y = aesthetic$y),
                          mapping = ggplot2::aes(x = x, y = y),
                          fill = ifelse(gh['showArea'][aesthetic$index], aesthetic$color, NA),
                          color = aesthetic$color,
                          size = as_ggplot_size(aesthetic$size),
                          serialaxes.data = char2num.data.frame(gh['data'][aesthetic$index, ]),
                          axes.sequence = gh['sequence'],
                          scaling = gh['scaling'],
                          axes.layout = gh['axesLayout'],
                          show.axes = gh['showAxes'],
                          show.enclosing = gh['showEnclosing'],
                          axescolour = as_hex6color(gh['axesColor']),
                          bboxcolour = as_hex6color(gh['bboxColor']),
                          linewidth = gh['linewidth'][aesthetic$index],
                          inherit.aes = FALSE
                        )
                    },
                    "text" = {
                      gh <- loon::l_create_handle(c(widget, aesthetic$glyph[1]))
                      label <- gh["text"][aesthetic$index]

                      ggObj <<- ggObj +
                        ggplot2::geom_text(
                          data = data.frame(x = aesthetic$x,
                                            y = aesthetic$y,
                                            label = label),
                          mapping = ggplot2::aes(x = x, y = y, label = label),
                          color = aesthetic$color,
                          size = as_r_text_size(aesthetic$size),
                          inherit.aes = FALSE
                        )
                    },
                    "primitive_glyph" = {

                      pch <- glyph_to_pch(aesthetic$glyph)
                      bounded_id <- pch %in% 21:24

                      x <- aesthetic$x
                      y <- aesthetic$y

                      if(sum(bounded_id, na.rm = TRUE) != 0) {

                        ggObj <- ggObj +
                          ggplot2::geom_point(
                            data = data.frame(x = x[bounded_id],
                                              y = y[bounded_id]),
                            mapping = ggplot2::aes(x = x,
                                                   y = y),
                            fill = aesthetic$color[bounded_id],
                            pch = pch[bounded_id],
                            size = as_r_point_size(aesthetic$size[bounded_id]),
                            colour = loon::l_getOption("foreground"),
                            inherit.aes = FALSE
                          )
                      }

                      if(sum(!bounded_id, na.rm = TRUE) != 0) {

                        ggObj <- ggObj +
                          ggplot2::geom_point(
                            data = data.frame(x = x[!bounded_id],
                                              y = y[!bounded_id]),
                            mapping = ggplot2::aes(x = x,
                                                   y = y),
                            color = aesthetic$color[!bounded_id],
                            pch = pch[!bounded_id],
                            size = as_r_point_size(aesthetic$size[!bounded_id]),
                            inherit.aes = FALSE
                          )
                      }

                      ggObj <<- ggObj
                    },
                    "pointrange" = {
                      gh <- loon::l_create_handle(c(widget, aesthetic$glyph[1]))

                      # showArea
                      pch <- ifelse(gh["showArea"], 21, 19)
                      fill <- ifelse(gh["showArea"], aesthetic$color, NA)

                      ggObj <<- ggObj +
                        ggplot2::geom_pointrange(
                          data = data.frame(x = aesthetic$x,
                                            y = aesthetic$y,
                                            ymin = gh["ymin"][aesthetic$index],
                                            ymax = gh["ymax"][aesthetic$index]),
                          mapping = ggplot2::aes(x = x, y = y, ymin = ymin, ymax = ymax),
                          color = aesthetic$color,
                          fill = fill,
                          pch = pch,
                          size = as_ggplot_size(aesthetic$size),
                          inherit.aes = FALSE
                        )
                    },
                    "image" = {
                      gh <- loon::l_create_handle(c(widget, aesthetic$glyph[1]))
                      tcl_img <- gh['images'][aesthetic$index]
                      size <- aesthetic$size
                      width_p <- height_p <- c()

                      images <- lapply(seq(length(tcl_img)),
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
                        ggmulti::geom_image_glyph(
                          data = data.frame(x = aesthetic$x,
                                            y = aesthetic$y),
                          mapping = ggplot2::aes(x = x,
                                                 y = y),
                          fill = aesthetic$color,
                          color = aesthetic$color,
                          size = as_ggplot_size(aesthetic$size, 1/6),
                          images = images,
                          imagewidth = adjust_image_size(width_p),
                          imageheight = adjust_image_size(height_p),
                          inherit.aes = FALSE
                        )
                    }
             )
           })
  }

  return(ggObj)
}

selection_color_labels <- function(x, name = "select") {

  select_color <- loon::l_getOption("select-color")
  if(select_color %in% x) {
    x[x == select_color] <- name
  }

  x
}
