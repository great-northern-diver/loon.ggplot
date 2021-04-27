scatterplotAsAesTRUE <- function(ggObj, widget, x, y, glyph, color, size, index) {

  pch <- glyph_to_pch(glyph)

  if (!any(is.na(pch)) && !any(pch %in% 21:24)) {

    size <- as_r_point_size(size)

    ggObj <- ggObj +
      ggplot2::geom_point(
        data = data.frame(x = x,
                          y = y,
                          color = color,
                          size = size),
        mapping = ggplot2::aes(x = x, y = y, color = color,
                               size = size),
        shape = pch,
        inherit.aes = FALSE
      )

  } else if (!any(is.na(pch)) && all(pch %in% 21:24)) {

    size <- as_r_point_size(size)

    # No NAs and ALL points with borders
    for(p in unique(pch)) {

      pid <- pch == p

      ggObj <- ggObj +
        ggplot2::geom_point(
          data = data.frame(x = x[pid],
                            y = y[pid],
                            fill = color[pid],
                            size = size[pid]),
          mapping = ggplot2::aes(x = x, y = y,
                                 fill = fill,
                                 size = size),
          shape = p,
          inherit.aes = FALSE
        )
    }

  } else {
    # possibly some NAs (means some points are text, polygons, images, etc.)
    # and/or a mix of regular and closed points.
    type <- sapply(glyph, function(glyph) loon::l_glyph_getType(widget, glyph))

    uni_type <- unique(type)
    # side effect to speed up the loop
    # feel free to change to make the code more readable
    lapply(uni_type,
           function(utype) {

             id <- which(type == utype)

             aesthetic <- list(
               x = x[id],
               y = y[id],
               glyph = glyph[id],
               color = color[id],
               size = size[id],
               index = index[id]
             )

             switch(utype,
                    "polygon" = {
                      gh <- loon::l_create_handle(c(widget, aesthetic$glyph[1]))

                      # `showArea` is a length n logical value
                      showArea <- gh['showArea'][aesthetic$index]

                      point_size <- as_ggplot_size(aesthetic$size)
                      size[id] <- point_size
                      size <<- size

                      if(sum(!showArea) > 0) {
                        ggObj <- ggObj +
                          ggmulti::geom_polygon_glyph(
                            data = data.frame(x = aesthetic$x[!showArea],
                                              y = aesthetic$y[!showArea],
                                              color = aesthetic$color[!showArea],
                                              size = point_size[!showArea]),
                            mapping = ggplot2::aes(x = x,
                                                   y = y,
                                                   color = color,
                                                   size = size),
                            fill = NA,
                            linewidth = gh['linewidth'][aesthetic$index][!showArea],
                            polygon_x = gh['x'][aesthetic$index][!showArea],
                            polygon_y = lapply(gh['y'][aesthetic$index], function(y) -y)[!showArea],
                            inherit.aes = FALSE
                          )
                      }

                      if(sum(showArea) > 0) {
                        ggObj <- ggObj +
                          ggmulti::geom_polygon_glyph(
                            data = data.frame(x = aesthetic$x[showArea],
                                              y = aesthetic$y[showArea],
                                              fill = aesthetic$color[showArea],
                                              size = point_size[showArea]),
                            mapping = ggplot2::aes(x = x,
                                                   y = y,
                                                   fill = fill,
                                                   size = size),
                            linewidth = gh['linewidth'][aesthetic$index][showArea],
                            polygon_x = gh['x'][aesthetic$index][showArea],
                            polygon_y = lapply(gh['y'][aesthetic$index], function(y) -y)[showArea],
                            inherit.aes = FALSE
                          )
                      }
                      # UPDATE ggObj
                      ggObj <<- ggObj
                    },
                    "serialaxes" = {
                      gh <- loon::l_create_handle(c(widget, aesthetic$glyph[1]))
                      # loon data will be converted into character by default

                      point_size <- as_ggplot_size(aesthetic$size)
                      size[id] <- point_size
                      size <<- size

                      # `showArea` is a length 1 logical value
                      showArea <- gh['showArea']

                      if(showArea) {
                        dat <- data.frame(x = aesthetic$x,
                                          y = aesthetic$y,
                                          fill = aesthetic$color,
                                          size = point_size)
                        mapping <- ggplot2::aes(x = x, y = y,
                                                fill = fill,
                                                size = size)

                      } else {
                        dat <- data.frame(x = aesthetic$x,
                                          y = aesthetic$y,
                                          color = aesthetic$color,
                                          size = point_size)

                        mapping <- ggplot2::aes(x = x, y = y,
                                                color = color,
                                                size = size)
                      }

                      ggObj <<- ggObj +
                        ggmulti::geom_serialaxes_glyph(
                          data = dat,
                          mapping = mapping,
                          serialaxes.data = char2num.data.frame(gh['data'][aesthetic$index, ]),
                          axes.sequence = gh['sequence'],
                          scaling = gh['scaling'],
                          andrews = gh['andrews'],
                          axes.layout = gh['axesLayout'],
                          show.axes = gh['showAxes'],
                          linewidth = gh['linewidth'][aesthetic$index],
                          show.enclosing = gh['showEnclosing'],
                          axescolour = as_hex6color(gh['axesColor']),
                          bboxcolour = as_hex6color(gh['bboxColor']),
                          inherit.aes = FALSE
                        )
                    },
                    "text" = {
                      gh <- loon::l_create_handle(c(widget, aesthetic$glyph[1]))
                      label <- gh["text"][aesthetic$index]

                      text_size <- as_r_text_size(aesthetic$size)
                      # update size by text adjustment
                      size[id] <- text_size
                      size <<- size

                      ggObj <<- ggObj +
                        ggplot2::geom_text(
                          data = data.frame(x = aesthetic$x,
                                            y = aesthetic$y,
                                            color = aesthetic$color,
                                            size = text_size,
                                            label = label),
                          mapping = ggplot2::aes(x = x, y = y,
                                                 label = label,
                                                 color = color,
                                                 size = size),
                          inherit.aes = FALSE
                        )
                    },
                    "primitive_glyph" = {

                      point_pch <- glyph_to_pch(aesthetic$glyph)
                      bounded_id <- point_pch %in% 21:24

                      x <- aesthetic$x
                      y <- aesthetic$y

                      point_size <- as_r_point_size(aesthetic$size)
                      size[id] <- point_size
                      size <<- size

                      if(sum(bounded_id, na.rm = TRUE) > 0) {

                        for(p in unique(point_pch[bounded_id])) {

                          pid <- point_pch[bounded_id] == p

                          ggObj <- ggObj +
                            ggplot2::geom_point(
                              data = data.frame(x = x[bounded_id][pid],
                                                y = y[bounded_id][pid],
                                                size = point_size[bounded_id][pid],
                                                fill = aesthetic$color[bounded_id][pid]),
                              mapping = ggplot2::aes(x = x,
                                                     y = y,
                                                     size = size,
                                                     fill = fill),
                              shape = p,
                              inherit.aes = FALSE
                            )
                        }
                      }

                      if(sum(!bounded_id, na.rm = TRUE) > 0) {

                        ggObj <- ggObj +
                          ggplot2::geom_point(
                            data = data.frame(x = x[!bounded_id],
                                              y = y[!bounded_id],
                                              color = aesthetic$color[!bounded_id],
                                              size = point_size[!bounded_id]),
                            mapping = ggplot2::aes(x = x,
                                                   y = y,
                                                   color = color,
                                                   size = size),
                            shape = point_pch[!bounded_id],
                            inherit.aes = FALSE
                          )
                      }

                      ggObj <<- ggObj
                    },
                    "pointrange" = {
                      gh <- loon::l_create_handle(c(widget, aesthetic$glyph[1]))
                      # showArea
                      point_pch <- ifelse(gh["showArea"], 1, 16)

                      point_size <- as_ggplot_size(aesthetic$size)
                      size[id] <- point_size
                      size <<- size

                      ggObj <<- ggObj +
                        ggplot2::geom_pointrange(
                          data = data.frame(x = aesthetic$x,
                                            y = aesthetic$y,
                                            ymin = gh["ymin"][aesthetic$index],
                                            ymax = gh["ymax"][aesthetic$index],
                                            color = aesthetic$color,
                                            size = point_size),
                          mapping = ggplot2::aes(x = x, y = y,
                                                 color = color,
                                                 size = size,
                                                 ymin = ymin,
                                                 ymax = ymax),
                          pch = point_pch,
                          inherit.aes = FALSE
                        )
                    },
                    "image" = {
                      gh <- loon::l_create_handle(c(widget, aesthetic$glyph[1]))
                      tcl_img <- gh['images'][aesthetic$index]

                      point_size <- as_ggplot_size(aesthetic$size)
                      size[id] <- point_size
                      size <<- size

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
                                            y = aesthetic$y,
                                            size = point_size,
                                            fill = aesthetic$color),
                          mapping = ggplot2::aes(x = x,
                                                 y = y,
                                                 size = size,
                                                 fill = fill),
                          color = aesthetic$color,
                          images = images,
                          imagewidth = adjust_image_size(width_p),
                          imageheight = adjust_image_size(height_p),
                          inherit.aes = FALSE
                        )
                    }
             )
           })
  }

  uni_color <- unique(color[!is.na(color)])
  if(length(uni_color) > 0) {

    ggObj <- ggObj +
      ggplot2::scale_color_manual(values = uni_color,
                                  labels = selection_color_labels(
                                    uni_color
                                  ),
                                  breaks = uni_color) +
      ggplot2::scale_fill_manual(values = uni_color,
                                 labels = selection_color_labels(
                                   uni_color
                                 ),
                                 breaks = uni_color)
  }

  if(length(uni_color) <= 1) {
    ggObj <- ggObj + ggplot2::guides(color = FALSE, fill = FALSE)
  }

  uni_size <- unique(size[!is.na(size)])
  if(length(uni_size) > 0) {
    ggObj <- ggObj +
      ggplot2::scale_size(range = range(size[!is.na(size)]))
  }

  if(length(uni_size) <= 1)
    ggObj <- ggObj + ggplot2::guides(size = FALSE)

  return(ggObj)
}

scatterplotAsAesFALSE <- function(ggObj, widget, x, y, glyph, color, size, index) {

  pch <- glyph_to_pch(glyph)

  if (!any(is.na(pch)) && !any(pch %in% 21:24)) {

    size <- as_ggplot_size(size)

    # No NAs and no points with borders
    ggObj <- ggObj +
      ggplot2::geom_point(
        data = data.frame(x = x,
                          y = y),
        mapping = ggplot2::aes(x = x, y = y),
        color = color,
        shape = pch,
        size = size,
        inherit.aes = FALSE
      )

  } else if (!any(is.na(pch)) && all(pch %in% 21:24)) {

    size <- as_ggplot_size(size)

    # No NAs and ALL points with borders
    ggObj <- ggObj +
      ggplot2::geom_point(
        data = data.frame(x = x,
                          y = y),
        mapping = ggplot2::aes(x = x, y = y),
        fill = color,
        size = size,
        color = loon::l_getOption("foreground"),
        shape = pch,
        inherit.aes = FALSE
      )
  } else {
    # possibly some NAs (means some points are text, polygons, images, etc.)
    # and/or a mix of regular and closed points.
    type <- sapply(glyph, function(glyph) loon::l_glyph_getType(widget, glyph))

    uni_type <- unique(type)
    # side effect to speed the loop
    # actually, for loop would not be much slower here (loop on type)
    # feel free to change to make the code more readable
    lapply(uni_type,
           function(utype) {

             id <- which(type == utype)

             aesthetic <- list(
               x = x[id],
               y = y[id],
               glyph = glyph[id],
               color = color[id],
               size = size[id],
               index = index[id]
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
                          andrews = gh['andrews'],
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
                            size = as_ggplot_size(aesthetic$size[bounded_id]),
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
                            size = as_ggplot_size(aesthetic$size[!bounded_id]),
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
                          size = as_ggplot_size(aesthetic$size),
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

