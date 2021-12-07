scatterplotAsAesTRUE <- function(ggObj, widget, x, y,
                                 glyph, color, size, index, selectedOnTop = TRUE) {

  pch <- glyph_to_pch(glyph)

  # points with boundary
  pointsWithBoundary <- pch %in% 21:24
  withBoundary <- FALSE
  fill <- color

  if (!any(is.na(pch)) && !any(pch %in% 21:24)) {

    size <- as_ggplot_size(size)

    ggObj <- ggObj +
      ggplot2::geom_point(
        data = data.frame(x = x,
                          y = y,
                          color = color,
                          size = size),
        mapping = ggplot2::aes(x = x, y = y, color = color,
                               size = size),
        shape = pch
      )

  } else if (!any(is.na(pch)) && all(pch %in% 21:24)) {

    withBoundary <- TRUE
    size <- as_ggplot_size(size)

    ggObj <- ggObj +
      ggplot2::geom_point(
        data = data.frame(x = x,
                          y = y,
                          fill = fill,
                          size = size),
        mapping = ggplot2::aes(x = x, y = y,
                               fill = fill,
                               size = size),
        shape = pch
      )

    # # No NAs and ALL points with borders
    # if(!selectedOnTop) {
    #
    #   # to preserve orders
    #   # the shape of the points may not be satisfying
    #   ggObj <- ggObj +
    #     ggplot2::geom_point(
    #       data = data.frame(x = x,
    #                         y = y,
    #                         fill = color,
    #                         size = size),
    #       mapping = ggplot2::aes(x = x, y = y,
    #                              fill = fill,
    #                              size = size),
    #       shape = pch
    #     )
    #
    # } else {
    #
    #   for(p in unique(pch)) {
    #
    #     pid <- pch == p
    #
    #     ggObj <- ggObj +
    #       ggplot2::geom_point(
    #         data = data.frame(x = x[pid],
    #                           y = y[pid],
    #                           fill = color[pid],
    #                           size = size[pid]),
    #         mapping = ggplot2::aes(x = x, y = y,
    #                                fill = fill,
    #                                size = size),
    #         shape = p
    #       )
    #   }
    # }
  } else {
    # possibly some NAs (means some points are text, polygons, images, etc.)
    # and/or a mix of regular and closed points.
    type <- sapply(glyph, function(g) loon::l_glyph_getType(widget, g))
    types <- paste(type, names(type), sep = ".")
    uniqueTypes <- unique(types)

    lenUniqueTypes <- length(uniqueTypes)
    if(lenUniqueTypes > 1 && !selectedOnTop) {
      warning("More than one non-primitive glyphs are detected. ",
              "The selected points will be always on top. ",
              "The displayed order may be different from the original data set order.",
              call. = FALSE)}

    for(utypes in uniqueTypes) {

      id <- which(types == utypes)

      aesthetic <- list(
        x = x[id],
        y = y[id],
        glyph = glyph[id],
        color = color[id],
        size = size[id],
        index = index[id]
      )

      utype <- strsplit(utypes, "[.]")[[1L]][1L]

      switch(utype,
             "polygon" = {
               gh <- loon::l_create_handle(c(widget, aesthetic$glyph[1L]))

               # `showArea` is a length `n` logical value
               showArea <- gh['showArea'][aesthetic$index]

               pointSize <- as_ggplot_size(aesthetic$size, type = "polygon", adjust = 0.6)
               size[id] <- pointSize

               if(!selectedOnTop && lenUniqueTypes == 1) {
                 if(all(showArea) || all(!showArea)) {
                   NULL
                 } else {
                   warning("To preserve the elements order, `showArea` must be either TRUE or FALSE. " ,
                           "The `showArea` is set TRUE",
                           call. = FALSE)
                   showArea <- rep(TRUE, length(id))
                 }
               }

               if(sum(!showArea) > 0) {
                 ggObj <- ggObj +
                   ggmulti::geom_polygon_glyph(
                     data = data.frame(x = aesthetic$x[!showArea],
                                       y = aesthetic$y[!showArea],
                                       color = aesthetic$color[!showArea],
                                       size = pointSize[!showArea]),
                     mapping = ggplot2::aes(x = x,
                                            y = y,
                                            color = color,
                                            size = size),
                     fill = NA,
                     linewidth = gh['linewidth'][aesthetic$index][!showArea],
                     polygon_x = gh['x'][aesthetic$index][!showArea],
                     polygon_y = lapply(gh['y'][aesthetic$index], function(y) -y)[!showArea]
                   )
               }

               if(sum(showArea) > 0) {
                 ggObj <- ggObj +
                   ggmulti::geom_polygon_glyph(
                     data = data.frame(x = aesthetic$x[showArea],
                                       y = aesthetic$y[showArea],
                                       fill = aesthetic$color[showArea],
                                       size = pointSize[showArea]),
                     mapping = ggplot2::aes(x = x,
                                            y = y,
                                            fill = fill,
                                            size = size),
                     colour = NA,
                     linewidth = gh['linewidth'][aesthetic$index][showArea],
                     polygon_x = gh['x'][aesthetic$index][showArea],
                     polygon_y = lapply(gh['y'][aesthetic$index], function(y) -y)[showArea]
                   )
               }
             },
             "serialaxes" = {

               gh <- loon::l_create_handle(c(widget, aesthetic$glyph[1L]))
               # loon data will be converted into character by default

               # make the scaling operation is applied on the whole data set
               # rather the subset of it
               serialaxes.data <- get_scaledData(char2num.data.frame(gh['data']),
                                                 scaling = gh['scaling'],
                                                 as.data.frame = TRUE)[aesthetic$index, ]

               sequence <- gh['sequence']
               axesLayout <- gh['axesLayout']
               lenSeq <- length(sequence)
               if(lenSeq == 0) lenSeq <- ncol(serialaxes.data)

               pointSize <- as_ggplot_size(aesthetic$size,
                                           type = axesLayout,
                                           p = lenSeq)
               size[id] <- pointSize

               # `showArea` is a length `1` logical value
               showArea <- gh['showArea']

               if(showArea) {
                 dat <- data.frame(x = aesthetic$x,
                                   y = aesthetic$y,
                                   fill = aesthetic$color,
                                   size = pointSize)
                 mapping <- ggplot2::aes(x = x, y = y,
                                         fill = fill,
                                         size = size)

               } else {
                 dat <- data.frame(x = aesthetic$x,
                                   y = aesthetic$y,
                                   color = aesthetic$color,
                                   size = pointSize)

                 mapping <- ggplot2::aes(x = x, y = y,
                                         color = color,
                                         size = size)
               }

               ggObj <- ggObj +
                 ggmulti::geom_serialaxes_glyph(
                   data = dat,
                   mapping = mapping,
                   serialaxes.data = serialaxes.data,
                   axes.sequence = sequence,
                   scaling = "none",
                   andrews = gh['andrews'],
                   axes.layout = axesLayout,
                   show.axes = gh['showAxes'],
                   linewidth = gh['linewidth'][aesthetic$index],
                   show.enclosing = gh['showEnclosing'],
                   axescolour = as_hex6color(gh['axesColor']),
                   bboxcolour = as_hex6color(gh['bboxColor'])
                 )
             },
             "text" = {
               gh <- loon::l_create_handle(c(widget, aesthetic$glyph[1L]))
               label <- gh["text"][aesthetic$index]

               text_size <- as_ggplot_size(aesthetic$size,
                                           type = "texts")
               # update size by text adjustment
               size[id] <- text_size

               ggObj <- ggObj +
                 ggplot2::geom_text(
                   data = data.frame(x = aesthetic$x,
                                     y = aesthetic$y,
                                     color = aesthetic$color,
                                     size = text_size,
                                     label = label),
                   mapping = ggplot2::aes(x = x, y = y,
                                          label = label,
                                          color = color,
                                          size = size)
                 )
             },
             "primitive_glyph" = {

               point_pch <- glyph_to_pch(aesthetic$glyph)
               pointsWithBoundary <- point_pch %in% 21:24

               xx <- aesthetic$x
               yy <- aesthetic$y

               pointSize <- as_ggplot_size(aesthetic$size)
               size[id] <- pointSize

               if(!selectedOnTop && lenUniqueTypes == 1) {

                 ggObj <- ggObj +
                   ggplot2::geom_point(
                     data = data.frame(x = xx,
                                       y = yy,
                                       size = pointSize,
                                       color = aesthetic$color,
                                       fill = aesthetic$color),
                     mapping = ggplot2::aes(x = x,
                                            y = y,
                                            colour = color,
                                            size = size,
                                            fill = fill),
                     shape = point_pch
                   )

               } else {

                 if(sum(pointsWithBoundary, na.rm = TRUE) > 0) {

                   for(p in unique(point_pch[pointsWithBoundary])) {

                     pid <- point_pch[pointsWithBoundary] == p

                     ggObj <- ggObj +
                       ggplot2::geom_point(
                         data = data.frame(x = xx[pointsWithBoundary][pid],
                                           y = yy[pointsWithBoundary][pid],
                                           size = pointSize[pointsWithBoundary][pid],
                                           fill = aesthetic$color[pointsWithBoundary][pid]),
                         mapping = ggplot2::aes(x = x,
                                                y = y,
                                                size = size,
                                                fill = fill),
                         shape = p
                       )
                   }
                 }

                 if(sum(!pointsWithBoundary, na.rm = TRUE) > 0) {

                   ggObj <- ggObj +
                     ggplot2::geom_point(
                       data = data.frame(x = xx[!pointsWithBoundary],
                                         y = yy[!pointsWithBoundary],
                                         color = aesthetic$color[!pointsWithBoundary],
                                         size = pointSize[!pointsWithBoundary]),
                       mapping = ggplot2::aes(x = x,
                                              y = y,
                                              color = color,
                                              size = size),
                       shape = point_pch[!pointsWithBoundary]
                     )
                 }

               }

             },
             "pointrange" = {
               gh <- loon::l_create_handle(c(widget, aesthetic$glyph[1L]))
               # showArea
               point_pch <- ifelse(gh["showArea"], 1, 19)

               # ggplot default value
               fatten <- 4
               pointSize <- as_ggplot_size(aesthetic$size)/fatten
               size[id] <- pointSize

               ggObj <- ggObj +
                 ggplot2::geom_pointrange(
                   data = data.frame(x = aesthetic$x,
                                     y = aesthetic$y,
                                     ymin = gh["ymin"][aesthetic$index],
                                     ymax = gh["ymax"][aesthetic$index],
                                     color = aesthetic$color,
                                     size = pointSize),
                   mapping = ggplot2::aes(x = x, y = y,
                                          color = color,
                                          size = size,
                                          ymin = ymin,
                                          ymax = ymax),
                   pch = point_pch
                 )
             },
             "image" = {
               gh <- loon::l_create_handle(c(widget, aesthetic$glyph[1L]))
               tcl_img <- gh['images'][aesthetic$index]

               ratio <- c()
               images <- lapply(seq(length(tcl_img)),
                                function(i) {

                                  height <- as.numeric(tcltk::tcl("image", "height", tcl_img[i]))
                                  width <- as.numeric(tcltk::tcl("image", "width", tcl_img[i]))

                                  r <- height/width
                                  ratio[i] <<- r

                                  img <- as.character(tcltk::tkimage.create("photo"))
                                  tcltk::tcl(tcltk::tcl("set", "::loon::Options(image_scale)"),
                                             tcl_img[i], round(width),
                                             round(height), img)
                                  # get the image
                                  image <- tcl_img_2_r_raster(img)
                                  tcl("image", "delete", img)
                                  image
                                })

               height <- as_ggplot_size(aesthetic$size,
                                        type = "image",
                                        ratio = ratio)
               width <- height/ratio
               # THIS IS A HACK!
               imageSize <- 0.6
               size[id] <- imageSize

               ggObj <- ggObj +
                 ggmulti::geom_image_glyph(
                   data = data.frame(x = aesthetic$x,
                                     y = aesthetic$y,
                                     fill = aesthetic$color),
                   mapping = ggplot2::aes(x = x,
                                          y = y,
                                          fill = fill),
                   size = imageSize,
                   color = aesthetic$color,
                   images = images,
                   imagewidth = width,
                   imageheight = height
                 )
             }
      )
    }
  }

  if(withBoundary) {
    ggObj <- ggObj +
      ggplot2::guides(
        fill = ggplot2::guide_legend(
          override.aes = list(fill = unique(fill),
                              shape = 21)
        )
      )
  }

  uniColor <- unique(color[!is.na(color)])
  if(length(uniColor) > 0) {

    ggObj <- ggObj +
      ggplot2::scale_color_manual(values = uniColor,
                                  labels = uniColor,
                                  breaks = uniColor) +
      ggplot2::scale_fill_manual(values = uniColor,
                                 labels = uniColor,
                                 breaks = uniColor)
  }

  # Discussion: should the legend of color and fill be omitted, if
  # the number of unique color/fill is 1?
  if(length(uniColor) <= 1)
    ggObj <- ggObj + ggplot2::guides(color = "none", fill = "none")

  uniSize <- unique(size[!is.na(size)])
  if(length(uniSize) > 0)
    ggObj <- ggObj +
    ggplot2::scale_size_identity(guide = "legend")

  if(length(uniSize) <= 1)
    ggObj <- ggObj + ggplot2::guides(size = "none")

  return(ggObj)
}

scatterplotAsAesFALSE <- function(ggObj, widget, x, y,
                                  glyph, color, size, index,
                                  selectedOnTop = TRUE) {

  pch <- glyph_to_pch(glyph)

  if (!any(is.na(pch))) {

    # points with boundary
    pointsWithBoundary <- pch %in% 21:24

    fill <- rep(NA, length(color))
    fill[pointsWithBoundary] <- color[pointsWithBoundary]
    color[pointsWithBoundary] <- loon::l_getOption("foreground")

    # No NAs and no points with borders
    ggObj <- ggObj +
      ggplot2::geom_point(
        fill = fill,
        size = as_ggplot_size(size),
        colour = color,
        shape = pch
      )

  } else {
    # possibly some NAs (means some points are text, polygons, images, etc.)
    # and/or a mix of regular and closed points.
    type <- sapply(glyph, function(glyph) loon::l_glyph_getType(widget, glyph))
    types <- paste(type, names(type), sep = ".")
    uniqueTypes <- unique(types)
    lenTypes <- length(uniqueTypes)

    lenUniqueTypes <- length(uniqueTypes)
    if(lenUniqueTypes > 1 && !selectedOnTop) {
      warning("More than one non-primitive glyphs are detected. ",
              "The selected points will be always on top. ",
              "The displayed order may be different from the original data set order.",
              call. = FALSE)
    }

    for(utypes in uniqueTypes) {

      id <- which(types == utypes)

      aesthetic <- list(
        x = x[id],
        y = y[id],
        glyph = glyph[id],
        color = color[id],
        size = size[id],
        index = index[id]
      )

      utype <- strsplit(utypes, "[.]")[[1L]][1L]

      switch(utype,
             "polygon" = {
               gh <- loon::l_create_handle(c(widget, aesthetic$glyph[1L]))

               ggObj <- ggObj +
                 do.call(ggmulti::geom_polygon_glyph,
                         remove_null(
                           data = if(lenTypes == 1) {
                             NULL
                           } else {
                             data.frame(x = aesthetic$x,
                                        y = aesthetic$y)
                           },
                           fill = ifelse(gh['showArea'][aesthetic$index],
                                         aesthetic$color,
                                         NA),
                           color = ifelse(gh['showArea'][aesthetic$index],
                                          loon::l_getOption("foreground"),
                                          aesthetic$color),
                           size = as_ggplot_size(aesthetic$size, type = "polygon", adjust = 0.6),
                           polygon_x = gh['x'][aesthetic$index],
                           polygon_y = lapply(gh['y'][aesthetic$index], function(y) -y),
                           linewidth = gh['linewidth'][aesthetic$index]
                         )
                 )
             },
             "serialaxes" = {
               gh <- loon::l_create_handle(c(widget, aesthetic$glyph[1L]))
               # loon data will be converted into character by default

               serialaxes.data <- get_scaledData(char2num.data.frame(gh['data']),
                                                 scaling = gh['scaling'],
                                                 as.data.frame = TRUE)[aesthetic$index, ]
               sequence <- gh['sequence']
               axesLayout <- gh['axesLayout']
               lenSeq <- length(sequence)
               if(lenSeq == 0) lenSeq <- ncol(serialaxes.data)

               ggObj <- ggObj +
                 do.call(
                   ggmulti::geom_serialaxes_glyph,
                   remove_null(
                     data = if(lenTypes == 1) {
                       NULL
                     } else {
                       data.frame(x = aesthetic$x,
                                  y = aesthetic$y)
                     },
                     mapping = ggplot2::aes(x = x, y = y),
                     fill = ifelse(gh['showArea'][aesthetic$index], aesthetic$color, NA),
                     color = aesthetic$color,
                     size = as_ggplot_size(aesthetic$size,
                                           type = axesLayout,
                                           p = lenSeq),
                     serialaxes.data = serialaxes.data,
                     axes.sequence = sequence,
                     scaling = "none",
                     andrews = gh['andrews'],
                     axes.layout = axesLayout,
                     show.axes = gh['showAxes'],
                     show.enclosing = gh['showEnclosing'],
                     axescolour = as_hex6color(gh['axesColor']),
                     bboxcolour = as_hex6color(gh['bboxColor']),
                     linewidth = gh['linewidth'][aesthetic$index]
                   )
                 )

             },
             "text" = {
               gh <- loon::l_create_handle(c(widget, aesthetic$glyph[1L]))
               label <- gh["text"][aesthetic$index]

               ggObj <- ggObj +
                 do.call(
                   ggplot2::geom_text,
                   remove_null(
                     data = if(lenTypes == 1) {
                       NULL
                     }else {
                       data.frame(x = aesthetic$x,
                                  y = aesthetic$y,
                                  label = label)
                     },
                     mapping = ggplot2::aes(label = label),
                     color = aesthetic$color,
                     size = as_ggplot_size(aesthetic$size,
                                           type = "texts")
                   )
                 )

             },
             "primitive_glyph" = {

               pch <- glyph_to_pch(aesthetic$glyph)
               pointsWithBoundary <- pch %in% 21:24

               xx <- aesthetic$x
               yy <- aesthetic$y

               fill <- rep(NA, length(aesthetic$color))
               colour <- aesthetic$color

               fill[pointsWithBoundary] <- colour[pointsWithBoundary]
               colour[pointsWithBoundary] <- loon::l_getOption("foreground")

               ggObj <- ggObj +
                 ggplot2::geom_point(
                   data = data.frame(x = xx,
                                     y = yy),
                   fill = fill,
                   pch = pch,
                   size = as_ggplot_size(aesthetic$size),
                   colour = colour
                 )
             },
             "pointrange" = {
               gh <- loon::l_create_handle(c(widget, aesthetic$glyph[1L]))

               # showArea
               pch <- ifelse(gh["showArea"], 1, 19)
               ymin <- gh["ymin"][aesthetic$index]
               ymax <- gh["ymax"][aesthetic$index]

               ggObj <- ggObj +
                 do.call(
                   ggplot2::geom_pointrange,
                   remove_null(
                     data = if(lenTypes == 1) {
                       NULL
                     } else {
                       data.frame(x = aesthetic$x,
                                  y = aesthetic$y,
                                  ymin = ymin,
                                  ymax = ymax)
                     },
                     mapping = ggplot2::aes(ymin = ymin, ymax = ymax),
                     color = aesthetic$color,
                     pch = pch,
                     size = as_ggplot_size(aesthetic$size)
                   )
                 )
             },
             "image" = {
               gh <- loon::l_create_handle(c(widget, aesthetic$glyph[1L]))
               tcl_img <- gh['images'][aesthetic$index]

               ratio <- c()
               images <- lapply(seq(length(tcl_img)),
                                function(i) {

                                  height <- as.numeric(tcltk::tcl("image", "height", tcl_img[i]))
                                  width <- as.numeric(tcltk::tcl("image", "width", tcl_img[i]))

                                  r <- height/width
                                  ratio[i] <<- r

                                  img <- as.character(tcltk::tkimage.create("photo"))
                                  tcltk::tcl(tcltk::tcl("set", "::loon::Options(image_scale)"),
                                             tcl_img[i], round(width),
                                             round(height), img)
                                  # get the image
                                  image <- tcl_img_2_r_raster(img)
                                  tcl("image", "delete", img)
                                  image
                                })


               height <- as_ggplot_size(aesthetic$size,
                                        type = "image",
                                        ratio = ratio)
               width <- height/ratio
               # THIS IS A HACK!
               imageSize <- 0.6

               ggObj <- ggObj +
                 do.call(
                   ggmulti::geom_image_glyph,
                   remove_null(
                     data = if(lenTypes == 1) {
                       NULL
                     } else {
                       data.frame(x = aesthetic$x,
                                  y = aesthetic$y)
                     },
                     fill = aesthetic$color,
                     color = aesthetic$color,
                     size = imageSize,
                     images = images,
                     imagewidth = width,
                     imageheight = height
                   )
                 )
             }
      )
    }
  }
  return(ggObj)
}
