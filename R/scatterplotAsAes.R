scatterplotAsAesTRUE <- function(ggObj, widget, x, y,
                                 glyph, color, size, index, selectedOnTop = TRUE) {

  pch <- glyph_to_pch(glyph)
  if(len_unique(pch) == 1L) pch <- pch[1L]

  # points with boundary
  pointsWithBoundary <- pch %in% 21:24
  withBoundary <- FALSE
  fill <- color

  if (!any(is.na(pch)) && !any(pointsWithBoundary)) {

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

  } else if (!any(is.na(pch)) && all(pointsWithBoundary)) {

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

  } else if (!any(is.na(pch))) {

    # NOTICE: the legend may be destroyed
    size <- as_ggplot_size(size)

    ggObj <- ggObj +
      ggplot2::geom_point(
        data = data.frame(x = x,
                          y = y,
                          color = color,
                          size = size),
        mapping = ggplot2::aes(x = x, y = y,
                               color = color,
                               size = size),
        shape = pch
      )

    warning("This transformation is not suggested. To preserve the data structure, ",
            "points with filled shapes (e.g., `ccircle`, `ctriangle`) will be changed as points with open shapes (e.g., `ocircle`, `otriangle`)",
            call. = FALSE)

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

                 linewidth <- gh['linewidth'][aesthetic$index][!showArea]
                 if(len_unique(linewidth) == 1L) linewidth <- linewidth[1L]

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
                     linewidth = linewidth,
                     polygon_x = gh['x'][aesthetic$index][!showArea],
                     polygon_y = lapply(gh['y'][aesthetic$index], function(y) -y)[!showArea]
                   )
               }

               if(sum(showArea) > 0) {

                 linewidth <- gh['linewidth'][aesthetic$index][showArea]
                 if(len_unique(linewidth) == 1L) linewidth <- linewidth[1L]

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
                     linewidth = linewidth,
                     polygon_x = gh['x'][aesthetic$index][showArea],
                     polygon_y = lapply(gh['y'][aesthetic$index],
                                        function(y) -y)[showArea]
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

               linewidth <- gh['linewidth'][aesthetic$index]
               if(len_unique(linewidth) == 1L) linewidth <- linewidth[1L]

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
                   linewidth = linewidth,
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

                 if(len_unique(point_pch) == 1L) point_pch <- point_pch[1L]

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
                   color = NA,
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
  if(len_unique(pch) == 1L) pch <- pch[1L]

  if (!any(is.na(pch))) {

    # points with boundary
    pointsWithBoundary <- pch %in% 21:24

    fill <- rep(NA, length(color))
    fill[pointsWithBoundary] <- color[pointsWithBoundary]
    if(len_unique(fill) == 1L) fill <- fill[1L]

    color[pointsWithBoundary] <- loon::l_getOption("foreground")
    if(len_unique(color) == 1L) color <- color[1L]

    size <- as_ggplot_size(size)
    if(len_unique(size) == 1L) size <- size[1L]

    # No NAs and no points with borders
    ggObj <- ggObj +
      ggplot2::geom_point(
        data = data.frame(x = x, y = y),
        mapping = aes(x = x, y = y),
        fill = fill,
        size = size,
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

               fill <- ifelse(gh['showArea'][aesthetic$index],
                              aesthetic$color,
                              NA)
               if(len_unique(fill) == 1L) fill <- fill[1L]

               color <- ifelse(gh['showArea'][aesthetic$index],
                               loon::l_getOption("foreground"),
                               aesthetic$color)
               if(len_unique(color) == 1L) color <- color[1L]

               size <- as_ggplot_size(aesthetic$size, type = "polygon", adjust = 0.6)
               if(len_unique(size) == 1L) size <- size[1L]

               linewidth <- gh['linewidth'][aesthetic$index]
               if(len_unique(linewidth) == 1L) linewidth <- linewidth[1L]

               ggObj <- ggObj +
                 do.call(ggmulti::geom_polygon_glyph,
                         remove_null(
                           data = if(lenTypes == 1) {
                             NULL
                           } else {
                             data.frame(x = aesthetic$x,
                                        y = aesthetic$y)
                           },
                           fill = fill,
                           color = color,
                           size = size,
                           polygon_x = gh['x'][aesthetic$index],
                           polygon_y = lapply(gh['y'][aesthetic$index], function(y) -y),
                           linewidth = linewidth
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

               fill <- ifelse(gh['showArea'][aesthetic$index], aesthetic$color, NA)
               if(len_unique(fill) == 1L) fill <- fill[1L]

               color <- aesthetic$color
               if(len_unique(color) == 1L) color <- color[1L]

               size <- as_ggplot_size(aesthetic$size,
                                      type = axesLayout,
                                      p = lenSeq)
               if(len_unique(size) == 1L) size <- size[1L]

               linewidth <- gh['linewidth'][aesthetic$index]
               if(len_unique(linewidth) == 1L) linewidth <- linewidth[1L]

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
                     fill = fill,
                     color = color,
                     size = size,
                     serialaxes.data = serialaxes.data,
                     axes.sequence = sequence,
                     scaling = "none",
                     andrews = gh['andrews'],
                     axes.layout = axesLayout,
                     show.axes = gh['showAxes'],
                     show.enclosing = gh['showEnclosing'],
                     axescolour = as_hex6color(gh['axesColor']),
                     bboxcolour = as_hex6color(gh['bboxColor']),
                     linewidth = linewidth
                   )
                 )

             },
             "text" = {
               gh <- loon::l_create_handle(c(widget, aesthetic$glyph[1L]))
               label <- gh["text"][aesthetic$index]

               color <- aesthetic$color
               if(len_unique(color) == 1L) color <- color[1L]

               size <- as_ggplot_size(aesthetic$size,
                                      type = "texts")
               if(len_unique(size) == 1L) size <- size[1L]

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
                     color = color,
                     size = size
                   )
                 )

             },
             "primitive_glyph" = {

               pch <- glyph_to_pch(aesthetic$glyph)
               if(len_unique(pch) == 1L) pch <- pch[1L]

               pointsWithBoundary <- pch %in% 21:24

               fill <- rep(NA, length(aesthetic$color))
               color <- aesthetic$color

               fill[pointsWithBoundary] <- color[pointsWithBoundary]
               color[pointsWithBoundary] <- loon::l_getOption("foreground")

               if(len_unique(fill) == 1L) fill <- fill[1L]
               if(len_unique(color) == 1L) color <- color[1L]

               size <- as_ggplot_size(aesthetic$size)
               if(len_unique(size) == 1L) size <- size[1L]

               ggObj <- ggObj +
                 ggplot2::geom_point(
                   data = data.frame(x = aesthetic$x,
                                     y = aesthetic$y),
                   fill = fill,
                   pch = pch,
                   size = size,
                   colour = color
                 )
             },
             "pointrange" = {
               gh <- loon::l_create_handle(c(widget, aesthetic$glyph[1L]))

               # showArea
               pch <- ifelse(gh["showArea"], 1, 19)
               ymin <- gh["ymin"][aesthetic$index]
               ymax <- gh["ymax"][aesthetic$index]

               color <- aesthetic$color
               if(len_unique(color) == 1L) color <- color[1L]

               size <- as_ggplot_size(aesthetic$size)
               if(len_unique(size) == 1L) size <- size[1L]

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
                     color = color,
                     pch = pch,
                     size = size
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
               if(len_unique(height) == 1L) height <- height[1L]

               width <- height/ratio
               if(len_unique(width) == 1L) width <- width[1L]

               # THIS IS A HACK!
               imageSize <- 0.6

               fill <- aesthetic$color
               if(len_unique(fill) == 1L) fill <- fill[1L]

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
                     fill = fill,
                     color = NA,
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
