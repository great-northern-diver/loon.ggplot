scatter_add_points_asAesTrue <- function(widget, ggObj, states, pch) {

  pointsWithBoundary <- pch %in% 21:24

  if (!any(is.na(pch)) && !any(pointsWithBoundary)) {

    ggObj <- ggObj +
      ggplot2::geom_point(
        data = states,
        mapping = ggplot2::aes(x = x, y = y,
                               color = color,
                               size = size),
        shape = pch
      )

  } else if (!any(is.na(pch)) && all(pointsWithBoundary)) {
    states$fill <- states$color
    ggObj <- ggObj +
      ggplot2::geom_point(
        data = states,
        mapping = ggplot2::aes(x = x, y = y,
                               fill = fill,
                               size = size),
        shape = pch
      )

  } else if (!any(is.na(pch))) {

    # NOTICE: the legend may be destroyed
    ggObj <- ggObj +
      ggplot2::geom_point(
        data = states,
        mapping = ggplot2::aes(x = x, y = y,
                               color = color,
                               size = size),
        shape = pch
      )

    warning("This transformation is not suggested. To preserve the data structure, ",
            "points with filled shapes (e.g., `ccircle`, `ctriangle`) ",
            "will be changed as points with open shapes (e.g., `ocircle`, `otriangle`)",
            call. = FALSE)

  }
  ggObj
}

scatter_add_points_asAesFalse <- function(widget, ggObj, states, pch) {


  color <- states$color
  size <- states$size
  # points with boundary
  pointsWithBoundary <- pch %in% 21:24

  fill <- rep(NA, length(color))
  fill[pointsWithBoundary] <- color[pointsWithBoundary]
  color[pointsWithBoundary] <- loon::l_getOption("foreground")
  size <- as_ggplot_size(size)

  if(len_unique(color) == 1L) color <- color[1L]
  if(len_unique(fill) == 1L) fill <- fill[1L]
  if(len_unique(size) == 1L) size <- size[1L]
  if(len_unique(pch) == 1L) pch <- pch[1L]

  # No NAs and no points with borders
  ggObj +
    ggplot2::geom_point(
      data = states,
      mapping = aes(x = x, y = y),
      fill = fill,
      size = size,
      colour = color,
      shape = pch
    )
}

scatter_add_polygon_asAesTrue <- function(widget, ggObj, aesthetic, facets) {

  if (is.null(facets)) {
    gh <- loon::l_create_handle(c(widget, aesthetic$glyph[1L]))

    polygon_x <- gh['x'][aesthetic$index]
    polygon_y <- lapply(gh['y'][aesthetic$index], function(y) -y)
    linewidth <- gh['linewidth'][aesthetic$index]
    # `showArea` is a length `n` logical value
    showArea <- gh['showArea'][aesthetic$index]

  } else {

    polygon_x <- list()
    polygon_y <- list()
    linewidth <- c()
    showArea <- c()
    for (i in seq_along(facets)) {
      facet <- facets[[i]]
      gh <- loon::l_create_handle(c(facet, aesthetic$glyph[1L]))
      id <- aesthetic$index[aesthetic$facetGroup == i]
      polygon_x <- c(polygon_x, gh['x'][id])
      polygon_y <- c(polygon_y, lapply(gh['y'][id], function(y) -y))
      linewidth <- c(linewidth, gh['linewidth'][id])
      showArea <- c(showArea, gh['showArea'][id])
    }
  }

  if(len_unique(showArea) > 1) {
    warning("To preserve the order of elements, `showArea` must be either TRUE or FALSE. " ,
            "The default `showArea` is TRUE",
            call. = FALSE)

    showArea <- TRUE
  } else {
    showArea <- showArea[1L]
  }

  if(showArea) {
    aesthetic$fill <- aesthetic$color
    mapping <- ggplot2::aes(x = x,
                            y = y,
                            fill = fill,
                            size = size)

  } else {
    mapping <- ggplot2::aes(x = x,
                            y = y,
                            color = color,
                            size = size)
  }

  if(len_unique(linewidth) == 1L) linewidth <- linewidth[1L]

  ggObj +
    ggmulti::geom_polygon_glyph(
      data = aesthetic,
      mapping = mapping,
      linewidth = linewidth,
      polygon_x = polygon_x,
      polygon_y = polygon_y
    )
}

scatter_add_polygon_asAesFalse <- function(widget, ggObj, aesthetic, facets) {

  if (is.null(facets)) {

    gh <- loon::l_create_handle(c(widget, aesthetic$glyph[1L]))

    fill <- ifelse(gh['showArea'][aesthetic$index],
                   aesthetic$color, NA)
    color <- ifelse(gh['showArea'][aesthetic$index],
                    loon::l_getOption("foreground"),
                    aesthetic$color)
    size <- as_ggplot_size(aesthetic$size, type = "polygon", adjust = 0.6)
    linewidth <- gh['linewidth'][aesthetic$index]
    polygon_x <- gh['x'][aesthetic$index]
    polygon_y <- lapply(gh['y'][aesthetic$index], function(y) -y)

  } else {

    polygon_x <- list()
    polygon_y <- list()
    linewidth <- c()
    showArea <- c()
    for (i in seq_along(facets)) {
      facet <- facets[[i]]
      gh <- loon::l_create_handle(c(facet, aesthetic$glyph[1L]))
      id <- aesthetic$index[aesthetic$facetGroup == i]
      polygon_x <- c(polygon_x, gh['x'][id])
      polygon_y <- c(polygon_y, lapply(gh['y'][id], function(y) -y))
      linewidth <- c(linewidth, gh['linewidth'][id])
      showArea <- c(showArea, gh['showArea'][id])
    }
    size <- as_ggplot_size(aesthetic$size, type = "polygon", adjust = 0.6)
    fill <- ifelse(showArea, aesthetic$color, NA)
    color <- ifelse(showArea, loon::l_getOption("foreground"), aesthetic$color)
  }

  if(len_unique(fill) == 1L) fill <- fill[1L]
  if(len_unique(color) == 1L) color <- color[1L]
  if(len_unique(size) == 1L) size <- size[1L]
  if(len_unique(linewidth) == 1L) linewidth <- linewidth[1L]

  ggObj +
    ggmulti::geom_polygon_glyph(
      data = aesthetic,
      mapping = ggplot2::aes(x = x,
                             y = y),
      fill = fill,
      color = color,
      size = size,
      polygon_x = polygon_x,
      polygon_y = polygon_y,
      linewidth = linewidth
    )
}

scatter_add_serialaxes_asAesTrue <- function(widget, ggObj, aesthetic, facets) {

  gh <- loon::l_create_handle(c(widget, aesthetic$glyph[1L]))

  if (is.null(facets)) {
    # loon data will be converted into character by default
    # make the scaling operation is applied on the whole data set
    # rather the subset of it
    serialaxes.data <- get_scaledData(char2num.data.frame(gh['data']),
                                      scaling = gh['scaling'],
                                      as.data.frame = TRUE)[aesthetic$index, ]


    linewidth <- gh['linewidth'][aesthetic$index]
  } else {

    linewidth <- c()
    serialaxes.data <- list()
    for (i in seq_along(facets)) {
      facet <- facets[[i]]
      gh <- loon::l_create_handle(c(facet, aesthetic$glyph[1L]))
      id <- aesthetic$index[aesthetic$facetGroup == i]
      linewidth <- c(linewidth, gh['linewidth'][id])
      serialaxes.data <- c(
        serialaxes.data,
        list(
          get_scaledData(char2num.data.frame(gh['data']),
                         scaling = gh['scaling'],
                         as.data.frame = TRUE)[id, ]
        )
      )
    }
    serialaxes.data <- do.call(rbind, serialaxes.data)
  }

  if(len_unique(linewidth) == 1L) linewidth <- linewidth[1L]

  # `showArea` is a length `1` logical value
  showArea <- gh['showArea']
  if(showArea) {
    aesthetic$fill <- aesthetic$color
    mapping <- ggplot2::aes(x = x, y = y,
                            fill = fill,
                            size = size)

  } else {
    mapping <- ggplot2::aes(x = x, y = y,
                            color = color,
                            size = size)
  }

  andrews <- gh['andrews']
  showAxes <- gh['showAxes']
  showEnclosing <- gh['showEnclosing']
  axescolour <- as_hex6color(gh['axesColor'])
  bboxcolour <- as_hex6color(gh['bboxColor'])
  sequence <- gh['sequence']
  axesLayout <- gh['axesLayout']

  args <- remove_null(
    list(
      data = aesthetic,
      mapping = mapping,
      serialaxes.data = serialaxes.data,
      axes.sequence = sequence,
      scaling = "none",
      andrews = andrews,
      axes.layout = axesLayout,
      show.axes = showAxes,
      linewidth = linewidth,
      show.enclosing = showEnclosing,
      axescolour = axescolour,
      bboxcolour = bboxcolour,
      colour = if(showArea) NA else NULL
    ),
    as_list = FALSE)

  ggObj +
    do.call(ggmulti::geom_serialaxes_glyph, args)
}

scatter_add_serialaxes_asAesFalse <- function(widget, ggObj, aesthetic, facets) {

  gh <- loon::l_create_handle(c(widget, aesthetic$glyph[1L]))
  # loon data will be converted into character by default
  serialaxes.data <- get_scaledData(char2num.data.frame(gh['data']),
                                    scaling = gh['scaling'],
                                    as.data.frame = TRUE)[aesthetic$index, ]
  sequence <- gh['sequence']
  axesLayout <- gh['axesLayout']
  lenSeq <- length(sequence)
  if(lenSeq == 0) lenSeq <- ncol(serialaxes.data)
  size <- as_ggplot_size(aesthetic$size,
                         type = axesLayout,
                         p = lenSeq)

  if (is.null(facets)) {
    linewidth <- gh['linewidth'][aesthetic$index]
  } else {
    linewidth <- c()
    serialaxes.data <- list()

    for (i in seq_along(facets)) {
      facet <- facets[[i]]
      gh <- loon::l_create_handle(c(facet, aesthetic$glyph[1L]))
      id <- aesthetic$index[aesthetic$facetGroup == i]
      linewidth <- c(linewidth, gh['linewidth'][id])

      sd <- get_scaledData(char2num.data.frame(gh['data']),
                           scaling = gh['scaling'],
                           as.data.frame = TRUE)[id, ]
      serialaxes.data <- c(serialaxes.data, list(sd))
    }
    serialaxes.data <- do.call(rbind, serialaxes.data)
  }
  if(gh['showArea']) {
    color <- NA
    fill <- aesthetic$color
  } else {
    fill <- NA
    color <- aesthetic$color
  }
  if(len_unique(fill) == 1L) fill <- fill[1L]
  if(len_unique(color) == 1L) color <- color[1L]
  if(len_unique(linewidth) == 1L) linewidth <- linewidth[1L]
  if(len_unique(size) == 1L) size <- size[1L]

  showArea <- gh['showArea']
  andrews <- gh['andrews']
  showAxes <- gh['showAxes']
  showEnclosing <- gh['showEnclosing']
  axescolour <- as_hex6color(gh['axesColor'])
  bboxcolour <- as_hex6color(gh['bboxColor'])
  sequence <- gh['sequence']
  axesLayout <- gh['axesLayout']

  ggObj +
    ggmulti::geom_serialaxes_glyph(
      data = aesthetic,
      mapping = ggplot2::aes(x = x, y = y),
      fill = fill,
      color = color,
      size = size,
      serialaxes.data = serialaxes.data,
      axes.sequence = sequence,
      scaling = "none",
      andrews = andrews,
      axes.layout = axesLayout,
      show.axes = showAxes,
      linewidth = linewidth,
      show.enclosing = showEnclosing,
      axescolour = axescolour,
      bboxcolour = bboxcolour
    )
}


scatter_add_text_asAesTrue <- function(widget, ggObj, aesthetic, facets) {

  if(is.null(facets)) {
    gh <- loon::l_create_handle(c(widget, aesthetic$glyph[1L]))
    label <- gh["text"][aesthetic$index]
  } else {
    label <- unlist(
      lapply(seq_along(facets),
             function(i) {
               facet <- facets[[i]]
               id <- aesthetic$index[aesthetic$facetGroup == i]
               gh <- loon::l_create_handle(c(facet, aesthetic$glyph[1L]))
               gh["text"][id]
             })
    )
  }

  aesthetic$label <- label

  ggObj +
    ggplot2::geom_text(
      data = aesthetic,
      mapping = ggplot2::aes(x = x, y = y,
                             label = label,
                             color = color,
                             size = size)
    )
}

scatter_add_text_asAesFalse <- function(widget, ggObj, aesthetic, facets) {

  if(is.null(facets)) {
    gh <- loon::l_create_handle(c(widget, aesthetic$glyph[1L]))
    label <- gh["text"][aesthetic$index]

  } else {

    label <- unlist(
      lapply(seq_along(facets),
             function(i) {
               facet <- facets[[i]]
               id <- aesthetic$index[aesthetic$facetGroup == i]
               gh <- loon::l_create_handle(c(facet, aesthetic$glyph[1L]))
               gh["text"][id]
             })
    )
  }

  aesthetic$label <- label

  color <- aesthetic$color
  size <- as_ggplot_size(aesthetic$size,
                         type = "texts")
  if(len_unique(color) == 1L) color <- color[1L]
  if(len_unique(size) == 1L) size <- size[1L]

  ggObj +
    ggplot2::geom_text(
      data = aesthetic,
      mapping = ggplot2::aes(x = x, y = y, label = label),
      color = color,
      size = size
    )
}

scatter_add_pointrange_asAesTrue <- function(widget, ggObj, aesthetic, facets) {

  if(is.null(facets)) {

    gh <- loon::l_create_handle(c(widget, aesthetic$glyph[1L]))
    # showArea
    pch <- ifelse(gh["showArea"], 1, 19)
    aesthetic$ymin <- gh["ymin"][aesthetic$index]
    aesthetic$ymax <- gh["ymax"][aesthetic$index]

  } else {

    ymin <- c()
    ymax <- c()
    pch <- c()
    for(i in seq_along(facets)) {

      facet <- facets[[i]]
      id <- aesthetic$index[aesthetic$facetGroup == i]
      gh <- loon::l_create_handle(c(facet, aesthetic$glyph[1L]))
      ymin <- c(ymin, gh["ymin"][id])
      ymax <- c(ymax, gh["ymax"][id])
      pch <- c(pch, ifelse(gh["showArea"], 1, 19))
    }
    if(len_unique(pch) > 1) {
      warning("To preserve the order of elements, `showArea` must be either TRUE or FALSE.",
              call. = FALSE)
    }
    pch <- pch[1L]
    aesthetic$ymin <- ymin
    aesthetic$ymax <- ymax
  }

  ggObj +
    ggplot2::geom_pointrange(
      data = aesthetic,
      mapping = ggplot2::aes(x = x, y = y,
                             color = color,
                             size = size,
                             ymin = ymin,
                             ymax = ymax),
      pch = pch
    )
}

scatter_add_pointrange_asAesFalse <- function(widget, ggObj, aesthetic, facets) {

  if(is.null(facets)) {

    gh <- loon::l_create_handle(c(widget, aesthetic$glyph[1L]))

    # showArea
    pch <- ifelse(gh["showArea"], 1, 19)
    ymin <- gh["ymin"][aesthetic$index]
    ymax <- gh["ymax"][aesthetic$index]

  } else {

    ymin <- c()
    ymax <- c()
    pch <- c()
    for(i in seq_along(facets)) {

      facet <- facets[[i]]
      id <- aesthetic$index[aesthetic$facetGroup == i]
      gh <- loon::l_create_handle(c(facet, aesthetic$glyph[1L]))
      ymin <- c(ymin, gh["ymin"][id])
      ymax <- c(ymax, gh["ymax"][id])
      pch <- c(pch, ifelse(gh["showArea"], 1, 19))
    }
    if(len_unique(pch) > 1) {
      warning("To preserve the order of elements, `showArea` must be either TRUE or FALSE.",
              call. = FALSE)
    }
    pch <- pch[1L]
  }

  aesthetic$ymin <- ymin
  aesthetic$ymax <- ymax
  color <- aesthetic$color
  size <- as_ggplot_size(aesthetic$size)
  if(len_unique(color) == 1L) color <- color[1L]
  if(len_unique(size) == 1L) size <- size[1L]

  ggObj +
    ggplot2::geom_pointrange(
      data = aesthetic,
      mapping = ggplot2::aes(x = x, y = y,
                             ymin = ymin, ymax = ymax),
      color = color,
      pch = pch,
      size = size
    )
}


scatter_add_image_asAesTrue <- function(widget, ggObj, aesthetic, facets, imageSize = 0.6) {

  if(is.null(facets)) {

    gh <- loon::l_create_handle(c(widget, aesthetic$glyph[1L]))
    tcl_img <- gh['images'][aesthetic$index]
    ratio <- c()
    images <- lapply(seq_along(tcl_img),
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

  } else {

    images <- list()
    width <- c()
    height <- c()
    for(i in seq_along(facets)) {

      facet <- facets[[i]]
      id <- aesthetic$index[aesthetic$facetGroup == i]
      gh <- loon::l_create_handle(c(facet, aesthetic$glyph[1L]))
      tcl_img <- gh['images'][id]
      ratio <- c()
      imgs <- lapply(seq_along(tcl_img),
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
      images <- c(images, imgs)
      height <- c(height,
                  as_ggplot_size(aesthetic$size,
                                 type = "image",
                                 ratio = ratio))
      width <- c(width, height/ratio)
    }
  }

  aesthetic$fill <- aesthetic$color
  ggObj +
    ggmulti::geom_image_glyph(
      data = aesthetic,
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

scatter_add_image_asAesFalse <- function(widget, ggObj, aesthetic, facets, imageSize = 0.6) {

  if(is.null(facets)) {

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
  } else {

    images <- list()
    width <- c()
    height <- c()
    for(i in seq_along(facets)) {

      facet <- facets[[i]]
      id <- aesthetic$index[aesthetic$facetGroup == i]
      gh <- loon::l_create_handle(c(facet, aesthetic$glyph[1L]))
      tcl_img <- gh['images'][id]
      ratio <- c()
      imgs <- lapply(seq_along(tcl_img),
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
      images <- c(images, imgs)
      height <- c(height,
                  as_ggplot_size(aesthetic$size,
                                 type = "image",
                                 ratio = ratio))
      width <- c(width, height/ratio)
    }
  }

  # THIS IS A HACK!
  imageSize <- 0.6
  fill <- aesthetic$color
  if(len_unique(fill) == 1L) fill <- fill[1L]
  if(len_unique(width) == 1L) width <- width[1L]
  if(len_unique(height) == 1L) height <- height[1L]

  ggObj +
    ggmulti::geom_image_glyph(
      data = aesthetic,
      mapping = ggplot2::aes(x = x,
                             y = y),
      fill = fill,
      color = NA,
      size = imageSize,
      images = images,
      imagewidth = width,
      imageheight = height
    )
}
