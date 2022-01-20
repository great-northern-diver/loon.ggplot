scatterplotAsAesTRUE <- function(ggObj, widget, states = data.frame(),
                                 selectedOnTop = TRUE, facets = NULL) {

  size <- states$size
  color <- states$color
  # glyph may have NAs
  glyph <- states$glyph
  pch <- glyph_to_pch(glyph)
  if(len_unique(pch) == 1L) pch <- pch[1L]

  if(!any(is.na(pch))) {

    size <- as_ggplot_size(size)
    states$size <- size
    ggObj <- scatter_add_points_asAesTrue(widget, ggObj, states, pch)

  } else {

    # possibly some NAs (means some points are text, polygons, images, etc.)
    # and/or a mix of regular and closed points.
    if(is.null(facets)) {

      type <- sapply(glyph,
                     function(g) loon::l_glyph_getType(widget, g))
      types <- paste(type, names(type), sep = ".")
      uniqueTypes <- unique(types)

      lenUniqueTypes <- length(uniqueTypes)
      if(lenUniqueTypes > 1 && !selectedOnTop) {
        warning("More than one non-primitive glyphs are detected. ",
                "The selected points will be always on top. ",
                "The displayed order may be different from the original data set order.",
                call. = FALSE)}

    } else {
      type <- unlist(
        lapply(seq_along(facets),
               function(i) {
                 facet <- facets[[i]]
                 glyph <- states$glyph[states$facetGroup == i]
                 sapply(glyph, function(g) loon::l_glyph_getType(facet, g))
               })
      )

      types <- paste(type, names(type), sep = ".")
      uniqueTypes <- unique(types)

      lenUniqueTypes <- length(uniqueTypes)

      if(lenUniqueTypes > 1) {
        stop("More than one non-primitive glyphs are detected. ",
             "When transforming loon to ggplot, the data structure is destroyed ",
             "(e.g., one layer in loon is mapped to more than one layers in ggplot2). ",
             call. = FALSE)
      }
    }

    for(utypes in uniqueTypes) {

      typeId <- which(types == utypes)
      aesthetic <- as.data.frame(lapply(states, function(state) state[typeId]))
      utype <- strsplit(utypes, "[.]")[[1L]][1L]

      switch(utype,
             "polygon" = {

               pointSize <- as_ggplot_size(aesthetic$size, type = "polygon", adjust = 0.6)
               aesthetic$size <- pointSize
               size[typeId] <- pointSize

               ggObj <- scatter_add_polygon_asAesTrue(widget = widget,
                                                      ggObj = ggObj,
                                                      aesthetic = aesthetic,
                                                      facets = facets)
             },
             "serialaxes" = {

               gh <- loon::l_create_handle(c(widget, aesthetic$glyph[1L]))
               sequence <- gh['sequence']
               lenSeq <- length(sequence)
               if(lenSeq == 0) lenSeq <- ncol(gh['data'])

               pointSize <- as_ggplot_size(aesthetic$size,
                                           type = gh['axesLayout'],
                                           p = lenSeq)
               aesthetic$size <- pointSize
               size[typeId] <- pointSize

               ggObj <- scatter_add_serialaxes_asAesTrue(widget = widget,
                                                         ggObj = ggObj,
                                                         aesthetic = aesthetic,
                                                         facets = facets)
             },
             "text" = {
               # update size by text adjustment
               textSize <- as_ggplot_size(aesthetic$size,
                                          type = "texts")
               aesthetic$size <- textSize
               size[typeId] <- textSize

               ggObj <- scatter_add_text_asAesTrue(widget = widget,
                                                   ggObj = ggObj,
                                                   aesthetic = aesthetic,
                                                   facets = facets)
             },
             "primitive_glyph" = {

               # NOTICE: the legend may be destroyed
               pointSize <- as_ggplot_size(aesthetic$size)
               size[typeId] <- pointSize
               aesthetic$size <- pointSize

               pointPch <- glyph_to_pch(aesthetic$glyph)
               if(len_unique(pointPch) == 1L) pointPch <- pointPch[1L]

               ggObj <- scatter_add_points_asAesTrue(widget, ggObj, aesthetic, pointPch)
             },
             "pointrange" = {

               # ggplot default value
               fatten <- 4
               pointSize <- as_ggplot_size(aesthetic$size)/fatten
               size[typeId] <- pointSize
               aesthetic$size <- pointSize

               ggObj <- scatter_add_pointrange_asAesTrue(widget, ggObj,
                                                         aesthetic, facets)
             },
             "image" = {

               # THIS IS A HACK!
               imageSize <- 0.6
               size[typeId] <- imageSize
               ggObj <- scatter_add_image_asAesTrue(widget, ggObj, aesthetic,
                                                    facets, imageSize = imageSize)
             }
      )
    }
  }

  if(all(pch %in% 21:24)) {
    ggObj <- ggObj +
      ggplot2::guides(
        fill = ggplot2::guide_legend(
          override.aes = list(fill = unique(color),
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

scatterplotAsAesFALSE <- function(ggObj, widget, states = data.frame(),
                                  selectedOnTop = TRUE, facets = NULL) {

  glyph <- states$glyph
  pch <- glyph_to_pch(glyph)
  if (!any(is.na(pch))) {

    ggObj <- scatter_add_points_asAesFalse(widget, ggObj, states, pch)

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

      typeId <- which(types == utypes)
      aesthetic <- as.data.frame(lapply(states, function(state) state[typeId]))
      utype <- strsplit(utypes, "[.]")[[1L]][1L]

      switch(utype,
             "polygon" = {
               ggObj <- scatter_add_polygon_asAesFalse(widget, ggObj, aesthetic, facets)
             },
             "serialaxes" = {
               ggObj <- scatter_add_serialaxes_asAesFalse(widget, ggObj, aesthetic, facets)
             },
             "text" = {
               ggObj <- scatter_add_text_asAesFalse(widget, ggObj, aesthetic, facets)
             },
             "primitive_glyph" = {
               pointsPch <- glyph_to_pch(aesthetic$glyph)
               ggObj <- scatter_add_points_asAesFalse(widget, ggObj, aesthetic, pointsPch)
             },
             "pointrange" = {
               ggObj <- scatter_add_pointrange_asAesFalse(widget, ggObj, aesthetic, facets)
             },
             "image" = {
               ggObj <- scatter_add_image_asAesFalse(widget, ggObj, aesthetic, facets)
             }
      )
    }
  }
  return(ggObj)
}
