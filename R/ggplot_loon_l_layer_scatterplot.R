ggplot.loon.l_layer_scatterplot <- function(target) {

  widget <- attr(target, "widget")
  states <- loon:::get_layer_states(target)

  if (!any(states$active)) {
    # No active points in scatterplot
    ggplot2::geom_blank()
  } else {

    display_order <- loon:::get_model_display_order(widget)

    active <- states$active
    selected <- states$selected[display_order][active]

    s_a <- list(x = states$x[display_order][active],
                y = states$y[display_order][active],
                glyph = states$glyph[display_order][active],
                color = loon:::get_display_color(states$color[display_order][active], selected),
                size = states$size[display_order][active],
                index = seq_along(states$x)[display_order][active]
    )

    pch <- loon:::glyph_to_pch(s_a$glyph)
    if (!any(is.na(pch)) && !any(pch %in% 21:24)) {

      # No NAs and no points with borders
      ggplot2::geom_point(
        data = data.frame(x = s_a$x,
                          y = s_a$y),
        mapping = aes(x = x, y = y),
        pch = pch,
        color = s_a$color,
        size = as_r_point_size(s_a$size)
      )
    } else if (!any(is.na(pch)) && all(pch %in% 21:24)) {

      # No NAs and ALL points with borders
      ggplot2::geom_point(
        data = data.frame(x = s_a$x,
                          y = s_a$y),
        mapping = aes(x = x, y = y),
        pch = pch,
        fill = s_a$color,
        color = loon::l_getOption("foreground"),
        size = as_r_point_size(s_a$size)
      )
    } else {
      # possibly some NAs (means some points are text, polygons, images, etc.)
      # and/or a mix of regular and closed points.
      scaleInfo <- loon:::get_glyph_scale_info(widget)
      names_scaleInfo <- names(scaleInfo)

      geoms <- ggplot2::geom_blank()

      hide <- lapply(seq_len(length(s_a$x)),
                     function(i) {

                       case_i <- list(x = s_a$x[i],
                                      y = s_a$y[i],
                                      glyph = s_a$glyph[i],
                                      color = s_a$color[i],
                                      size = s_a$size[i],
                                      index = s_a$index[i]
                       )

                       type <- loon::l_glyph_getType(widget, case_i$glyph)

                       scaleInfoLocation <- (grepl(case_i$glyph, names_scaleInfo) == TRUE)

                       if (sum(scaleInfoLocation) > 1 &
                           case_i$glyph %in% c("circle", "square", "triangle")
                       ) { # Then there has been some redundant selection
                         scaleInfoLocation <-  scaleInfoLocation &
                           (grepl("ocircle", names_scaleInfo) != TRUE) &
                           (grepl("ccircle", names_scaleInfo) != TRUE) &
                           (grepl("osquare", names_scaleInfo) != TRUE) &
                           (grepl("csquare", names_scaleInfo) != TRUE) &
                           (grepl("otriangle", names_scaleInfo) != TRUE) &
                           (grepl("ctriangle", names_scaleInfo) != TRUE)
                       }

                       case_i$scaleInfo <- scaleInfo[[which(scaleInfoLocation == TRUE)]]

                       geoms <<- geoms + geom_glyph_add(widget, structure(list(), class=type), case_i)
                     })

    }
  }
}
