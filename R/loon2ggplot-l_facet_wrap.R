#' @rdname loon2ggplot
#' @export
loon2ggplot.l_facet_wrap <- function(target, asAes = TRUE, selectedOnTop = TRUE,
                                     showNearestColor = FALSE, ...) {

  tryCatch(
    expr = {

      subtitles <- list(...)$subtitles %||% l_getSubtitles(target)
      facetsLabels <- subtitles$facetsLabels
      levels <- subtitles$levels
      # widgets in a loon facet object can have different layers
      # (after creating an l_facet object,
      # people can still modify each of them individually),
      # however, ggplot2 cannot. Therefore, layers in the first plot in
      # the facet will be referred.
      n <- length(target)
      lp <- loon2ggplot(target[[1L]],
                        asAes = asAes, selectedOnTop = selectedOnTop,
                        showNearestColor = showNearestColor,
                        facets = target,
                        facetsLabels = facetsLabels,
                        levels = levels, ...)

      if(!is.null(subtitles$xlabel) || subtitles$xlabel != "") {
        lp$labels$x <- subtitles$xlabel
        lp$theme$axis.title.x <- ggplot2::element_text()
      }
      if(!is.null(subtitles$ylabel) || subtitles$ylabel != "") {
        lp$labels$y <- subtitles$ylabel
        lp$theme$axis.title.y <- ggplot2::element_text()
      }
      if(!is.null(subtitles$ylabel) || subtitles$ylabel != "") {
        lp$labels$title <- subtitles$title
        lp$theme$axis.title <- ggplot2::element_text()
      }

      locations <- loon::l_getLocations(target)

      drop <- subtitles$drop %||% FALSE
      scales <- subtitles$scales %||% "fixed"
      if(grepl("free", scales)) {
        lp$coordinates$limits <- list(x = NULL, y = NULL)
        lp$coordinates$expand <- TRUE
      }

      lp +
        ggplot2::facet_wrap(facets = rownames(facetsLabels),
                            nrow = locations$nrow,
                            ncol = locations$ncol,
                            strip.position = subtitles$labelsLocation,
                            scales = scales,
                            drop = drop)
    },
    error = function(e) {

      warning(e$message,
              ". The plots will be constructed by `patchwork`.", call. = FALSE)

      # pack plots via `patchwork`
      patchwork_facet_wrap(target, asAes = asAes, selectedOnTop = selectedOnTop,
                           showNearestColor = showNearestColor, ...)
    }
  )
}

patchwork_facet_wrap <- function(target, asAes = TRUE, selectedOnTop = TRUE,
                                 showNearestColor = FALSE, ...) {

  args <- loon::l_get_arrangeGrobArgs(target)
  # return loonGrobs
  grobs <- args$grobs

  plots <- lapply(seq_along(target),
                  function(i) {

                    grob <- grobs[[i]]$grobs
                    grobNames <- names(grob)

                    plot <- lapply(seq_along(grob),
                                   function(j) {

                                     if(!grepl("facet.label", grobNames[[j]])) {
                                       p <- loon2ggplot(target[[i]], asAes,
                                                        selectedOnTop,
                                                        showNearestColor) +
                                         themeNULL()
                                       return(p)
                                     }

                                     labelGrob <- grob[[j]]

                                     childrenOrder <- labelGrob$childrenOrder
                                     textPath <- childrenOrder[grepl("text", childrenOrder)]
                                     rectPath <- childrenOrder[grepl("rect", childrenOrder)]

                                     label <- grid::getGrob(labelGrob, textPath)$label
                                     labelFontsize <- grid::getGrob(labelGrob, textPath)$gp$fontsize
                                     labelbg <- grid::getGrob(labelGrob, rectPath)$gp$fill

                                     ggplot2::ggplot() +
                                       ggplot2::geom_text(data = data.frame(x = 0, y = 0, label = label),
                                                          mapping = aes(x = x, y = y, label = label),
                                                          size = labelFontsize/ggplot2::.pt) +
                                       themeNULL(
                                         panel.background = ggplot2::element_rect(fill = labelbg),
                                         plot.margin = grid::unit(rep(0, 4), "lines"),
                                         panel.grid.minor = ggplot2::element_blank(),
                                         panel.grid.major = ggplot2::element_blank()
                                       )
                                   })

                    positions <- grobs[[i]]$layout
                    plot$design <- do.call(c,
                                           lapply(seq(nrow(positions)),
                                                  function(j) {
                                                    pos <- as.list(positions[j, ])
                                                    do.call(patchwork::area,
                                                            pos[c("t", "l", "b", "r")])
                                                  }))

                    ggCompound(plot,
                               fill.bg = loon::l_getOption("canvas_bg_guides"),
                               colour.bg = loon::l_getOption("canvas_bg_guides"))
                  })

  layout_matrix <- args$layout_matrix
  positions <- layout_matrix2positions(layout_matrix, n = length(grobs))

  plots$design <- do.call(c,
                          lapply(seq(nrow(positions)),
                                 function(i) {
                                   do.call(patchwork::area,
                                           positions[i, ])
                                 }))
  ggCompound(plots, setBackground = FALSE) &
    ggplot2::labs(x = args$bottom,
                  y = args$left)
}
