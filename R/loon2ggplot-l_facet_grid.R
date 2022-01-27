#' @rdname loon2ggplot
#' @export
loon2ggplot.l_facet_grid <- function(target, asAes = TRUE, selectedOnTop = TRUE,
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

      facetsColLabels <- subtitles$facetsColLabels
      facetsRowLabels <- subtitles$facetsRowLabels

      cols <- rownames(facetsColLabels)
      rows <- rownames(facetsRowLabels)

      if(length(rows) == 0 && length(cols) > 0) {
        formula <- as.formula(paste0("~", paste(cols, collapse = "+")))
      } else if (length(rows) > 0 && length(cols) == 0) {
        formula <- as.formula(paste0(paste(rows, collapse = "+"), "~."))
      } else if (length(rows) > 0 && length(cols) > 0) {
        formula <- as.formula(paste0(paste(rows, collapse = "+"), "~",
                                     paste(cols, collapse = "+")))
      } else stop("No `rows` and `cols`", call. = FALSE)

      labelsLocation <- subtitles$labelsLocation
      if (labelsLocation[1] == "top" && labelsLocation[2] == "right") {
        switch <- NULL
      } else if (labelsLocation[1] == "top" && labelsLocation[2] == "left") {
        switch <- "y"
      } else if (labelsLocation[1] == "bottom" && labelsLocation[2] == "right") {
        switch <- "x"
      } else {
        # "bottom", "left"
        switch <- "both"
      }

      drop <- subtitles$drop %||% FALSE
      scales <- subtitles$scales %||% "fixed"
      if(grepl("free", scales)) {
        lp$coordinates$limits <- list(x = NULL, y = NULL)
        lp$coordinates$expand <- TRUE
      }

      lp +
        ggplot2::facet_grid(formula,
                            switch = switch,
                            scales = scales,
                            drop = drop)
    },
    error = function(e) {

      warning(e$message,
              ". The plots will be constructed by `patchwork`.", call. = FALSE)

      patchwork_facet_grid(target, asAes = asAes, selectedOnTop = selectedOnTop,
                           showNearestColor = showNearestColor, ...)
    }
  )
}

patchwork_facet_grid <- function(target, asAes = TRUE, selectedOnTop = TRUE,
                                 showNearestColor = FALSE, ...) {

  args <- loon::l_get_arrangeGrobArgs(target)
  # they are loonGrobs
  grobs <- args$grobs
  grobsNames <- vapply(grobs, function(grob) grob$name, character(1L))

  ggplots <- loon2ggplot.l_compound(target, asAes, selectedOnTop,
                                    showNearestColor, ...)

  # drop guides
  ggplots <- ggplots &
    themeNULL()

  # labels
  # plot (< 1.3.8: arrange ; >= 1.3.8: plots)
  labelGrobs <- grobs[which(!grobsNames %in% c("plots", "arrange"))]
  labels <- c()
  labelsbg <- c()
  labelsFontsize <- c()
  angle <- c()
  for(labelGrob in labelGrobs) {

    childrenOrder <- labelGrob$childrenOrder
    textPath <- childrenOrder[grepl("text", childrenOrder)]
    rectPath <- childrenOrder[grepl("rect", childrenOrder)]

    textGrob <- grid::getGrob(labelGrob, textPath)
    labels <- c(labels, textGrob$label)
    labelsFontsize <- c(labelsFontsize, textGrob$gp$fontsize)
    angle <- c(angle, textGrob$rot)

    labelsbg <- c(labelsbg, grid::getGrob(labelGrob, rectPath)$gp$fill)
  }

  ggLabels <- lapply(seq_along(labelGrobs),
                     function(i) {
                       ggplot2::ggplot() +
                         ggplot2::geom_text(data = data.frame(x = 0, y = 0, label = labels[i]),
                                            mapping = aes(x = x, y = y, label = label),
                                            size = labelsFontsize[i]/ggplot2::.pt,
                                            angle = angle[i]) +
                         themeNULL(
                           panel.background = ggplot2::element_rect(fill = labelsbg[i]),
                           plot.margin = grid::unit(rep(0, 4), "lines"),
                           panel.grid.minor = ggplot2::element_blank(),
                           panel.grid.major = ggplot2::element_blank()
                         )
                     })
  layout_matrix <- args$layout_matrix
  positions <- layout_matrix2positions(layout_matrix, n = length(grobs))

  plots <- c(list(ggplots), ggLabels)
  plots$design <- do.call(c,
                          lapply(seq(nrow(positions)),
                                 function(i) {
                                   do.call(patchwork::area,
                                           positions[i, ])
                                 }))
  ggCompound(plots) &
    ggplot2::labs(x = args$bottom,
                  y = args$left)
}
