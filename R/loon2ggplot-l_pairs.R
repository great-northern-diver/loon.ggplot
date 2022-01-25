#' @rdname loon2ggplot
#' @export
loon2ggplot.l_pairs <- function(target, asAes = TRUE, selectedOnTop = TRUE,
                                showNearestColor = FALSE, ...) {

  # old version; rely on GGally
  # locations <- g_getLocations(target)
  # gm <- GGally::ggmatrix(
  #   plots = g_getPlots(target, asAes = asAes, selectedOnTop = selectedOnTop,
  #                      showNearestColor = showNearestColor),
  #   nrow = locations$nrow,
  #   ncol = locations$ncol,
  #   byrow = FALSE,
  #   showXAxisPlotLabels = FALSE,
  #   showYAxisPlotLabels = FALSE) +
  #   theme(plot.background = ggplot2::element_rect(
  #     fill = loon::l_getOption("canvas_bg_guides")
  #   ))
  # return(gm)

  # new version; rely on patchwork
  locations <- loon::l_getLocations(target)
  layout_matrix <- locations$layout_matrix
  plots <- lapply(target,
                  function(x) {
                    loon2ggplot(x, asAes = asAes, selectedOnTop = selectedOnTop,
                                showNearestColor = showNearestColor, ...) +
                      themeNULL() # a wrap of `theme()`
                  }
  )

  # texts on the diagonal
  plotsSummary <- pack_texts(plots, layout_matrix,
                             texts = pairs_text(target))

  plots <- plotsSummary$plots
  layout_matrix <- plotsSummary$layout_matrix
  # layout_matrix
  #     [, 1] [, 2]
  # [1, ]   1     NA
  # [2, ]   2     2
  # corresponding `tlbr` structure
  # l r t b
  # 1 1 1 1
  # 1 2 2 2
  positions <- layout_matrix2positions(layout_matrix, n = length(plots))

  plots$design <- do.call(c,
                          lapply(seq(nrow(positions)),
                                 function(i) {
                                   do.call(patchwork::area,
                                           positions[i, ])
                                 }))
  ggCompound(plots,
             fill.bg = loon::l_getOption("canvas_bg_guides"),
             colour.bg = loon::l_getOption("canvas_bg_guides"))
}

pack_texts <- function(plots, layout_matrix, texts, default_text_size = 3) {

  nrow <- nrow(layout_matrix)
  ncol <- ncol(layout_matrix)

  if(is.na(layout_matrix[1,ncol])) {
    showHistOnEdge <- TRUE
    showHistOnDiag <- FALSE
  } else {
    if(is.na(layout_matrix[1,1])) {
      showHistOnEdge <- FALSE
      showHistOnDiag <- FALSE
    } else {
      showHistOnEdge <- FALSE
      showHistOnDiag <- TRUE
    }
  }

  if(!showHistOnEdge && showHistOnDiag)
    return(
      list(plots = plots,
           layout_matrix = layout_matrix)
    )

  if(showHistOnEdge && !showHistOnDiag) {
    textsPos <- 2 + (nrow + 1) * (1:(ncol-1) - 1)
  } else if (!showHistOnEdge && !showHistOnDiag) {
    textsPos <- 1 + (nrow + 1) * (1:ncol - 1)
  } else {
    # showHistOnEdge = TRUE
    # showHistOnDiag = TRUE
    stop("`showHistOnEdge` and `showHistOnDiag` cannot be true simultaneously.", call. = FALSE)
  }

  textsId <- seq_along(textsPos) + max(layout_matrix, na.rm = TRUE)

  for(i in seq_along(textsPos)) {
    pos <- textsPos[i]
    coli <- ceiling(pos/nrow)
    rowi <- pos %% nrow
    if(rowi == 0) rowi <- nrow

    layout_matrix[rowi, coli] <- textsId[i]

    plots[[texts[i]]] <-  ggplot2::ggplot() +
      ggplot2::geom_text(
        data = data.frame(x = 0, y = 0, label = texts[i]),
        mapping = ggplot2::aes(
          x = x,
          y = y,
          label = label
        ),
        size = default_text_size) +
      themeNULL(panel.grid.minor = ggplot2::element_blank(),
                panel.grid.major = ggplot2::element_blank())
  }

  return(
    list(
      plots = plots,
      layout_matrix = layout_matrix
    )
  )
}

