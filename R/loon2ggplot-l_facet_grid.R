#' @rdname loon2ggplot
#' @export
loon2ggplot.l_facet_grid <- function(target, asAes = TRUE, selectedOnTop = TRUE,
                                     showNearestColor = FALSE, ...) {

  args <- loon::l_get_arrangeGrobArgs(target)
  # they are loonGrobs
  grobs <- args$grobs
  grobsNames <- vapply(grobs, function(grob) grob$name, character(1L))

  ggplots <- loon2ggplot.l_compound(target, asAes, selectedOnTop,
                                    showNearestColor)

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
  positions <- layout_matrix2tlbr(layout_matrix, n = length(grobs))

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
