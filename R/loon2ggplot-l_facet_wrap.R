#' @rdname loon2ggplot
#' @export
loon2ggplot.l_facet_wrap <- function(target, asAes = TRUE, selectedOnTop = TRUE,
                                     showNearestColor = FALSE, ...) {

  args <- loon::l_get_arrangeGrobArgs(target)
  # they are loonGrobs
  grobs <- args$grobs

  plots <- lapply(seq_along(target),
                  function(i) {

                    grob <- grobs[[i]]$grobs
                    grobNames <- names(grob)

                    plot <- lapply(seq_along(grob),
                                   function(j) {

                                     if(!grepl("facet.label", grobNames[[j]])) {
                                       p <- loon.ggplot(target[[i]], asAes,
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
  positions <- layout_matrix2tlbr(layout_matrix, n = length(grobs))

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