# hide axes, labels, legend
themeNULL <- function(panel.background = ggplot2::element_rect(fill = loon::l_getOption("canvas_bg_guides")),
                      axis.ticks = ggplot2::element_blank(),
                      axis.text.x = ggplot2::element_blank(),
                      axis.text.y = ggplot2::element_blank(),
                      axis.title = ggplot2::element_blank(),
                      axis.title.x = ggplot2::element_blank(),
                      axis.title.y = ggplot2::element_blank(),
                      plot.title = ggplot2::element_blank(),
                      plot.margin = grid::unit(rep(0.1, 4), "lines"),
                      legend.position = "none", ...) {

  themeArgs <- list(panel.background = panel.background,
                    axis.ticks = axis.ticks,
                    axis.text.x = axis.text.x,
                    axis.text.y = axis.text.y,
                    axis.title = axis.title,
                    axis.title.x = axis.title.x,
                    axis.title.y = axis.title.y,
                    plot.title = plot.title,
                    plot.margin = plot.margin,
                    legend.position = legend.position)

  do.call(ggplot2::theme, c(themeArgs, list(...)))
}
