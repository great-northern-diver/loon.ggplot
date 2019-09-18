#' @rdname ggplot2.loon
#' @export
#' @examples
#' library(GGally)
#' p <- l_pairs(iris, showHistograms = TRUE)
#' g <- ggplot2.loon(p)
#' g + ggtitle("Iris pairs plot")
ggplot2.loon.l_pairs <- function(target, ...) {

  widget <- target
  remove(target)

  locations <- loon::l_getLocations(widget)
  nrow <- locations$nrow
  ncol <- locations$ncol
  layout_matrix <- locations$layout_matrix

  ggplots <- suppressMessages(
    lapply(1:length(widget),
           function(i) {
             ggplot2.loon(widget[[i]])
           })
  )

  gm <- GGally::ggmatrix(
    plots = wrap_paris_plots(
      ggplots = ggplots,
      layout_matrix,
      nrow,
      ncol,
      texts = pairs_text(widget)
    ),
    nrow = nrow,
    ncol = ncol,
    byrow = FALSE,
    showXAxisPlotLabels = FALSE,
    showYAxisPlotLabels = FALSE) +
    theme(plot.background = ggplot2::element_rect(fill = loon::l_getOption("canvas_bg_guides")))
  return(gm)
}


wrap_paris_plots <- function(ggplots, layout_matrix,
                             nrow = NULL, ncol = NULL,
                             texts = NULL) {

  stopifnot(
    exprs = {
      !missing(ggplots)
      !missing(layout_matrix)
      !is.null(texts)
    }
  )

  default_text_size <- 3
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

  layout_matrix <- as.vector(layout_matrix)

  nrow <- nrow %||% dim(layout_matrix)[1]
  ncol <- ncol %||% dim(layout_matrix)[2]

  plots <- lapply(1:(nrow * ncol),
                  function(i) {
                    plot_id <- layout_matrix[i]

                    if(showHistOnEdge && !showHistOnDiag) {

                      texts_pos <- 2 + (nrow + 1) * (1:(ncol-1) - 1)

                      if(is.na(plot_id)) {
                        if(i %in% texts_pos) {
                          ggplot2::ggplot() +
                            ggplot2::geom_text(
                              data = data.frame(x = 0, y = 0, label = texts[texts_pos %in% i]),
                              mapping = aes(
                                x = x,
                                y = y,
                                label = label
                              ),
                              size = default_text_size) +
                            theme(panel.grid.minor = ggplot2::element_blank(),
                                  panel.grid.major= ggplot2::element_blank(),
                                  panel.background = ggplot2::element_rect(fill = loon::l_getOption("canvas_bg_guides")))
                        } else NULL
                      } else {
                        ggplots[[plot_id]]
                      }

                    } else if(!showHistOnEdge && showHistOnDiag) {

                      if(is.na(plot_id)) {
                        NULL
                      } else {
                        ggplots[[plot_id]]

                      }

                    } else if(!showHistOnEdge && !showHistOnDiag) {

                      texts_pos <- 1 + (nrow + 1) * (1:ncol - 1)

                      if(is.na(plot_id)) {
                        if(i %in% texts_pos) {
                          ggplot2::ggplot() +
                            ggplot2::geom_text(
                              data = data.frame(x = 0, y = 0, label = texts[texts_pos %in% i]),
                              mapping = aes(
                                x = x,
                                y = y,
                                label = label
                              ),
                              size = default_text_size) +
                            theme(panel.grid.minor = ggplot2::element_blank(),
                                  panel.grid.major= ggplot2::element_blank(),
                                  panel.background = ggplot2::element_rect(fill = loon::l_getOption("canvas_bg_guides")))
                        } else NULL
                      } else {
                        ggplots[[plot_id]]
                      }

                    } else NULL

                  }
  )


}

pairs_text <- function(widget) {

  texts <- lapply(widget,
                  function(w) {
                    if(inherits(w, "l_plot")) {
                      c(w['ylabel'], w['xlabel'])
                    }
                  })
  unique(unlist(texts))
}
