#' @title get \code{ggplot}s
#' @description For the target compound loon plot, determines all the \code{ggplot}s based on
#' the compound \code{loon} plot.
#'
#' @param target the (compound) loon plot
#' @inheritParams loon2ggplot
#'
#' @return a list of \code{ggplot}s.
#'
#' @seealso \code{\link{l_getPlots}}, \code{\link{g_getLocations}}
#'
#' @export
g_getPlots <- function(target, asAes = TRUE, selectedOnTop = TRUE,
                       showNearestColor = FALSE) {
  UseMethod('g_getPlots', target)
}

#' @export
#' @rdname g_getPlots
g_getPlots.default <- function(target, asAes = TRUE, selectedOnTop = TRUE,
                               showNearestColor = FALSE) {

  # locations
  locations <- g_getLocations(target)
  nrow <- locations$nrow
  ncol <- locations$ncol
  layout_matrix <- locations$layout_matrix

  # plots
  ggplots <- suppressMessages(
    lapply(seq(length(target)),
           function(i) {
             loon2ggplot(target[[i]], asAes = asAes,
                         selectedOnTop = selectedOnTop,
                         showNearestColor = showNearestColor)
           })
  )

  layout_matrix <- as.vector(layout_matrix)

  plots <- lapply(seq(nrow * ncol),
                  function(i) {
                    plot_id <- layout_matrix[i]
                    if(is.na(plot_id)) {
                      NULL
                    } else {
                      ggplots[[plot_id]]
                    }
                  }
  )

  return(plots)
}

#' @export
#' @rdname g_getPlots
g_getPlots.l_pairs <- function(target, asAes = TRUE, selectedOnTop = TRUE,
                               showNearestColor = FALSE) {

  # locations
  locations <- g_getLocations(target)
  nrow <- locations$nrow
  ncol <- locations$ncol
  layout_matrix <- locations$layout_matrix

  # plots
  ggplots <- suppressMessages(
    lapply(seq(length(target)),
           function(i) {
             loon2ggplot(target[[i]], asAes = asAes,
                         selectedOnTop = selectedOnTop,
                         showNearestColor = showNearestColor)
           })
  )

  wrap_pairs_plots(
    ggplots = ggplots,
    layout_matrix = layout_matrix,
    nrow = nrow,
    ncol = ncol,
    texts = pairs_text(target)
  )
}

wrap_pairs_plots <- function(ggplots,
                             layout_matrix,
                             nrow = NULL,
                             ncol = NULL,
                             texts = NULL,
                             default_text_size = 3) {

  stopifnot(
    exprs = {
      !missing(ggplots)
      !missing(layout_matrix)
      !is.null(texts)
    }
  )


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

  nrow <- nrow %||% dim(layout_matrix)[1]
  ncol <- ncol %||% dim(layout_matrix)[2]
  layout_matrix <- as.vector(layout_matrix)

  plots <- lapply(seq(nrow * ncol),
                  function(i) {
                    plot_id <- layout_matrix[i]

                    if(showHistOnEdge && !showHistOnDiag) {

                      texts_pos <- 2 + (nrow + 1) * (1:(ncol-1) - 1)

                      if(is.na(plot_id)) {
                        if(i %in% texts_pos) {
                          x <- 0
                          y <- 0
                          label <- texts[texts_pos %in% i]
                          ggplot2::ggplot() +
                            ggplot2::geom_text(
                              data = data.frame(x = x, y = y, label = label),
                              mapping = ggplot2::aes(
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

                          x <- 0
                          y <- 0
                          label <- texts[texts_pos %in% i]

                          ggplot2::ggplot() +
                            ggplot2::geom_text(
                              data = data.frame(x = x, y = y, label = label),
                              mapping = ggplot2::aes(
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
  return(plots)
}

pairs_text <- function(target) {

  texts <- lapply(target,
                  function(w) {
                    if(inherits(w, "l_plot")) {
                      c(w['ylabel'], w['xlabel'])
                    }
                  })
  unique(unlist(texts))
}
