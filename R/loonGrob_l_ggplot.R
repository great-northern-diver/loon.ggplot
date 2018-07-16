#' @title Create a grid grob from a loon widget handle
#'
#' @description Grid grobs are useful to create publication quality graphics.
#'
#' @param target loon target. \code{S3} method, see \link{loonGrob}
#' @param name name of loon grob
#' @param gp graphical parameter settings of loon grob
#' @param vp viewport of loon grob
#'
#' @return a grid grob
#'
#' @import grid
#' @importFrom gridExtra arrangeGrob
#'
#' @export
#'
#' @examples
#' p <- ggplot(mtcars, aes(mpg, wt, colour = cyl)) + geom_point()+ facet_wrap(~gear)
#' g <- loon.ggplot(p)
#' library(grid)
#' grid.newpage(); grid.draw(loonGrob(g))


loonGrob.l_ggplot <- function(target, name = NULL, gp = NULL, vp = NULL) {

  widget <- target
  len_widget <- length(widget)

  if (len_widget == 1) {
    gTree(
      children = gList(
        loonGrob(widget[[1]])
      ),
      name = name, gp = gp, vp = vp
    )
  } else {
    names <- names(widget)
    namesSplit <- strsplit(names, split = "")
    ggLayout <- t(sapply(namesSplit, function(i){
      xpos <- which(i %in% "x" == TRUE)
      ypos <- which(i %in% "y" == TRUE)
      len_str <- length(i)
      c(as.numeric(paste0(i[(xpos + 1) : (ypos - 1)], collapse = "")),
        as.numeric(paste0(i[(ypos + 1) : (len_str)], collapse = "")))
    }))
    # colnames(ggLayout) <- c("ROW", "COL")
    rownames <- seq_len(len_widget)
    layoutDim <- apply(ggLayout, 2, max)
    # layout matrix
    layout_matrix <- matrix(rep(NA, layoutDim[1] * layoutDim[2]), nrow = layoutDim[1])
    # set xlabel ylabel, ...
    xlabel_original <- c()
    ylabel_original <- c()
    # minimumMargins_original <- list()
    labelMargins_original <- list()
    scalesMargins_original <- list()
    for(i in 1:len_widget) {
      ggLayouti <- ggLayout[i, ]
      # set layout matrix
      layout_matrix[ggLayouti[1], ggLayouti[2]] <- rownames[i]
      # store xlabel
      xlabel_original <- c(xlabel_original, widget[[i]]['xlabel'])
      # store ylabel
      ylabel_original <- c(ylabel_original, widget[[i]]['ylabel'])
      # store minimum margins
      # minimumMargins_original[[i]] <- widget[[i]]['minimumMargins']
      # store label margins
      labelMargins_original[[i]] <- widget[[i]]['labelMargins']
      # store scale margins
      scalesMargins_original[[i]] <- widget[[i]]['scalesMargins']
    }
    # labels
    # xlabel <- paste0(unique(sapply(widget, function(w)w['xlabel'])), collapse = " ")
    xlabel <- paste0(unique(xlabel_original), collapse = " ")
    ylabel <- paste0(unique(ylabel_original), collapse = " ")

    # loon grobs
    lgrob <- lapply(seq_len(len_widget), function(i) {
      widgeti <- widget[[i]]
      l_configure(widgeti,
                  xlabel = "",
                  ylabel = "",
                  # minimumMargins = c(20, 20, 20, 20),
                  scalesMargins = c(15, 10, 0, 0),
                  labelMargins = c(15, 10, 35, 0)
      )
      loonGrob(widgeti)
    })
    # recover loon plot
    recover <- lapply(seq_len(len_widget), function(i){
      widgeti <- widget[[i]]
      l_configure(widgeti,
                  xlabel = xlabel_original[i],
                  ylabel = ylabel_original[i],
                  # minimumMargins = minimumMargins_original[[i]],
                  scalesMargins = scalesMargins_original[[i]],
                  labelMargins = labelMargins_original[[i]]
      )
    })
    # draw loon grobs
    gTree(
      children = gList(
        gridExtra::arrangeGrob(grobs = lgrob,
                               layout_matrix = layout_matrix,
                               left = if (ylabel == "") NA else ylabel,
                               bottom = if (xlabel == "") NA else xlabel,
                               name = "l_ggplot")
      ),
      name = name, gp = gp, vp = vp
    )
  }
}
