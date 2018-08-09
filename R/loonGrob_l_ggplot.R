#' @title Create a grid grob from a loon widget handle
#'
#' @description Grid grobs are useful to create publication quality graphics.
#' @param target loon target. \code{S3} method, see \link{loonGrob}
#' @return a grid grob
#'
#' @import grid
#' @importFrom gridExtra arrangeGrob
#'
#'
#' @examples
#' p <- ggplot(mtcars, aes(mpg, wt, colour = cyl)) + geom_point()+ facet_wrap(~gear)
#' g <- loon.ggplot(p)
#' library(grid)
#' grid.newpage(); grid.draw(loonGrob(g))


#' @export
loonGrob_getPlots.l_ggplot <- function(target){
  # throw errors if not loon widget
  lapply(target,
         function(tar){
           l_throwErrorIfNotLoonWidget(tar)
         }
  )
  widget <- target
  len_widget <- length(widget)
  if(len_widget == 1) {
    new_widget <- widget
    recover.args <- NULL
  } else {
    # set xlabel ylabel, ...
    xlabels <- lapply(widget,
                      function(w){
                        w['xlabel']
                      }
    )
    ylabels <- lapply(widget,
                      function(w){
                        w['ylabel']
                      }
    )
    labelMargins <- lapply(widget,
                           function(w){
                             w['labelMargins']
                           }
    )
    scalesMargins <- lapply(widget,
                            function(w){
                              w['scalesMargins']
                            }
    )
    new_widget <- lapply(widget,
                         function(w){
                           w['xlabel'] <- ""
                           w['ylabel'] <- ""
                           w['scalesMargins'] <- c(15, 10, 0, 0)
                           w['labelMargins'] <- c(15, 20, 35, 0)
                           w
                         }
    )
    class(new_widget) <- class(widget)
    recover.args <- list(xlabels = xlabels,
                        ylabels = ylabels,
                        labelMargins = labelMargins,
                        scalesMargins = scalesMargins
    )
  }
  list(
    widget = new_widget,
    recover.args = recover.args
  )
}

#' @export
loonGrob_getLocations.l_ggplot <- function(target){

  widget <- target
  len_widget <- length(widget)
  # layout_matrix
  if(len_widget == 1) {
    layout_matrix <- matrix(1, ncol = 1, nrow = 1)
    xlabel <- ylabel <- NULL
  } else {
    names <- names(widget)
    namesSplit <- strsplit(names, split = "")
    ggLayout <- t(sapply(namesSplit,
                         function(i){
                           xpos <- which(i %in% "x" == TRUE)
                           ypos <- which(i %in% "y" == TRUE)
                           len_str <- length(i)
                           c(as.numeric(paste0(i[(xpos + 1) : (ypos - 1)], collapse = "")),
                             as.numeric(paste0(i[(ypos + 1) : (len_str)], collapse = "")))
                         })
    )
    # colnames(ggLayout) <- c("ROW", "COL")
    rownames <- seq_len(len_widget)
    layoutDim <- apply(ggLayout, 2, max)
    # layout matrix
    layout_matrix <- matrix(rep(NA, layoutDim[1] * layoutDim[2]), nrow = layoutDim[1])
    for(i in 1:len_widget) {
      ggLayouti <- ggLayout[i, ]
      # set layout matrix
      layout_matrix[ggLayouti[1], ggLayouti[2]] <- rownames[i]
    }
    xlabels <- lapply(widget,
                      function(w){
                        w['xlabel']
                      }
    )
    ylabels <- lapply(widget,
                      function(w){
                        w['ylabel']
                      }
    )
    xlabel <- paste0(unique(xlabels), collapse = " ")
    ylabel <- paste0(unique(ylabels), collapse = " ")
  }

  arrangeGrob.args <- list(
    name =  "l_ggplot",
    layout_matrix = layout_matrix,
    bottom = xlabel,
    left = ylabel
  )

  list(
    arrangeGrob.args = arrangeGrob.args
  )
}

#' @export
loonGrob_recoverLoonWidgets.l_ggplot <- function(widget, recover.args){

  if(!is.null(recover.args)) {
    len_widget <- length(widget)
    # xlabels
    xlabels <- recover.args$xlabels
    # ylabels
    ylabels <- recover.args$ylabels
    # scalesMargins
    scalesMargins <- recover.args$scalesMargins
    # labelMargins
    labelMargins <- recover.args$labelMargins

    lapply(seq_len(len_widget),
           function(i){
             widgeti <- widget[[i]]
             l_configure(widgeti,
                         xlabel = xlabels[[i]],
                         ylabel = ylabels[[i]],
                         labelMargins = labelMargins[[i]],
                         scalesMargins = scalesMargins[[i]])
           }
    )
  } else NULL

}
