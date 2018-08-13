#' @title Create a grid grob from a loon widget handle
#'
#' @description Grid grobs are useful to create publication quality graphics.
#' @param target loon target. \code{S3} method, see \link{loonGrob}
#' @param name a character identifier for the grob. Used to find the grob on the display list and/or as a child of another grob.
#' @param gp A gpar object, typically the output from a call to the function gpar. This is basically a list of graphical parameter settings.
#' @param vp a \link{viewport} object (or NULL).
#'
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
loonGrob_layoutType.l_ggplot <- function(target) {
  "arrangeGrobArgs"
}

#' @export
l_get_arrangeGrobArgs.l_ggplot <- function(target){
  widget <- target
  len_widget <- length(widget)

  # loon grobs
  lgrobs <- lapply(widget,
                   function(w) {
                     loonGrob(w)
                   }
  )
  # label
  xlabel <- paste0(unique(sapply(widget, function(w)w['xlabel'])), collapse = " ")
  ylabel <- paste0(unique(sapply(widget, function(w)w['ylabel'])), collapse = " ")
  title_subtitle <- unique(sapply(widget, function(w)w['title']))
  if(all(str_detect(title_subtitle, "[%+%]"))) {
    title <- unique(sapply(strsplit(title_subtitle, split = "%+%", fixed = TRUE), function(char) char[1]))
    subtitle <- sapply(strsplit(title_subtitle, split = "%+%", fixed = TRUE), function(char) char[-1])
  } else {
    title <- NA
    subtitle <- title_subtitle
  }

  if (len_widget == 1) {
    layout_matrix <- matrix(1, nrow = 1, ncol = 1)
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
    values <- seq_len(len_widget)
    layoutDim <- apply(ggLayout, 2, max)
    # layout matrix
    layout_matrix <- matrix(rep(NA, layoutDim[1] * layoutDim[2]), nrow = layoutDim[1])
    for(i in 1:len_widget) {
      ggLayouti <- ggLayout[i, ]
      # set layout matrix
      layout_matrix[ggLayouti[1], ggLayouti[2]] <- values[i]
    }
  }

  list(
    grobs = lgrobs,
    layout_matrix = layout_matrix,
    left = if (ylabel == "") NA else ylabel,
    bottom = if (xlabel == "") NA else xlabel,
    top = grid::textGrob(title, x = 0, hjust = 0),
    name = "l_ggplot"
  )
}
