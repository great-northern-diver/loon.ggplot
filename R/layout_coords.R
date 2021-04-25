#' @title layout matrix
#' @description return the layout matrix of a list of loon plots
#' @param target an object \code{ggplot2loon()} returns
#' @export
#' @return a layout coordinate matrix
layout_coords <- function(target) {
  UseMethod("layout_coords", target)
}

#' @export
layout_coords.l_plot <- function(target) {
  ggLayout <- matrix(c(1,1), nrow = 1)
  colnames(ggLayout) <- c("row", "col")
  ggLayout
}

#' @export
layout_coords.l_hist <- function(target) {
  ggLayout <- matrix(c(1,1), nrow = 1)
  colnames(ggLayout) <- c("row", "col")
  ggLayout
}

#' @export
layout_coords.l_facet_ggplot <- function(target) {

  plots <- unclass(target)

  ggLayout <- as.data.frame(
    t(sapply(strsplit(names(plots), split = ""),
             function(i){
               xpos <- which(i %in% "x" == TRUE)
               ypos <- which(i %in% "y" == TRUE)
               len_str <- length(i)
               c(as.numeric(paste0(i[(xpos + 1) : (ypos - 1)], collapse = "")),
                 as.numeric(paste0(i[(ypos + 1) : (len_str)], collapse = "")))
             })
    )
  )
  colnames(ggLayout) <- c("row", "col")
  ggLayout
}
