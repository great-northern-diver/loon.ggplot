#' @title layout matrix
#' @description return the layout matrix of a list of loon plots
#' @param target an object `loon.ggplot()` returns
#'
#' @examples
#' p <- ggplot(mpg, aes(displ, cty)) + geom_point() + ggtitle("foo") +
#'  facet_grid(rows = vars(drv))
#' g <- loon.ggplot(p)
#' layout_matrix(g)
#' # l_getLocations(g)
#'
#'
#' @export
layout_matrix <- function(target) {
  UseMethod("layout_matrix", target)
}

#' @export
layout_matrix.l_plot <- function(target) {
  ggLayout <- matrix(c(1,1), nrow = 1)
  colnames(ggLayout) <- c("ROW", "COL")
  ggLayout
}

#' @export
layout_matrix.l_hist <- function(target) {
  ggLayout <- matrix(c(1,1), nrow = 1)
  colnames(ggLayout) <- c("ROW", "COL")
  ggLayout
}

#' @export
layout_matrix.l_ggplot <- function(target) {
  plots <- target$plots
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
  colnames(ggLayout) <- c("ROW", "COL")
  ggLayout
}
