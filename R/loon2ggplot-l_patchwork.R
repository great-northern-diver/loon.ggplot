#' @rdname loon2ggplot
#' @export
loon2ggplot.l_patchwork <- function(target, asAes = TRUE, selectedOnTop = TRUE,
                                    showNearestColor = FALSE, ...) {
  # get loon plot paths
  n <- length(target)
  plots <- stats::setNames(
    lapply(target,
           function(x){
             loon2ggplot(x, asAes = asAes,
                         selectedOnTop = selectedOnTop,
                         showNearestColor = showNearestColor,
                         ...)
           }),
    paste0("plot", seq(n)))

  n <- length(plots)
  positions <- matrix(NA,
                          nrow = n,
                          ncol = 4)
  layout <- strsplit(names(target), "t|b|l|r")

  for(i in seq(n)) {
    positions[i, ] <- na.omit(as.numeric(layout[[i]]))
  }
  colnames(positions) <- c("t", "l", "b", "r")

  plots$design <- do.call(c,
                          lapply(seq(nrow(positions)),
                                 function(i) {
                                   do.call(patchwork::area,
                                           as.list(positions[i, ]))
                                 }))

  ggCompound(plots, setBackground = FALSE)
}
