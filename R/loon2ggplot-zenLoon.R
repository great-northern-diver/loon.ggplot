#' @rdname loon2ggplot
#' @export
loon2ggplot.zenLoon <- function(target, asAes = TRUE, selectedOnTop = TRUE,
                                showNearestColor = FALSE, ...) {
  # get loon plot paths
  parent <- target$loon$ID
  children <- as.character(tcltk::tkwinfo("child",  parent))

  # get ggplots
  plots <- stats::setNames(
    lapply(children,
           function(x){
             loon2ggplot(loon::l_getFromPath(x),
                         asAes, selectedOnTop,
                         showNearestColor)
           }),
    paste0("plot", seq_along(children)))

  # get locations
  positions <- target$layout$boundingBoxes
  colnames(positions) <- c("l", "r", "b", "t")
  #       [, 1]
  # [1, ]    A
  # [2, ]    B
  # [3, ]    B
  # [4, ]    C
  # in zen
  # the top locations of `A` `B` `C` are set as
  # **1 0 -2 (l --> s)**
  # the bottom locations of `A` `B` `C` are set as
  # **0 -2 -3 (l --> s)**
  # the difference is
  # **1 2 1**

  # in patchwork
  # the top locations of `A` `B` `C` are set as
  # **1 2 4 (s --> l)**
  # the bottom locations of `A` `B` `C` are set as
  ## **1 3 4 (s --> l)**
  # the difference is
  # **0 1 0**

  positions[, "b"] <- -positions[, "b"]
  positions[, "t"] <- -positions[, "t"]

  # set top and bottom above zero
  mintb <- min(positions[, "b"], positions[, "t"])
  positions[, "b"] <- positions[, "b"] - mintb
  positions[, "t"] <- positions[, "t"] - mintb

  # set right and left above zero
  minrl <- min(positions[, "r"], positions[, "l"])
  positions[, "r"] <- positions[, "r"] - minrl
  positions[, "l"] <- positions[, "l"] - minrl

  if(min(positions[, "t"]) == 0)
    positions[, "t"] <- positions[, "t"] + 1
  if(min(positions[, "l"]) == 0)
    positions[, "l"] <- positions[, "l"] + 1

  plots$design <- do.call(c,
                          lapply(seq(nrow(positions)),
                                 function(i) {
                                   do.call(patchwork::area,
                                           as.list(positions[i, ]))
                                 }))

  ggCompound(plots,
             fill.bg = loon::l_getOption("canvas_bg_guides"),
             colour.bg = loon::l_getOption("canvas_bg_guides"))
}
