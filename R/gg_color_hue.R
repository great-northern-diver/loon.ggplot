# #' @title Default \code{ggplot} color palette
# #' @description Generate default discrete \code{ggplot} color
# #' @param n the number of colors
gg_color_hue <- function(n) {
  hues <- seq(15, 375, length = n + 1)
  grDevices::hcl(h = hues, l = 65, c = 100)[1:n]
}
