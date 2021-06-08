l_allNDimStateNames <- function(plots = c("l_plot", "l_plot3D", "l_serialaxes", "l_hist")) {
  states <- lapply(plots,
                   function(plot) {
                     loon::l_nDimStateNames(plot)
                   })
  unique(unlist(states))
}
