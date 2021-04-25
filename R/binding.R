bindingX <- function(loonplot, synchronizeBindingsX) {
  UseMethod("bindingX", loonplot)
}

bindingX.l_facet_ggplot <- function(loonplot, synchronizeBindingsX) {
  lps <- l_getPlots(loonplot)
  lapply(lps,
         function(lp) {
           tcl(lp, 'systembind', 'state', 'add',
               c('zoomX', 'panX', 'deltaX', 'swapAxes'),
               synchronizeBindingsX)
         }
  )
}

bindingX.l_plot <- function(loonplot, synchronizeBindingsX) {
  tcl(loonplot, 'systembind', 'state', 'add',
      c('zoomX', 'panX', 'deltaX', 'swapAxes'),
      synchronizeBindingsX)
}

bindingX.l_hist<- function(loonplot, synchronizeBindingsX) {
  tcl(loonplot, 'systembind', 'state', 'add',
      c('zoomX', 'panX', 'deltaX', 'swapAxes'),
      synchronizeBindingsX)
}

bindingY <- function(loonplot, synchronizeBindingsY) {
  UseMethod("bindingY", loonplot)
}

bindingY.l_facet_ggplot <- function(loonplot, synchronizeBindingsY) {
  lps <- l_getPlots(loonplot)
  lapply(lps,
         function(lp) {
           tcl(lp, 'systembind', 'state', 'add',
               c('zoomY', 'panY', 'deltaY', 'swapAxes'),
               synchronizeBindingsY)
         }
  )
}

bindingY.l_plot<- function(loonplot, synchronizeBindingsY) {
  tcl(loonplot, 'systembind', 'state', 'add',
      c('zoomY', 'panY', 'deltaY', 'swapAxes'),
      synchronizeBindingsY)
}

bindingY.l_hist<- function(loonplot, synchronizeBindingsY) {
  tcl(loonplot, 'systembind', 'state', 'add',
      c('zoomY', 'panY', 'deltaY', 'swapAxes'),
      synchronizeBindingsY)
}
