synchronize <- function(target, ...) {
  UseMethod("synchronize", target)
}

synchronize.default <- function(target, ...) {

  args <- list(...)
  scales_free_x <- args$scales_free_x
  scales_free_y <- args$scales_free_y

  busy <- FALSE
  if(!scales_free_x & !scales_free_y) {
    # scales fixed
    synchronizeBindings <- function(W) {
      if (!busy) {
        busy <<- TRUE
        class(W) <- "loon"
        zoomX <- W['zoomX']; zoomY <- W['zoomY']
        panX <- W['panX']; panY <- W['panY']
        deltaX <- W['deltaX']; deltaY <- W['deltaY']
        swapAxes <- W['swapAxes']

        lapply(target,
               function(p) {
                 loon::l_configure(p, swapAxes = swapAxes,
                                   zoomX = zoomX, panX = panX, deltaX = deltaX,
                                   zoomY = zoomY, panY = panY, deltaY = deltaY)
               }
        )
        busy <<- FALSE
        tcltk::tcl('update', 'idletasks')
      }
    }

    lapply(target,
           function(p) {
             tcltk::tcl(p, 'systembind', 'state', 'add',
                        c('zoomX', 'panX', 'zoomY', 'panY', 'deltaX', 'deltaY', 'swapAxes'),
                        synchronizeBindings)
           })

  } else if (scales_free_x & !scales_free_y) {
    # y scales are fixed, x scales are free
    synchronizeBindings <- function(W) {
      if (!busy) {
        busy <<- TRUE
        class(W) <- "loon"
        zoomY <- W['zoomY']
        panY <- W['panY']
        deltaY <- W['deltaY']
        swapAxes <- W['swapAxes']

        lapply(target,
               function(p) {
                 loon::l_configure(p, zoomY=zoomY, panY=panY, deltaY=deltaY, swapAxes = swapAxes)
               }
        )
        busy <<- FALSE
        tcltk::tcl('update', 'idletasks')

      }
    }

    lapply(target,
           function(p) {
             tcltk::tcl(p, 'systembind', 'state', 'add',
                        c('zoomY', 'panY', 'deltaY', 'swapAxes'),
                        synchronizeBindings)
           })

  } else if (!scales_free_x & scales_free_y) {
    # x scales are fixed, y scales are free
    synchronizeBindings <- function(W) {
      if (!busy) {
        busy <<- TRUE
        class(W) <- "loon"
        zoomX <- W['zoomX']
        panX <- W['panX']
        deltaX <- W['deltaX']
        swapAxes <- W['swapAxes']

        lapply(target,
               function(p) {
                 loon::l_configure(p, zoomX=zoomX, panX=panX, deltaX=deltaX, swapAxes = swapAxes)
               }
        )
        busy <<- FALSE
        tcltk::tcl('update', 'idletasks')

      }
    }

    lapply(target,
           function(p) {
             tcltk::tcl(p, 'systembind', 'state', 'add',
                        c('zoomX', 'panX', 'deltaX', 'swapAxes'),
                        synchronizeBindings)
           })
  } else NULL
  tcltk::.Tcl('set ::loon::Options(printInConfigurationWarning) FALSE')
}

synchronize.l_ggmatrix <- function(target, ...) {

  busy <- FALSE
  plots <- l_getPlots(target)
  # scales fixed
  synchronizeBindingsX <- function(W) {
    if (!busy) {
      busy <<- TRUE
      class(W) <- "loon"
      zoomX <- W['zoomX']
      panX <- W['panX']
      deltaX <- W['deltaX']
      swapAxes <- W['swapAxes']

      lapply(plots,
             function(p) {
               l_configure(p, swapAxes = swapAxes,
                           zoomX = zoomX, panX = panX, deltaX = deltaX)
             }
      )
      busy <<- FALSE
      tcl('update', 'idletasks')
    }
  }

  synchronizeBindingsY <- function(W) {
    if (!busy) {
      busy <<- TRUE
      class(W) <- "loon"
      zoomY <- W['zoomY']
      panY <- W['panY']
      deltaY <- W['deltaY']
      swapAxes <- W['swapAxes']

      lapply(plots,
             function(p) {
               l_configure(p, swapAxes = swapAxes,
                           zoomY = zoomY, panY = panY, deltaY = deltaY)
             }
      )
      busy <<- FALSE
      tcl('update', 'idletasks')
    }
  }

  layout <- layout_coords(target)

  for(row in unique(layout$row)) {
    loonplots <- plots[which(layout$row == row)]
    lapply(loonplots,
           function(loonplot) {
             bindingY(loonplot, synchronizeBindingsY)
           }
    )
  }

  for(column in unique(layout$col)) {
    loonplots <-plots[which(layout$col == column)]
    lapply(loonplots,
           function(loonplot) {
             bindingX(loonplot, synchronizeBindingsX)
           }
    )
  }

  .Tcl('set ::loon::Options(printInConfigurationWarning) FALSE')
}
