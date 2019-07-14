scalesSynchronize <- function(plots, scales_free_x, scales_free_y){

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

        lapply(plots,
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

    lapply(plots, function(p) {
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

        lapply(plots,
               function(p) {
                 loon::l_configure(p, zoomY=zoomY, panY=panY, deltaY=deltaY, swapAxes = swapAxes)
               }
        )
        busy <<- FALSE
        tcltk::tcl('update', 'idletasks')

      }
    }

    lapply(plots, function(p) {
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

        lapply(plots,
               function(p) {
                 loon::l_configure(p, zoomX=zoomX, panX=panX, deltaX=deltaX, swapAxes = swapAxes)
               }
        )
        busy <<- FALSE
        tcltk::tcl('update', 'idletasks')

      }
    }

    lapply(plots, function(p) {
      tcltk::tcl(p, 'systembind', 'state', 'add',
                 c('zoomX', 'panX', 'deltaX', 'swapAxes'),
                 synchronizeBindings)
    })
  } else {
    NULL
  }
  tcltk::.Tcl('set ::loon::Options(printInConfigurationWarning) FALSE')
}
