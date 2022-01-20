#' @export
ggplot2loon.patchwork <- function(ggObj, ..., activeGeomLayers = integer(0),
                                  layerId = NULL, scaleToFun = NULL,
                                  ggGuides = FALSE, parent = NULL, pack = TRUE,
                                  exteriorLabelProportion = 1/5,
                                  canvasHeight = 700, canvasWidth = 850) {
  # default args for each individual ggplot
  args <- c(
    list(activeGeomLayers = activeGeomLayers,
         parent = parent,
         ggGuides = ggGuides,
         pack = FALSE,
         layerId = layerId,
         scaleToFun = scaleToFun,
         exteriorLabelProportion = exteriorLabelProportion),
    ...
  )
  patchwork <- ggObj

  ggplots <- patchwork2ggplots(patchwork)
  position <- patchwork4position(patchwork)
  # get layout_matrix
  layout_matrix <- positions2layout_matrix(position)

  lapply(ggplots,
         function(ggplot) {
           if(inherits(ggplot, "patchwork")) {
             stop("Currently, `ggplot2loon()` can transform a `patchwork` object ",
                  "whose children are `ggplot` objects only (not nested `patchwork` objects). ",
                  "Fortunately, you can set the argument `design` in `plot_layout()` ",
                  "to arrange multiple ggplots in a single panel. See vignette `ggplots --> loon plots` for more details."
                  )
           }
  })

  if(is.null(parent)) {
    parent <- loon::l_toplevel()
    subwin <- loon::l_subwin(parent, 'patchwork')
    tcltk::tktitle(parent) <- paste("loon.ggplot", "--path:", subwin)
    parent <- as.character(tcltk::tcl('frame', subwin))
  }

  args$parent <- parent
  if(is.null(args$linkingGroup)) {
    args$linkingGroup <- parent
    message("The linkingGroup is set as ", parent,
            " for all plots by default.")
  }

  # get unpacked loon plots
  loonplots <- list()

  for(i in seq_along(ggplots)) {

    lp <- do.call(
      ggplot2loon,
      c(list(ggObj = ggplots[[i]]), args)
    )

    if(is.l_facet(lp)) {
      k <- length(loonplots) + 1
      loonplots <- c(loonplots, lp)
      # update layout_matrix
      layout_matrix <- update_layout_matrix(layout_matrix, k, length(lp) - 1)
      lp_layout_matrix <- loon::l_getLocations(lp)$layout_matrix + k - 1
      layout_matrix <- layout_matrixExtend(layout_matrix,
                                           k,
                                           lp_layout_matrix)
    } else loonplots <- c(loonplots, list(lp))
  }

  position <- layout_matrix2positions(layout_matrix)
  t <- position$t
  l <- position$l
  b <- position$b
  r <- position$r

  maxRow <- max(c(t, b))
  heightUnit <- round(canvasHeight/maxRow)
  maxCol <- max(c(l, r))
  widthUnit <- round(canvasWidth/maxCol)

  names(loonplots) <- paste0("t", t, "l", l, "b", b, "r", r)

  if(pack) {

    for(i in seq_along(loonplots)) {

      rowspan <- (b[i] - t[i] + 1)
      plotHeight <- rowspan * heightUnit
      columnspan <- (r[i] - l[i] + 1)
      plotWidth <- columnspan * widthUnit

      tcltk::tkconfigure(paste(loonplots[[i]], '.canvas', sep=''),
                         width = plotWidth,
                         height = plotHeight)
      tcltk::tkgrid(loonplots[[i]],
                    row = t[i] - 1,
                    column= l[i] - 1,
                    rowspan = rowspan,
                    columnspan = columnspan,
                    sticky = "nesw")

      # tk column configure
      for (ii in (l[i] - 1):(l[i] + columnspan - 2)) {
        tcltk::tkgrid.columnconfigure(parent, ii, weight=1)
      }
      # tk row configure
      for (ii in (t[i] - 1):(t[i] + rowspan - 2)) {
        tcltk::tkgrid.rowconfigure(parent, ii, weight=1)
      }
    }
    tcltk::tkpack(parent, fill="both", expand=TRUE)
    tcltk::tkconfigure(parent, bg = "white")
  }

  structure(
    loonplots,
    class = c("l_patchwork", "l_compound", "loon")
  )
}

patchwork2ggplots <- function(patchwork) {

  if(!is.patchwork(patchwork)) return(patchwork)

  ggObj <- patchwork
  # all plots are in patchwork
  ggplots <- patchwork$patches$plots
  ## ggObj is the last plot
  ggObj$patches <- NULL
  class(ggObj) <- c("gg", "ggplot")
  ggplots <- c(ggplots, list(ggObj))

  ggplots
}

patchwork4position <- function(patchwork) {

  layout <- patchwork$patches$layout
  position <- layout$design
  n <- length(patchwork$patches$plots) + 1

  if(is.null(position)) {

    nrow <- layout$nrow %||% 1
    ncol <- layout$ncol %||% ceiling(n/nrow)

    # TODO
    # byrow <- layout$byrow %||% TRUE
    # widths <- layout$widths %||% rep(1, ncol)
    # heights <- layout$heights %||% rep(1, nrow)

    # turn nrow, ncol and byrow as a `design` layout matrix
    position <- expand.grid(t = seq_len(nrow), l = seq_len(ncol))
    position$b <- position$t
    position$r <- position$l
    position <- position[order(position$t), ]
  }
  position
}

# getAllPlots <- function(patchwork) {
#   plots <- patchwork2ggplots(patchwork)
#   lapply(plots,
#          function(plot) {
#            if(!is.patchwork(plot)) return(plot)
#            getAllPlots(plot)
#          })
# }
# Flatten a list in R based on conditions by rrapply::rrapply
# See https://stackoverflow.com/questions/68355657/flatten-a-list-in-r-based-on-conditions


