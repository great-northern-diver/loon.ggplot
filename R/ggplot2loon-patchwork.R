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

  # all plots are in patchwork
  ggplots <- patchwork$patches$plots
  ## ggObj is the last plot
  ggObj$patches <- NULL
  class(ggObj) <- c("gg", "ggplot")
  ggplots <- c(ggplots, list(ggObj))
  n <- length(ggplots)

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

  layout <- patchwork$patches$layout
  design <- layout$design

  if(length(design) == 0) {

    nrow <- layout$nrow %||% 1
    ncol <- layout$ncol %||% ceiling(n/nrow)

    # TODO
    # byrow <- layout$byrow %||% TRUE
    # widths <- layout$widths %||% rep(1, ncol)
    # heights <- layout$heights %||% rep(1, nrow)

    # turn nrow, ncol and byrow as a `design` layout matrix
    design <- expand.grid(t = seq_len(nrow), l = seq_len(ncol))
    design$b <- design$t
    design$r <- design$l
    design <- design[order(design$t), ]
  }

  t <- design$t
  l <- design$l
  b <- design$b
  r <- design$r

  maxRow <- max(c(t, b))
  heightUnit <- round(canvasHeight/maxRow)
  maxCol <- max(c(l, r))
  widthUnit <- round(canvasWidth/maxCol)

  if(is.null(parent)) {
    parent <- loon::l_toplevel()
    subwin <- loon::l_subwin(parent, 'patchwork')
    tcltk::tktitle(parent) <- paste("loon.ggplot", "--path:", subwin)
    parent <- as.character(tcltk::tcl('frame', subwin))
  }

  args$parent <- parent
  if(is.null(args$linkingGroup)) {
    args$linkingGroup <- parent
    message("The linkingGroup is set as ", parent, " for all plots by default.")
  }

  loonplots <- list()
  names <- c()

  for(i in seq(n)) {

    rowspan <- (b[i] - t[i] + 1)
    plotHeight <- rowspan * heightUnit
    columnspan <- (r[i] - l[i] + 1)
    plotWidth <- columnspan * widthUnit

    args$canvasHeight <- plotHeight
    args$canvasWidth <- plotWidth

    lp <- do.call(
      ggplot2loon,
      c(list(ggObj = ggplots[[i]]), args)
    )

    loonplots[[i]] <- lp
    names[i] <- paste0("t", t[i], "l", l[i], "b", b[i], "r", r[i])

    tcltk::tkconfigure(paste(lp, '.canvas', sep=''),
                       width = plotWidth,
                       height = plotHeight)
    tcltk::tkgrid(lp,
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

  if(pack)
    tcltk::tkpack(parent, fill="both", expand=TRUE)

  names(loonplots) <- names
  structure(
    loonplots,
    class = c("l_patchwork", "l_compound", "loon")
  )
}
