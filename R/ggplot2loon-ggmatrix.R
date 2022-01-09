#' @export
ggplot2loon.ggmatrix <- function(ggObj, ..., activeGeomLayers = integer(0),
                                 layerId = NULL, scaleToFun = NULL,
                                 ggGuides = FALSE, parent = NULL, pack = TRUE,
                                 exteriorLabelProportion = 1/5,
                                 canvasHeight = 700, canvasWidth = 850) {
  # default args
  args <- c(
    list(activeGeomLayers = activeGeomLayers,
         parent = parent,
         ggGuides = ggGuides,
         pack = FALSE,
         layerId = layerId,
         scaleToFun = scaleToFun,
         exteriorLabelProportion = exteriorLabelProportion,
         canvasHeight = canvasHeight,
         canvasWidth = canvasWidth),
    ...
  )

  if(is.null(parent)) {
    parent <- l_toplevel()
    subwin <- loon::l_subwin(parent, 'ggplot')
    tcltk::tktitle(parent) <- paste("loon.ggplot", "--path:", subwin)
    parent <- as.character(tcltk::tcl('frame', subwin))
  }

  nrow <- ggObj$nrow
  ncol <- ggObj$ncol
  byrow <- ggObj$byrow

  ggplots <- ggObj$plots
  nplots <- length(ggplots)

  # modify ggplot
  ggplots <- lapply(ggplots,
                    function(plot){
                      if(ggplot2::is.ggplot(plot)) {
                        plot
                      } else if(is.ggmatrix_plot_obj(plot)) {
                        plot$fn(ggObj$data, plot$mapping)
                      } else {
                        stop("Not implemented yet", call. = FALSE)
                      }
                    }
  )

  # labels
  title <- ggObj$title
  xlab <- ggObj$xlab
  ylab <- ggObj$ylab
  xAxisLabels <- ggObj$xAxisLabels
  yAxisLabels <- ggObj$yAxisLabels
  showXAxisPlotLabels <- ggObj$showXAxisPlotLabels & !is.null(xAxisLabels)
  showYAxisPlotLabels <- ggObj$showYAxisPlotLabels & !is.null(yAxisLabels)

  # positions
  start.xpos <- if(!is.null(ylab)) 1 else 0
  start.ypos <- if(!is.null(title)){
    ifelse(showXAxisPlotLabels, 2, 1)
  } else {
    ifelse(showXAxisPlotLabels, 1, 0)
  }

  # layout
  ggLayout <- lapply(ggplots,
                     function(plot){
                       build <- ggplot_build(plot)
                       build$layout
                     })
  # span
  span <- round(1/exteriorLabelProportion)
  rowspan <- span
  columnspan <- span
  lapply(seq(nplots),
         function(i){
           plot <- ggplots[[i]]
           layout <- ggLayout[[i]]$layout
           rowspan <<- max(rowspan, span*layout$ROW)
           columnspan <<- max(columnspan, span*layout$COL)
         }
  )

  if(byrow) {
    outside <- nrow
    inside <- ncol
  } else {
    outside <- ncol
    inside <- nrow
  }

  loonplots <- list()
  names <- c()
  for(i in 1:outside) {
    for(j in 1:inside){
      plotId <- (i - 1) * inside + j
      if(plotId > nplots) {
        break
      } else {
        # one facet
        args$parent <- parent
        args$showLabels <- FALSE

        lp <- do.call(
          ggplot2loon.ggplot,
          c(list(ggObj = ggplots[[plotId]]), args)
        )

        loonplots[[plotId]] <- lp
        names[plotId] <- if(byrow) paste0("x", i, "y", j) else paste0("x", j, "y", i)

        if(dim(ggLayout[[plotId]]$layout)[1] == 1) {
          tcltk::tkconfigure(paste(lp,'.canvas',sep=''),
                             width = canvasWidth/ncol,
                             height = canvasHeight/nrow)
          row.start <- if(byrow) (i - 1) *  rowspan + start.ypos else (j - 1) *  rowspan + start.ypos
          col.start <- if(byrow) (j - 1) * columnspan + start.xpos else (i - 1) * columnspan + start.xpos
          tcltk::tkgrid(lp,
                        row = row.start,
                        column= col.start,
                        rowspan = rowspan,
                        columnspan = columnspan,
                        sticky = "nesw")
          # tk column configure
          for (ii in col.start:(col.start + columnspan - 1)) {
            tcltk::tkgrid.columnconfigure(parent, ii, weight=1)
          }
          # tk row configure
          for (ii in row.start:(row.start + rowspan - 1)) {
            tcltk::tkgrid.rowconfigure(parent, ii, weight=1)
          }
        } else {
          # multiple facets
          lplots <- lp$plots
          layout <- ggLayout[[plotId]]$layout
          numofROW <- max(layout$ROW)
          numofCOL <- max(layout$COL)

          lapply(lplots,
                 function(lplot){
                   tcltk::tkconfigure(paste(lplot,'.canvas',sep=''),
                                      width = canvasWidth/ncol/numofCOL,
                                      height = canvasHeight/nrow/numofROW)
                 }
          )

          # show strips?
          showStrips <- ggObj$showStrips
          layout <- ggLayout[[plotId]]$layout

          FacetWrap <- is.FacetWrap(ggObj$facet)
          FacetGrid <- is.FacetGrid(ggObj$facet)

          # row subtitle names
          fun <- if(FacetGrid) {
            # facets separated by facet_grid(), pack plots and labels
            facet_grid_tkpack
          } else if(FacetWrap) {
            facet_wrap_tkpack
          } else
            stop("Not implemented yet. So far, only `facet_wrap()` and `facet_grid()` are allowed to split panels",
                 call. = FALSE)

          do.call(
            fun,
            list(
              plotId = plotId,
              ggLayout = ggLayout,
              showStrips = showStrips,
              lplots = lplots,
              numofROW = numofROW,
              numofCOL = numofCOL,
              byrow = byrow,
              start.ypos = start.ypos,
              start.xpos = start.xpos,
              rowspan = rowspan,
              columnspan = columnspan,
              span = span,
              rownames = rownames,
              colnames = colnames,
              i = i,
              j = j,
              ncol = ncol,
              nrow = nrow,
              parent = parent,
              layout = layout
            )
          )
        }
      }
    }
  }

  modify_loon_tk_labes(parent = parent,
                       pack = pack,
                       title = title,
                       xlab = xlab,
                       ylab = ylab,
                       xAxisLabels = xAxisLabels,
                       yAxisLabels = yAxisLabels,
                       start.xpos = start.xpos,
                       start.ypos = start.ypos,
                       columnspan = columnspan,
                       rowspan = rowspan,
                       ncol = ncol,
                       nrow = nrow,
                       showXAxisPlotLabels = showXAxisPlotLabels,
                       showYAxisPlotLabels = showYAxisPlotLabels)

  names(loonplots) <- names
  structure(
    loonplots,
    class = c("l_ggmatrix", "l_compound", "loon")
  )
}

##################################### modify loon tk labels #####################################
modify_loon_tk_labes <- function(parent = tcltk::tktoplevel(),
                                 pack = TRUE,
                                 title = NULL,
                                 xlab = NULL,
                                 ylab = NULL,
                                 xAxisLabels = NULL,
                                 yAxisLabels = NULL,
                                 start.xpos = 1,
                                 start.ypos = 1,
                                 columnspan = 1,
                                 rowspan = 1,
                                 ncol = 1,
                                 nrow = 1,
                                 showXAxisPlotLabels = FALSE,
                                 showYAxisPlotLabels = FALSE) {

  # show title
  if(!is.null(title)) {
    tit <- as.character(
      tcltk::tcl('label',
                 as.character(loon::l_subwin(parent,'title')),
                 text= title,
                 bg = set_tkLabel()$titleBackground,
                 fg = set_tkLabel()$titleForeground,
                 borderwidth = set_tkLabel()$titleBorderwidth,
                 relief = set_tkLabel()$titleRelief)
    )
    tcltk::tkconfigure(tit,
                       font = tcltk::tkfont.create(size = 16, weight="bold"))
    tcltk::tkgrid(tit, row = 0, column = start.xpos,
                  rowspan = 1, columnspan = columnspan * ncol,
                  sticky="w")
  }

  # show x axis labels
  if(showXAxisPlotLabels) {
    for(i in 1:length(xAxisLabels)){
      xAxisLabel <- as.character(
        tcltk::tcl('label',
                   as.character(loon::l_subwin(parent,
                                               paste0('xAxisLabel-', i))),
                   text= xAxisLabels[i],
                   bg = set_tkLabel()$xlabelBackground,
                   fg = set_tkLabel()$xlabelForeground,
                   borderwidth = set_tkLabel()$xlabelBorderwidth,
                   relief = set_tkLabel()$xlabelRelief)
      )
      tcltk::tkgrid(xAxisLabel,
                    row = start.ypos - 1,
                    column = start.xpos + (i - 1) * columnspan,
                    rowspan = 1,
                    columnspan = columnspan,
                    sticky="nesw")
    }
  }

  # show y axis labels
  if(showYAxisPlotLabels) {
    for(i in 1:length(yAxisLabels)){
      yAxisLabel <- as.character(
        tcltk::tcl('label',
                   as.character(loon::l_subwin(parent,
                                               paste0('yAxisLabel-', i))),
                   text= paste(paste0(" ", strsplit(yAxisLabels[i], "")[[1L]], " "),
                               collapse = "\n"),
                   bg = set_tkLabel()$ylabelBackground,
                   fg = set_tkLabel()$ylabelForeground,
                   borderwidth = set_tkLabel()$ylabelBorderwidth,
                   relief = set_tkLabel()$ylabelRelief)
      )
      tcltk::tkgrid(yAxisLabel,
                    row = start.ypos + (i - 1)* rowspan,
                    column = start.xpos + columnspan * ncol,
                    rowspan = rowspan,
                    columnspan = 1,
                    sticky="nesw")
    }
  }

  # x labels
  if(!is.null(xlab)){
    xlabel <- as.character(
      tcltk::tcl('label',
                 as.character(loon::l_subwin(parent,'xlabel')),
                 text= xlab,
                 bg = set_tkLabel()$xlabelBackground,
                 fg = set_tkLabel()$xlabelForeground,
                 borderwidth = set_tkLabel()$xlabelBorderwidth,
                 relief = set_tkLabel()$xlabelRelief)
    )
    tcltk::tkgrid(xlabel,
                  row = rowspan * nrow + start.ypos,
                  column = start.xpos,
                  rowspan = 1,
                  columnspan = columnspan * ncol,
                  sticky="nesw")
  }

  if(!is.null(ylab)){
    ylabel <- as.character(
      tcltk::tcl('label',
                 as.character(loon::l_subwin(parent,'ylabel')),
                 text= paste(paste0(" ", strsplit(ylab, "")[[1]], " "), collapse = "\n"),
                 bg = set_tkLabel()$ylabelBackground,
                 fg = set_tkLabel()$ylabelForeground,
                 borderwidth = set_tkLabel()$ylabelBorderwidth,
                 relief = set_tkLabel()$ylabelRelief)
    )
    tcltk::tkgrid(ylabel,
                  row = start.ypos,
                  column = 0,
                  rowspan = rowspan * nrow,
                  columnspan = 1,
                  sticky="nesw")
  }

  if(pack)
    tcltk::tkpack(parent, fill="both", expand=TRUE)
}
