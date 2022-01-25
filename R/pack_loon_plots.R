pack_loon_plots <- function(plotInfo = list(),
                            ggObj,
                            parent = NULL) {

  plots <- plotInfo$plots
  numOfPlots <- length(plots)
  display_info <- plotInfo$display_info

  # get from environment
  xlabel <- plotInfo$xlabel
  ylabel <- plotInfo$ylabel
  span <- plotInfo$span
  row.span <- plotInfo$row.span
  column.span <- plotInfo$column.span
  start.ypos <- plotInfo$start.ypos
  start.xpos <- plotInfo$start.xpos

  if(display_info$swapAxes) {
    label <- ylabel
    ylabel <- xlabel
    xlabel <- label
  }

  # synchronize binding
  synchronize(plots,
              scales_free_x = ggObj$facet$params$free$x %||% FALSE,
              scales_free_y = ggObj$facet$params$free$y %||% FALSE)

  # pack xlabel and ylabel
  if(!is.null(xlabel) && numOfPlots > 1) {
    xlab <- as.character(tcltk::tcl('label',
                                    as.character(loon::l_subwin(parent,'xlabel')),
                                    text= xlabel,
                                    bg = set_tkLabel()$xlabelBackground,
                                    fg = set_tkLabel()$xlabelForeground,
                                    borderwidth = set_tkLabel()$xlabelBorderwidth,
                                    relief = set_tkLabel()$xlabelRelief
    ))
    tcltk::tkgrid(xlab, row = row.span + start.ypos, column = start.xpos,
                  rowspan = 1, columnspan = column.span,
                  sticky="nesw")
  }
  if(!is.null(ylabel) && numOfPlots > 1) {
    ylab <- as.character(tcltk::tcl('label',
                                    as.character(loon::l_subwin(parent,'ylabel')),
                                    text= paste(paste0(" ", strsplit(ylabel, "")[[1]], " "),
                                                collapse = "\n"),
                                    bg = set_tkLabel()$ylabelBackground,
                                    fg = set_tkLabel()$ylabelForeground,
                                    borderwidth = set_tkLabel()$ylabelBorderwidth,
                                    relief = set_tkLabel()$ylabelRelief)
    )
    tcltk::tkgrid(ylab, row = start.ypos, column = 0,
                  rowspan = row.span, columnspan = 1,
                  sticky="nesw")
  }

  # FacetGrid; subtitle by row?
  if(!is.null(display_info$rowSubtitles) && plotInfo$FacetGrid) {

    uniqueRowSubtitles <- unique(display_info$rowSubtitles)

    for(i in seq(length(uniqueRowSubtitles))) {
      rowSub <- as.character(
        tcltk::tcl('label',
                   as.character(
                     loon::l_subwin(parent,
                                    paste0('rowlabel-',
                                           'facet:grid-',
                                           'byCOLS:', plotInfo$byCOLS, '-',
                                           'byROWS:', plotInfo$byROWS, '-',
                                           'x', i, '-',
                                           'drop:', ggObj$facet$params$drop, '-',
                                           'scales:', facet_scales(ggObj$facet$params$free)))
                   ),
                   text = paste(paste0(" ", strsplit(uniqueRowSubtitles[i], "")[[1]], " "),
                                collapse = "\n"),
                   bg = set_tkLabel()$labelBackground,
                   fg = set_tkLabel()$labelForeground,
                   borderwidth = set_tkLabel()$labelBorderwidth,
                   relief = set_tkLabel()$labelRelief)
      )
      tcltk::tkgrid(rowSub,
                    row = start.ypos + (i - 1) * span,
                    column = start.xpos + column.span,
                    rowspan = span, columnspan = 1,
                    sticky="nesw")
    }
  }
  # FacetGrid; subtitle by col?
  if(!is.null(display_info$colSubtitles) &&  plotInfo$FacetGrid) {
    uniqueColSubtitles <- unique(display_info$colSubtitles)
    for(i in seq(length(uniqueColSubtitles))) {

      colSub <- as.character(
        tcltk::tcl('label',
                   as.character(
                     loon::l_subwin(parent,
                                    paste0('columnlabel-',
                                           'facet:grid-',
                                           'byCOLS:', plotInfo$byCOLS, '-',
                                           'byROWS:', plotInfo$byROWS, '-',
                                           'y', i, '-',
                                           'drop:', ggObj$facet$params$drop, '-',
                                           'scales:', facet_scales(ggObj$facet$params$free)))
                   ),
                   text= uniqueColSubtitles[i],
                   bg = set_tkLabel()$labelBackground,
                   fg = set_tkLabel()$labelForeground,
                   borderwidth = set_tkLabel()$labelBorderwidth,
                   relief = set_tkLabel()$labelRelief)
      )
      tcltk::tkgrid(colSub,
                    row = start.ypos - 1,
                    column = start.xpos + (i - 1) * span,
                    rowspan = 1, columnspan = span,
                    sticky="nesw")
    }
  }

  if(!is.null(plotInfo$title)  && numOfPlots > 1) {
    titleFont <- if(display_info$start.subtitlepos == start.ypos)
      tcltk::tkfont.create(size = 16)
    else
      tcltk::tkfont.create(size = 16, weight="bold")
    tit <- as.character(tcltk::tcl('label',
                                   as.character(loon::l_subwin(parent,'title')),
                                   text= plotInfo$title,
                                   bg = set_tkLabel()$titleBackground,
                                   fg = set_tkLabel()$titleForeground,
                                   borderwidth = set_tkLabel()$titleBorderwidth,
                                   relief = set_tkLabel()$titleRelief))
    tcltk::tkconfigure(tit, font = titleFont)
    tcltk::tkgrid(tit, row = 0, column = title_pos(hjust = plotInfo$ggBuild$plot$theme$plot.title$hjust,
                                                   start.xpos = start.xpos,
                                                   column.span = column.span),
                  rowspan = 1, columnspan = column.span,
                  sticky="w")
  }

  tcltk::tkpack(parent, fill="both", expand=TRUE)
}

title_pos <- function(hjust, start.xpos, column.span) {

  hjust <- hjust %||% 0

  if(hjust < 0)
    hjust <- 0
  else if(hjust > 1)
    hjust <- 1
  else NULL

  start.xpos + floor(column.span * hjust)
}
