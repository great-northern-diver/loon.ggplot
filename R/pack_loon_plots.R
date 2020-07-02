pack_loon_plots <- function(plots_info = list(),
                            ggObj,
                            parent = NULL,
                            tkLabels = NULL) {

  plots <- plots_info$plots
  display_info <- plots_info$display_info

  # get from environment
  xlabel <- plots_info$xlabel
  ylabel <- plots_info$ylabel
  span <- plots_info$span
  row.span <- plots_info$row.span
  column.span <- plots_info$column.span
  start.ypos <- plots_info$start.ypos
  start.xpos <- plots_info$start.xpos

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
  if(!is.null(xlabel) & tkLabels){
    xlab <- as.character(tcltk::tcl('label',
                                    as.character(loon::l_subwin(parent,'label')),
                                    text= xlabel, background = "white"))
    tcltk::tkgrid(xlab, row = row.span + start.ypos, column = start.xpos,
                  rowspan = 1, columnspan = column.span,
                  sticky="nesw")
  }
  if(!is.null(ylabel) & tkLabels){
    ylab <- as.character(tcltk::tcl('label',
                                    as.character(loon::l_subwin(parent,'label')),
                                    text= paste(paste0(" ", strsplit(ylabel, "")[[1]], " "),
                                                collapse = "\n"),
                                    background = "white")
    )
    tcltk::tkgrid(ylab, row = start.ypos, column = 0,
                  rowspan = row.span, columnspan = 1,
                  sticky="nesw")
  }

  # is_facet_grid; subtitle by row?
  if(!is.null(display_info$rowSubtitles) &  plots_info$is_facet_grid & tkLabels) {
    uniqueRowSubtitles <- unique(display_info$rowSubtitles)
    for(i in 1:length(uniqueRowSubtitles)){
      rowSub <- as.character(tcltk::tcl('label',
                                        as.character(loon::l_subwin(parent,'label')),
                                        text= paste(paste0(" ", strsplit(uniqueRowSubtitles[i], "")[[1]], " "),
                                                    collapse = "\n"),
                                        background = "grey90"))
      tcltk::tkgrid(rowSub,
                    row = start.ypos + (i - 1)* span,
                    column = start.xpos + column.span,
                    rowspan = span, columnspan = 1,
                    sticky="nesw")
    }
  }
  # is_facet_grid; subtitle by col?
  if(!is.null(display_info$colSubtitles) &  plots_info$is_facet_grid & tkLabels) {
    uniqueColSubtitles <- unique(display_info$colSubtitles)
    for(i in 1:length(uniqueColSubtitles)){
      colSub <- as.character(tcltk::tcl('label', as.character(loon::l_subwin(parent,'label')),
                                        text= uniqueColSubtitles[i], background = "grey90"))
      tcltk::tkgrid(colSub,
                    row = start.ypos - 1,
                    column = start.xpos + (i - 1) * span,
                    rowspan = 1, columnspan = span,
                    sticky="nesw")
    }
  }

  if(!is.null(plots_info$title) & tkLabels) {
    titleFont <- if(display_info$start.subtitlepos == start.ypos)
      tcltk::tkfont.create(size = 16)
    else
      tcltk::tkfont.create(size = 16, weight="bold")
    tit <- as.character(tcltk::tcl('label',
                                   as.character(loon::l_subwin(parent,'label')),
                                   text= plots_info$title))
    tcltk::tkconfigure(tit, font = titleFont)
    tcltk::tkgrid(tit, row = 0, column = title_pos(hjust = plots_info$ggBuild$plot$theme$plot.title$hjust,
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
