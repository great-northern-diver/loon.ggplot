pack_loon_plots <- function(envir = parent.frame()) {

  plots_info <- get("plots_info", envir = envir)
  ggObj <- get("ggObj", envir = envir)

  plots <- plots_info$plots
  display_info <- plots_info$display_info

  # get from environment
  xlabel <- get("xlabel", envir = envir)
  ylabel <- get("ylabel", envir = envir)
  tkLabels <- get("tkLabels", envir = envir)
  parent <- get("parent", envir = envir)
  span <- get("span", envir = envir)
  row.span <- get("row.span", envir = envir)
  column.span <- get("column.span", envir = envir)
  start.ypos <- get("start.ypos", envir = envir)
  start.xpos <- get("start.xpos", envir = envir)

  if(display_info$swapAxes) {
    label <- ylabel
    ylabel <- xlabel
    xlabel <- label
  }

  # synchronize binding
  synchronize(plots,
              scales_free_x = if(is.null(ggObj$facet$params$free$x)) FALSE else ggObj$facet$params$free$x,
              scales_free_y = if(is.null(ggObj$facet$params$free$y)) FALSE else ggObj$facet$params$free$y)

  # pack xlabel and ylabel
  if(!is.null(xlabel) & tkLabels){
    xlab <- as.character(tcltk::tcl('label', as.character(loon::l_subwin(parent,'label')),
                                    text= xlabel, background = "white"))
    tcltk::tkgrid(xlab, row = row.span + start.ypos, column = start.xpos,
                  rowspan = 1, columnspan = column.span,
                  sticky="nesw")
  }
  if(!is.null(ylabel) & tkLabels){
    ylab <- as.character(tcltk::tcl('label', as.character(loon::l_subwin(parent,'label')),
                                    text= paste(paste0(" ", strsplit(ylabel, "")[[1]], " "), collapse = "\n"),
                                    background = "white")
    )
    tcltk::tkgrid(ylab, row = start.ypos, column = 0,
                  rowspan = row.span, columnspan = 1,
                  sticky="nesw")
  }

  # is_facet_grid; subtitle by row?
  if(!is.null(display_info$rowSubtitles) &  get("is_facet_grid", envir = envir) & tkLabels) {
    uniqueRowSubtitles <- unique(display_info$rowSubtitles)
    for(i in 1:length(uniqueRowSubtitles)){
      rowSub <- as.character(tcltk::tcl('label', as.character(loon::l_subwin(parent,'label')),
                                        text= paste(paste0(" ", strsplit(uniqueRowSubtitles[i], "")[[1]], " "), collapse = "\n"),
                                        background = "grey90"))
      tcltk::tkgrid(rowSub, row = start.ypos + (i - 1)* span,
                    column = start.xpos + column.span,
                    rowspan = span, columnspan = 1,
                    sticky="nesw")
    }
  }
  # is_facet_grid; subtitle by col?
  if(!is.null(display_info$colSubtitles) &  get("is_facet_grid", envir = envir) & tkLabels) {
    uniqueColSubtitles <- unique(display_info$colSubtitles)
    for(i in 1:length(uniqueColSubtitles)){
      colSub <- as.character(tcltk::tcl('label', as.character(loon::l_subwin(parent,'label')),
                                        text= uniqueColSubtitles[i], background = "grey90"))
      tcltk::tkgrid(colSub, row = start.ypos - 1,
                    column = start.xpos + (i - 1) * span,
                    rowspan = 1, columnspan = span,
                    sticky="nesw")
    }
  }

  if(!is.null(get("title", envir = envir)) & tkLabels) {
    titleFont <- if(display_info$start.subtitlepos == start.ypos) tkfont.create(size = 16) else tkfont.create(size = 16, weight="bold")
    tit <- as.character(tcltk::tcl('label', as.character(loon::l_subwin(parent,'label')),
                                   text= get("title", envir = envir), background = "white"))
    tkconfigure(tit, font = titleFont)
    tcltk::tkgrid(tit, row = 0, column = start.xpos,
                  rowspan = 1, columnspan = column.span,
                  sticky="w")
  }
}
