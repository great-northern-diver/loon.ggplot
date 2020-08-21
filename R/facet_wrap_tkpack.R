facet_wrap_tkpack <- function(plotId, ggLayout, showStrips, lplots, numofROW, numofCOL, byrow, start.ypos,
                              start.xpos, rowspan, columnspan, span, rownames, colnames, i, j, ncol, nrow,
                              parent, layout) {
  if(is.null(showStrips)) showStrips <- TRUE
  # column subtitle names
  numOfSubtitles <- 0
  colnames <- apply(ggLayout[[plotId]]$layout[, names(ggLayout[[plotId]]$facet_params$facets)],
                    1,
                    function(name) {
                      numOfSubtitles <<- length(name)
                      paste(name, collapse = "\n")
                    }
  )

  if(showStrips) {
    # pack plots
    lapply(1:length(lplots),
           function(k){
             lplot <- lplots[[k]]
             theRow <- layout$ROW[k] - 1
             theCOL <- layout$COL[k] - 1
             row.start <- if(byrow) {
               (i - 1) *  rowspan + start.ypos + theRow * span + numOfSubtitles
             } else {
               (j - 1) *  rowspan + start.ypos + theRow * span + numOfSubtitles
             }
             col.start <- if(byrow) {
               (j - 1) * columnspan + start.xpos + theCOL * span
             } else {
               (i - 1) * columnspan + start.xpos + theCOL * span
             }
             row.span <- span - numOfSubtitles
             col.span <- span
             tcltk::tkgrid(lplot,
                           row = row.start,
                           column= col.start,
                           rowspan = row.span,
                           columnspan = col.span,
                           sticky="nesw"
             )
             # tk column row configure
             for (ii in col.start:(col.start + col.span - 1)) {
               tcltk::tkgrid.columnconfigure(parent, ii, weight=1)
             }
             for (ii in row.start:(row.start + row.span - 1)) {
               tcltk::tkgrid.rowconfigure(parent, ii, weight=1)
             }
           }
    )
    # pack column names
    lapply(1:length(colnames),
           function(k) {
             colname <- as.character(tcltk::tcl('label',
                                                as.character(loon::l_subwin(parent,'label')),
                                                text= colnames[k],
                                                bg = set_tkLabel()$labelBackground,
                                                fg = set_tkLabel()$labelForeground,
                                                borderwidth = set_tkLabel()$labelBorderwidth,
                                                relief = set_tkLabel()$labelRelief))
             theRow <- layout$ROW[k] - 1
             theCOL <- layout$COL[k] - 1
             tcltk::tkgrid(colname,
                           row = if(byrow) {
                             (i - 1) * rowspan + start.ypos + theRow * span
                           } else {
                             (j - 1) * rowspan + start.ypos + theRow * span
                           },
                           column= if(byrow) {
                             (j - 1) * columnspan + start.xpos + theCOL * span
                           } else {
                             (i - 1) * columnspan + start.xpos + theCOL * span
                           },
                           rowspan = numOfSubtitles,
                           columnspan = span,
                           sticky="nesw"
             )
           }
    )
  } else {
    # pack plots
    lapply(1:length(lplots),
           function(k){
             lplot <- lplots[[k]]
             theRow <- layout$ROW[k] - 1
             theCOL <- layout$COL[k] - 1
             row.start <- if(byrow) {
               (i - 1) *  rowspan + start.ypos + theRow * span
             } else {
               (j - 1) *  rowspan + start.ypos + theRow * span
             }
             col.start <- if(byrow) {
               (j - 1) * columnspan + start.xpos + theCOL * span
             } else {
               (i - 1) * columnspan + start.xpos + theCOL * span
             }
             row.span <- span
             col.span <- span
             tcltk::tkgrid(lplot,
                           row = row.start,
                           column= col.start,
                           rowspan = row.span,
                           columnspan = col.span,
                           sticky="nesw"
             )
             # tk column row configure
             for (ii in col.start:(col.start + col.span - 1)) {
               tcltk::tkgrid.columnconfigure(parent, ii, weight=1)
             }
             for (ii in row.start:(row.start + row.span - 1)) {
               tcltk::tkgrid.rowconfigure(parent, ii, weight=1)
             }
           }
    )
  }
}
