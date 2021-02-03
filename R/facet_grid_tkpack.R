facet_grid_tkpack <- function(plotId, ggLayout, showStrips, lplots, numofROW, numofCOL, byrow,
                              start.ypos, start.xpos, rowspan, columnspan, span, rownames, colnames,
                              i, j, ncol, nrow, parent, layout) {
  rownames <- unique(
    as.character(
      ggLayout[[plotId]]$layout[, names(ggLayout[[plotId]]$facet_params$rows)]
    )
  )
  # column subtitle names
  colnames <- unique(
    as.character(
      ggLayout[[plotId]]$layout[, names(ggLayout[[plotId]]$facet_params$cols)]
    )
  )

  if(is.null(showStrips)) {
    # pack plots
    lapply(1:length(lplots),
           function(k){
             lplot <- lplots[[k]]
             theRow <- layout$ROW[k] - 1
             theCOL <- layout$COL[k] - 1
             row.start <- if(byrow) {
               if(theRow == 0 & i == 1) {
                 (i - 1) *  rowspan + start.ypos + theRow * span + 1
               } else {
                 (i - 1) *  rowspan + start.ypos + theRow * span
               }
             } else {
               if(theRow == 0 & j == 1) {
                 (j - 1) *  rowspan + start.ypos + theRow * span + 1
               } else {
                 (j - 1) *  rowspan + start.ypos + theRow * span
               }
             }
             col.start <- if(byrow) {
               (j - 1) * columnspan + start.xpos + theCOL * span
             } else {
               (i - 1) * columnspan + start.xpos + theCOL * span
             }
             row.span <- if(byrow){
               if(theRow == 0 & i == 1) span - 1 else span
             } else {
               if(theRow == 0 & j == 1) span - 1 else span
             }
             col.span <- if(byrow) {
               if(theCOL == max(layout$COL - 1, na.rm = TRUE) & j == ncol) span - 1 else span
             } else {
               if(theCOL == max(layout$COL - 1, na.rm = TRUE) & j == ncol) span - 1 else span
             }
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

    if(byrow) {
      # pack column names
      if(i == 1) {
        lapply(1:length(colnames),
               function(k) {
                 colname <- as.character(tcltk::tcl('label',
                                                    as.character(loon::l_subwin(parent,'label')),
                                                    text= colnames[k],
                                                    bg = set_tkLabel()$labelBackground,
                                                    fg = set_tkLabel()$labelForeground,
                                                    borderwidth = set_tkLabel()$labelBorderwidth,
                                                    relief = set_tkLabel()$labelRelief)
                 )
                 tcltk::tkgrid(colname,
                               row = (i - 1) * rowspan + start.ypos,
                               column= (j - 1) * columnspan + start.xpos + (k - 1) * span,
                               rowspan = 1,
                               columnspan = if(k == length(colnames) & j == ncol) span - 1 else span,
                               sticky="nesw"
                 )
               }
        )
      } else NULL # no need to pack labels
      # pack row names
      if(j == ncol) {
        lapply(1:length(rownames),
               function(k) {
                 rowname <- as.character(
                   tcltk::tcl('label', as.character(loon::l_subwin(parent,'label')),
                              text= paste(paste0(" ", strsplit(rownames[k], "")[[1]], " "),
                                          collapse = "\n"),
                              bg = set_tkLabel()$labelBackground,
                              fg = set_tkLabel()$labelForeground,
                              borderwidth = set_tkLabel()$labelBorderwidth,
                              relief = set_tkLabel()$labelRelief)
                 )

                 tcltk::tkgrid(rowname,
                               row = (i - 1) * rowspan + start.ypos + (k - 1) * span + if(k == 1 & i == 1) 1 else 0,
                               column= j * columnspan + start.xpos - 1,
                               rowspan = if(k == 1 & i == 1) span - 1 else span,
                               columnspan = 1,
                               sticky="nesw"
                 )
               }
        )
      } else NULL

    } else {
      # pack column names
      if(j == 1) {
        lapply(1:length(colnames),
               function(k) {
                 colname <- as.character(tcltk::tcl('label',
                                                    as.character(loon::l_subwin(parent,'label')),
                                                    text= colnames[k],
                                                    bg = set_tkLabel()$labelBackground,
                                                    fg = set_tkLabel()$labelForeground,
                                                    borderwidth = set_tkLabel()$labelBorderwidth,
                                                    relief = set_tkLabel()$labelRelief))
                 tcltk::tkgrid(colname,
                               row = (j - 1) * rowspan + start.ypos,
                               column= (i - 1) * columnspan + start.xpos + (k - 1) * span,
                               rowspan = 1,
                               columnspan = if(k == length(colnames) & i == ncol) span - 1 else span,
                               sticky="nesw"
                 )
               }
        )
      } else NULL # no need to pack labels
      # pack row names
      if(i == ncol) {
        lapply(1:length(rownames),
               function(k) {
                 rowname <- as.character(
                   tcltk::tcl('label', as.character(loon::l_subwin(parent,'label')),
                              text= paste(paste0(" ", strsplit(rownames[k], "")[[1]], " "), collapse = "\n"),
                              bg = set_tkLabel()$labelBackground,
                              fg = set_tkLabel()$labelForeground,
                              borderwidth = set_tkLabel()$labelBorderwidth,
                              relief = set_tkLabel()$labelRelief)
                 )

                 tcltk::tkgrid(rowname,
                               row = (j - 1) * rowspan + start.ypos + (k - 1) * span + if(k == 1 & j == 1) 1 else 0,
                               column= i * columnspan + start.xpos - 1,
                               rowspan = if(k == 1 & j == 1) span - 1 else span,
                               columnspan = 1,
                               sticky="nesw"
                 )
               }
        )
      } else NULL
    }
  } else {
    if(showStrips) {
      # pack plots
      lapply(1:length(lplots),
             function(k){
               lplot <- lplots[[k]]
               theRow <- layout$ROW[k] - 1
               theCOL <- layout$COL[k] - 1
               row.start <- if(byrow) {
                 if(theRow == 0) {
                   (i - 1) *  rowspan + start.ypos + theRow * span + 1
                 } else {
                   (i - 1) *  rowspan + start.ypos + theRow * span
                 }
               } else {
                 if(theRow == 0) {
                   (j - 1) *  rowspan + start.ypos + theRow * span + 1
                 } else {
                   (j - 1) *  rowspan + start.ypos + theRow * span
                 }
               }
               col.start <- if(byrow) {
                 (j - 1) * columnspan + start.xpos + theCOL * span
               } else {
                 (i - 1) * columnspan + start.xpos + theCOL * span
               }
               row.span <- if(theRow == 0) span - 1 else span
               col.span <- if(theCOL == max(layout$COL - 1, na.rm = TRUE)) span - 1 else span
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
               tcltk::tkgrid(colname,
                             row = if(byrow) {
                               (i - 1) * rowspan + start.ypos
                             } else {
                               (j - 1) * rowspan + start.ypos
                             },
                             column= if(byrow) {
                               (j - 1) * columnspan + start.xpos + (k - 1) * span
                             } else {
                               (i - 1) * columnspan + start.xpos + (k - 1) * span
                             },
                             rowspan = 1,
                             columnspan = if(k == length(colnames)) span - 1 else span,
                             sticky="nesw"
               )
             }
      )
      # pack row names
      lapply(1:length(rownames),
             function(k) {
               rowname <- as.character(
                 tcltk::tcl('label',
                            as.character(loon::l_subwin(parent,'label')),
                            text= paste(paste0(" ", strsplit(rownames[k], "")[[1]], " "), collapse = "\n"),
                            bg = set_tkLabel()$labelBackground,
                            fg = set_tkLabel()$labelForeground,
                            borderwidth = set_tkLabel()$labelBorderwidth,
                            relief = set_tkLabel()$labelRelief)
               )

               tcltk::tkgrid(rowname,
                             row = if(byrow) {
                               (i - 1) * rowspan + start.ypos + (k - 1) * span + if(k == 1) 1 else 0
                             } else {
                               (j - 1) * rowspan + start.ypos + (k - 1) * span + if(k == 1) 1 else 0
                             },
                             column= if(byrow) {
                               j * columnspan + start.xpos - 1
                             } else {
                               i * columnspan + start.xpos - 1
                             },
                             rowspan = if(k == 1) span - 1 else span,
                             columnspan = 1,
                             sticky="nesw"
               )
             }
      )
    } else {
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
               tcltk::tkgrid(lplot,
                             row = row.start,
                             column= col.start,
                             rowspan = span,
                             columnspan = span,
                             sticky="nesw")
               # tk column row configure
               for (ii in col.start:(col.start + span - 1)) {
                 tcltk::tkgrid.columnconfigure(parent, ii, weight=1)
               }
               for (ii in row.start:(row.start + span - 1)) {
                 tcltk::tkgrid.rowconfigure(parent, ii, weight=1)
               }
             }
      )
    }
  }
}
