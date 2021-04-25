#' @export
loonGrob_layoutType.l_facet_ggplot <- function(target) {
  "arrangeGrobArgs"
}

#' @export
l_get_arrangeGrobArgs.l_facet_ggplot <- function(target) {

  plots <- l_getPlots(target)
  len_plots <- length(plots)

  subtitles <- l_getSubtitles(target)
  xlabel <- subtitles$xlabel
  ylabel <- subtitles$ylabel
  title <- subtitles$title
  FacetWrap <- subtitles$FacetWrap
  FacetGrid <- subtitles$FacetGrid
  colSubtitles <- subtitles$colSubtitles
  rowSubtitles <- subtitles$rowSubtitles
  byCOLS <- subtitles$byCOLS
  byROWS <- subtitles$byROWS

  layout <- layout_coords(target)
  layoutDim <- apply(layout, 2, max)
  nrow <- layoutDim[1]
  ncol <- layoutDim[2]

  # ttheme_default will give the background with light grey and dark grey, one by one
  # tt <- gridExtra::ttheme_default(base_size = 8)
  # ttheme_minimal will give the same color background
  tt <- gridExtra::ttheme_minimal(
    base_size = 8,
    core=list(bg_params = list(fill = tryCatch(loon::l_getOption("facetLabelBackground"),
                                               error = function(e) "gray80"),
                               col = "white"))
  )

  if(FacetWrap && length(colSubtitles) + length(rowSubtitles) > 0) {

    subtitle <- c(colSubtitles, rowSubtitles)
    # loon grobs
    lgrobs <- do.call(gList,
                      lapply(seq(nrow),
                             function(i){
                               rowi_columnIds <- which(layout$row == i)
                               if(length(rowi_columnIds) > 0) {

                                 aGrob <- gridExtra::arrangeGrob(
                                   grobs = setNames(
                                     lapply(rowi_columnIds,
                                            function(rowi_columnId){
                                              g <- loon::loonGrob(plots[[rowi_columnId]])
                                              g$name <- as.character(plots[[rowi_columnId]])
                                              g
                                            }
                                     ),
                                     as.character(plots[rowi_columnIds])
                                   ),
                                   nrow = 1,
                                   # ncol = ncol,
                                   ncol = length(rowi_columnIds),
                                   name = paste(c("row", i, "arrangeGrob"), collapse = " ")
                                 )

                                 tG <- gridExtra::tableGrob(matrix(subtitle[rowi_columnIds],
                                                                   ncol = length(rowi_columnIds)),
                                                            theme = tt)
                                 rbind(tG, aGrob, size = "last")
                               } else grid::nullGrob(name = "null: no grob")
                             }
                      )
    )
    # layout matrix
    layout_matrix <- matrix(rep(1:nrow, each = ncol), nrow = nrow, byrow = TRUE)
    # last row
    lastRowFacets <- length(which(layout$row == nrow))
    if(lastRowFacets != ncol) {
      layout_matrix[nrow, ] <- c(rep(nrow, lastRowFacets) , rep(NA, (ncol - lastRowFacets)))
    }

  } else if(FacetGrid && length(colSubtitles) + length(rowSubtitles) > 0) {

    if(byCOLS && !byROWS) {
      # loon grobs
      aGrob <- gridExtra::arrangeGrob(
        grobs = setNames(
          lapply(plots,
                 function(plot) {
                   g <- loon::loonGrob(plot)
                   g$name <- as.character(plot)
                   g
                 }
          ),
          as.character(plots)
        ),
        nrow = 1,
        ncol = length(colSubtitles),
        name = paste(c("byColumn", "arrangeGrob"), collapse = " ")
      )
      tG <- gridExtra::tableGrob(matrix(colSubtitles, ncol = length(colSubtitles)),
                                 theme = tt)

      lgrobs <- gList(
        rbind(tG, aGrob, size = "last")
      )

    } else if(!byCOLS && byROWS) {
      # loon grobs
      aGrob <- gridExtra::arrangeGrob(
        grobs = setNames(
          lapply(plots,
                 function(plot) {
                   g <- loon::loonGrob(plot)
                   g$name <- as.character(plot)
                   g
                 }
          ),
          as.character(plots)
        ) ,
        nrow = length(rowSubtitles),
        ncol = 1,
        name = paste(c("byRow", "arrangeGrob"), collapse = " ")
      )
      tG <- gridExtra::tableGrob(matrix(rowSubtitles, nrow = length(rowSubtitles)),
                                 theme = tt)

      lgrobs <- gList(
        cbind(aGrob, tG, size = "first")
      )

    } else if(byCOLS && byROWS) {

      uniqueColSubtitles <- unique(colSubtitles)
      uniqueRowSubtitles <- unique(rowSubtitles)
      aGrob <- gridExtra::arrangeGrob(
        grobs = setNames(
          lapply(plots,
                 function(plot) {
                   g <- loon::loonGrob(plot)
                   g$name <- as.character(plot)
                   g
                 }
          ), as.character(plots)
        ),
        nrow = length(uniqueRowSubtitles),
        ncol = length(uniqueColSubtitles),
        name = paste(c("byRow", "byColumn", "arrangeGrob"), collapse = " ")
      )

      tG_row <- gridExtra::tableGrob(matrix(uniqueRowSubtitles,
                                            nrow = length(uniqueRowSubtitles)),
                                     theme = tt)
      lgrobs_row <- cbind(aGrob, tG_row, size = "first")

      tG_col <- gridExtra::tableGrob(
        matrix(c(uniqueColSubtitles, ""),
               ncol = length(uniqueColSubtitles) + 1),
        theme = tt)

      lgrobs <- gList(
        rbind(tG_col, lgrobs_row, size = "last")
      )

    } else lgrobs <- grid::nullGrob(name = "null grob")

    layout_matrix <- matrix(1, nrow = 1, ncol = 1)

  } else {

    ######### deprecated ###########
    # layout matrix
    layout_matrix <- matrix(rep(NA, nrow * ncol), nrow = nrow)
    for(i in 1:len_plots) {
      layout_i <- unlist(layout[i, ])
      # set layout matrix
      layout_matrix[layout_i[1], layout_i[2]] <- i
    }
    ylabel <- if(length(plots) == 1) {
      if(plots$x1y1['showLabels']) NULL else ylabel
    } else NULL
    xlabel <- if(length(plots) == 1) {
      if(plots$x1y1['showLabels']) NULL else xlabel
    } else NULL

    lgrobs <- setNames(
      lapply(plots,
             function(plot) {
               g <- loon::loonGrob(plot)
               g$name <- as.character(plot)
               g
             }
      ), as.character(plots)
    )
  }

  list(
    grobs = lgrobs,
    layout_matrix = layout_matrix,
    left = ylabel,
    bottom = xlabel,
    top = grid::textGrob(title, x = 0, hjust = 0),
    name = "l_facet_ggplot"
  )
}


#' @export
l_getPlots.l_facet_ggplot <- function(target) {
  plots  <- unclass(target)
  lapply(plots,
         function(plot)
           loon::l_throwErrorIfNotLoonWidget(plot)
  )
  plots
}

#' @export
l_getPlots.l_ggmatrix <- function(target) {
  l_getPlots.l_facet_ggplot(target)
}

#' @export
l_getLocations.l_facet_ggplot <-  function(target) {
  plots <- l_getPlots(target)
  layout <- layout_coords(target)
  layoutDim <- apply(layout, 2, max)
  nrow <- layoutDim[1]
  ncol <- layoutDim[2]

  # layout matrix
  layout_matrix <- matrix(rep(NA, nrow * ncol), nrow = nrow)
  for(i in 1:length(plots)) {
    layout_i <- unlist(layout[i, ])
    # set layout matrix
    layout_matrix[layout_i[1], layout_i[2]] <- i
  }
  list(
    layout_matrix = layout_matrix,
    nrow = nrow(layout_matrix),
    ncol = ncol(layout_matrix)
  )
}

#' @export
l_getLocations.l_ggmatrix <- function(target) {
  l_getLocations.l_facet_ggplot(target)
}

l_getSubtitles <- function(target) {

  # find the parent `tk` window name
  parent <- as.character(tkwinfo("parent",  target[[1L]]))
  # access all children
  children <- as.character(tkwinfo("child",  parent))

  xLabelPathName <- children[grepl("xlabel", children)]
  yLabelPathName <- children[grepl("ylabel", children)]
  titlePathName <- children[grepl("title", children)]

  # label
  xlabel <- if(length(xLabelPathName) > 0) {
    paste0(as.character(tkcget(xLabelPathName, "-text")), collapse = " ")
  } else {
    NULL
  }
  ylabel <- if(length(yLabelPathName) > 0) {
    paste0(as.character(tkcget(yLabelPathName, "-text")), collapse = "")
  } else {
    NULL
  }
  title <- if(length(titlePathName) > 0) {
    paste0(as.character(tkcget(titlePathName, "-text")), collapse = " ")
  } else {
    NULL
  }

  # FacetWrap or FacetGrid
  FacetWrap <- any(grepl("wrap", children))
  FacetGrid <- any(grepl("grid", children))

  # title column subtitle or row subtitle
  if(FacetWrap) {

    columnlabelPathName <- children[grepl("columnlabel", children)]
    colSubtitles <- if(length(columnlabelPathName) > 0L) {
      unname(
        sapply(columnlabelPathName,
               function(path) {
                 paste0(as.character(tkcget(path, "-text")), collapse = " ")
               })
      )
    } else NULL
    rowSubtitles <- NULL

    labelPathName1L <- c(columnlabelPathName)[1L]
    pathSplit <- strsplit(labelPathName1L, "-")[[1L]]
    byCOLSChar <- pathSplit[grepl("byCOLS", pathSplit)]
    byCOLS <- grepl("TRUE", byCOLSChar)
    byROWSChar <- pathSplit[grepl("byROWS", pathSplit)]
    byROWS <- grepl("TRUE", byROWSChar)

  } else if (FacetGrid) {

    columnlabelPathName <- children[grepl("columnlabel", children)]
    rowlabelPathName <- children[grepl("rowlabel", children)]

    colSubtitles <- if(length(columnlabelPathName) > 0) {
      unname(
        sapply(columnlabelPathName,
               function(path) {
                 paste0(as.character(tkcget(path, "-text")), collapse = " ")
               })
      )
    } else NULL

    rowSubtitles <- if(length(rowlabelPathName) > 0) {
      unname(
        sapply(rowlabelPathName,
               function(path) {
                 paste0(as.character(tkcget(path, "-text")), collapse = "")
               })
      )
    } else NULL

    labelPathName1L <- c(columnlabelPathName, rowlabelPathName)[1L]
    pathSplit <- strsplit(labelPathName1L, "-")[[1L]]
    byCOLSChar <- pathSplit[grepl("byCOLS", pathSplit)]
    byCOLS <- grepl("TRUE", byCOLSChar)
    byROWSChar <- pathSplit[grepl("byROWS", pathSplit)]
    byROWS <- grepl("TRUE", byROWSChar)

  } else {
    colSubtitles <- NULL
    rowSubtitles <- NULL
    byCOLS <- FALSE
    byROWS <- FALSE
  }

  list(
    xlabel = xlabel,
    ylabel = ylabel,
    title = title,
    FacetWrap = FacetWrap,
    FacetGrid = FacetGrid,
    colSubtitles = colSubtitles,
    rowSubtitles = rowSubtitles,
    byCOLS = byCOLS,
    byROWS = byROWS
  )
}
