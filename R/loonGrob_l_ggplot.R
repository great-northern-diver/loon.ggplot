#' @title loonGrob layoutType
#'
#' @description a layout type, to be used in \code{\link{loonGrob.l_compound}}
#' @param target `l_ggplot` loon target.
#'
#' @return "arrangeGrobArgs"
#'
#'
#' @seealso \code{\link{l_get_arrangeGrobArgs.l_ggplot}}
#'
#' @examples
#' p <- ggplot(mtcars, aes(mpg, wt, colour = cyl)) + geom_point()+ facet_wrap(~gear)
#' g <- loon.ggplot(p)
#' library(grid)
#' grid.newpage(); grid.draw(loonGrob(g))

#' @export
loonGrob_layoutType.l_ggplot <- function(target) {
  "arrangeGrobArgs"
}



#' @title get arrangeGrob arguments
#'
#' @description to get arrangeGrob arguments and used in \code{\link{loonGrob.l_compound}}
#' @param target `l_ggplot` loon target.
#'
#' @return arrangeGrob arguments, see \code{\link{arrangeGrob}}
#'
#' @import grid
#' @importFrom gridExtra arrangeGrob
#'
#' @seealso \code{\link{loonGrob_layoutType.l_ggplot}}
#'
#' @examples
#' p <- ggplot(mtcars, aes(mpg, wt, colour = cyl)) + geom_point()+ facet_wrap(~gear)
#' g <- loon.ggplot(p)
#' library(grid)
#' grid.newpage(); grid.draw(loonGrob(g))

#' @export
l_get_arrangeGrobArgs.l_ggplot <- function(target){
  widget <- target$plots
  len_widget <- length(widget)

  # label
  xlabel <- paste0(unique(sapply(widget, function(w)w['xlabel'])), collapse = " ")
  xlabel <- if (xlabel == "") NULL else xlabel
  ylabel <- paste0(unique(sapply(widget, function(w)w['ylabel'])), collapse = " ")
  ylabel <- if (ylabel == "") NULL else ylabel


  # is_facet_wrap or is_facet_grid
  facet <- target$facet
  is_facet_wrap <- facet$is_facet_wrap
  is_facet_grid <- facet$is_facet_grid

  # title column subtitle or row subtitle
  titles <- target$titles
  title <- titles$title
  colSubtitles <- titles$colSubtitles
  rowSubtitles <- titles$rowSubtitles

  ggLayout <- as.data.frame(
    t(sapply(strsplit(names(widget), split = ""),
             function(i){
               xpos <- which(i %in% "x" == TRUE)
               ypos <- which(i %in% "y" == TRUE)
               len_str <- length(i)
               c(as.numeric(paste0(i[(xpos + 1) : (ypos - 1)], collapse = "")),
                 as.numeric(paste0(i[(ypos + 1) : (len_str)], collapse = "")))
             })
    )
  )
  colnames(ggLayout) <- c("ROW", "COL")
  layoutDim <- apply(ggLayout, 2, max)
  numofROW <- layoutDim[1]
  numofCOL <- layoutDim[2]

  tt <- gridExtra::ttheme_default(base_size = 8)
  if(is_facet_wrap & length(colSubtitles) + length(rowSubtitles) > 0) {
    subtitle <- c(colSubtitles, rowSubtitles)
    # loon grobs
    lgrobs <- do.call(gList,
                      lapply(1:numofROW,
                             function(i){
                               rowi_columnIds <- which(ggLayout$ROW == i)
                               if(length(rowi_columnIds) > 0){
                                 lgrob <- lapply(rowi_columnIds,
                                                 function(rowi_columnId){
                                                   loonGrob(widget[[rowi_columnId]])
                                                 }
                                 )

                                 aGrob <- arrangeGrob(grobs = lgrob,
                                                      nrow = 1,
                                                      # ncol = numofCOL,
                                                      ncol = length(rowi_columnIds),
                                                      name = paste0("row", i))

                                 tG <- tableGrob(matrix(subtitle[rowi_columnIds],
                                                        ncol = length(rowi_columnIds)),
                                                 theme = tt)
                                 rbind(tG, aGrob, size = "last")
                               } else NULL
                             }
                      )
    )
    # layout matrix
    layout_matrix <- matrix(rep(1:numofROW, each = numofCOL), nrow = numofROW, byrow = TRUE)
    # last row
    lastRowFacets <- length(which(ggLayout$ROW == numofROW))
    if(lastRowFacets != numofCOL) {
      layout_matrix[numofROW, ] <- c(rep(numofROW, lastRowFacets) , rep(NA, (numofCOL - lastRowFacets)))
    }

  } else if(is_facet_grid & length(colSubtitles) + length(rowSubtitles) > 0) {

    if(facet$byCOLS & !facet$byROWS) {
      # loon grobs
      aGrob <- arrangeGrob(
        grobs = lapply(widget,
                       function(w) {
                         loonGrob(w)
                       }
        ),
        nrow = 1,
        ncol = length(colSubtitles)
      )
      tG <- tableGrob(matrix(colSubtitles, ncol = length(colSubtitles)), theme = tt)

      lgrobs <- gList(
        rbind(tG, aGrob, size = "last")
      )

    } else if(!facet$byCOLS & facet$byROWS) {
      # loon grobs
      aGrob <- arrangeGrob(
        grobs = lapply(widget,
                       function(w) {
                         loonGrob(w)
                       }
        ),
        nrow = length(rowSubtitles),
        ncol = 1
      )
      tG <- tableGrob(matrix(rowSubtitles, nrow = length(rowSubtitles)), theme = tt)

      lgrobs <- gList(
        cbind(aGrob, tG, size = "first")
      )

    } else if(facet$byCOLS & facet$byROWS) {
      uniqueColSubtitles <- unique(colSubtitles)
      uniqueRowSubtitles <- unique(rowSubtitles)
      aGrob <- arrangeGrob(
        grobs = lapply(widget,
                       function(w) {
                         loonGrob(w)
                       }
        ),
        nrow = length(uniqueRowSubtitles),
        ncol = length(uniqueColSubtitles)
      )

      tG_row <- tableGrob(matrix(uniqueRowSubtitles, nrow = length(uniqueRowSubtitles)), theme = tt)
      lgrobs_row <- cbind(aGrob, tG_row, size = "first")
      tG_col <- tableGrob(matrix(c(uniqueColSubtitles, ""), ncol = length(uniqueColSubtitles) + 1), theme = tt)

      lgrobs <- gList(
        rbind(tG_col, lgrobs_row, size = "last")
      )

    } else lgrobs <- grob()

    layout_matrix <- matrix(1, nrow = 1, ncol = 1)

  } else {
    # loon grobs
    lgrobs <- lapply(widget,
                     function(w) {
                       loonGrob(w)
                     }
    )

    # layout matrix
    layout_matrix <- matrix(rep(NA, numofROW * numofCOL), nrow = numofROW)
    for(i in 1:len_widget) {
      ggLayouti <- unlist(ggLayout[i, ])
      # set layout matrix
      layout_matrix[ggLayouti[1], ggLayouti[2]] <- i
    }
    ylabel <- if(length(widget) == 1) {
      if(widget$x1y1['showLabels']) NULL else ylabel
    } else NULL
    xlabel <- if(length(widget) == 1) {
      if(widget$x1y1['showLabels']) NULL else xlabel
    } else NULL
  }

  list(
    grobs = lgrobs,
    layout_matrix = layout_matrix,
    left = ylabel,
    bottom = xlabel,
    top = grid::textGrob(title, x = 0, hjust = 0),
    name = "l_ggplot"
  )
}
