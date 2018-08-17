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
  widget <- target
  len_widget <- length(widget)

  if (len_widget == 1) {
    # loon grobs
    lgrobs <- lapply(widget,
                     function(w) {
                       loonGrob(w)
                     }
    )

    layout_matrix <- matrix(1, nrow = 1, ncol = 1)
    list(
      grobs = lgrobs,
      layout_matrix = layout_matrix,
      name = "l_ggplot"
    )
  } else {
    # label
    xlabel <- paste0(unique(sapply(widget, function(w)w['xlabel'])), collapse = " ")
    ylabel <- paste0(unique(sapply(widget, function(w)w['ylabel'])), collapse = " ")
    title_subtitle <- unique(sapply(widget, function(w)w['title']))
    if(all(str_detect(title_subtitle, "[%+%]"))) {
      title <- unique(sapply(strsplit(title_subtitle, split = "%+%", fixed = TRUE), function(char) char[1]))
      subtitle <- sapply(strsplit(title_subtitle, split = "%+%", fixed = TRUE), function(char) char[-1])
    } else {
      title <- NULL
      subtitle <- title_subtitle
    }

    names <- names(widget)
    namesSplit <- strsplit(names, split = "")
    ggLayout <- as.data.frame(
      t(sapply(namesSplit,
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

    if(length(subtitle) > 1) {
      # loon grobs
      lgrobs <- do.call(gList,
                        lapply(1:numofROW,
                               function(i){
                                 rowi_columnIds <- which(ggLayout$ROW == i)
                                 if(length(rowi_columnIds) > 0){
                                   # loonGrobs
                                   # columns <- rep(NA, numofCOL)
                                   # columns[ggLayout$COL[rowi_columnIds]] <- rowi_columnIds
                                   # lgrob <- lapply(columns,
                                   #                 function(column){
                                   #                   if(is.na(column)) {
                                   #                     grob()
                                   #                   } else {
                                   #                     loonGrob(widget[[column]])
                                   #                   }
                                   #                 }
                                   # )
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
                                   # title
                                   tt <- ttheme_default(base_size = 8)
                                   # subt <- rep(NA, numofCOL)
                                   # subt[ggLayout$COL[rowi_columnIds]] <- subtitle[rowi_columnIds]

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
        ggLayouti <- ggLayout[i, ]
        # set layout matrix
        layout_matrix[ggLayouti[1], ggLayouti[2]] <- i
      }
    }
    list(
      grobs = lgrobs,
      layout_matrix = layout_matrix,
      left = if (ylabel == "") NULL else ylabel,
      bottom = if (xlabel == "") NULL else xlabel,
      top = grid::textGrob(title, x = 0, hjust = 0),
      name = "l_ggplot"
    )
  }
}
