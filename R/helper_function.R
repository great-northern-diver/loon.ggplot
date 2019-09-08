# 1. l_layer_lines does not work well on "dash" (l_layer_line is fine) *
# 2. arrow for every l_layer_line and l_layer_lines need to be added *
# 3. legend
# 4. transparent color (maybe impossible to do) *
# 5. specfic TODOs (before some loonLayer functions)
# 6. ggplot_build: need to rebuild for some specific data (eg: ts data) *
# 7. geom_histogram: transform to l_hist() or just leave it as l_plot() adding l_layer_rectangles() *
# 8. bar labels *

########################################### helper function ###########################################
hex6to12 <- function(col){
  if(is.null(col) ) {""} else{
    num <- length(col)
    sapply(1:num,
           function(i){
             if(is.na(col[i]) | col[i] == "NA") ""
             else{
               # ARGB is 8 digits, with last two representing transparency.
               # We have to erase last two digits (TK color codes do not include transparency information)
               splitCol <- unlist(strsplit(col[i], split = ""))
               if("#" %in% splitCol & length(splitCol) > 7 ) loon::l_hexcolor(paste(splitCol[1:7], collapse = ""))
               else if("#" %in% splitCol & length(splitCol) < 7) ""
               else loon::l_hexcolor(col[i])
             }
           }
    )
  }
}

pch_to_glyph <- function(pch, alpha) {
  len <- length(pch)

  switchPch <- function(pch){
    switch(
      as.character(pch),
      "16" = "circle" ,
      "1" = "ocircle",
      "21" = "ccircle",
      "15" = "square",
      "0" = "osquare",
      "22" = "csquare",
      "17" = "triangle",
      "2" = "otriangle",
      "24" = "ctriangle",
      "18" = "diamond",
      "5" = "odiamond",
      "23" = "cdiamond",
      {
        # warning("pch type ", glyph, " will be mapped to circle")
        "circle"
      }
    )
  }

  vapply(1:len,
         function(i) {
           if(is.na(alpha[i])){
             switchPch(pch[i])
           } else {
             if(alpha[i] < 0.5 ){
               switch(
                 as.character( pch[i] ),
                 "16" = "ocircle" ,
                 "1" = "ocircle",
                 "21" = "ocircle",
                 "15" = "osquare",
                 "0" = "osquare",
                 "22" = "osquare",
                 "17" = "otriangle",
                 "2" = "otriangle",
                 "24" = "otriangle",
                 "18" = "odiamond",
                 "5" = "odiamond",
                 "23" = "odiamond",
                 {
                   # warning("pch type ", glyph, " will be mapped to circle")
                   "ocircle"
                 }
               )
             } else {
               switchPch(pch[i])
             }
           }
         }, character(1))
}



as_loon_size <- function(s, type) {
  if(is.null(s) ) 1 else
    switch(type,
           "points" = ceiling( s^2 / 1.5),
           "lines" = 2 * s,
           "texts" = ceiling(s^2 / 1.5),
           {
             # unspecified type
             ""
           }
    )
}

as_loon_hvjust <- function(hjust, vjust) {
  if(length(hjust) != length(vjust) ) NULL
  else {
    len <- length(hjust)
    sapply(1:len,
           function(i){
             if(hjust[i] == 0.5 & vjust[i] == 0.5) "center"
             else if(hjust[i] == 0.5 & vjust[i] > 0.5) "n"
             else if(hjust[i] > 0.5 & vjust[i] > 0.5) "ne"
             else if(hjust[i] > 0.5 & vjust[i] == 0.5) "e"
             else if(hjust[i] > 0.5 & vjust[i] < 0.5) "se"
             else if(hjust[i] == 0.5 & vjust[i] < 0.5) "s"
             else if(hjust[i] < 0.5 & vjust[i] < 0.5) "sw"
             else if(hjust[i] < 0.5 & vjust[i] == 0.5) "w"
             else if(hjust[i] < 0.5 & vjust[i] > 0.5) "nw"
           })
  }
}

as_loon_dash <- function(linetype){

  lapply(linetype,
         function(l){
           if(l == 1 | is.na(l) ) ""
           else if(l == 2) rep(1, 4)
           else if(l == 3) rep(1,2)
           else rep("", length(l))
         })

}

is.CoordPolar <- function(ggplotPanel_params){
  isyRange <- is.null(ggplotPanel_params$y.range)
  isxRange <- is.null(ggplotPanel_params$x.range)
  isthetaRange <- is.null(ggplotPanel_params$theta.range)
  isrRange <- is.null(ggplotPanel_params$r.range)
  isyRange & isxRange & !isthetaRange & !isrRange
}

abline2xy <- function(xrange, yrange, slope, intercept){
  x <- xrange
  if(slope > 0) {
    y <- c(intercept + slope * x[1], intercept + slope * x[2])
    if(y[1] < yrange[1] ) {
      x[1] <- (yrange[1] - intercept)/ slope
      y[1] <- yrange[1]
    }
    if(y[2] > yrange[2]){
      x[2] <- (yrange[2] - intercept)/ slope
      y[2] <- yrange[2]
    }
  } else {
    y <- c(intercept + slope * x[2], intercept + slope * x[1])
    if(y[1] < yrange[1] ) {
      x[2] <- (yrange[1] - intercept)/ slope
      y[1] <- yrange[1]
      y <- rev(y)
    }
    if(y[2] > yrange[2]){
      x[1] <- (yrange[2] - intercept)/ slope
      y[2] <- yrange[2]
      y <- rev(y)
    }
  }
  list(x = x, y = y)
}

loonDefaultLinkingKey <- function(data) {
  paste0(seq(0, length(row.names(data)) - 1))
}

loonLinkingKey <- function(data, args) {
  if (is.data.frame(data) & !"waiver" %in% class(data)) {
    # check linkingKey
    if (is.null(args[['linkingKey']])) {
      # default linkingKey
      loonDefaultLinkingKey(data)
    } else {
      args[['linkingKey']]
    }
  } else {
    if (is.null(args[['linkingKey']])) {
      NULL
    } else {
      args[['linkingKey']]
    }
  }
}

loonItemLabel <- function(data, args) {
  if (is.data.frame(data) & !"waiver" %in% class(data)) {
    # check itemLabel
    if (is.null(args[['itemLabel']])) {
      # default itemLabel
      row.names(data)
    } else {
      args[['itemLabel']]
    }
  } else NULL
}

wrap_num <- function(ggLayout, is_facet_wrap, is_facet_grid, tkLabels){
  # dim2layout_matrix <- dim(layout_matrix)[2]
  # dim2layout_matrix - 5
  if(is_facet_wrap | !tkLabels) {
    length(names(ggLayout$facet_params$facets))
  } else if(is_facet_grid) {
    length(names(ggLayout$facet_params$rows)) + length(names(ggLayout$facet_params$cols))
  } else 0
}

get_importantLayers <- function(len_layers, ggplotObject){
  layerNames <- lapply(seq_len(len_layers),
                       function(j) {
                         className <- class(ggplotObject$layers[[j]]$geom)
                         className[-which(className %in% c("ggproto"  ,"gg" ,"Geom"))]
                       })

  # take the point layer as l_plot
  pointLayers <- which(sapply(layerNames,
                              function(layerName){
                                "GeomPoint" %in% layerName
                              }) == TRUE
  )

  # take the histogram layer as l_hist
  histogramLayers <- which(sapply(seq_len(length(layerNames)),
                                  function(j){
                                    # it could be bar plot
                                    is.histogram_condition1 <- all(c("GeomBar", "GeomRect") %in% layerNames[[j]])
                                    # stat class of geom_bar is StatCount
                                    is.histogram_condition2 <- "StatBin" %in% class(ggplotObject$layers[[j]]$stat)
                                    is.histogram_condition1 & is.histogram_condition2
                                  }) == TRUE
  )

  # boxlayer
  boxplotLayers <- which(sapply(layerNames,
                                function(layerName){
                                  "GeomBoxplot" %in% layerName
                                }) == TRUE
  )

  # curvelayer
  curveLayers <- which(sapply(layerNames,
                              function(layerName){
                                "GeomCurve" %in% layerName
                              }) == TRUE
  )
  list(pointLayers = pointLayers,
       histogramLayers = histogramLayers,
       boxplotLayers = boxplotLayers,
       curveLayers = curveLayers)
}

get_activeInfo <- function(importantLayers, activeGeomLayers, len_layers){

  pointLayers <- importantLayers$pointLayers
  histogramLayers <- importantLayers$histogramLayers
  boxplotLayers <- importantLayers$boxplotLayers

  point_hist_layers <- c(pointLayers, histogramLayers)
  if (length(activeGeomLayers) == 0) {
    if(length(point_hist_layers) != 0) {
      activeGeomLayers <- min(point_hist_layers)
      activeModel <- if(activeGeomLayers %in% pointLayers) "l_plot" else "l_hist"
    } else {
      activeModel <- "l_plot"
    }
  } else {
    if(max(activeGeomLayers) > len_layers)
      stop("the activeGeomLayers is out of bound")
    canBeActive <- activeGeomLayers %in% c(point_hist_layers, boxplotLayers)
    if(all(canBeActive)) {
      if(all(activeGeomLayers %in% pointLayers)) { # all point layers?
        activeModel <- "l_plot"
      } else if(all(activeGeomLayers %in% histogramLayers)) { # all histogram layers?
        activeModel <- "l_hist"
        if (length(activeGeomLayers) > 1) {
          # multiple histograms?
          message("only one histogram layer is drawn as active (model) layer\n",
                  "the rest will be added as l_layer_rectangles")
          activeGeomLayers <- activeGeomLayers[1]
        }
      } else if(any(activeGeomLayers %in% pointLayers) & any(activeGeomLayers %in% histogramLayers)) {
        stop("histogram layer and point layer cannot be active at the same time")
      } else if(any(activeGeomLayers %in% boxplotLayers) & any(activeGeomLayers %in% histogramLayers)){
        stop("histogram layer and boxplot layer cannot be active at the same time")
      } else {
        # boxplot Layer?
        activeGeomLayers <- integer(0)
        activeModel <- "l_plot"
      }
    } else {
      stop("This layer cannot be active")
    }
  }
  list(activeModel = activeModel,
       activeGeomLayers = activeGeomLayers)
}

get_subtitle <- function(layoutByROWS, layoutByCOLS, layout_matrix, ggLayout, numOfSubtitles,
                         byROWS, byCOLS ,panelNum, is_facet_wrap, is_facet_grid, tkLabels){
  if(is_facet_wrap | !tkLabels) {
    colSubtitle <- if (numOfSubtitles > 0) {
      paste(
        sapply(
          layout_matrix[panelNum, names(ggLayout$facet_params$facets)],
          as.character
        ),
        collapse = "\n"
      )
    } else NULL
    rowSubtitle <- NULL
  } else if(is_facet_grid) {
    if(byROWS & !byCOLS) {
      rowSubtitle <- paste(sapply(layout_matrix[panelNum, layoutByROWS], as.character), collapse = "\n")
      colSubtitle <- NULL
    } else if(!byROWS & byCOLS){
      rowSubtitle <- NULL
      colSubtitle <- paste(sapply(layout_matrix[panelNum, layoutByCOLS], as.character), collapse = "\n")
    } else if(byROWS & byCOLS){
      rowSubtitle <- paste(sapply(layout_matrix[panelNum, layoutByROWS], as.character), collapse = "\n")
      colSubtitle <- paste(sapply(layout_matrix[panelNum, layoutByCOLS], as.character), collapse = "\n")
    } else {
      rowSubtitle <- NULL
      colSubtitle <- NULL
    }
  } else {
    rowSubtitle <- NULL
    colSubtitle <- NULL
  }
  list(colSubtitle = colSubtitle,
       rowSubtitle = rowSubtitle)
}
