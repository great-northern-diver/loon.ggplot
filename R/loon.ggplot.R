#' @title Create a loon plot from a ggplot2 object
#'
#' @description Interactive loon plots from ggplots
#'
#' @param ggplotObject a ggplot object
#' @param ... named arguments to modify loon plot states
#'
#' @return a loon widget
#'
#'
#' @import ggplot2 loon tcltk stats methods
#'
#' @export
#'
#' @examples
#'
#'  library(ggplot2)
#'  library(loon)
#'  p <- ggplot(mtcars, aes(mpg, wt, colour = cyl)) + geom_point()
#'  p
#'  g <- loon.ggplot(p)
#'
#'  df <- data.frame(x = 1:3, y = 1:3, colour = c(1,3,5))
#'  xgrid <- with(df, seq(min(x), max(x), length = 50))
#'  interp <- data.frame(
#'    x = xgrid,
#'    y = approx(df$x, df$y, xout = xgrid)$y,
#'    colour = approx(df$x, df$colour, xout = xgrid)$y)
#'
#'  p <- ggplot(interp, aes(x, y, colour = colour)) +
#'    geom_line(size = 2) +
#'    geom_point(data = df, size = 5)
#'  p
#'  g <- loon.ggplot(p)

loon.ggplot <- function(ggplotObject, ... ){

  # lables
  ggLabels <- list(
    title  = if( is.null(ggplotObject$labels$title) ) "" else ggplotObject$labels$title ,
    xlabel = if( is.null(ggplotObject$labels$x) ) ""  else ggplotObject$labels$x ,
    ylabel = if( is.null(ggplotObject$labels$y) ) ""  else ggplotObject$labels$y
  )

  # ggplot_build
  buildggplotObject <-  ggBuild2Loon(ggplotObject)

  # layout
  ggLayout <- buildggplotObject$layout$layout

  # panel_params
  ggplotPanel_params <- buildggplotObject$layout$panel_params

  # number of panels
  panelNum <- dim(ggLayout)[1]

  tt <- tktoplevel()

  # length layers
  len_layers <- length(ggplotObject$layers)

  # some default setting
  zoomX <- zoomY <- 5/6
  swapAxes <- FALSE
  showGuides <- TRUE
  showScales <- TRUE

  p <- lapply(seq_len(panelNum), function(i){

    # subtitle
    if(dim(ggLayout)[2] == 6){
      if(ggLabels$title == "") title <- as.character(ggLayout[i, 4]) else
        title <- paste(c(ggLabels$title, ":", as.character(ggLayout[i, 4]) ), collapse = "")
    }else title <- ggLabels$title

    # is polar coord?
    isCoordPolar <- is.CoordPolar(ggplotPanel_params[[i]])

    if(isCoordPolar){

      # theta can be NULL (if not coordPolar), "x" or "y"
      theta <- ggplotObject$coordinates$theta
      if(theta == "y")  swapAxes <- TRUE
      showGuides <- FALSE
      showScales <- FALSE
    } else {

      # swap or not
      if( which( names(ggplotPanel_params[[i]]) %in% "y.range"  == T ) <
          which( names(ggplotPanel_params[[i]]) %in% "x.range"  == T ) ) swapAxes <- TRUE

      # set panX, panY, deltaX, deltaY
      if(swapAxes) {
        panY <- ggplotPanel_params[[i]]$x.range[1]
        panX <- ggplotPanel_params[[i]]$y.range[1]
        deltaY <- diff(ggplotPanel_params[[i]]$x.range) * zoomX
        deltaX <- diff(ggplotPanel_params[[i]]$y.range) * zoomY
      } else {
        panX <- ggplotPanel_params[[i]]$x.range[1]
        panY <- ggplotPanel_params[[i]]$y.range[1]
        deltaX <- diff(ggplotPanel_params[[i]]$x.range) * zoomX
        deltaY <- diff(ggplotPanel_params[[i]]$y.range) * zoomY
      }

    }
    if (len_layers != 0) {
      layerNames <- sapply(1:len_layers, function(j) {
        className <- class(ggplotObject$layers[[j]]$geom)
        className[-which(className %in% c("ggproto"  ,"gg" ,"Geom"))]
      })
      # take the point layer as scatterplot ("model" in loon)
      pointsLayerId <- which(sapply(layerNames, function(l){"GeomPoint" %in% l}) == TRUE)

      if (length(pointsLayerId) != 0) {

        # combine points layers
        pointData <- lapply(pointsLayerId, function(l){
          Layerl <- buildggplotObject$data[[l]]
          data <- Layerl[Layerl$PANEL == i, ]
          x <- data$x
          y <- data$y
          label <- data$label
          color <- sapply(1:dim(data)[1], function(j){
            if(data$shape[j] %in% 21:24 ){
              hex6to12(data$fill[j])
            }else {
              hex6to12(data$colour[j])
            }
          } )
          glyph <- pch_to_glyph(data$shape, data$alpha)
          size <- as_loon_size( data$size , "points" )

          if (is.null(label)) {
            label <- paste0("item", seq_len(length(x)) - 1)
          }

          data.frame(x = x, y = y, label = label, color = color, glyph = glyph, size = size)
        })

        pointData <- do.call(rbind, pointData)

        if(isCoordPolar) {
          coordPolarxy <- cartesianxy2Polarxy(NULL, theta = theta,
                                              data = pointData,
                                              ggplotPanel_params = ggplotPanel_params[[i]])
          x <- coordPolarxy$x
          y <- coordPolarxy$y
        } else {
          x <- pointData$x
          y <- pointData$y
        }

        loonPlot <- l_plot(parent = tt, x = x, y = y, size = pointData$size,
                           title = title, color = as.character( pointData$color),
                           glyph = as.character( pointData$glyph),
                           xlabel = ggLabels$xlabel, ylabel = ggLabels$ylabel,
                           itemLabel = as.character(pointData$label),
                           showGuides = showGuides, showScales = showScales,
                           showLabels = TRUE, showItemLabels = TRUE, swapAxes = swapAxes, ...)

        loon_layers <- sapply(1:len_layers, function(j){
          if( ! j %in% pointsLayerId ){
            loonLayer(widget = loonPlot, layerGeom = ggplotObject$layers[[j]],
                      data =  buildggplotObject$data[[j]][buildggplotObject$data[[j]]$PANEL == i, ],
                      ggplotPanel_params = ggplotPanel_params[[i]], theta = theta
            )
          }
        })

        # recover the points layer to the original position
        if(length(pointsLayerId) != len_layers){

          otherLayerId <- (1:len_layers)[-pointsLayerId]
          minOtherLayerId <- min(otherLayerId)
          maxPointsLayerId <- max(pointsLayerId)

          if(maxPointsLayerId > minOtherLayerId){
            modelLayerup <- sapply( seq_len(length(which(otherLayerId < maxPointsLayerId) == T)), function(j){
              l_layer_raise(loonPlot, "model")
            })
          }
        }
      } else {
        loonPlot <- l_plot(parent = tt, title = title,
                           xlabel = ggLabels$xlabel, ylabel = ggLabels$ylabel,
                           showGuides = showGuides,
                           showScales = showScales,
                           showLabels = TRUE, swapAxes = swapAxes, ...)

        loon_layers <- sapply(1:len_layers, function(j){

          loonLayer(widget = loonPlot, layerGeom = ggplotObject$layers[[j]],
                    data =  buildggplotObject$data[[j]][buildggplotObject$data[[j]]$PANEL == i, ],
                    ggplotPanel_params = ggplotPanel_params[[i]], theta = theta)
        })
      }
    } else loonPlot <- l_plot(parent = tt, title = title,
                              xlabel = ggLabels$xlabel, ylabel = ggLabels$ylabel,
                              showGuides = showGuides,
                              showScales = showScales,
                              showLabels = TRUE, swapAxes = swapAxes, ...)


    tkgrid(loonPlot, row = ggLayout[i,]$ROW,
           column=ggLayout[i,]$COL, sticky="nesw")
    tkgrid.columnconfigure(tt, ggLayout[i,]$COL, weight=1)
    tkgrid.rowconfigure(tt, ggLayout[i,]$ROW, weight=1)

    # draw specific guides for coord polar
    if(isCoordPolar){
      polarGuides <- coordPolarGuides(loonPlot, ggplotPanel_params[[i]], theta)
      # lower to bottom
      children <- l_layer_getChildren(loonPlot)
      # the length of children is at least two
      sapply(1:(length(children) - 1), function(l){l_layer_lower(loonPlot, polarGuides)})
      l_scaleto_world(loonPlot)
    } else {
      l_configure(loonPlot, panX=panX, panY=panY, deltaX= deltaX,
                  deltaY=deltaY, zoomX = zoomX, zoomY = zoomY)
    }
    loonPlot
  })

  class(p) <- c("l_ggplot", "loon")
  p

}

# 1. l_layer_lines does not work well on "dash" (l_layer_line is fine)
# 2. arrow for every l_layer_line and l_layer_lines need to be added
# 3. legend
# 4. transparent color (maybe impossible to do) *
# 5. specfic TODOs (before some loonLayer functions)
# 6. ggplot_build: need to rebuild for some specific data (eg: ts data) *
# 7. geom_histogram: transform to l_hist() or just leave it as l_plot() adding l_layer_rectangles()
# 8. bar labels

########################################### helper function ###########################################
hex6to12 <- function(col){
  if(is.null(col) ) {""} else{
    num <- length(col)
    sapply(1:num, function(i){
      if(is.na(col[i]) | col[i] == "NA") ""
      else{
        # ARGB is 8 digits, with last two representing transparency.
        # We have to erase last two digits (TK color codes do not include transparency information)
        splitCol <- unlist(strsplit(col[i], split = ""))
        if("#" %in% splitCol & length(splitCol) > 7 ) l_hexcolor(paste(splitCol[1:7], collapse = ""))
        else if("#" %in% splitCol & length(splitCol) < 7) ""
        else l_hexcolor(col[i])
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

  vapply(1:len, function(i) {
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
           "points" = ceiling( s^2 / 2),
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
    sapply(1:len, function(i){
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

  lapply(linetype, function(l){
    if(l == 1 | is.na(l) ) ""
    else if(l == 2) rep(1, 4)
    else if(l == 3) rep(1,2)
    else rep(1, length(l))
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
