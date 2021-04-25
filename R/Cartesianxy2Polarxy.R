#' @title Transform the x, y positions from a Cartesian coordinate to a polar coordinate
#' @description Used in the `loonLayer` construction to access the x, y positions embedded in the
#' polar coordinate system.
#' @param layerGeom A \code{ggplot} layer object
#' @param coordinates A \code{ggplot} object coordinate system
#' @param data the data used for the transformation
#' @param ggplotPanelParams some non-data panel parameters,
#' i.e. the range of theta, the range of radius, theta major, theta minor, etc. It is obtained from the
#' \code{ggplot_build(p)$layout$panel_params} where "p" is a \code{ggplot} object
#' @param ... for further use
#' @export
#' @examples
#' p <- ggplot(mtcars, aes(wt, mpg)) +
#'        geom_point() +
#'        coord_polar()
#'
#' layerGeom <- p$layers[[1L]]$geom
#' coordinates <- p$coordinates
#' build <- ggplot_build(p)
#' data <- build$data[[1L]]
#' ggplotPanelParams <- build$layout$panel_params[[1L]]
#'
#' polarXY <- Cartesianxy2Polarxy(layerGeom, coordinates, data, ggplotPanelParams)
#' plot(polarXY$x, polarXY$y)

Cartesianxy2Polarxy <- function(layerGeom, coordinates, data, ggplotPanelParams, ...){
  UseMethod("Cartesianxy2Polarxy", layerGeom$geom)
}

#' @export
Cartesianxy2Polarxy.default <- function(layerGeom = NULL, coordinates, data, ggplotPanelParams, ...){
  x <- data$x
  y <- data$y
  theta <- coordinates$theta
  theta.range <- ggplotPanelParams$theta.range
  r.range <- ggplotPanelParams$r.range
  if(theta == "x"){
    angle <- 2*pi * (x - theta.range[1]) / (theta.range[2] - theta.range[1])
    x <- sin(angle) * (y - r.range[1]) / (r.range[2] - r.range[1])
    y <- cos(angle) * (y - r.range[1]) / (r.range[2] - r.range[1])
  } else if(theta == "y"){
    angle <- 2*pi * (y - theta.range[1]) / (theta.range[2] - theta.range[1])
    y <- sin(angle) * (x - r.range[1]) / (r.range[2] - r.range[1])
    x <- cos(angle) * (x - r.range[1]) / (r.range[2] - r.range[1])
  } else {
    x <- NA
    y <- NA
  }
  list(x = x, y= y)
}


#' @export
Cartesianxy2Polarxy.GeomRect <-  function(layerGeom = NULL, coordinates, data, ggplotPanelParams, ...){
  theta.range <- ggplotPanelParams$theta.range
  r.range <- ggplotPanelParams$r.range
  theta <- coordinates$theta
  # arbitrary choice
  seqLen <- 50
  if(theta == "x"){
    # is infinite?
    if(is.infinite(data$xmin) ) data$xmin <- theta.range[1]
    if(is.infinite(data$xmax) ) data$xmax <- theta.range[2]
    if(is.infinite(data$ymin) ) data$ymin <- r.range[1]
    if(is.infinite(data$ymax) ) data$ymax <- r.range[2]
    y <- rep(c(data$ymin, data$ymax), each = seqLen)
    xSeq <- seq(data$xmin, data$xmax, length.out = seqLen)
    x <- c(xSeq, rev(xSeq))
    angle <- 2 * pi * (x - theta.range[1]) / (theta.range[2] - theta.range[1])
    x <- sin(angle) * (y - r.range[1]) / (r.range[2] - r.range[1])
    y <- cos(angle) * (y - r.range[1]) / (r.range[2] - r.range[1])
  } else if(theta == "y"){
    # is infinite?
    if(is.infinite(data$xmin) ) data$xmin <- r.range[1]
    if(is.infinite(data$xmax) ) data$xmax <- r.range[2]
    if(is.infinite(data$ymin) ) data$ymin <- theta.range[1]
    if(is.infinite(data$ymax) ) data$ymax <- theta.range[2]
    x <- rep(c(data$xmin, data$xmax), each = seqLen)
    ySeq <- seq(data$ymin, data$ymax, length.out = seqLen)
    y <- c(ySeq, rev(ySeq))
    angle <- 2*pi * (y - theta.range[1]) / (theta.range[2] - theta.range[1])
    y <- sin(angle) * (x - r.range[1]) / (r.range[2] - r.range[1])
    x <- cos(angle) * (x - r.range[1]) / (r.range[2] - r.range[1])
  } else {
    x <- NA
    y <- NA }

  list(x = x, y= y)
}


#' @export
Cartesianxy2Polarxy.GeomVline <- function(layerGeom = NULL, coordinates, data, ggplotPanelParams, ...){
  theta.range <- ggplotPanelParams$theta.range
  r.range <- ggplotPanelParams$r.range
  theta <- coordinates$theta
  if(theta == "x"){
    x <- rep(data$xintercept, 2)
    y <- r.range
    angle <- 2*pi * (x - theta.range[1]) / (theta.range[2] - theta.range[1])
    x <- sin(angle) * (y - r.range[1]) / (r.range[2] - r.range[1])
    y <- cos(angle) * (y - r.range[1]) / (r.range[2] - r.range[1])
  } else if(theta == "y"){
    angle <- seq(0, 2*pi, length.out = 50)
    y <- sin(angle) * (data$xintercept - r.range[1]) / (r.range[2] - r.range[1])
    x <- cos(angle) * (data$xintercept - r.range[1]) / (r.range[2] - r.range[1])
  } else {
    x <- NA
    y <- NA}
  list(x = x, y= y)
}


#' @export
Cartesianxy2Polarxy.GeomHline <- function(layerGeom = NULL, coordinates, data, ggplotPanelParams, ...){
  theta.range <- ggplotPanelParams$theta.range
  r.range <- ggplotPanelParams$r.range
  theta <- coordinates$theta
  if(theta == "x"){
    angle <- seq(0, 2*pi, length.out = 50)
    y <- sin(angle) * (data$yintercept - r.range[1]) / (r.range[2] - r.range[1])
    x <- cos(angle) * (data$yintercept - r.range[1]) / (r.range[2] - r.range[1])
  } else if(theta == "y"){
    y <- rep(data$yintercept, 2)
    x <- r.range
    angle <- 2*pi * (y - theta.range[1]) / (theta.range[2] - theta.range[1])
    y <- sin(angle) * (x - r.range[1]) / (r.range[2] - r.range[1])
    x <- cos(angle) * (x - r.range[1]) / (r.range[2] - r.range[1])
  } else {
    x <- NA
    y <- NA}
  list(x = x, y= y)
}


#' @export
Cartesianxy2Polarxy.GeomAbline <- function(layerGeom = NULL, coordinates, data, ggplotPanelParams, ...) {

  theta.range <- ggplotPanelParams$theta.range
  r.range <- ggplotPanelParams$r.range
  theta <- coordinates$theta

  if(theta == "x"){
    coordCartesianxy <- abline2xy(theta.range, r.range, data$slope, data$intercept)
    x <- coordCartesianxy$x
    y <- coordCartesianxy$y
  } else if(theta == "y"){
    coordCartesianxy <- abline2xy(r.range, theta.range, data$slope, data$intercept)
    x <- coordCartesianxy$x
    y <- coordCartesianxy$y
  } else {
    x <- NA
    y <- NA
  }
  Cartesianxy2Polarxy.GeomPath(NULL, coordinates, data = data.frame(x = x, y = y), ggplotPanelParams, ...)
}


#' @export
Cartesianxy2Polarxy.GeomSegment <- function(layerGeom = NULL, coordinates, data, ggplotPanelParams, ...){
  # arbitrary choice
  seqLen <- 50
  x <- seq(data$x, data$xend, length.out = seqLen)
  y <- seq(data$y, data$yend, length.out = seqLen)
  theta.range <- ggplotPanelParams$theta.range
  r.range <- ggplotPanelParams$r.range
  theta <- coordinates$theta
  if(theta == "x"){
    angle <- 2*pi * (x - theta.range[1]) / (theta.range[2] - theta.range[1])
    x <- sin(angle) * (y - r.range[1]) / (r.range[2] - r.range[1])
    y <- cos(angle) * (y - r.range[1]) / (r.range[2] - r.range[1])
  } else if(theta == "y"){
    angle <- 2*pi * (y - theta.range[1]) / (theta.range[2] - theta.range[1])
    y <- sin(angle) * (x - r.range[1]) / (r.range[2] - r.range[1])
    x <- cos(angle) * (x - r.range[1]) / (r.range[2] - r.range[1])
  } else {
    x <- NA
    y <- NA}
  list(x = x, y= y)
}


#' @export
Cartesianxy2Polarxy.GeomPath <- function(layerGeom = NULL, coordinates, data, ggplotPanelParams, ...){
  n <- dim(data)[1]
  theta.range <- ggplotPanelParams$theta.range
  r.range <- ggplotPanelParams$r.range
  theta <- coordinates$theta
  x <- list()
  y <- list()
  # arbitrary choice
  seqLen <- 50
  for(i in 1:(n-1)){
    x[[i]] <- seq(data$x[i], data$x[i+1], length.out = seqLen)
    y[[i]] <- seq(data$y[i], data$y[i+1], length.out = seqLen)
  }
  x <- unlist(x)
  y <- unlist(y)
  if(theta == "x"){
    angle <- 2*pi * (x - theta.range[1]) / (theta.range[2] - theta.range[1])
    x <- sin(angle) * (y - r.range[1]) / (r.range[2] - r.range[1])
    y <- cos(angle) * (y - r.range[1]) / (r.range[2] - r.range[1])
  } else if(theta == "y"){
    angle <- 2*pi * (y - theta.range[1]) / (theta.range[2] - theta.range[1])
    y <- sin(angle) * (x - r.range[1]) / (r.range[2] - r.range[1])
    x <- cos(angle) * (x - r.range[1]) / (r.range[2] - r.range[1])
  } else {
    x <- NA
    y <- NA
  }
  list(x = x, y= y)
}
