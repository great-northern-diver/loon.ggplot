cartesianxy2Polarxy <- function(layerGeom, theta, data, ggplotPanel_params, ...){
  UseMethod("cartesianxy2Polarxy", layerGeom$geom)
}

cartesianxy2Polarxy.default <- function(layerGeom = NULL, theta, data, ggplotPanel_params){
  x <- data$x
  y <- data$y
  theta.range <- ggplotPanel_params$theta.range
  r.range <- ggplotPanel_params$r.range
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



cartesianxy2Polarxy.GeomRect <-  function(layerGeom = NULL, theta, data, ggplotPanel_params){
  theta.range <- ggplotPanel_params$theta.range
  r.range <- ggplotPanel_params$r.range
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



cartesianxy2Polarxy.GeomVline <- function(layerGeom = NULL, theta, data, ggplotPanel_params){
  theta.range <- ggplotPanel_params$theta.range
  r.range <- ggplotPanel_params$r.range
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



cartesianxy2Polarxy.GeomHline <- function(layerGeom = NULL, theta, data, ggplotPanel_params){
  theta.range <- ggplotPanel_params$theta.range
  r.range <- ggplotPanel_params$r.range
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



cartesianxy2Polarxy.GeomAbline <- function(layerGeom = NULL, theta, data, ggplotPanel_params){
  theta.range <- ggplotPanel_params$theta.range
  r.range <- ggplotPanel_params$r.range
  intercept <- data$intercept
  slope <- data$slope
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
  cartesianxy2Polarxy.GeomPath(NULL, theta, data = data.frame(x = x, y = y), ggplotPanel_params)
}



cartesianxy2Polarxy.GeomSegment <- function(layerGeom = NULL, theta, data, ggplotPanel_params){
  x <- c(data$x, data$xend)
  y <- c(data$y, data$yend)
  theta.range <- ggplotPanel_params$theta.range
  r.range <- ggplotPanel_params$r.range
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



cartesianxy2Polarxy.GeomPath <- function(layerGeom = NULL, theta, data, ggplotPanel_params){
  n <- dim(data)[1]
  theta.range <- ggplotPanel_params$theta.range
  r.range <- ggplotPanel_params$r.range
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
