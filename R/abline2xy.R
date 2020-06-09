abline2xy <- function(xrange, yrange, slope, intercept){

  x <- xrange
  y <- c(intercept + slope * x[1], intercept + slope * x[2])

  if(y[1] < yrange[1] ) {
    x[1] <- (yrange[1] - intercept)/ slope
    y[1] <- yrange[1]
  }
  if(y[2] > yrange[2]){
    x[2] <- (yrange[2] - intercept)/ slope
    y[2] <- yrange[2]
  }
  list(x = x, y = y)
}
