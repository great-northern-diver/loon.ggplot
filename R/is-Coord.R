#' @title Is polar coordinate system?
#' @description Determine whether the \code{ggplot} object has polar coordinate system
#' @param coord A \code{ggplot} object coordinate system
#' @export
is.CoordPolar <- function(coord) {
  "CoordPolar" %in% class(coord)
}

is.CoordSerialaxes <- function(coord) {
  "CoordSerialaxes" %in% class(coord)
}

is.CoordFlip <- function(coord) {
  "CoordFlip" %in% class(coord)
}
