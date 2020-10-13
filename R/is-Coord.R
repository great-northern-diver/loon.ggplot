is.CoordPolar <- function(coord){
  "CoordPolar" %in% class(coord)
}

is.CoordSerialaxes <- function(coord) {
  "CoordSerialaxes" %in% class(coord)
}

is.CoordFlip <- function(coord) {
  "CoordFlip" %in% class(coord)
}
