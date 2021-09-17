#' @title get locations for \code{ggmatrix}
#' @description  For the target compound \code{loon} plot, determines location in \code{ggmatrix}
#'
#' @param target the (compound) loon plot whose locations are needed to lay out.
#' @return a list of an appropriate subset of the named location arguments
#' `c("ncol", "nrow", "layout_matrix", "heights", "widths")`. \code{layout_matrix}
#' is an \code{nrow} by \code{ncol} matrix whose entries identify the location
#' of each plot in \code{g_getPlots()} by their index.
#'
#' @seealso \code{\link{l_getLocations}}, \code{\link{g_getPlots}}
g_getLocations <- function(target) {
  UseMethod('g_getLocations', target)
}

#' @rdname g_getLocations
g_getLocations.default <- function(target) {

  # arguments check would be done in `l_getLocations`
  locations <- loon::l_getLocations(target)
  nrow <- locations$nrow
  ncol <- locations$ncol
  layout_matrix <- locations$layout_matrix

  layout_matrix <- c(layout_matrix)
  layout_matrix[duplicated(layout_matrix)] <- NA

  list(
    nrow = nrow,
    ncol = ncol,
    layout_matrix = matrix(layout_matrix, nrow = nrow),
    heights = locations$heights,
    widths = locations$widths
  )
}

#' @rdname g_getLocations
g_getLocations.l_pairs <- function(target) {

  # arguments check would be done in `l_getLocations`
  locations <- loon::l_getLocations(target)
  nrow <- locations$nrow
  ncol <- locations$ncol
  layout_matrix <- locations$layout_matrix

  m <- na.omit(c(layout_matrix))
  dup_id <- unique(m[duplicated(m)])

  if(length(dup_id) == 0) return(locations)
  if(length(dup_id) > 2) {
    stop(
      "Can `l_pairs()` accept other objects (except `l_seiralaxes()`) having larger layout size? If so, Fix ME!"
    )
  }
  layout_matrix[layout_matrix == dup_id] <- NA
  layout_matrix[nrow, 1] <- dup_id
  list(
    nrow = nrow,
    ncol = ncol,
    layout_matrix = layout_matrix,
    heights = locations$heights,
    widths = locations$widths
  )
}
