#' @title scale data
#' @description It is mainly used in serial axes
#' @param data A data frame
#' @param sequence vector with variable names that defines the axes sequence.
#' If \code{NULL}, it will be set as the column names automatically.
#' @param scaling one of 'variable', 'data', 'observation' or 'none' to specify how the data is scaled.
#' @param displayOrder the order of the display
#' @param keep If \code{TRUE}, return the variables not shown in \code{sequence} as well;
#' else only return the variables defined in \code{sequence}.
#' @param as.data.frame Return a matrix or a data.frame
#' @import utils
#' @export
get_scaledData <- function(data,
                           sequence = NULL,
                           scaling = c('variable', 'data', 'observation', 'none'),
                           displayOrder = NULL,
                           keep = FALSE,
                           as.data.frame = FALSE) {

  if(missing(data)) return(NULL)

  scaling <- match.arg(scaling)
  displayOrder <- displayOrder %||% seq(nrow(data))

  f <- utils::getFromNamespace("get_scaledData", "loon")

  if(keep && !is.null(sequence)) {

    colNames <- colnames(data)
    leftNames <- setdiff(colNames, sequence)

    leftData <- data[, leftNames]
    scaledData <- data[, sequence]

    d <- suppressWarnings(f(data = scaledData, sequence = sequence,
                            scaling = scaling, displayOrder = displayOrder))
    rightNames <- colnames(d)

    # f return a matrix
    d <- cbind(leftData, d)
    colnames(d) <- c(leftNames, rightNames)
  } else {
    d <- suppressWarnings(f(data = data, sequence = sequence, scaling = scaling, displayOrder = displayOrder))
  }

  if(as.data.frame)
    as.data.frame(d, stringsAsFactors = FALSE)
  else
    as.matrix(d)
}
