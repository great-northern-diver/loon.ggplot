#' @title Return the subtitles
#' @name l_getSubtitles
#' @param target an \code{l_facet_ggplot} object. If the ggplot object is
#' faceted (either by \code{facet_wrap} or \code{facet_grid}),
#' an \code{l_facet_ggplot} object will be returned once it is turned
#' to a \code{loon} plot.
#' @return A list of labels, i.e. subtitles, labels, title, etc
#' @export
#' @examples
#' if(interactive()) {
#' p <- ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   facet_wrap(vars(class))
#' lp <- loon.ggplot(p)
#' l_getSubtitles(lp)
#' }
#'
l_getSubtitles <- function(target) {
  UseMethod("l_getSubtitles", target)
}

#' @export
#' @rdname l_getSubtitles
l_getSubtitles.l_facet_ggplot <- function(target) {

  labels <- facet_getLabels(target)
  tkLabelPathNames <- labels$tkLabelPathNames

  # FacetWrap or FacetGrid
  FacetWrap <- any(grepl("wrap", tkLabelPathNames))
  FacetGrid <- any(grepl("grid", tkLabelPathNames))

  # default settings
  colSubtitles <- NULL
  rowSubtitles <- NULL
  byCOLS <- FALSE
  byROWS <- FALSE
  facetsLabels <- NULL
  levels <- NULL
  labelsLocation <- NULL
  drop <- NULL
  facetsColLabels <- data.frame()
  facetsRowLabels <- data.frame()
  scales <- NULL

  # title column subtitle or row subtitle
  if(FacetWrap) {

    columnlabelPathName <- tkLabelPathNames[grepl("columnlabel", tkLabelPathNames)]
    if(length(columnlabelPathName) > 0L) {

      names <- names(target)
      facetsLabels <- stats::setNames(
        do.call(data.frame,
                lapply(names,
                       function(name) {
                         labelij <- columnlabelPathName[grepl(name, columnlabelPathName)]
                         strsplit(tcltk::tclvalue(tcltk::tkcget(labelij, "-text")), "\n")[[1L]]
                       })),
        nm = names
      )
      rownames(facetsLabels) <- paste0("facet", seq(nrow(facetsLabels)))

      levels <- lapply(seq(nrow(facetsLabels)),
                       function(i) {
                         unique(unlist(facetsLabels[i, ]))
                       })

      colSubtitles <- apply(facetsLabels, 2, function(x) paste0(x, collapse = " "))
    }

    # TODO c("top") is default
    labelsLocation <- "top"
    labelPathName1L <- c(columnlabelPathName)[1L]
    pathSplit <- strsplit(labelPathName1L, "-")[[1L]]
    byCOLSChar <- pathSplit[grepl("byCOLS", pathSplit)]
    byCOLS <- grepl("TRUE", byCOLSChar)
    byROWSChar <- pathSplit[grepl("byROWS", pathSplit)]
    byROWS <- grepl("TRUE", byROWSChar)
    # drop or not
    dropChar <- pathSplit[grepl("drop", pathSplit)]
    drop <- grepl("TRUE", dropChar)
    # scales
    scalesChar <- pathSplit[grepl("scales", pathSplit)]
    scales <- strsplit(scalesChar, ":")[[1L]][2]

  } else if (FacetGrid) {

    columnlabelPathName <- tkLabelPathNames[grepl("columnlabel", tkLabelPathNames)]
    rowlabelPathName <- tkLabelPathNames[grepl("rowlabel", tkLabelPathNames)]

    locations <- l_getLocations(target)
    names <- names(target)
    offset <- 0
    if(length(columnlabelPathName) > 0) {

      colLabels <- lapply(columnlabelPathName,
                          function(name) {
                            strsplit(tcltk::tclvalue(tcltk::tkcget(name, "-text")), "\n")[[1L]]
                          })
      facetsColLabels <- stats::setNames(
        as.data.frame(
          rep(colLabels, locations$nrow)
        ), nm = names
      )
      offset <- nrow(facetsColLabels)
      rownames(facetsColLabels) <- paste0("facet", seq(offset))

      colSubtitles <- apply(facetsColLabels, 2, function(x) paste0(x, collapse = " "))
    }

    facetsLabels <- facetsColLabels

    if(length(rowlabelPathName) > 0) {

      rowLabels <- lapply(rowlabelPathName,
                          function(name) {
                            # TODO too fragile; need better ways to find subtitles
                            str <- strsplit(tcltk::tclvalue(tcltk::tkcget(name, "-text")),
                                            "\n \n \n")[[1L]]
                            unname(sapply(str,
                                          function(x) {
                                            gsub(" |\n", "", x)
                                          }))
                          })

      facetsRowLabels <- stats::setNames(
        as.data.frame(
          rep(rowLabels, each = locations$ncol)
        ), nm = names
      )
      rownames(facetsRowLabels) <- paste0("facet", seq(nrow(facetsRowLabels)) + offset)

      rowSubtitles <- apply(facetsRowLabels, 2, function(x) paste0(x, collapse = " "))

      if(offset > 0) {
        facetsLabels <- rbind(facetsLabels,
                              facetsRowLabels)
      } else {
        facetsLabels <- facetsRowLabels
      }
    }

    levels <- lapply(seq(nrow(facetsLabels)),
                     function(i) {
                       unique(unlist(facetsLabels[i, ]))
                     })

    # TODO c("top", "right") is default
    labelsLocation <- c("top", "right")
    labelPathName1L <- c(columnlabelPathName, rowlabelPathName)[1L]
    pathSplit <- strsplit(labelPathName1L, "-")[[1L]]
    byCOLSChar <- pathSplit[grepl("byCOLS", pathSplit)]
    byCOLS <- grepl("TRUE", byCOLSChar)
    byROWSChar <- pathSplit[grepl("byROWS", pathSplit)]
    byROWS <- grepl("TRUE", byROWSChar)
    # drop or not
    dropChar <- pathSplit[grepl("drop", pathSplit)]
    drop <- grepl("TRUE", dropChar)
    # scales
    scalesChar <- pathSplit[grepl("scales", pathSplit)]
    scales <- strsplit(scalesChar, ":")[[1L]][2]

  } else {
    stop("`facet_wrap()` or `facet_grid()`?", call. = FALSE)
  }

  list(
    xlabel = labels$xlabel,
    ylabel = labels$ylabel,
    title = labels$title,
    labelsLocation = labelsLocation,
    facetsLabels = facetsLabels,
    levels = levels,
    drop = drop,
    scales = scales,
    FacetWrap = FacetWrap,
    FacetGrid = FacetGrid,
    colSubtitles = colSubtitles,
    rowSubtitles = rowSubtitles,
    byCOLS = byCOLS,
    byROWS = byROWS,
    facetsColLabels = facetsColLabels,
    facetsRowLabels = facetsRowLabels
  )
}

#' @export
#' @rdname l_getSubtitles
l_getSubtitles.l_facet_wrap <- function(target) {

  labels <- facet_getLabels(target)
  tkLabelPathNames <- labels$tkLabelPathNames
  labelPathNames <- labels$labelPathNames

  if(any(grepl("bottom", tkLabelPathNames))) {
    labelsLocation <- "bottom"
  } else {
    labelsLocation <- "top"
  }

  names <- names(target)
  facetsLabels <- stats::setNames(
    do.call(data.frame,
            lapply(names,
                   function(name) {
                     labelij <- labelPathNames[grepl(name, labelPathNames)]
                     unname(vapply(labelij,
                                   function(l)
                                     paste0(as.character(tcltk::tkcget(l, "-text")),
                                            collapse = " "),
                                   character(1L)))
                   })),
    nm = names
  )
  rownames(facetsLabels) <- paste0("facet", seq(nrow(facetsLabels)))

  levels <- lapply(seq(nrow(facetsLabels)),
                   function(i) {
                     unique(unlist(facetsLabels[i, ]))
                   })

  list(
    title = labels$title,
    xlabel = labels$xlabel,
    ylabel = labels$ylabel,
    labelsLocation = labelsLocation,
    facetsLabels = facetsLabels,
    levels = levels
  )
}

#' @export
#' @rdname l_getSubtitles
l_getSubtitles.l_facet_grid <- function(target) {

  labels <- facet_getLabels(target)
  tkLabelPathNames <- labels$tkLabelPathNames
  tkColumnlabelPathNames <- tkLabelPathNames[grepl("columnlabel", tkLabelPathNames)]
  tkRowlabelPathNames <- tkLabelPathNames[grepl("rowlabel", tkLabelPathNames)]

  labelsLocation <- c("top", "right")

  if(any(grepl("bottom", tkColumnlabelPathNames))) {
    labelsLocation[1L] <- "bottom"
  }

  if(any(grepl("left", tkRowlabelPathNames))) {
    labelsLocation[2L] <- "left"
  }

  n <- length(target)
  names <- names(target)
  pat <- "^.*extend.*?([0-9]+)"
  offset <- 0

  if(length(tkColumnlabelPathNames) > 0) {
    extendCol <- as.numeric(gsub(pat, "\\1", tkColumnlabelPathNames))
    uniCol <- unique(extendCol)
    facetsColLabels <- stats::setNames(
      as.data.frame(
        do.call(rbind,
                lapply(uniCol,
                       function(col) {
                         labelij <- rep(tkColumnlabelPathNames[extendCol == col], each = col)
                         colLables <- unname(vapply(labelij,
                                                    function(l)
                                                      paste0(as.character(tcltk::tkcget(l, "-text")),
                                                             collapse = " "),
                                                    character(1L)))
                         rep(colLables, each = n/length(colLables))
                       }))
      ),
      nm = names
    )
    offset <- nrow(facetsColLabels)
    rownames(facetsColLabels) <- paste0("facet", seq(offset))
  } else {
    facetsColLabels <- data.frame()
  }

  if(length(tkRowlabelPathNames) > 0) {
    extendRow <- as.numeric(gsub(pat, "\\1", tkRowlabelPathNames))
    uniRow <- unique(extendRow)
    facetsRowLabels <- stats::setNames(
      as.data.frame(
        do.call(rbind,
                lapply(uniRow,
                       function(row) {
                         labelij <- rep(tkRowlabelPathNames[extendRow == row], each = row)
                         rowLables <- unname(vapply(labelij,
                                                    function(l)
                                                      paste0(as.character(tcltk::tkcget(l, "-text")),
                                                             collapse = " "),
                                                    character(1L)))

                         rep_len(rowLables, n)
                       }))
      ),
      nm = names
    )
    rownames(facetsRowLabels) <- paste0("facet", seq(nrow(facetsRowLabels)) + offset)
  } else {
    facetsRowLabels <- data.frame()
  }

  facetsLabels <- rbind(facetsColLabels, facetsRowLabels)
  levels <- lapply(seq(nrow(facetsLabels)),
                   function(i) {
                     unique(unlist(facetsLabels[i, ]))
                   })

  list(
    title = labels$title,
    xlabel = labels$xlabel,
    ylabel = labels$ylabel,
    labelsLocation = labelsLocation,
    facetsLabels = facetsLabels,
    facetsColLabels = facetsColLabels,
    facetsRowLabels = facetsRowLabels,
    levels = levels
  )
}

facet_getLabels <- function(target) {
  # find the parent tk window name
  parent <- as.character(tkwinfo("parent",  target[[1L]]))
  # access all children
  children <- as.character(tkwinfo("child",  parent))

  # a trick here
  # `xlabel` and `ylabel` both include text `label`
  tkLabelPathNames <- children[grepl("label", children)]

  # xlabel, ylabel and title
  xl <- which(grepl("xlabel", tkLabelPathNames))
  xLabelPathName <- tkLabelPathNames[xl]
  yl <- which(grepl("ylabel", tkLabelPathNames))
  yLabelPathName <- tkLabelPathNames[yl]
  ti <- which(grepl("title", children))
  titlePathName <- children[ti]

  xlabel <- tryCatch(
    expr = {
      paste0(as.character(tcltk::tkcget(xLabelPathName, "-text")),
             collapse = "")
    },
    error = function(e) {character(0L)}
  )

  ylabel <- tryCatch(
    expr = {
      paste0(as.character(tcltk::tkcget(yLabelPathName, "-text")),
             collapse = "")
    },
    error = function(e) {character(0L)}
  )

  title <- tryCatch(
    expr = {
      paste0(as.character(tcltk::tkcget(titlePathName, "-text")),
             collapse = "")
    },
    error = function(e) {character(0L)}
  )

  # labels
  if(length(c(xl, yl)) == 0) {
    labelPathNames <- tkLabelPathNames
  } else {
    labelPathNames <- tkLabelPathNames[-c(xl, yl)]
  }

  list(
    xlabel = xlabel,
    ylabel = ylabel,
    title = title,
    tkLabelPathNames = tkLabelPathNames,
    labelPathNames = labelPathNames
  )
}
