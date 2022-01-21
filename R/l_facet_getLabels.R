l_facet_wrap_getLabels <- function(target) {
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
      paste0(as.character(tkcget(xLabelPathName, "-text")),
             collapse = " ")
    },
    error = function(e) {character(0L)}
  )

  ylabel <- tryCatch(
    expr = {
      paste0(as.character(tkcget(yLabelPathName, "-text")),
             collapse = " ")
    },
    error = function(e) {character(0L)}
  )

  title <- tryCatch(
    expr = {
      paste0(as.character(tkcget(titlePathName, "-text")),
             collapse = " ")
    },
    error = function(e) {character(0L)}
  )

  if(any(grepl("bottom", tkLabelPathNames))) {
    labelsLocation <- "bottom"
  } else {
    labelsLocation <- "top"
  }

  # labels
  if(length(c(xl, yl)) == 0) {
    labelPathNames <- tkLabelPathNames
  } else {
    labelPathNames <- tkLabelPathNames[-c(xl, yl)]
  }

  names <- names(target)
  facetsLabels <- stats::setNames(
    do.call(data.frame,
            lapply(names,
                   function(name) {
                     labelij <- labelPathNames[grepl(name, labelPathNames)]
                     unname(vapply(labelij,
                                   function(l)
                                     paste0(as.character(tkcget(l, "-text")),
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
    title = title,
    xlabel = xlabel,
    ylabel = ylabel,
    labelsLocation = labelsLocation,
    facetsLabels = facetsLabels,
    levels = levels
  )
}

l_facet_grid_getLabels <- function(target) {

  # find the parent tk window name
  parent <- as.character(tkwinfo("parent",  target[[1L]]))
  # access all children
  children <- as.character(tkwinfo("child",  parent))

  # a trick here
  # `xlabel` and `ylabel` both include text `label`
  tkColumnlabelPathNames <- children[grepl("columnlabel", children)]
  tkRowlabelPathNames <- children[grepl("rowlabel", children)]

  # xlabel, ylabel and title
  xLabelPathName <- children[grepl("xlabel", children)]
  yLabelPathName <- children[grepl("ylabel", children)]
  titlePathName <- children[grepl("title", children)]

  xlabel <- tryCatch(
    expr = {
      paste0(as.character(tkcget(xLabelPathName, "-text")),
             collapse = " ")
    },
    error = function(e) {character(0L)}
  )

  ylabel <- tryCatch(
    expr = {
      paste0(as.character(tkcget(yLabelPathName, "-text")),
             collapse = " ")
    },
    error = function(e) {character(0L)}
  )

  title <- tryCatch(
    expr = {
      paste0(as.character(tkcget(titlePathName, "-text")),
             collapse = " ")
    },
    error = function(e) {character(0L)}
  )


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
                                                      paste0(as.character(tkcget(l, "-text")),
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
                                                      paste0(as.character(tkcget(l, "-text")),
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
    title = title,
    xlabel = xlabel,
    ylabel = ylabel,
    labelsLocation = labelsLocation,
    facetsLabels = facetsLabels,
    facetsColLabels = facetsColLabels,
    facetsRowLabels = facetsRowLabels,
    levels = levels
  )
}
