ggEdges <- function(ggObj, states) {

  active <- states$active
  activeNode <- states$nodes[active]
  activeX <- states$x[active]
  activeY <- states$y[active]
  isActiveEdge <- states$activeEdge

  # add edges and labels
  lapply(seq_len(length(activeNode)),
         function(i) {
           # edges
           nodeFrom <- activeNode[i]
           nodeFrom_EdgeId <- which(states$from[isActiveEdge] == nodeFrom)
           if (length(nodeFrom_EdgeId) != 0) {
             nodeTo <- states$to[isActiveEdge][nodeFrom_EdgeId]
             nodeTo_CoordId <- which(activeNode %in% nodeTo)
             numNodesTo <- length(nodeTo_CoordId)

             x <- c(rep(activeX[i], numNodesTo), activeX[nodeTo_CoordId])
             y <- c(rep(activeY[i], numNodesTo), activeY[nodeTo_CoordId])
             id <- rep(nodeTo_CoordId, 2)

             ggObj <<- ggObj +
               ggplot2::geom_path(
                 data = data.frame(
                   x = x,
                   y = y,
                   id = id
                 ),
                 mapping = ggplot2::aes(x = x, y = y, group = id, colour = as.factor(id)),
                 colour = rep(states$colorEdge[isActiveEdge][nodeFrom_EdgeId][nodeTo %in% activeNode], 2),
                 inherit.aes = FALSE
               )
           }

           # do not need to return anything
           NULL
         }
  )

  return(ggObj)
}

ggLabels <- function(ggObj, states) {

  active <- states$active
  activeNode <- states$nodes[active]
  activeX <- states$x[active]
  activeY <- states$y[active]
  activeAngle <- states$orbitAngle[active]
  orbitDistance <- states$orbitDistance

  # add edges and labels
  lapply(seq_len(length(activeNode)),
         function(i) {
           # labels
           if(states$showOrbit) {


             x <- activeX[i] + mm2native(orbitDistance) * cos(activeAngle[i])
             y <- activeY[i] + mm2native(orbitDistance) * sin(activeAngle[i])
             label <- activeNode[i]

             ggObj <<- ggObj +
               ggplot2::geom_text(
                 data = data.frame(
                   x = x,
                   y = y,
                   label = label
                 ),
                 mapping = ggplot2::aes(x = x, y = y, label = label),
                 colour= loon::l_getOption("foreground"),
                 inherit.aes = FALSE
               )
           }
           # do not need to return anything
           NULL
         }
  )

  return(ggObj)
}

ggNodes <- function(ggObj, states) {

  active <- states$active
  # add nodes
  # is there a fill colour?
  pch <- glyph_to_pch(states$glyph[active])
  colour <- get_display_color(states$color[active],
                                     states$selected[active])
  fill <- rep(NA, length(colour))

  # if filled, colour would be the boudary colour
  fill[pch %in% 21:24] <- colour[pch %in% 21:24]
  colour[pch %in% 21:24] <- loon::l_getOption("foreground")

  x <- states$x[active]
  y <- states$y[active]

  ggObj <- ggObj +
    ggplot2::geom_point(
      data = data.frame(x = x, y = y),
      mapping = ggplot2::aes(x = x, y = y),
      shape = pch,
      colour = colour,
      fill = fill,
      size = as_ggplot_size(states$size[active]),
      inherit.aes = FALSE
    )

  return(ggObj)
}

ggNavPaths <- function(ggObj, states, nav_ids, widget) {

  # navigator path
  ## navigators are always active
  x <- states$x
  y <- states$y
  node <- states$nodes

  # TODO: random guess
  fromLineSize <- 3
  toLineSize <- 1

  # add edges and labels
  lapply(nav_ids,
         function(nav_id) {

           navigator <- loon::l_create_handle(c(widget, nav_id))

           color <- as_hex6color(navigator['color'])
           from <- navigator['from']
           to <- navigator['to']
           prop <- navigator['proportion']

           if(length(from) != 0 && length(to) != 0) {

             fromId <- sapply(1:length(from), function(i){which(node %in% from[i] == TRUE)})
             toId <- sapply(1:length(to), function(i){which(node %in% to[i] == TRUE)})

             # unvisited paths
             if(length(to) >= 2) {
               ## ggobj has been modified in children envir
               lapply(1:(length(to) - 1),
                      function(i){

                        ggObj <<- ggObj +
                          ggplot2::geom_path(
                            data = data.frame(
                              x = c(x[toId[i]], x[toId[i+1]]),
                              y = c(y[toId[i]], y[toId[i+1]])
                            ),
                            mapping = ggplot2::aes(x = x, y = y),
                            colour = color,
                            size = toLineSize,
                            inherit.aes = FALSE
                          )
                      }
               )
             }

             # visited paths
             if(length(from) >= 2) {
               ## ggobj has been modified in children envir
               lapply(1:(length(from) - 1),
                      function(i) {
                        ggObj <<- ggObj +
                          ggplot2::geom_path(
                            data = data.frame(
                              x = c(x[fromId[i]], x[fromId[i+1]]),
                              y = c(y[fromId[i]], y[fromId[i+1]])
                            ),
                            mapping = ggplot2::aes(x = x, y = y),
                            colour = color,
                            size = fromLineSize,
                            inherit.aes = FALSE
                          )
                      }
               )
             }

             xn <- (1 - prop) * x[fromId[length(fromId)]] + prop * x[toId[1]]
             yn <- (1 - prop) * y[fromId[length(fromId)]] + prop * y[toId[1]]

             # part visited and part unvisited
             ## modify ggObj in parent "ggAddNavPath" envir
             ggObj <<- ggObj +
               ggplot2::geom_path(
                 data = data.frame(
                   x = c(x[fromId[length(fromId)]], xn),
                   y = c(y[fromId[length(fromId)]], yn)
                 ),
                 mapping = ggplot2::aes(x = x, y = y),
                 colour = color,
                 size = fromLineSize,
                 inherit.aes = FALSE
               ) +
               ggplot2::geom_path(
                 data = data.frame(
                   x = c(xn, x[toId[1]]),
                   y = c(yn, y[toId[1]])
                 ),
                 mapping = ggplot2::aes(x = x, y = y),
                 colour = color,
                 size = toLineSize,
                 inherit.aes = FALSE
               )
           }

           # do not need to return anything
           NULL
         }
  )

  return(ggObj)
}

ggNavPoints <- function(ggObj, states, nav_ids, widget) {

  activeNavigator <- widget["activeNavigator"]

  # navigator path
  ## navigators are always active
  x <- states$x
  y <- states$y
  node <- states$nodes

  sel_color <- as.character(loon::l_getOption("select-color"))
  if (grepl("^#", sel_color) && nchar(sel_color) == 13) {
    sel_color <- loon::hex12tohex6(sel_color)
  }

  # TODO: random guess
  navPointsSize <- 12
  navTextSize <- 3
  navDotsSize <- 2
  stroke <- 2

  # add edges and labels
  lapply(nav_ids,
         function(nav_id) {

           navigator <- loon::l_create_handle(c(widget, nav_id))

           color <- as_hex6color(navigator['color'])
           from <- navigator['from']
           to <- navigator['to']
           prop <- navigator['proportion']
           label <- navigator['label']

           if(length(activeNavigator) != 0) {
             if(activeNavigator == navigator) {
               boundColor <- sel_color
             } else {
               boundColor <- loon::l_getOption("foreground")
               stroke <- 1
             }
           } else {
             boundColor <- loon::l_getOption("foreground")
             stroke <- 1
           }

           fromId <- sapply(1:length(from), function(i){which(node %in% from[i] == TRUE)})
           toId <- sapply(1:length(to), function(i){which(node %in% to[i] == TRUE)})

           if(length(from) == 0) {

             ggObj <<- ggObj +
               ggplot2::geom_point(
                 data = data.frame(x = 0.1, y = 0.9),
                 mapping = ggplot2::aes(x = x, y = y),
                 size = navPointsSize,
                 fill = color,
                 colour = boundColor,
                 stroke = stroke,
                 shape = 21,
                 inherit.aes = FALSE
               ) +
               ggplot2::geom_text(
                 data = data.frame(x = 0.1, y = 0.9, label = paste(label, collapse = " ")),
                 mapping = ggplot2::aes(x = x, y = y, label = label),
                 colour = loon::l_getOption("foreground"),
                 size = navTextSize,
                 inherit.aes = FALSE
               )
           } else if(length(from) == 1 & length(to) == 0) {

             ggObj <<- ggObj +
               ggplot2::geom_point(
                 data = data.frame(x = x[fromId], y = y[fromId]),
                 mapping = ggplot2::aes(x = x, y = y),
                 size = navPointsSize,
                 fill = color,
                 colour = boundColor,
                 stroke = stroke,
                 shape = 21,
                 inherit.aes = FALSE
               ) +
               ggplot2::geom_text(
                 data = data.frame(x = x[fromId], y = y[fromId],
                                   label = paste(label, collapse = " ")),
                 mapping = ggplot2::aes(x = x, y = y, label = label),
                 colour = loon::l_getOption("foreground"),
                 size = navTextSize,
                 inherit.aes = FALSE
               )
           } else {

             xx <- (1 - prop) * x[fromId[length(fromId)]] + prop * x[toId[1]]
             yy <- (1 - prop) * y[fromId[length(fromId)]] + prop * y[toId[1]]

             ggObj <<- ggObj +
               ggplot2::geom_point(
                 data = data.frame(
                   x = x[toId[length(toId)]],
                   y = y[toId[length(toId)]]
                 ),
                 mapping = ggplot2::aes(x = x, y = y),
                 size = navDotsSize,
                 fill = color,
                 colour = loon::l_getOption("foreground"),
                 inherit.aes = FALSE,
                 shape = 21,
               ) +
               ggplot2::geom_point(
                 data = data.frame(x = xx, y = yy),
                 mapping = ggplot2::aes(x = x, y = y),
                 size = navPointsSize,
                 fill = color,
                 colour = boundColor,
                 stroke = stroke,
                 shape = 21,
                 inherit.aes = FALSE
               ) +
               ggplot2::geom_text(
                 data = data.frame(x = x[fromId], y = y[fromId],
                                   label = paste(label, collapse = " ")),
                 mapping = ggplot2::aes(x = x, y = y, label = label),
                 colour = loon::l_getOption("foreground"),
                 size = navTextSize,
                 inherit.aes = FALSE
               )
           }

           # do not need to return anything
           NULL
         }
  )

  return(ggObj)
}

# Do u have better ideas?
mm2native <- function(x) x/50
