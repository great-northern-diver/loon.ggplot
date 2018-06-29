

coordPolarGuides <- function(widget, ggplotPanel_params, theta){

  theta.range <- ggplotPanel_params$theta.range
  r.range <- ggplotPanel_params$r.range

  # theta labels
  theta.major <- ggplotPanel_params$theta.major
  theta.labels <- ggplotPanel_params$theta.labels

  # radius labels
  r.major <- ggplotPanel_params$r.major
  r.labels <- ggplotPanel_params$r.labels

  # drawing lines
  theta.minor <- ggplotPanel_params$theta.minor

  # drawing ovals
  if(length(r.major) >= 1) {
      r.minor <- c(r.major, max(r.range) + diff(r.range)/8)
  } else r.minor <- c(min(r.range), max(r.range) + diff(r.range)/8)

  radius <- (r.minor - r.range[1])/ diff(r.range)
  textRadius <- (r.major - r.range[1])/ diff(r.range)
  maxRadius <- max(radius)
  angles <- 2 * pi * (theta.minor - theta.range[1])/diff(theta.range)
  textAngles <- 2 * pi * (theta.major - theta.range[1])/diff(theta.range)
  # set group
  guidesGroup <- l_layer_group(widget, "polar guides")
  # draw grey background
  extendRange <- grDevices::extendrange(c(-maxRadius, maxRadius))
  rectLayer <- l_layer_rectangle(widget, x = extendRange, y = extendRange,
                                 color = "#EBEBEBEBEBEB", linecolor = "", parent = guidesGroup)
  if(theta == "x"){
    # draw radius label
    textRadiiLayer <- lapply(1:length(textRadius), function(i){
      l_layer_text(widget, x = extendRange[1], y = textRadius[i], color = "black", parent = guidesGroup,
                   text = r.labels[i], size = 9)
    })
    # draw ovals
    ovalsLayer <- lapply(radius, function(l){
      xleft <- ybottom <- -l
      xright <- ytop <- l
      l_layer_oval(widget, x = c(xleft, xright), y = c(ybottom, ytop),
                   color = "", linecolor = "white", parent = guidesGroup)
    })
    # draw lines
    linesLayer <- lapply(angles, function(l){
      xend <- maxRadius * sin(l)
      yend <- maxRadius * cos(l)
      l_layer_line(widget, x = c(0, xend), y = c(0, yend),
                   color = "white", parent = guidesGroup)
    })
    # draw lines label
    linesLabelLayer <- lapply(1:length(textAngles), function(i){
      l_layer_text(widget, x = maxRadius * sin(textAngles[i]), y = maxRadius * cos(textAngles[i]),
                   color = "black", parent = guidesGroup, text = theta.labels[i], size = 9)
    })
  } else if(theta == "y") {
    textRadiiLayer <- lapply(1:length(textRadius), function(i){
      l_layer_text(widget, y = extendRange[1], x = textRadius[i], color = "black", parent = guidesGroup,
                   text = r.labels[i], size = 9)
    })
    # draw ovals
    ovalsLayer <- lapply(radius, function(l){
      xleft <- ybottom <- -l
      xright <- ytop <- l
      l_layer_oval(widget, y = c(xleft, xright), x = c(ybottom, ytop),
                   color = "", linecolor = "white", parent = guidesGroup)
    })
    # draw lines
    linesLayer <- lapply(angles, function(l){
      xend <- maxRadius * sin(l)
      yend <- maxRadius * cos(l)
      l_layer_line(widget, y = c(0, xend), x = c(0, yend),
                   color = "white", parent = guidesGroup)
    })
    # draw lines label
    linesLabelLayer <- lapply(1:length(textAngles), function(i){
      l_layer_text(widget, y = maxRadius * sin(textAngles[i]), x = maxRadius * cos(textAngles[i]),
                   color = "black", parent = guidesGroup, text = theta.labels[i], size = 9)
    })
  } else NULL

  guidesGroup
}
