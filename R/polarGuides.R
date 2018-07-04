
polarGuides <- function(widget, ggplotPanel_params, swapAxes){

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
  guidesGroup <- l_layer_group(widget, "Polar Guides")
  # draw grey background
  extendRange <- grDevices::extendrange(c(-maxRadius, maxRadius))
  rectLayer <- l_layer_rectangle(widget,
                                 x = extendRange,
                                 y = extendRange,
                                 color = widget['guidesBackground'],
                                 linecolor = "",
                                 parent = guidesGroup)
  if (swapAxes) {
    # draw radius label
    labelsGroup <- l_layer_group(widget, "radius labels", parent = guidesGroup)
    textRadiiLayer <- lapply(1:length(textRadius), function(i){
      l_layer_text(widget, x = textRadius[i], y = extendRange[1], color = widget['foreground'], parent = labelsGroup,
                   text = r.labels[i], size = 9, anchor = "e")
    })
    # draw ovals
    ovalGroup <- l_layer_group(widget, "circle guides", parent = guidesGroup)
    ovalsLayer <- lapply(radius, function(l){
      xleft <- ybottom <- -l
      xright <- ytop <- l
      l_layer_oval(widget, y = c(xleft, xright), x = c(ybottom, ytop),
                   color = "", linecolor = widget['guidelines'], parent = ovalGroup)
    })
    # draw lines
    linesGroup <- l_layer_group(widget, "radius guides", parent = guidesGroup)
    linesLayer <- lapply(angles, function(l){
      xend <- maxRadius * sin(l)
      yend <- maxRadius * cos(l)
      l_layer_line(widget, y = c(0, xend), x = c(0, yend),
                   color = widget['guidelines'], parent = linesGroup)
    })
    # draw lines label
    line_labelsGroup <- l_layer_group(widget, "line labels", parent = guidesGroup)
    linesLabelLayer <- lapply(1:length(textAngles), function(i){
      l_layer_text(widget, y = maxRadius * sin(textAngles[i]), x = maxRadius * cos(textAngles[i]),
                   color = widget['foreground'], parent = line_labelsGroup, text = theta.labels[i], size = 9)
    })
  } else {
    # draw radius label
    labelsGroup <- l_layer_group(widget, "radius labels", parent = guidesGroup)
    textRadiiLayer <- lapply(1:length(textRadius), function(i){
      l_layer_text(widget, x = extendRange[1], y = textRadius[i], color = widget['foreground'], parent = labelsGroup,
                   text = r.labels[i], size = 9, anchor = "e")
    })
    # draw ovals
    ovalGroup <- l_layer_group(widget, "circle guides", parent = guidesGroup)
    ovalsLayer <- lapply(radius, function(l){
      xleft <- ybottom <- -l
      xright <- ytop <- l
      l_layer_oval(widget, x = c(xleft, xright), y = c(ybottom, ytop),
                   color = "", linecolor = widget['guidelines'], parent = ovalGroup)
    })
    # draw lines
    linesGroup <- l_layer_group(widget, "radius guides", parent = guidesGroup)
    linesLayer <- lapply(angles, function(l){
      xend <- maxRadius * sin(l)
      yend <- maxRadius * cos(l)
      l_layer_line(widget, x = c(0, xend), y = c(0, yend),
                   color = widget['guidelines'], parent = linesGroup)
    })
    # draw lines label
    line_labelsGroup <- l_layer_group(widget, "line labels", parent = guidesGroup)
    linesLabelLayer <- lapply(1:length(textAngles), function(i){
      l_layer_text(widget, x = maxRadius * sin(textAngles[i]), y = maxRadius * cos(textAngles[i]),
                   color = widget['foreground'], parent = line_labelsGroup, text = theta.labels[i], size = 9)
    })
  }

  guidesGroup
}
