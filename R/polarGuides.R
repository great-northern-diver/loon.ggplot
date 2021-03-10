
polarGuides <- function(widget, ggplotPanelParams, swapAxes, theme){

  panel.background_fill <- if(is.null(theme$panel.background$fill) & length(theme) == 0)  {
    widget['guidesBackground']
  } else hex6to12(theme$panel.background$fill)

  panel.guideline_color <- if(is.null(theme$panel.grid$colour)) {
    widget['guidelines']
  } else hex6to12(theme$panel.grid$colour)

  axis.text_color <- if(is.null(theme$axis.text$colour)) {
    widget['foreground']
  } else hex6to12(theme$axis.text$colour)

  axis.text_size <- as.loon_axisSize(as.numeric(theme$axis.text$size))

  theta.range <- ggplotPanelParams$theta.range
  r.range <- ggplotPanelParams$r.range

  # theta labels
  theta.major <- ggplotPanelParams$theta.major
  theta.labels <- ggplotPanelParams$theta.labels

  # radius labels
  r.major <- ggplotPanelParams$r.major
  r.labels <- ggplotPanelParams$r.labels

  # drawing lines
  theta.minor <- if(is.null(ggplotPanelParams$theta.minor)) theta.major else ggplotPanelParams$theta.minor

  # drawing ovals
  if(length(r.major) >= 1) {
    r.minor <- c(r.major, max(r.range, na.rm = TRUE) + diff(r.range, na.rm = TRUE)/8)
  } else r.minor <- c(min(r.range, na.rm = TRUE), max(r.range, na.rm = TRUE) + diff(r.range, na.rm = TRUE)/8)

  radii <- (r.minor - r.range[1])/ diff(r.range, na.rm = TRUE)
  textRadii <- (r.major - r.range[1])/ diff(r.range, na.rm = TRUE)
  maxRadius <- max(radii, na.rm = TRUE)
  angles <- 2 * pi * (theta.minor - theta.range[1])/diff(theta.range, na.rm = TRUE)
  textAngles <- 2 * pi * (theta.major - theta.range[1])/diff(theta.range, na.rm = TRUE)
  # set group
  guidesGroup <- loon::l_layer_group(widget, "Polar Guides")
  # draw background
  range <- c(-maxRadius, maxRadius)
  extendRange <- grDevices::extendrange(range)
  rectLayer <- loon::l_layer_rectangle(widget,
                                       x = extendRange,
                                       y = extendRange,
                                       color = panel.background_fill,
                                       linecolor = "",
                                       parent = guidesGroup)
  if (swapAxes) {
    # draw ovals
    ovalGroup <- loon::l_layer_group(widget, "theta guides", parent = guidesGroup)
    ovalsLayer <- lapply(radii,
                         function(radius){
                           xleft <- ybottom <- -radius
                           xright <- ytop <- radius
                           loon::l_layer_oval(widget,
                                              y = c(xleft, xright),
                                              x = c(ybottom, ytop),
                                              color = "",
                                              linecolor = panel.guideline_color,
                                              parent = ovalGroup)
                         })
    # draw lines
    linesGroup <- loon::l_layer_group(widget, "radius guides", parent = guidesGroup)
    linesLayer <- lapply(angles,
                         function(angle){
                           xend <- maxRadius * sin(angle)
                           yend <- maxRadius * cos(angle)
                           loon::l_layer_line(widget,
                                              y = c(0, xend),
                                              x = c(0, yend),
                                              color = panel.guideline_color,
                                              parent = linesGroup)
                         })
    # axis
    if(length(theme$axis.text) != 0 | length(theme) == 0) {
      # draw lines aixs
      line_labelsGroup <- loon::l_layer_group(widget, "theta axis", parent = guidesGroup)
      linesLabelLayer <- lapply(1:length(textAngles),
                                function(i){
                                  loon::l_layer_text(widget,
                                                     y = maxRadius * sin(textAngles[i]),
                                                     x = maxRadius * cos(textAngles[i]),
                                                     color = axis.text_color,
                                                     parent = line_labelsGroup,
                                                     text = theta.labels[i],
                                                     size = axis.text_size)
                                })
      # draw radius axis
      labelsGroup <- loon::l_layer_group(widget, "radius axis", parent = guidesGroup)
      textRadiiLayer <- lapply(1:length(textRadii),
                               function(i){
                                 loon::l_layer_text(widget,
                                                    x = textRadii[i],
                                                    y = extendRange[1],
                                                    color = axis.text_color,
                                                    parent = labelsGroup,
                                                    text = r.labels[i],
                                                    size = axis.text_size,
                                                    anchor = "e")
                               })
    }
  } else {
    # draw ovals
    ovalGroup <- loon::l_layer_group(widget, "theta guides", parent = guidesGroup)
    ovalsLayer <- lapply(radii,
                         function(radius){
                           xleft <- ybottom <- -radius
                           xright <- ytop <- radius
                           loon::l_layer_oval(widget,
                                              x = c(xleft, xright),
                                              y = c(ybottom, ytop),
                                              color = "",
                                              linecolor = panel.guideline_color,
                                              parent = ovalGroup)
                         })
    # draw lines
    linesGroup <- loon::l_layer_group(widget, "radius guides", parent = guidesGroup)
    linesLayer <- lapply(angles,
                         function(angle){
                           xend <- maxRadius * sin(angle)
                           yend <- maxRadius * cos(angle)
                           loon::l_layer_line(widget,
                                              x = c(0, xend),
                                              y = c(0, yend),
                                              color = panel.guideline_color,
                                              parent = linesGroup)
                         })
    # axis
    if(length(theme$axis.text) != 0 | length(theme) == 0) {
      # draw radius label
      labelsGroup <- loon::l_layer_group(widget, "radius axis", parent = guidesGroup)
      textRadiiLayer <- lapply(1:length(textRadii),
                               function(i){
                                 loon::l_layer_text(widget,
                                                    x = extendRange[1],
                                                    y = textRadii[i],
                                                    color = axis.text_color,
                                                    parent = labelsGroup,
                                                    text = r.labels[i],
                                                    size = axis.text_size,
                                                    anchor = "e")
                               })
      # draw lines label
      line_labelsGroup <- loon::l_layer_group(widget, "theta axis", parent = guidesGroup)
      linesLabelLayer <- lapply(1:length(textAngles),
                                function(i){
                                  loon::l_layer_text(widget,
                                                     x = maxRadius * sin(textAngles[i]),
                                                     y = maxRadius * cos(textAngles[i]),
                                                     color = axis.text_color,
                                                     parent = line_labelsGroup,
                                                     text = theta.labels[i],
                                                     size = axis.text_size)
                                })
    }
  }
  # set border if it has
  if(length(theme$panel.border) != 0) {
    panel.border_color <- hex6to12(theme$panel.border$colour)
    borderGroup <- loon::l_layer_group(widget, "borders", parent = guidesGroup)
    borderLayer <- loon::l_layer_lines(widget,
                                       x = list(rep(extendRange[1],2),  extendRange, rep(extendRange[2],2), rev(extendRange)),
                                       y = list(extendRange, rep(extendRange[2],2), rev(extendRange), rep(extendRange[1],2)),
                                       color = panel.border_color,
                                       parent = borderGroup)
  }

  guidesGroup
}


as.loon_axisSize<- function(size) {
  if(length(size) == 0) 9
  else {
    if(is.na(size)) {
      9
    } else 10*size
  }
}
