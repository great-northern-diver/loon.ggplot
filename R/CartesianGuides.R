
CartesianGuides <- function(widget, ggplotPanel_params, swapAxes){


  if (swapAxes) {

    x.range <- ggplotPanel_params$y.range
    y.range <- ggplotPanel_params$x.range

    # x labels
    x.major_source <- ggplotPanel_params$y.major_source
    x.labels <- ggplotPanel_params$y.labels

    # y labels
    y.major_source <- ggplotPanel_params$x.major_source
    y.labels <- ggplotPanel_params$x.labels

    # drawing lines
    x.minor_source <- if(is.null(ggplotPanel_params$y.minor_source)) {
      x.major_source
    } else {ggplotPanel_params$y.minor_source}

    y.minor_source <- if(is.null(ggplotPanel_params$x.minor_source)) {
      y.major_source
    } else {ggplotPanel_params$x.minor_source}

  } else {

    x.range <- ggplotPanel_params$x.range
    y.range <- ggplotPanel_params$y.range

    # x labels
    x.major_source <- ggplotPanel_params$x.major_source
    x.labels <- ggplotPanel_params$x.labels

    # y labels
    y.major_source <- ggplotPanel_params$y.major_source
    y.labels <- ggplotPanel_params$y.labels

    # drawing lines
    x.minor_source <- if(is.null(ggplotPanel_params$x.minor_source)) {
      x.major_source
    } else {ggplotPanel_params$x.minor_source}

    y.minor_source <- if(is.null(ggplotPanel_params$y.minor_source)) {
      y.major_source
    } else {ggplotPanel_params$y.minor_source}

  }
  extend.xrange <- grDevices::extendrange(x.range)
  extend.yrange <- grDevices::extendrange(y.range)

  # set group
  guidesGroup <- l_layer_group(widget, "CartesianGuides Guides")
  # draw grey background
  rectLayer <- l_layer_rectangle(widget,
                                 x = x.range,
                                 y = y.range,
                                 color = widget['guidesBackground'],
                                 linecolor = "",
                                 parent = guidesGroup)
  # lines
  xlinesGroup <- l_layer_group(widget, "x guide lines", parent = guidesGroup)
  xlinesLayer <- lapply(x.minor_source, function(i){
    l_layer_line(widget, x = rep(i, 2), y = y.range, color = widget['guidelines'], parent = xlinesGroup)
  })

  ylinesGroup <- l_layer_group(widget, "y guide lines", parent = guidesGroup)
  ylinesLayer <- lapply(y.minor_source, function(i){
    l_layer_line(widget, x = x.range, y = rep(i,2), color = widget['guidelines'], parent = ylinesGroup)
  })
  # labels
  xlabelGroup <- l_layer_group(widget, "x labels", parent = guidesGroup)
  xlinesLayer <- lapply(seq_len(length(x.major_source)), function(i){
    l_layer_text(widget, x = x.major_source[i], y = extend.yrange[1], color = widget['foreground'], parent = xlabelGroup,
                 text = x.labels[i], size = 9, anchor = "s")
  })
  ylabelGroup <- l_layer_group(widget, "y labels", parent = guidesGroup)
  ylinesLayer <- lapply(seq_len(length(y.major_source)), function(i){
    l_layer_text(widget, x = extend.xrange[1], y = y.major_source[i], color = widget['foreground'], parent = ylabelGroup,
                 text = y.labels[i], size = 9, anchor = "w")
  })

  guidesGroup
}
