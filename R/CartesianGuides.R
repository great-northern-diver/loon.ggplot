
CartesianGuides <- function(widget, ggplotPanel_params, swapAxes, theme){

  panel.background_fill <- if(is.null(theme$panel.background$fill) & length(theme) == 0)  {
    widget['guidesBackground']
  } else hex6to12(theme$panel.background$fill)

  panel.guideline_color <- if(is.null(theme$panel.grid$colour)) {
    widget['guidelines']
  } else hex6to12(theme$panel.grid$colour)

  axis.text_color <- if(is.null(theme$axis.text$colour)) {
    widget['foreground']
  } else hex6to12(theme$axis.text$colour)
  # as.loon_axisSize is in the file polarGuides.R
  axis.text_size <- as.loon_axisSize(as.numeric(theme$axis.text$size))

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
                                 color = panel.background_fill,
                                 linecolor = "",
                                 parent = guidesGroup)
  # lines
  xlinesGroup <- l_layer_group(widget, "x guide lines", parent = guidesGroup)
  xlinesLayer <- lapply(x.minor_source,
                        function(i){
                          l_layer_line(widget,
                                       x = rep(i, 2),
                                       y = y.range,
                                       color = panel.guideline_color,
                                       parent = xlinesGroup)
                        })

  ylinesGroup <- l_layer_group(widget, "y guide lines", parent = guidesGroup)
  ylinesLayer <- lapply(y.minor_source,
                        function(i){
                          l_layer_line(widget,
                                       x = x.range,
                                       y = rep(i,2),
                                       color = panel.guideline_color,
                                       parent = ylinesGroup)
                        })
  # axis
  if(length(theme$axis.text) != 0 | length(theme) == 0) {
    xlabelGroup <- l_layer_group(widget, "x axis", parent = guidesGroup)
    xlinesLayer <- lapply(seq_len(length(x.major_source)),
                          function(i){
                            l_layer_text(widget,
                                         x = x.major_source[i],
                                         y = extend.yrange[1],
                                         color = axis.text_color,
                                         parent = xlabelGroup,
                                         text = x.labels[i],
                                         size = axis.text_size,
                                         anchor = "s")
                          })
    ylabelGroup <- l_layer_group(widget, "y axis", parent = guidesGroup)
    ylinesLayer <- lapply(seq_len(length(y.major_source)),
                          function(i){
                            l_layer_text(widget,
                                         x = extend.xrange[1],
                                         y = y.major_source[i],
                                         color = axis.text_color,
                                         parent = ylabelGroup,
                                         text = y.labels[i],
                                         size = axis.text_size,
                                         anchor = "w")
                          })

  }

  # set border if it has
  if(length(theme$panel.border) != 0) {
    panel.border_color <- hex6to12(theme$panel.border$colour)
    borderGroup <- l_layer_group(widget, "borders", parent = guidesGroup)
    borderLayer <- l_layer_lines(widget,
                                 x = list(rep(x.range[1],2),  x.range, rep(x.range[2],2), rev(x.range)),
                                 y = list(y.range, rep(y.range[2],2), rev(y.range), rep(y.range[1],2)),
                                 color = panel.border_color,
                                 parent = borderGroup)
  }

  guidesGroup
}
