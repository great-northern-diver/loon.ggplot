
CartesianGuides <- function(widget, ggplotPanelParams, swapAxes, theme){

  panel.background_fill <- theme$panel.background$fill %||% widget['guidesBackground']

  panel.guideline_color <- theme$panel.grid$colour %||% widget['guidelines']

  axis.text_color <- theme$axis.text$colour %||% widget['foreground']
  # as.loon_axisSize is in the file polarGuides.R

  axis.text_size <- as.loon_axisSize(as.numeric(theme$axis.text$size))

  if (swapAxes) {

    x.range <- ggplotPanelParams$y.range
    y.range <- ggplotPanelParams$x.range

    # x labels
    x.major_source <- ggplotPanelParams$y.major_source
    x.labels <- ggplotPanelParams$y.labels

    # y labels
    y.major_source <- ggplotPanelParams$x.major_source
    y.labels <- ggplotPanelParams$x.labels

    # drawing lines
    x.minor_source <- ggplotPanelParams$y.minor_source %||% x.major_source
    y.minor_source <- ggplotPanelParams$x.minor_source %||% y.major_source

  } else {

    x.range <- ggplotPanelParams$x.range
    y.range <- ggplotPanelParams$y.range

    # x labels
    x.major_source <- ggplotPanelParams$x.major_source
    x.labels <- ggplotPanelParams$x.labels %||% ggplotPanelParams$x.sec$limits
    if(is.numeric(x.labels)) x.labels <- x.major_source

    # y labels
    y.major_source <- ggplotPanelParams$y.major_source
    y.labels <- ggplotPanelParams$y.labels %||% ggplotPanelParams$y.sec$limits
    if(is.numeric(y.labels)) y.labels <- y.major_source

    # drawing lines
    x.minor_source <- ggplotPanelParams$x.minor_source %||% x.major_source

    y.minor_source <- ggplotPanelParams$y.minor_source %||% y.major_source


  }
  extend.xrange <- grDevices::extendrange(x.range)
  extend.yrange <- grDevices::extendrange(y.range)

  # set group
  guidesGroup <- loon::l_layer_group(widget, "CartesianGuides Guides")
  # draw grey background
  rectLayer <- loon::l_layer_rectangle(widget,
                                       x = x.range,
                                       y = y.range,
                                       color = panel.background_fill,
                                       linecolor = "",
                                       parent = guidesGroup)
  # lines
  xlinesGroup <- loon::l_layer_group(widget, "x guide lines", parent = guidesGroup)
  xlinesLayer <- lapply(x.minor_source,
                        function(i){
                          loon::l_layer_line(widget,
                                             x = rep(i, 2),
                                             y = y.range,
                                             color = panel.guideline_color,
                                             parent = xlinesGroup)
                        })

  ylinesGroup <- loon::l_layer_group(widget, "y guide lines", parent = guidesGroup)
  ylinesLayer <- lapply(y.minor_source,
                        function(i){
                          loon::l_layer_line(widget,
                                             x = x.range,
                                             y = rep(i,2),
                                             color = panel.guideline_color,
                                             parent = ylinesGroup)
                        })
  # axis
  if(length(theme$axis.text) != 0 || length(theme) == 0) {

    xlabelGroup <- loon::l_layer_group(widget, "x axis", parent = guidesGroup)
    xlinesLayer <- lapply(seq_len(length(x.major_source)),
                          function(i){
                            if(!is.na(x.major_source[i]))
                              loon::l_layer_text(widget,
                                                 x = x.major_source[i],
                                                 y = extend.yrange[1],
                                                 color = axis.text_color,
                                                 parent = xlabelGroup,
                                                 text = x.labels[i] %||% x.major_source[i],
                                                 size = axis.text_size,
                                                 anchor = "s")
                          })

    ylabelGroup <- loon::l_layer_group(widget, "y axis", parent = guidesGroup)
    ylinesLayer <- lapply(seq_len(length(y.major_source)),
                          function(i){
                            if(!is.na(y.major_source[i]))
                              loon::l_layer_text(widget,
                                                 x = extend.xrange[1],
                                                 y = y.major_source[i],
                                                 color = axis.text_color,
                                                 parent = ylabelGroup,
                                                 text = y.labels[i] %||% y.major_source[i],
                                                 size = axis.text_size,
                                                 anchor = "w")
                          })

  }

  # set border if it has
  if(length(theme$panel.border) != 0) {
    panel.border_color <- hex6to12(theme$panel.border$colour)
    borderGroup <- loon::l_layer_group(widget, "borders", parent = guidesGroup)
    borderLayer <- loon::l_layer_lines(widget,
                                       x = list(rep(x.range[1],2),  x.range, rep(x.range[2],2), rev(x.range)),
                                       y = list(y.range, rep(y.range[2],2), rev(y.range), rep(y.range[1],2)),
                                       color = panel.border_color,
                                       parent = borderGroup)
  }

  guidesGroup
}
