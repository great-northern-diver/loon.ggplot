loonHistogram <- function(ggBuild, ggLayout_start_pos, ggLayout, ggplotPanel_params, ggplotObject, ggLabels,
                          active_geomLayers, panelIndex, column_names, dataFrame, mapping.x, mapping.y, wrap.num,
                          toplevel, subtitle, showGuides, showScales, swapAxes, linkingKey, args){
  # set binwidth
  hist_data <- ggBuild$data[[active_geomLayers]]
  binwidth_vec <- hist_data[hist_data$PANEL == panelIndex, ]$xmax - hist_data[hist_data$PANEL == panelIndex, ]$xmin
  binwidth <- binwidth_vec[!is.na(binwidth_vec)][1]
  hist_x <- as.numeric(with(dataFrame, eval(parse(text = mapping.x))))
  # one facet
  if (wrap.num == 0) {
    isPanel_i.hist_x <- rep(TRUE, length(hist_x))
  } else {
    # multiple facets
    panel_i.list <- lapply((1:wrap.num + ggLayout_start_pos),
                           function(j) {
                             c(names(ggLayout[panelIndex, ])[j], as.character(ggLayout[panelIndex, j]))
                           })
    isPanel_i.hist_x_TorF <- lapply(seq_len(length(panel_i.list)),
                                    function(j){
                                      unlist(dataFrame[, panel_i.list[[j]][1]]) == panel_i.list[[j]][2]
                                    })
    # one condition or multiple conditions; "if else" is not necessary, but for faster speed
    isPanel_i.hist_x <- if (length(isPanel_i.hist_x_TorF) == 1) {
      isPanel_i.hist_x_TorF[[1]]
    } else {
      sapply(seq_len(length(hist_x)),
             function(j) {
               all(sapply(seq_len(length(isPanel_i.hist_x_TorF)),
                          function(l){
                            isPanel_i.hist_x_TorF[[l]][j]
                          }))
             })
    }
  }
  hist_values <- hist_x[isPanel_i.hist_x]
  # histogram start value, end value
  start_value <- min(hist_data[hist_data$PANEL == panelIndex, ]$xmin[hist_data[hist_data$PANEL == panelIndex, ]$ymax != 0],
                     na.rm = TRUE)
  end_value <- max(hist_data[hist_data$PANEL == panelIndex, ]$xmax, na.rm = TRUE)
  # any x y limit?
  x.limits <- ggBuild$layout$panel_scales_x[[active_geomLayers]]$limits
  y.limits <- ggBuild$layout$panel_scales_y[[active_geomLayers]]$limits
  in_x.limits <- in_y.limits <- rep(TRUE, length(hist_values))

  if (!is.null(x.limits)) {
    if(is.na(x.limits[1])) x.limits[1] <- ggplotPanel_params[[panelIndex]]$x.range[1]
    if(is.na(x.limits[2])) x.limits[2] <- ggplotPanel_params[[panelIndex]]$x.range[2]
    in_x.limits <- hist_values > x.limits[1] & hist_values < x.limits[2]
  }

  if (!is.null(y.limits)) {
    if(is.na(y.limits[1])) y.limits[1] <- max(0, ggplotPanel_params[[panelIndex]]$y.range[1])
    if(is.na(y.limits[2])) y.limits[2] <- ggplotPanel_params[[panelIndex]]$y.range[2]
    bins <- 0
    while ((start_value + bins * binwidth) <= end_value) {

      bin_id <- if (bins == 0) {
        which((start_value + bins * binwidth <= hist_values &
                 start_value + (bins + 1) * binwidth >= hist_values) == TRUE)
      } else {
        which((start_value + bins * binwidth < hist_values &
                 start_value + (bins + 1) * binwidth >= hist_values) == TRUE)
      }
      bin_height <- length(bin_id)
      if (bin_height != 0) {
        if(bin_height < y.limits[1] | bin_height > y.limits[2]) {
          in_y.limits[bin_id] <- FALSE
        }
      }
      bins <- bins + 1
    }
  }
  in_limits <- in_x.limits & in_y.limits
  # hist_values should be in the x y limits
  hist_values <- hist_values[in_limits]

  # reset the minimum "hist_values" to be the start value
  hist_values[which(hist_values == min(hist_values))[1]] <- start_value

  color <- hex6to12(hist_data$fill[1])
  colorStackingOrder <- "selected"
  # set stack color
  if (!is.null(ggplotObject$labels$fill)) {
    # fill color bin?
    if (ggplotObject$labels$fill != "fill") {

      color_var <- unlist(
        dataFrame[, which(stringr::str_detect(ggplotObject$labels$fill, column_names) == TRUE)]
      )
      panel_i.color_var <- color_var[isPanel_i.hist_x][in_limits]
      fill_color <- hex6to12(unique(hist_data$fill))
      levels <- rev(levels(as.factor(color_var)))
      if (length(fill_color) == length(levels)) {
        color <- rep(NA, length(hist_values))
        for(j in seq_len(length(levels))){
          color[which(panel_i.color_var %in% levels[j])] <- fill_color[j]
        }
        colorStackingOrder <- c("selected", fill_color)
      }
    }
  }

  # show outline color or not
  showOutlines <- if (any(!hex6to12(hist_data$colour) %in% "" )) TRUE else FALSE
  # set linkingKey
  linkingKey <- linkingKey[isPanel_i.hist_x][in_limits]
  # set yshows
  yshows <- "frequency"
  if (length(mapping.y) != 0) {
    if(any(str_detect(as.character(mapping.y), "density"))) yshows <- "density"
  }
  if (!is.null(ggplotObject$layers[[active_geomLayers]]$mapping$y)) {
    if(any(str_detect(as.character(ggplotObject$layers[[active_geomLayers]]$mapping$y), "density"))) yshows <- "density"
  }
  # loon histogram
  l_hist(parent = toplevel,
         x = hist_values,
         color = color,
         title = subtitle,
         binwidth = binwidth + 1e-6, # need more thoughts
         xlabel = ggLabels$xlabel,
         ylabel = ggLabels$ylabel,
         showGuides = showGuides,
         showScales = showScales,
         showOutlines = showOutlines,
         swapAxes = swapAxes,
         colorStackingOrder = colorStackingOrder,
         yshows = yshows,
         linkingKey = linkingKey,
         showStackedColors = TRUE,
         linkingGroup = args$linkingGroup)
}


