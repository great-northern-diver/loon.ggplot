#' @importFrom dplyr group_by summarise n

loonHistogram <- function(ggBuild, ggLayout, layout, ggplotPanel_params, ggObj,
                          activeGeomLayers, panelIndex, column_names, dataFrame, mapping, numOfSubtitles,
                          parent, showGuides, showScales, swapAxes, linkingKey, showLabels,
                          xlabel, ylabel, loonTitle, is_facet_wrap, is_facet_grid) {
  x <- ggObj$layers[[activeGeomLayers]]$stat
  UseMethod("loonHistogram", x)
}



loonHistogram.StatBin <- function(ggBuild, ggLayout, layout, ggplotPanel_params, ggObj,
                                  activeGeomLayers, panelIndex, column_names, dataFrame,
                                  mapping, numOfSubtitles,
                                  parent, showGuides, showScales, swapAxes, linkingKey, showLabels,
                                  xlabel, ylabel, loonTitle, is_facet_wrap, is_facet_grid) {

  # set binwidth
  hist_data <- ggBuild$data[[activeGeomLayers]]
  binwidth_vec <- hist_data[hist_data$PANEL == panelIndex, ]$xmax - hist_data[hist_data$PANEL == panelIndex, ]$xmin
  binwidth <- binwidth_vec[!is.na(binwidth_vec)][1]
  hist_x <- as.numeric(rlang::eval_tidy(rlang::quo(!!mapping$x),  dataFrame))
  # facet index
  facet_id <- catch_facet_id(numOfSubtitles, hist_x, is_facet_wrap, is_facet_grid,
                             layout, ggLayout, panelIndex, dataFrame)
  hist_values <- hist_x[facet_id]

  # any x y limit?
  x.limits <- ggBuild$layout$panel_scales_x[[1]]$limits
  y.limits <- ggBuild$layout$panel_scales_y[[1]]$limits

  bin_info <- catch_bin_info(hist_values, hist_data, x.limits, y.limits,
                             facet_id, in_limits, dataFrame, ggObj, column_names,
                             activeGeomLayers,
                             panelIndex = panelIndex,
                             ggplotPanel_params = ggplotPanel_params,
                             binwidth = binwidth)

  hist_values <- bin_info$hist_values
  fill <- bin_info$fill
  colorStackingOrder <- bin_info$colorStackingOrder
  in_limits <- bin_info$in_limits

  # show outline color or not
  if (any(!hex6to12(hist_data$colour) %in% "" )) {
    showOutlines <- TRUE
    colorOutline <- hex6to12(Filter(function(k) k != "", unique(hex6to12(hist_data$colour)))[1])
  } else {
    showOutlines <- FALSE
    colorOutline <- hex6to12("black")
  }

  # loon histogram
  loon::l_hist(parent = parent,
               x = hist_values,
               color = fill,
               binwidth = binwidth + 1e-6, # need more thoughts
               showLabels = showLabels,
               showGuides = showGuides,
               showScales = showScales,
               showOutlines = showOutlines,
               colorOutline = colorOutline,
               swapAxes = swapAxes,
               colorStackingOrder = colorStackingOrder,
               yshows = get_yshows(mapping,
                                   layMapping  = ggObj$layers[[activeGeomLayers]]$mapping),
               linkingKey = linkingKey[facet_id][in_limits],
               showStackedColors = TRUE,
               xlabel = if(is.null(xlabel)) "" else xlabel,
               ylabel = if(is.null(ylabel)) "" else ylabel,
               title = loonTitle)
}

loonHistogram.StatCount <- function(ggBuild, ggLayout, layout, ggplotPanel_params, ggObj,
                                    activeGeomLayers, panelIndex, column_names, dataFrame,
                                    mapping, numOfSubtitles,
                                    parent, showGuides, showScales, swapAxes, linkingKey, showLabels,
                                    xlabel, ylabel, loonTitle, is_facet_wrap, is_facet_grid) {

  hist_data <- ggBuild$data[[activeGeomLayers]]
  hist_x <- rlang::eval_tidy(rlang::quo(!!mapping$x),  dataFrame)
  # grab the facet index
  facet_id <- catch_facet_id(numOfSubtitles, hist_x, is_facet_wrap, is_facet_grid,
                             layout, ggLayout, panelIndex, dataFrame)
  hist_values <- hist_x[facet_id]
  remove(hist_x)

  # any x y limit?
  # x.limits is a char
  x.limits <- ggBuild$layout$panel_scales_x[[1]]$limits
  # y.limits is a num
  y.limits <- ggBuild$layout$panel_scales_y[[1]]$limits

  bin_info <- catch_bin_info(hist_values, hist_data, x.limits, y.limits,
                             facet_id, in_limits, dataFrame, ggObj, column_names,
                             activeGeomLayers)

  hist_values <- bin_info$hist_values
  fill <- bin_info$fill
  colorStackingOrder <- bin_info$colorStackingOrder
  in_limits <- bin_info$in_limits

  # show outline color or not
  if (any(!hex6to12(hist_data$colour) %in% "" )) {
    showOutlines <- TRUE
    colorOutline <- hex6to12(Filter(function(k) k != "", unique(hex6to12(hist_data$colour)))[1])
  } else {
    showOutlines <- FALSE
    colorOutline <- hex6to12("black")
  }

  # loon histogram
  loon::l_hist(parent = parent,
               x = hist_values,
               color = fill,
               showLabels = showLabels,
               showGuides = showGuides,
               showScales = showScales,
               showOutlines = showOutlines,
               colorOutline = colorOutline,
               colorStackingOrder = colorStackingOrder,
               swapAxes = swapAxes,
               origin = 0.5,
               yshows = get_yshows(mapping,
                                   layMapping  = ggObj$layers[[activeGeomLayers]]$mapping),
               linkingKey = linkingKey[facet_id][in_limits],
               showStackedColors = TRUE,
               xlabel = if(is.null(xlabel)) "" else xlabel,
               ylabel = if(is.null(ylabel)) "" else ylabel,
               title = loonTitle)
}

# catch_bin_info is mainly used to catch the order of the stacked bins
# when users set ylim or xlim, the bins would change.
catch_bin_info <- function(hist_values, hist_data, x.limits, y.limits,
                           facet_id, in_limits, dataFrame,
                           ggObj, column_names, activeGeomLayers, ...) {

  x <- ggObj$layers[[activeGeomLayers]]$stat
  UseMethod("catch_bin_info", x)
}

catch_bin_info.StatBin <- function(hist_values, hist_data, x.limits, y.limits,
                                   facet_id, in_limits, dataFrame,
                                   ggObj, column_names, activeGeomLayers, ...) {
  args <- list(...)
  panelIndex <- args$panelIndex
  ggplotPanel_params <- args$ggplotPanel_params
  binwidth <- args$binwidth

  # histogram start value, end value
  start_value <- min(hist_data[hist_data$PANEL == panelIndex, ]$xmin[hist_data[hist_data$PANEL == panelIndex, ]$ymax != 0],
                     na.rm = TRUE)
  end_value <- max(hist_data[hist_data$PANEL == panelIndex, ]$xmax, na.rm = TRUE)
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

  # reset the minimum "hist_values" to be the new start value
  hist_values[which(hist_values == min(hist_values))[1]] <- start_value

  # bin fills
  uni_fill <- hex6to12(unique(hist_data$fill))
  fill <- rep(uni_fill, length(hist_values))
  # set stack color
  if (!is.null(ggObj$labels$fill)) {
    # fill: panel i && in limits
    fill_var <- unlist(
      dataFrame[, which(stringr::str_detect(ggObj$labels$fill, column_names) == TRUE)]
    )[facet_id][in_limits]
    levels <- rev(levels(as.factor(fill_var)))
    if (length(uni_fill) == length(levels)) {
      fill <- rep(NA, length(hist_values))
      for(j in seq_len(length(levels))){
        fill[which(fill_var %in% levels[j])] <- uni_fill[j]
      }
    }
  }

  colorStackingOrder <- c("selected", uni_fill)

  list(
    fill = fill,
    hist_values = hist_values,
    colorStackingOrder = colorStackingOrder,
    in_limits = in_limits
  )
}

catch_bin_info.StatCount <- function(hist_values, hist_data, x.limits, y.limits,
                                     facet_id, in_limits, dataFrame,
                                     ggObj, column_names, activeGeomLayers, ...) {

  in_x.limits <- in_y.limits <- rep(TRUE, length(hist_values))
  colorStackingOrder <- 'selected'

  if (!is.null(x.limits)) {
    in_x.limits <- hist_values %in% x.limits
  }

  position <- ggObj$layers[[activeGeomLayers]]$position
  reverse <- position$reverse
  operations <- position_operation(position,
                                   hist_values,
                                   sep = "&",
                                   ggObj = ggObj,
                                   dataFrame = dataFrame,
                                   column_names = column_names,
                                   facet_id = facet_id)

  fill_var <- operations$fill_var
  hist_values <- operations$hist_values

  if(!is.null(ggObj$labels$fill) && ggObj$labels$fill != ggObj$labels$x && is(position, "PositionStack")) {

    y <- hist_data$y
    levels_bar <- levels(factor(hist_values))

    # ylim is set some y is dropped
    if(any(is.na(y))) {
      na_y <- is.na(y)
      drops <- levels_bar[hist_data$x[na_y]]
      drop_count <- hist_data$count[na_y]

      group_by_table <- data.frame(fill = fill_var, hist_values = hist_values) %>%
        dplyr::group_by(fill, hist_values) %>%
        dplyr::summarise(n = dplyr::n())

      drop_fill <- group_by_table$fill[(group_by_table$hist_values %in% drops) &
                                         (group_by_table$n %in% drop_count)]

      in_y.limits <- !((hist_values %in% drops) & (fill_var %in% drop_fill))
    }

    in_limits <- in_y.limits & in_x.limits
    hist_values <- hist_values[in_limits]
    fill_var <- fill_var[in_limits]
    fill_levels <- levels(as.factor(fill_var))
    if(is.null(reverse) || !reverse) {
      fill_levels <- rev(fill_levels)
    }

  } else {

    if (!is.null(y.limits)) {
      counts <- table(hist_values)
      name_counts <- names(counts)

      in_y.limits <- hist_values %in%
        name_counts[y.limits[1] <= counts & y.limits[2] >= counts]
    }
    in_limits <- in_y.limits & in_x.limits
    hist_values <- hist_values[in_limits]
    fill_levels <- levels(as.factor(fill_var))
    if(!is.null(reverse) && reverse) {
      fill_levels <- rev(fill_levels)
    }
  }

  uni_fill <- hex6to12(unique(hist_data$fill))

  if(length(uni_fill) == length(fill_levels)) {

    fill <- rep(NA, length(hist_values))
    for(j in seq_len(length(fill_levels))){
      fill[which(fill_var %in% fill_levels[j])] <- uni_fill[j]
    }
  } else {
    fill <- rep(uni_fill, length(hist_values))
  }

  colorStackingOrder <- c("selected", uni_fill)

  list(
    fill = fill,
    hist_values = hist_values,
    colorStackingOrder = colorStackingOrder,
    in_limits = in_limits
  )
}

# bar position operation
# PositionStack is stack the colored bins
# PositionDodge is place the stacked bar side by side
position_operation <- function(position, hist_values, sep = "&", ...) {
  UseMethod("position_operation", position)
}
position_operation.PositionStack <- function(position, hist_values, sep = "&", ...) {
  args <- list(...)
  ggObj <- args$ggObj
  dataFrame <- args$dataFrame
  column_names <- args$column_names
  facet_id <- args$facet_id

  if(!is.null(ggObj$labels$fill) && ggObj$labels$fill != ggObj$labels$x) {

    fill_var <- unlist(
      dataFrame[, which(stringr::str_detect(ggObj$labels$fill, column_names) == TRUE)]
    )[facet_id]

  } else {
    fill_var <- hist_values
  }

  list(
    hist_values = hist_values,
    fill_var = fill_var
  )
}

position_operation.PositionDodge <- function(position, hist_values, sep = "&", ...) {

  args <- list(...)
  ggObj <- args$ggObj
  dataFrame <- args$dataFrame
  column_names <- args$column_names
  facet_id <- args$facet_id

  if(!is.null(ggObj$labels$fill) && ggObj$labels$fill != ggObj$labels$x) {

    fill_var <- unlist(
      dataFrame[, which(stringr::str_detect(ggObj$labels$fill, column_names) == TRUE)]
    )[facet_id]

    if(length(hist_values) == length(fill_var)) {

      # TODO Class PositionDodge2 (with reverse = TRUE)
      # reverse <- position$reverse
      # if(!is.null(reverse) && reverse) {
      #
      #   # reorder levels
      #   hist_values_levels <- c(t(outer(levels(factor(hist_values)), rev(levels(factor(fill_var))), paste, sep = sep)))
      #   hist_values <- factor(paste(hist_values, fill_var, sep = sep))
      #   levels(hist_values) <- hist_values_levels[hist_values_levels %in% unique(hist_values)]
      #
      # } else {
      #   hist_values <- paste(hist_values, fill_var, sep = sep)
      # }
      hist_values <- paste(hist_values, fill_var, sep = sep)
      message("Viewport is changed. Set `l_scaleto_world` to get the world view")
    } else
      warning("The length does not match. It should not happen.", call. = FALSE)

  } else {
    fill_var <- hist_values
  }

  list(
    hist_values = hist_values,
    fill_var = fill_var
  )
}

# to get the right values in this facet
catch_facet_id <- function(numOfSubtitles, hist_x, is_facet_wrap, is_facet_grid,
                           layout, ggLayout, panelIndex, dataFrame) {

  # one facet
  if (numOfSubtitles == 0) {
    facet_id <- rep(TRUE, length(hist_x))
  } else {
    mapping.names_pos <-   if(is_facet_wrap) {
      which(colnames(layout) == names(ggLayout$facet_params$facets))
    } else if(is_facet_grid) {
      which(colnames(layout) == c(names(ggLayout$facet_params$rows),names(ggLayout$facet_params$cols)))
    } else NULL

    panel_i.list <- lapply(mapping.names_pos,
                           function(j) {
                             c(names(layout[panelIndex, ])[j], as.character(layout[panelIndex, j]))
                           }
    )
    facet_id_TorF <- lapply(seq_len(length(panel_i.list)),
                            function(j){
                              unlist(dataFrame[, panel_i.list[[j]][1]]) == panel_i.list[[j]][2]
                            }
    )
    # one condition or multiple conditions; "if else" is not necessary, but for faster speed
    facet_id <- if (length(facet_id_TorF) == 1) {
      facet_id_TorF[[1]]
    } else {
      sapply(seq_len(length(hist_x)),
             function(j) {
               all(sapply(seq_len(length(facet_id_TorF)),
                          function(l){
                            facet_id_TorF[[l]][j]
                          }))
             })
    }
  }

  facet_id
}

# density or frequency?
get_yshows <- function(mapping, layMapping) {

  if (!is.null(mapping$y)) {
    if(grepl("density", rlang::expr_text(mapping$y))) return("density")
  }
  if (!is.null(layMapping$y)) {
    if(grepl("density", rlang::expr_text(layMapping$y))) return("density")
  }
  return("frequency")
}
