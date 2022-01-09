l_histogram <- function(ggBuild, ggLayout, layout, ggplotPanelParams, ggObj,
                        activeGeomLayers, panelIndex, dataFrame, mapping, numOfSubtitles,
                        parent, showGuides, showScales, swapAxes, linkingKey, nDimStates,
                        showLabels, xlabel, ylabel, loonTitle, FacetWrap, FacetGrid) {
  x <- ggObj$layers[[activeGeomLayers]]$stat
  UseMethod("l_histogram", x)
}

l_histogram.StatBin <- function(ggBuild, ggLayout, layout, ggplotPanelParams, ggObj,
                                activeGeomLayers, panelIndex, dataFrame,
                                mapping, numOfSubtitles,
                                parent, showGuides, showScales, swapAxes, linkingKey, nDimStates,
                                showLabels, xlabel, ylabel, loonTitle, FacetWrap, FacetGrid) {

  # set binwidth
  ## panel i hist values
  hist_data <- ggBuild$data[[activeGeomLayers]]
  id <- hist_data$PANEL == panelIndex
  hist_data <- hist_data[id, ]
  flipped_aes <- any(hist_data$flipped_aes) %||% FALSE

  if(flipped_aes) {
    binwidth_vec <- hist_data$ymax - hist_data$ymin
    binwidth <- binwidth_vec[!is.na(binwidth_vec)][1]
    hist_x <- as.numeric(rlang::eval_tidy(rlang::quo(!!mapping$y),  dataFrame))
    # label swap
    temp <- xlabel
    xlabel <- ylabel
    ylabel <- temp
  } else {
    binwidth_vec <- hist_data$xmax - hist_data$xmin
    binwidth <- binwidth_vec[!is.na(binwidth_vec)][1]
    hist_x <- as.numeric(rlang::eval_tidy(rlang::quo(!!mapping$x),  dataFrame))
  }
  # facet index
  facet_id <- catch_facet_id(numOfSubtitles, hist_x, FacetWrap, FacetGrid,
                             layout, ggLayout, panelIndex, dataFrame)
  hist_values <- hist_x[facet_id]

  # any x y limit?
  x.limits <- ggBuild$layout$panel_scales_x[[1]]$limits
  y.limits <- ggBuild$layout$panel_scales_y[[1]]$limits

  bin_info <- catch_bin_info(hist_values, hist_data, flipped_aes, x.limits, y.limits,
                             facet_id, in_limits, dataFrame, ggObj,
                             activeGeomLayers,
                             panelIndex = panelIndex,
                             ggplotPanelParams = ggplotPanelParams,
                             binwidth = binwidth)

  hist_values <- bin_info$hist_values
  fill <- bin_info$fill
  colorStackingOrder <- bin_info$colorStackingOrder
  in_limits <- bin_info$in_limits

  # show outline color or not
  if (any(!hex6to12(hist_data$colour) %in% "" )) {
    showOutlines <- TRUE
    colorOutline <- Filter(function(k) k != "", unique(hex6to12(hist_data$colour)))[1]
  } else {
    showOutlines <- FALSE
    colorOutline <- "black"
  }

  if(swapAxes) {
    if(flipped_aes) swapAxes <- FALSE
    else swapAxes <- TRUE
  } else {
    if(flipped_aes) swapAxes <- TRUE
    else swapAxes <- FALSE
  }

  dat <- modify_n_dim_data(nDimStates,
                           data.frame(
                             x = hist_values,
                             color = hex6to12(fill),
                             linkingKey = linkingKey[facet_id][in_limits]
                           ),  id)

  histList <- remove_null(
    c(
      list(
        parent = parent,
        binwidth = binwidth, # need more thoughts
        showLabels = showLabels,
        showGuides = showGuides,
        showScales = showScales,
        showOutlines = showOutlines,
        colorOutline = colorOutline,
        swapAxes = swapAxes,
        colorStackingOrder = colorStackingOrder,
        yshows = get_yshows(mapping,
                            layMapping  = ggObj$layers[[activeGeomLayers]]$mapping,
                            flipped_aes),
        showStackedColors = TRUE,
        xlabel = if(is.null(xlabel)) "" else xlabel,
        ylabel = if(is.null(ylabel)) "" else ylabel,
        title = loonTitle
      ),
      dat
    ), as_list = FALSE)

  # loon histogram
  do.call(loon::l_hist, histList)
}

l_histogram.StatCount <- function(ggBuild, ggLayout, layout, ggplotPanelParams, ggObj,
                                  activeGeomLayers, panelIndex, dataFrame,
                                  mapping, numOfSubtitles,
                                  parent, showGuides, showScales, swapAxes, linkingKey, nDimStates,
                                  showLabels, xlabel, ylabel, loonTitle, FacetWrap, FacetGrid) {

  hist_data <- ggBuild$data[[activeGeomLayers]]
  id <- hist_data$PANEL == panelIndex
  hist_data <- hist_data[id, ]
  flipped_aes <- any(hist_data$flipped_aes) %||% FALSE

  if(flipped_aes) {
    hist_x <- rlang::eval_tidy(rlang::quo(!!mapping$y),  dataFrame)
    # label swap
    temp <- xlabel
    xlabel <- ylabel
    ylabel <- temp
  } else {
    hist_x <- rlang::eval_tidy(rlang::quo(!!mapping$x),  dataFrame)
  }

  # grab the facet index
  facet_id <- catch_facet_id(numOfSubtitles, hist_x, FacetWrap, FacetGrid,
                             layout, ggLayout, panelIndex, dataFrame)
  hist_values <- hist_x[facet_id]
  remove(hist_x)

  # any x y limit?
  # x.limits is a char
  x.limits <- ggBuild$layout$panel_scales_x[[1]]$limits
  # y.limits is a num
  y.limits <- ggBuild$layout$panel_scales_y[[1]]$limits

  bin_info <- catch_bin_info(hist_values = hist_values,
                             hist_data = hist_data,
                             flipped_aes = flipped_aes,
                             x.limits = x.limits, y.limits = y.limits,
                             facet_id = facet_id, in_limits = in_limits,
                             dataFrame = dataFrame, ggObj = ggObj,
                             activeGeomLayers = activeGeomLayers)

  hist_values <- bin_info$hist_values
  fill <- bin_info$fill
  colorStackingOrder <- bin_info$colorStackingOrder
  in_limits <- bin_info$in_limits

  # show outline color or not
  if (any(!(hist_data$colour) %in% "" )) {
    showOutlines <- TRUE
    colorOutline <- Filter(function(k) k != "", hex6to12(unique(hist_data$colour)))[1]
  } else {
    showOutlines <- FALSE
    colorOutline <- ("black")
  }

  if(swapAxes) {
    if(flipped_aes) swapAxes <- FALSE
    else swapAxes <- TRUE
  } else {
    if(flipped_aes) swapAxes <- TRUE
    else swapAxes <- FALSE
  }

  dat <- modify_n_dim_data(nDimStates,
                           data.frame(
                             x = hist_values,
                             color = hex6to12(fill),
                             linkingKey = linkingKey[facet_id][in_limits]
                           ),  id)

  histList <- remove_null(
    c(
      list(
        parent = parent,
        showLabels = showLabels,
        showGuides = showGuides,
        showScales = showScales,
        showOutlines = showOutlines,
        colorOutline = colorOutline,
        colorStackingOrder = colorStackingOrder,
        swapAxes = swapAxes,
        origin = 0.5,
        binwidth = 1,
        yshows = get_yshows(mapping,
                            layMapping  = ggObj$layers[[activeGeomLayers]]$mapping,
                            flipped_aes),
        showStackedColors = TRUE,
        xlabel = if(is.null(xlabel)) "" else xlabel,
        ylabel = if(is.null(ylabel)) "" else ylabel,
        title = loonTitle
      ),
      dat
    ), as_list = FALSE)

  # loon histogram
  do.call(loon::l_hist, histList)
}

# catch_bin_info is mainly used to catch the order of the stacked bins
# when users set ylim or xlim, the bins would change.
catch_bin_info <- function(hist_values, hist_data, flipped_aes = FALSE, x.limits, y.limits,
                           facet_id, in_limits, dataFrame,
                           ggObj, activeGeomLayers, ...) {

  x <- ggObj$layers[[activeGeomLayers]]$stat
  UseMethod("catch_bin_info", x)
}

catch_bin_info.StatBin <- function(hist_values, hist_data, flipped_aes, x.limits, y.limits,
                                   facet_id, in_limits, dataFrame,
                                   ggObj, activeGeomLayers, ...) {
  args <- list(...)
  panelIndex <- args$panelIndex
  ggplotPanelParams <- args$ggplotPanelParams
  binwidth <- args$binwidth

  position <- ggObj$layers[[activeGeomLayers]]$position
  reverse <- position$reverse %||% FALSE

  getBins <- function(start_value, end_value, binwidth, hist_values, limits, in_limits) {
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
        if(bin_height < limits[1] | bin_height > limits[2]) {
          in_limits[bin_id] <- FALSE
        }
      }
      bins <- bins + 1
    }
    return(
      list(
        bins = bins,
        in_limits = in_limits
      )
    )
  }

  # histogram start value, end value
  if(flipped_aes) {

    start_value <- min(hist_data$ymin[hist_data$xmax != 0],
                       na.rm = TRUE)
    end_value <- max(hist_data$ymax, na.rm = TRUE)
    in_x.limits <- in_y.limits <- rep(TRUE, length(hist_values))

    if (!is.null(y.limits)) {
      if(is.na(y.limits[1])) y.limits[1] <- ggplotPanelParams[[panelIndex]]$y.range[1]
      if(is.na(y.limits[2])) y.limits[2] <- ggplotPanelParams[[panelIndex]]$y.range[2]
      in_y.limits <- (hist_values > y.limits[1]) & (hist_values < y.limits[2])
    }

    if (!is.null(x.limits)) {
      if(is.na(x.limits[1])) x.limits[1] <- max(0, ggplotPanelParams[[panelIndex]]$x.range[1])
      if(is.na(x.limits[2])) x.limits[2] <- ggplotPanelParams[[panelIndex]]$x.range[2]
      binsLim <- getBins(start_value = start_value,
                         end_value = end_value,
                         binwidth = binwidth,
                         hist_values = hist_values,
                         limits = x.limits,
                         in_limits = in_x.limits)
      bins <- binsLim$bins
      in_x.limits <- binsLim$in_limits
    }

  } else {
    start_value <- min(hist_data$xmin[hist_data$ymax != 0],
                       na.rm = TRUE)
    end_value <- max(hist_data$xmax, na.rm = TRUE)
    in_x.limits <- in_y.limits <- rep(TRUE, length(hist_values))

    if (!is.null(x.limits)) {
      if(is.na(x.limits[1])) x.limits[1] <- ggplotPanelParams[[panelIndex]]$x.range[1]
      if(is.na(x.limits[2])) x.limits[2] <- ggplotPanelParams[[panelIndex]]$x.range[2]
      in_x.limits <- hist_values > x.limits[1] & hist_values < x.limits[2]
    }

    if (!is.null(y.limits)) {
      if(is.na(y.limits[1])) y.limits[1] <- max(0, ggplotPanelParams[[panelIndex]]$y.range[1])
      if(is.na(y.limits[2])) y.limits[2] <- ggplotPanelParams[[panelIndex]]$y.range[2]

      binsLim <- getBins(start_value = start_value,
                         end_value = end_value,
                         binwidth = binwidth,
                         hist_values = hist_values,
                         limits = y.limits,
                         in_limits = in_y.limits)
      bins <- binsLim$bins
      in_y.limits <- binsLim$in_limits
    }
  }
  in_limits <- in_x.limits & in_y.limits
  # hist_values should be in the x y limits
  hist_values <- hist_values[in_limits]

  # reset the minimum "hist_values" to be the new start value
  hist_values[which(hist_values == min(hist_values))[1]] <- start_value

  # bin fills
  uni_fill <- unique(hist_data$fill)
  fill <- rep(uni_fill[1], length(hist_values))
  # set stack color
  if (!is.null(ggObj$labels$fill)) {
    # fill: panel i && in limits
    fill_quo <- ggObj$layers[[activeGeomLayers]]$mapping$fill %||% ggObj$mapping$fill
    fill_var <- unlist(
      rlang::eval_tidy(rlang::quo(!!fill_quo),  dataFrame)
    )[facet_id][in_limits]
    levels <- levels(as.factor(fill_var))
    if (length(uni_fill) == length(levels)) {
      fill <- rep(NA, length(hist_values))
      for(j in seq_len(length(levels))){
        fill[which(fill_var %in% levels[j])] <- uni_fill[j]
      }
    }
  }

  if(ggplot2Version < "3.3.0") {

    colorStackingOrder <- c("selected",
                            if(reverse) {
                              hex6to12(rev(uni_fill))
                            } else {
                              hex6to12(uni_fill)
                            }
    )

  } else {

    colorStackingOrder <- c("selected",
                            if(reverse) {
                              hex6to12(uni_fill)
                            } else {
                              hex6to12(rev(uni_fill))
                            }
    )
  }

  list(
    fill = fill,
    hist_values = hist_values,
    colorStackingOrder = colorStackingOrder,
    in_limits = in_limits
  )
}

catch_bin_info.StatCount <- function(hist_values, hist_data, flipped_aes, x.limits, y.limits,
                                     facet_id, in_limits, dataFrame,
                                     ggObj, activeGeomLayers, ...) {

  in_x.limits <- in_y.limits <- rep(TRUE, length(hist_values))
  colorStackingOrder <- 'selected'

  position <- ggObj$layers[[activeGeomLayers]]$position
  reverse <- position$reverse %||% FALSE

  if(flipped_aes) {

    if (!is.null(y.limits)) {
      in_y.limits <- hist_values %in% y.limits
    }

    operations <- position_operation(position,
                                     hist_values,
                                     activeGeomLayers,
                                     sep = "&",
                                     ggObj = ggObj,
                                     dataFrame = dataFrame,
                                     facet_id = facet_id)

    fill_var <- operations$fill_var
    hist_values <- operations$hist_values

    if(!is.null(ggObj$labels$fill) && ggObj$labels$fill != ggObj$labels$y && is(position, "PositionStack")) {

      x <- hist_data$x
      levels_bar <- levels(factor(hist_values))

      # ylim is set some y is dropped
      if(any(is.na(x))) {
        na_x <- is.na(x)
        drops <- levels_bar[hist_data$y[na_x]]
        drop_count <- hist_data$count[na_x]

        # group_by_table <- data.frame(fill = fill_var, hist_values = hist_values) %>%
        #   dplyr::group_by(fill, hist_values) %>%
        #   dplyr::summarise(n = dplyr::n())

        group_by_table <- group(fill_var, hist_values)

        drop_fill <- group_by_table$fill[(group_by_table$hist_values %in% drops) &
                                           (group_by_table$n %in% drop_count)]

        in_x.limits <- !((hist_values %in% drops) & (fill_var %in% drop_fill))
      }

      in_limits <- in_y.limits & in_x.limits
      hist_values <- hist_values[in_limits]
      fill_var <- fill_var[in_limits]

    } else {

      if (!is.null(x.limits)) {
        counts <- table(hist_values)
        name_counts <- names(counts)

        in_x.limits <- hist_values %in%
          name_counts[x.limits[1] <= counts & x.limits[2] >= counts]
      }
      in_limits <- in_y.limits & in_x.limits
      hist_values <- hist_values[in_limits]
    }

  } else {
    if (!is.null(x.limits)) {
      in_x.limits <- hist_values %in% x.limits
    }

    operations <- position_operation(position,
                                     hist_values,
                                     activeGeomLayers,
                                     sep = "&",
                                     ggObj = ggObj,
                                     dataFrame = dataFrame,
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

        # group_by_table <- data.frame(fill = fill_var, hist_values = hist_values) %>%
        #   dplyr::group_by(fill, hist_values) %>%
        #   dplyr::summarise(n = dplyr::n())

        group_by_table <- group(fill_var, hist_values)

        drop_fill <- group_by_table$fill[(group_by_table$hist_values %in% drops) &
                                           (group_by_table$n %in% drop_count)]

        in_y.limits <- !((hist_values %in% drops) & (fill_var %in% drop_fill))
      }

      in_limits <- in_y.limits & in_x.limits
      hist_values <- hist_values[in_limits]
      fill_var <- fill_var[in_limits]

    } else {

      if (!is.null(y.limits)) {
        counts <- table(hist_values)
        name_counts <- names(counts)

        in_y.limits <- hist_values %in%
          name_counts[y.limits[1] <= counts & y.limits[2] >= counts]
      }
      in_limits <- in_y.limits & in_x.limits
      hist_values <- hist_values[in_limits]
    }
  }

  ggFill <- match_fill(hist_values, fill_var, hist_data, flipped_aes = flipped_aes)
  fill_levels <- names(ggFill)

  fill <- rep(NA, length(hist_values))
  for(j in seq_len(length(fill_levels))){
    fill[which(fill_var %in% fill_levels[j])] <- ggFill[j]
  }

  if(length(hist_values) < length(fill)) {
    hist_values <- rep_len(hist_values, length(fill))
  } else if(length(hist_values) > length(fill)) {
    fill <- rep_len(fill, length(hist_values))
  } else NULL

  if(ggplot2Version < "3.3.0") {

    colorStackingOrder <- c("selected",
                            if(reverse)
                              hex6to12(unique(ggFill))
                            else
                              hex6to12(rev(unique(ggFill)))
    )

  } else {

    colorStackingOrder <- c("selected",
                            if(reverse)
                              hex6to12(unique(ggFill[order(fill_levels)]))
                            else
                              hex6to12(unique(rev(ggFill[order(fill_levels)])))
    )
  }

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
position_operation <- function(position, hist_values, activeGeomLayers, sep = "&", ...) {
  UseMethod("position_operation", position)
}
position_operation.PositionStack <- function(position, hist_values, activeGeomLayers, sep = "&", ...) {
  args <- list(...)
  ggObj <- args$ggObj
  dataFrame <- args$dataFrame
  facet_id <- args$facet_id

  if(!is.null(ggObj$labels$fill) && ggObj$labels$fill != ggObj$labels$x) {

    fill_quo <- ggObj$layers[[activeGeomLayers]]$mapping$fill %||% ggObj$mapping$fill
    fill_var <- unlist(
      rlang::eval_tidy(rlang::quo(!!fill_quo),  dataFrame)
    )[facet_id]

  } else {
    fill_var <- hist_values
  }

  list(
    hist_values = hist_values,
    fill_var = fill_var
  )
}

position_operation.PositionDodge <- function(position, hist_values, activeGeomLayers, sep = "&", ...) {

  args <- list(...)
  ggObj <- args$ggObj
  dataFrame <- args$dataFrame
  facet_id <- args$facet_id

  if(!is.null(ggObj$labels$fill) && ggObj$labels$fill != ggObj$labels$x) {


    fill_quo <- ggObj$layers[[activeGeomLayers]]$mapping$fill %||% ggObj$mapping$fill
    fill_var <- unlist(
      rlang::eval_tidy(rlang::quo(!!fill_quo),  dataFrame)
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
      message("Viewport has been changed. Set `l_scaleto_world` to get the world view plot")
    } else
      stop("The length of `hist_values` and `fill_var` does not match.",
           call. = FALSE)

  } else {
    fill_var <- hist_values
  }

  list(
    hist_values = hist_values,
    fill_var = fill_var
  )
}

# to get the right values in this facet
catch_facet_id <- function(numOfSubtitles, hist_x, FacetWrap, FacetGrid,
                           layout, ggLayout, panelIndex, dataFrame) {

  # one facet
  if (numOfSubtitles == 0) {
    facet_id <- rep(TRUE, length(hist_x))
  } else {
    mapping.names_pos <-   if(FacetWrap) {
      which(colnames(layout) == names(ggLayout$facet_params$facets))
    } else if(FacetGrid) {
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


match_fill <- function(hist_values, fill_var, hist_data,
                       flipped_aes = FALSE,
                       ggplot2Version = utils::packageVersion("ggplot2")) {

  symbol <- "<&&>"
  fill <- hist_data$fill

  if(ggplot2Version < "3.3.0") {
    level <- levels(factor(paste(fill_var, hist_values, sep = symbol)))

    x <- stats::setNames(fill[order(hist_data$group)],
                         vapply(strsplit(level, symbol), function(x) x[1], character(1)))
  } else {
    level <- levels(factor(paste(hist_values, fill_var, sep = symbol)))

    x <- stats::setNames(if(flipped_aes) fill[order(hist_data$y)] else fill[order(hist_data$x)],
                         vapply(strsplit(level, symbol), function(x) x[2], character(1)))
  }
  x[!duplicated(names(x))]
}

# density or frequency?
get_yshows <- function(mapping, layMapping, flipped_aes = FALSE) {

  if(flipped_aes) {

    if (!is.null(mapping$x)) {
      if(grepl("density", rlang::expr_text(mapping$x))) return("density")
    }
    if (!is.null(layMapping$x)) {
      if(grepl("density", rlang::expr_text(layMapping$x))) return("density")
    }

  } else {

    if (!is.null(mapping$y)) {
      if(grepl("density", rlang::expr_text(mapping$y))) return("density")
    }
    if (!is.null(layMapping$y)) {
      if(grepl("density", rlang::expr_text(layMapping$y))) return("density")
    }

  }
  return("frequency")
}

## It is equivalent to
# data.frame(x, y) %>%
#  group_by(x, y) %>%
#  summarise(n = n())
group <- function(x, y) {
  names <- c(deparse(substitute(x)), deparse(substitute(y)))

  unix <- unique(x)
  uniy <- unique(y)

  xchar <- as.character(x)
  ychar <- as.character(y)

  expand <- expand.grid(unix, uniy)

  n <- apply(expand,
             MARGIN = 1,
             function(v) {
               v <- as.character(v)
               sum(xchar == v[1] & ychar == v[2])
             })

  id <- setdiff(seq(length(n)), which(n == 0))

  stats::setNames(
    cbind(expand, n)[id, ],
    c(names, "n")
  )
}

