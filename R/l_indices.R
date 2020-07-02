l_plot_indices <- function(ggBuild, activeGeomLayers, panelIndex) {
  unlist(
    lapply(activeGeomLayers,
           function(activeGeomLayer){
             activeLayer <- ggBuild$data[[activeGeomLayer]]
             which(activeLayer$PANEL == panelIndex)
           }
    )
  )
}

# it is a wrapper of the function `catch_facet_id`
l_hist_indices <- function(ggBuild, activeGeomLayers, panelIndex, mapping, dataFrame,
                               numOfSubtitles, is_facet_wrap, is_facet_grid,
                               layout, ggLayout) {

  hist_data <- ggBuild$data[[activeGeomLayers]]

  flipped_aes <- any(hist_data$flipped_aes) %||% FALSE

  hist_x <- if(flipped_aes) {
    rlang::eval_tidy(rlang::quo(!!mapping$y),  dataFrame)
  } else {
    rlang::eval_tidy(rlang::quo(!!mapping$x),  dataFrame)
  }

  catch_facet_id(numOfSubtitles, hist_x, is_facet_wrap, is_facet_grid,
                 layout, ggLayout, panelIndex, dataFrame)
}
