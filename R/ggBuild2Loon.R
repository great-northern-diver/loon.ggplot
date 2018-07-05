# adding labels to ggplot_build points layer

ggBuild2Loon <- function(ggplotObject){

  len_layers <- length(ggplotObject$layers)
  ggBuild <- ggplot2::ggplot_build(ggplotObject)
  input <- ggplotObject$data

  # different ggplot2 versions have different names
  if(is_devtools_ggplot2()){
    ggLayout <- ggBuild$layout$layout
    # panel_params
    ggplotPanel_params <- ggBuild$layout$panel_params
  } else {
    ggLayout <- ggBuild$layout$panel_layout
    # panel_params
    ggplotPanel_params <- ggBuild$layout$panel_ranges
    message(
      "devtools version ggplot2 is highly recommanded in `loon.ggplot`\n",
      "install it with: `devtools::install_github('tidyverse/ggplot2')`\n",
      "before you start, make sure package `rlang` is installed"
    )
  }

  # if not, no input data in ggplot()
  if(is.data.frame(input)){
    # length of layer is 0?
    if(len_layers != 0){
      # any infinite value?
      for (i in 1:len_layers) {
        buildData  <- ggBuild$data[[i]]
        unique_panel <- as.numeric(unique(buildData$PANEL))
        for(j in 1:length(unique_panel)){
          isPanel.j <- buildData$PANEL == j
          if (!is.null(buildData$x)) {
            x.range <- ggplotPanel_params[[j]]$x.range
            buildData_x_panel_j <- buildData$x[isPanel.j]
            ggBuild$data[[i]]$x[isPanel.j][is.infinite(buildData_x_panel_j) & buildData_x_panel_j < 0] <- x.range[1]
            ggBuild$data[[i]]$x[isPanel.j][is.infinite(buildData_x_panel_j) & buildData_x_panel_j > 0] <- x.range[2]
          }
          if (!is.null(buildData$y)) {
            y.range <- ggplotPanel_params[[j]]$y.range
            buildData_y_panel_j <- buildData$y[isPanel.j]
            ggBuild$data[[i]]$y[isPanel.j][is.infinite(buildData_y_panel_j) & buildData_y_panel_j < 0] <- y.range[1]
            ggBuild$data[[i]]$y[isPanel.j][is.infinite(buildData_y_panel_j) & buildData_y_panel_j > 0] <- y.range[2]
          }
        }
      }

      layerNames <- sapply(1:len_layers, function(j) {
        className <- class(ggplotObject$layers[[j]]$geom)
        className[-which(className %in% c("ggproto"  ,"gg" ,"Geom"))]
      })

      pointsLayerId <- which(sapply(layerNames, function(l){"GeomPoint" %in% l}) == TRUE)
      # point layer?
      if( length(pointsLayerId) != 0 ){

        multiFacets <- FALSE
        # is multi facets?
        if(dim(ggLayout)[2] == 6) {
          multiFacets <- TRUE
          panelMatch <- which( grepl(colnames(ggLayout)[4], colnames(input)) == TRUE )
          factors <- input[,   panelMatch]
          if (is.list(factors)) {
            factors <- unlist(factors)
          }
          panelLevels <- levels(as.factor(factors))
        }

        label <- row.names(input)
        lenPointsLayer <- length(pointsLayerId)
        # start loop
        for(i in 1:lenPointsLayer){

          buildData  <- ggBuild$data[[pointsLayerId[i]]]
          numOfObsi <- dim(buildData)[1]

          ggBuild$data[[pointsLayerId[i]]]$label <- if (numOfObsi == dim(input)[1]) {
            if (!multiFacets) {
              label
            } else {
              panelValues <- as.character(factors)
              labelOrder <- unlist(lapply(panelLevels, function(j){
                which(panelValues %in% j)
              }))
              label[labelOrder]
            }
          } else {
            # the ggplot input data is not the geom_point data
            # in other words, geom_point() layer add new dataset
            if (lenPointsLayer == 1) {
              paste0("item", c(1:numOfObsi))
            } else {
              paste0("item", c(1:numOfObsi), "pointsLayer", i)
            }
          }

        }
        # end loop
      }
    }
  }
  list(ggBuild = ggBuild,
       ggLayout = ggLayout,
       ggplotPanel_params = ggplotPanel_params
       )
}


# many names are changed after version 2.2.1
is_devtools_ggplot2 <- function() {
  packageVersion("ggplot2") > "2.2.1"
}
