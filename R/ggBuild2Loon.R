# adding labels to ggplot_build points layer

ggBuild2Loon <- function(ggplotObject, linkingKey = NULL){

  len_layers <- length(ggplotObject$layers)
  ggBuild <-  suppressMessages(ggplot2::ggplot_build(ggplotObject))
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
  # the parttern of ggLayout
  # PANEL, ROW, COL, ..., SCALE_X, SCALE_Y. In general, it is 3
  ggLayout_start_pos <- max(which(colnames(ggLayout) == "COL"),
                            which(colnames(ggLayout) == "ROW"))

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
            ggBuild$data[[i]]$x[isPanel.j][is.infinite(buildData_x_panel_j) &
                                             buildData_x_panel_j < 0] <- x.range[1]
            ggBuild$data[[i]]$x[isPanel.j][is.infinite(buildData_x_panel_j) &
                                             buildData_x_panel_j > 0] <- x.range[2]
          }
          if (!is.null(buildData$y)) {
            y.range <- ggplotPanel_params[[j]]$y.range
            buildData_y_panel_j <- buildData$y[isPanel.j]
            ggBuild$data[[i]]$y[isPanel.j][is.infinite(buildData_y_panel_j) &
                                             buildData_y_panel_j < 0] <- y.range[1]
            ggBuild$data[[i]]$y[isPanel.j][is.infinite(buildData_y_panel_j) &
                                             buildData_y_panel_j > 0] <- y.range[2]
          }
        }
      }

      layerNames <- lapply(1:len_layers,
                           function(j) {
                             className <- class(ggplotObject$layers[[j]]$geom)
                             className[-which(className %in% c("ggproto"  ,"gg" ,"Geom"))]
                           })

      pointsLayerId <- which(sapply(layerNames,
                                    function(layerName){
                                      "GeomPoint" %in% layerName
                                      }
                                    ) == TRUE)

      # do we have a point layer?
      if (length(pointsLayerId) != 0) {
        multiFacets <- FALSE
        wrap.num <- wrap_num(ggLayout)
        # is multiple facets?
        if(wrap.num > 0) {
          multiFacets <- TRUE
        }

        lenPointsLayer <- length(pointsLayerId)

        # start loop
        for(i in 1:lenPointsLayer){
          buildData  <- ggBuild$data[[pointsLayerId[i]]]
          numOfObservation <- dim(buildData)[1]
          ggBuild$data[[pointsLayerId[i]]]$label <-
            if (numOfObservation == dim(input)[1]) {
              if (!multiFacets) {
                # one facet linkingKey
                linkingKey
              } else {
                panelMatch <- sapply(seq_len(wrap.num),
                                     function (j) {
                                       which(str_detect(colnames(ggLayout)[j + ggLayout_start_pos],
                                                        colnames(input)) == TRUE)
                                     })
                panelMatch.len <- length(panelMatch)
                panelLevels <- list()
                factors <- list()
                panelLevels.len <- c()
                for (j in 1:panelMatch.len) {
                  factors[[j]] <- as.factor(unlist(input[,   panelMatch[j]]))
                  panelLevels[[j]] <- levels(factors[[j]])
                  panelLevels.len[j] <- length(panelLevels[[j]])
                }
                if(panelMatch.len == 1){
                  panelValues <- as.character(factors[[1]])
                  # label order
                  linkingKey_order <- unlist(
                    lapply(panelLevels[[1]],
                           function(panelLevel){
                             which(panelValues %in% panelLevel)
                           }))
                  linkingKey[linkingKey_order]
                } else {
                  depth <- cum_multiply(panelLevels.len)
                  numOfLoop <- depth[1]
                  depth <- depth[-1]
                  # label order
                  linkingKey_order <- unlist(
                    lapply(seq_len(numOfLoop),
                           function(j){
                             id <- c()
                             divider <- j
                             for (k in 1:(panelMatch.len - 1)) {
                               # last list index
                               id[k] <- ceiling(divider / depth[k])
                               divider <- divider %% depth[k]
                             }
                             last.id <- j %% depth[length(depth)]
                             if (last.id == 0) {last.id <- depth[length(depth)]}
                             id <- c(id, last.id)
                             factors_index <- lapply(seq_len(length(id)),
                                                     function(k){
                                                       fact <- panelLevels[[k]][id[k]]
                                                       which(factors[[k]] %in% fact == TRUE)
                                                     })
                             common_index <- factors_index[[1]]
                             for (k in 1:(length(factors_index) - 1)) {
                               common_index <- intersect(common_index, factors_index[[k+1]])
                             }
                             common_index
                           }))
                  linkingKey[linkingKey_order]
                }
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

cum_multiply <- function(vec) {
  rev_vec <- rev(vec)
  output <- c()
  for(i in 1: length(rev_vec) ){
    if (i == 1) {
      output[i] <- rev_vec[i]
    } else {
      output[i] <- output[i - 1] * rev_vec[i]
    }
  }
  rev(output)
}
