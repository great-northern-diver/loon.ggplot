# adding labels to ggplot_build points layer
ggplot2Version <- utils::packageVersion("ggplot2")

ggBuild2Loon <- function(ggObj, linkingKey = NULL, itemLabel = NULL){

  len_layers <- length(ggObj$layers)
  ggBuild <-  suppressMessages(ggplot_build(ggObj))
  input <- ggObj$data

  ggLayout <- ggBuild$layout

  # different ggplot2 versions have different names
  if(ggplot2Version > "2.2.1"){
    layout <- ggLayout$layout
    # panel_params

    ggplotPanelParams <- lapply(ggBuild$layout$panel_params,
                                 function(panel_param) {
                                   # x
                                   x.major_source <- panel_param$x.major_source %||% panel_param$x$breaks
                                   x.minor_source <- panel_param$x.minor_source %||% panel_param$x$minor_breaks
                                   x.labels <- panel_param$x.labels %||% panel_param$x$labels
                                   # y
                                   y.major_source <- panel_param$y.major_source %||% panel_param$y$breaks
                                   y.minor_source <- panel_param$y.minor_source %||% panel_param$y$minor_breaks
                                   y.labels <- panel_param$y.labels %||% panel_param$y$labels
                                   # adjust major source
                                   if(!is.numeric(x.major_source)) x.major_source <- attr(x.major_source, "pos")
                                   if(!is.numeric(y.major_source)) y.major_source <- attr(y.major_source, "pos")

                                   c(
                                     panel_param,
                                     list(x.major_source = x.major_source,
                                          x.minor_source = x.minor_source,
                                          x.labels = x.labels,
                                          y.major_source = y.major_source,
                                          y.minor_source = y.minor_source,
                                          y.labels = y.labels)
                                   )
                                 })
  } else {
    layout <- ggLayout$panel_layout
    # panel_params
    ggplotPanelParams <- ggBuild$layout$panel_ranges
    message(
      "devtools version ggplot2 is highly recommanded in `loon.ggplot`\n",
      "install it with: `devtools::install_github('tidyverse/ggplot2')`\n",
      "before you start, make sure package `rlang` is installed"
    )
  }
  FacetWrap <- is.FacetWrap(ggObj$facet)
  FacetGrid <- is.FacetGrid(ggObj$facet)

  mapping.names <-   if(FacetWrap) {
    names(ggLayout$facet_params$facets)
  } else if(FacetGrid) {
    c(names(ggLayout$facet_params$rows),names(ggLayout$facet_params$cols))
  } else NULL

  # if not, no input data in ggplot()
  if(is.data.frame(input)){
    # length of layer is 0?
    if(len_layers != 0){

      ggBuild_data <- ggBuild$data
      # any infinite value?
      for (i in 1:len_layers) {

        buildData  <- ggBuild_data[[i]]
        unique_panel <- as.numeric(unique(buildData$PANEL))

        for(j in 1:length(unique_panel)){
          isPanel.j <- buildData$PANEL == j
          if (!is.null(buildData$x)) {
            x.range <- ggplotPanelParams[[j]]$x.range
            buildData_x_panel_j <- buildData$x[isPanel.j]
            ggBuild_data[[i]]$x[isPanel.j][is.infinite(buildData_x_panel_j) &
                                             buildData_x_panel_j < 0] <- x.range[1]
            ggBuild_data[[i]]$x[isPanel.j][is.infinite(buildData_x_panel_j) &
                                             buildData_x_panel_j > 0] <- x.range[2]
          }
          if (!is.null(buildData$y)) {
            y.range <- ggplotPanelParams[[j]]$y.range
            buildData_y_panel_j <- buildData$y[isPanel.j]
            ggBuild_data[[i]]$y[isPanel.j][is.infinite(buildData_y_panel_j) &
                                             buildData_y_panel_j < 0] <- y.range[1]
            ggBuild_data[[i]]$y[isPanel.j][is.infinite(buildData_y_panel_j) &
                                             buildData_y_panel_j > 0] <- y.range[2]
          }
        }
      }

      layerNames <- lapply(1:len_layers,
                           function(j) {
                             className <- class(ggObj$layers[[j]]$geom)
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
        wrap.num <- wrap_num(ggLayout = ggLayout,
                             FacetWrap = FacetWrap,
                             FacetGrid = FacetGrid)
        # is multiple facets?
        if(wrap.num > 0) {
          multiFacets <- TRUE
        }

        lenPointsLayer <- length(pointsLayerId)

        # start loop
        for(i in 1:lenPointsLayer){

          buildData  <- ggBuild_data[[pointsLayerId[i]]]
          numOfObservation <- dim(buildData)[1]

          if (numOfObservation == dim(input)[1]) {
            if (!multiFacets) {

              # one facet linkingKey itemLabel
              ggBuild_data[[pointsLayerId[i]]]$linkingKey <- linkingKey
              ggBuild_data[[pointsLayerId[i]]]$itemLabel <- itemLabel

            } else {
              # multiple facet linkingKey
              # which variable is used to separate facet

              # In version below 3.1.0, ggplot_build automatically order points by PANEL and group
              # In the version 3.1.0, the order is kept by orginal data set
              if(ggplot2Version >= "3.1.0") {

                ggBuild_data[[pointsLayerId[i]]]$linkingKey <- linkingKey
                ggBuild_data[[pointsLayerId[i]]]$itemLabel <- itemLabel

              } else {

                panelMatch <- sapply(mapping.names,
                                     function (mapping.name) {

                                       which(grepl(mapping.name, colnames(input), ignore.case = TRUE))

                                       # which(stringr::str_detect(mapping.name, colnames(input)) == TRUE)
                                     }
                )
                panelMatch.len <- length(panelMatch)
                panelLevels <- list()
                factors <- list()
                panelLevels.len <- c()
                # determine the factors and levels of those variables used for separating
                for (j in 1:panelMatch.len) {
                  factors[[j]] <- as.factor(unlist(input[,   panelMatch[j]]))
                  panelLevels[[j]] <- levels(factors[[j]])
                  panelLevels.len[j] <- length(panelLevels[[j]])
                }
                # if we just have one variable used for separating
                if(panelMatch.len == 1){
                  # factors
                  panelValues <- as.character(factors[[1]])
                  # label order
                  reOrder <- unlist(
                    lapply(panelLevels[[1]],
                           function(panelLevel){
                             which(panelValues %in% panelLevel)
                           }))
                  ggBuild_data[[pointsLayerId[i]]]$linkingKey <- linkingKey[reOrder]
                  ggBuild_data[[pointsLayerId[i]]]$itemLabel <- itemLabel[reOrder]
                } else {
                  ################ cum_multiply
                  # suppose we have two variables used for separating, first variable has 6 factors, second has 3
                  # First: I II III IV V VI
                  # Second: A B C
                  # The first group is determind by IA (match these two factors), then the second is determined by IB, ..., the last is VIC.
                  # In other word, ggplot_build reorder the data set by
                  # IA, IA, ... IA, IB,..., IB, ..., VIC
                  # thus we have to do the loop from 1 to 18(3 * 6)

                  # give the number of loop and the depth in each loop

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

                  depth <- cum_multiply(panelLevels.len)
                  numOfLoop <- depth[1]
                  depth <- depth[-1]
                  # find label order
                  reOrder <- unlist(
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
                           }
                    )
                  )
                  ggBuild_data[[pointsLayerId[i]]]$linkingKey <- linkingKey[reOrder]
                  ggBuild_data[[pointsLayerId[i]]]$itemLabel <- itemLabel[reOrder]
                }
              }
            }
          }
        }
        # end loop
      }
      ggBuild$data <- ggBuild_data
    }
  }
  list(ggBuild = ggBuild,
       ggLayout = ggLayout,
       layout = layout,
       ggplotPanelParams = ggplotPanelParams,
       FacetWrap = FacetWrap,
       FacetGrid = FacetGrid
  )
}
