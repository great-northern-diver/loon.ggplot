
ggBuild2Loon <- function(ggplotObject){
  len_layers <- length(ggplotObject$layers)
  ggBuild <- ggplot2::ggplot_build(ggplotObject)
  input <- ggplotObject$data
  n <- dim(input)[1]
  # if not, no input data in ggplot()
  if(is.data.frame(input)){
    # itemLabel
    label <- row.names(input)
    colNames <- colnames(input)
    if(len_layers != 0){
      # match point itemlabel
      layerNames <- sapply(1:len_layers, function(j) {
        className <- class(ggplotObject$layers[[j]]$geom)
        className[-which(className %in% c("ggproto"  ,"gg" ,"Geom"))]
      })

      pointsLayerId <- which(sapply(layerNames, function(l){"GeomPoint" %in% l}) == TRUE)

      if( length(pointsLayerId) != 0 ){

        mapX <- try(as.character(ggplotObject$mapping$x[2]), silent = TRUE)
        mapY <- try(as.character(ggplotObject$mapping$y[2]), silent = TRUE)

        if (!all(c(grepl("Error in", mapX),
                   grepl("Error in", mapY)))){
          if (length(mapX)!=0 & length(mapY)!=0) {

            if (mapX %in% colnames(input)) {xx <- input[,   mapX]
            } else if( grepl("[()]", mapX)){
              str <- sapply(colNames, function(i) grepl(i, mapX))
              xx <- input[,   which(str== T) ]
            } else {
              xx <- NA
            }

            if (mapY %in% colnames(input)) {yy <- input[,   mapY]
            } else if( grepl("[()]", mapY) ){
              str <- sapply(colNames, function(i) grepl(i, mapY))
              yy <- input[,   which(str== T) ]
            } else {
              yy <- NA
            }

            if (any(!is.na(xx)) & any(!is.na(yy))) {

              input_xy <- data.frame(x = xx ,
                                     y = yy)

              for(i in 1:length(pointsLayerId)){

                buildData  <- ggBuild$data[[pointsLayerId[i]]]
                if (dim(buildData)[1] == dim(input)[1]) {
                  itemLabel <- c()
                  for(j in 1:dim(buildData)[1]){
                    id <- which(input_xy[,1] %in% buildData[j,]$x== T & input_xy[,2] %in% buildData[j,]$y == T)[1]
                    if(is.na(id)){
                      itemLabel[j] <- NA
                    } else {
                      itemLabel[j] <- label[id]
                      input_xy[id,1] <- NA
                      input_xy[id,2] <- NA
                    }
                  }
                  ggBuild$data[[pointsLayerId[i]]]$label <- itemLabel
                }
              }
            }
          }
        }
      }
    }
  }
  ggBuild
}
