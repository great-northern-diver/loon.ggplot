modify_loon_plots <- function(plotInfo = list()) {

  args <- plotInfo$args
  indices <- plotInfo$indices

  plots <- plotInfo$plots
  display_info <- plotInfo$display_info

  isCoordSerialaxes <- plotInfo$isCoordSerialaxes

  # set linkingGroup
  lapply(plots,
         function(plot) {
           loon::l_configure(plot, linkingGroup = args$linkingGroup,
                             sync = plotInfo$sync)
         }
  )

  if (plotInfo$panelNum == 1) {
    gp <- plots$x1y1

    model_layer_nDimStates <- l_nDimStateNames(gp)
  } else {
    gp <- list(
      plots = plots,
      facet = list(
        FacetWrap = plotInfo$FacetWrap,
        FacetGrid = plotInfo$FacetGrid,
        byCOLS = plotInfo$byCOLS,
        byROWS = plotInfo$byROWS
      ),
      titles = list(
        title = plotInfo$title,
        colSubtitles = display_info$colSubtitles,
        rowSubtitles = display_info$rowSubtitles
      )
    )
    class(gp) <- c("l_ggplot", "l_compound", "loon")

    model_layer_nDimStates <- l_allNDimStateNames(gp$plots)
  }

  if(isCoordSerialaxes) return(gp)

  # set args
  new_args <- setNames(
    lapply(seq_len(length(args) + 1) - 1,
           function(j){
             if(j == 0) {
               gp
             } else {
               # linking states
               if(names(args)[j] %in% c("linkingKey", "linkingGroup", "sync")) NULL
               # itemLabel state
               else if(names(args)[j] == "itemLabel") NULL
               # 3D states
               else if(names(args)[j] %in% c("z", "zlabel", "axisScaleFactor")) NULL
               else {
                 # loon states
                 name <- names(args)[j]
                 if(!name %in% model_layer_nDimStates) return(args[[j]])
                 if(length(args[[j]]) == 1) return(args[[j]])

                 facet_nDimStates(args[[j]], indices)
               }
             }
           }
    ),
    c("target", names(args))
  )
  new_args <- Filter(Negate(is.null), new_args)

  if(length(new_args) > 1) {
    do.call(l_configure, new_args)
  }
  return(gp)
}

facet_nDimStates <- function(state, indices) {

  len_state <- length(state)
  len_indices <- length(unlist(indices))

  if(len_state != len_indices)
    stop("The length of n dimensional state is ",
         len_state,
         " which is not equal to the length of the facet index ",
         len_indices,
         call. = FALSE)

  if(length(indices) == 1) return(state[unlist(indices)])

  lapply(indices,
         function(id) {
           state[id]
         })
}
