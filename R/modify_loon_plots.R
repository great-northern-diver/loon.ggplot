modify_loon_plots <- function(plotInfo = list()) {

  args <- plotInfo$args
  indices <- plotInfo$indices

  plots <- plotInfo$plots

  isCoordSerialaxes <- plotInfo$isCoordSerialaxes

  # set linkingGroup
  lapply(plots,
         function(plot) {
           loon::l_configure(plot, linkingGroup = args$linkingGroup,
                             sync = plotInfo$sync)
           configure_l_hist(plot, layerId = plotInfo$layerId)
         }
  )

  if (plotInfo$panelNum == 1) {
    gp <- plots$x1y1

    # model_layer_nDimStates <- l_nDimStateNames(gp)
  } else {
    gp <- plots
    class(gp) <- c("l_facet_ggplot", "l_facet", "l_compound", "loon")

    # turn off labels
    lapply(gp, function(g) g['showLabels'] <- FALSE)
    # model_layer_nDimStates <- l_allNDimStateNames(plots)
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
                 # name <- names(args)[j]
                 # if(!name %in% model_layer_nDimStates) return(args[[j]])
                 # if(length(args[[j]]) == 1) return(args[[j]])
                 # facet_nDimStates(args[[j]], indices)
                 args[[j]]
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

# This function has two jobs
## 1. Suppose the transformed `loon` histograms are joined into a linking group,
##    the color of each bin could be different from the original `ggplot`.
##    Thus, the state `colorStackingOrder` (queried from the `ggplot` object)
##    will be reset to the default settings
##    (it is meaningless to set the `colorStackingOrder` to some color that never appears)
## 2. If the ggplot histogram "y shows" is density,
##    the area of each category (grouped by color) is 1;
##    however, for an `l_hist` widget, the whole area is one and the area of each category
##    is proportional to the counts. Thus, when the transformation occurs,
##    the y limits are identical but the display is very different. To fix it,
##    the y limits are released and a message is given to users to explain what happens.
configure_l_hist <- function(widget, layerId = 0L) {

  if(!inherits(widget, "l_hist")) return(NULL)

  # job 1: modify the state `colorStackingOrder`
  colorStackingOrder <- widget['colorStackingOrder']
  uniColor <- unique(widget['color'])

  if(!all(uniColor %in% colorStackingOrder))
    widget['colorStackingOrder'] <- "selected"
  # job 2: if the state `yshows` of this histogram is density, rescale the plot and leave message
  if(widget['yshows'] == "frequency" || length(uniColor) == 1) return(NULL)

  if(layerId == 0) {
    loon::l_scaleto_world(widget)
    message("In `ggplot`, if the `y` of a histogram is density, ",
            "the area of each category (grouped by color) is 1; ",
            "while in `loon`, the whole area is one and the area of each category ",
            "is proportional to the counts. ",
            "Thus, when the transformation occurs, ",
            "the display could be very different. ",
            "To adjust it, the histogram is scaled to the world view.")
  }
}

## this cannot be called at the end of the execution
## the main reason is that NA values would change the number of observations
# facet_nDimStates <- function(state, indices) {
#
#   len_state <- length(state)
#   len_indices <- length(unlist(indices))
#
#   if(len_state != len_indices)
#     stop("The length of n dimensional state is ",
#          len_state,
#          " which is not equal to the length of the facet index ",
#          len_indices,
#          call. = FALSE)
#
#   if(length(indices) == 1) return(state[unlist(indices)])
#
#   lapply(indices,
#          function(id) {
#            state[id]
#          })
# }
