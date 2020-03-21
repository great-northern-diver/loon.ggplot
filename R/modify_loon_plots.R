modify_loon_plots <- function(plots_info = list()) {

  args <- plots_info$args

  plots <- plots_info$plots
  display_info <- plots_info$display_info

  # set linkingGroup
  lapply(plots,
         function(plot){
           loon::l_configure(plot, linkingGroup = args$linkingGroup,
                             sync = plots_info$sync)
         }
  )

  if (plots_info$panelNum == 1) {
    gp <- plots$x1y1
  } else {
    gp <- list(
      plots = plots,
      facet = list(
        is_facet_wrap = plots_info$is_facet_wrap,
        is_facet_grid = plots_info$is_facet_grid,
        byCOLS = plots_info$byCOLS,
        byROWS = plots_info$byROWS
      ),
      titles = list(
        title = plots_info$title,
        colSubtitles = display_info$colSubtitles,
        rowSubtitles = display_info$rowSubtitles
      )
    )
    class(gp) <- c("l_ggplot", "l_compound", "loon")
  }

  # set args
  new_args <- setNames(
    lapply(seq_len(length(args) + 1) - 1,
           function(j){
             if(j == 0) {
               gp
             } else {
               if(names(args)[j] == "linkingKey") NULL
               else if(names(args)[j] == "linkingGroup") NULL
               else if(names(args)[j] == "itemLabel") NULL
               else if(names(args)[j] == "sync") NULL
               else args[[j]]
             }
           }
    ),
    c("target", names(args))
  )
  new_args <- Filter(Negate(is.null), new_args)

  if(length(new_args) > 1){
    do.call(loon::l_configure, new_args)
  }
  return(gp)
}
