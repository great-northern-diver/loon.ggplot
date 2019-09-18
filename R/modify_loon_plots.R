modify_loon_plots <- function(plots,
                              args,
                              display_info,
                              envir = parent.frame()) {

  # set linkingGroup
  lapply(plots,
         function(plot){
           loon::l_configure(plot, linkingGroup = args$linkingGroup,
                             sync = get("sync", envir = envir))
         }
  )

  if (get("panelNum", envir = envir) == 1) {
    gp <- plots$x1y1
  } else {
    gp <- list(
      plots = plots,
      facet = list(
        is_facet_wrap = get("is_facet_wrap", envir = envir),
        is_facet_grid = get("is_facet_grid", envir = envir),
        byCOLS = get("byCOLS", envir = envir),
        byROWS = get("byROWS", envir = envir)
      ),
      titles = list(
        title = get("title", envir = envir),
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
