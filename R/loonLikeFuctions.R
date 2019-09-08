#'@export
names.l_ggplot <- function(x) {attr(x, "names")}

#' @export
l_cget.l_ggplot <- function(target, state) {

  widgets <- target$plots
  plotNames <- names(widgets)
  plots <- lapply(plotNames,
                  function(plotName) {
                    widgets[[plotName]]
                  })
  setNames(lapply(plots, loon::l_cget, state),
           plotNames)
}


#' @export
l_configure.l_ggplot <- function(target, ...) {

  args <- list(...)

  if(is.null(args$sync)) {

    states <- names(args)
    sync <- "pull"
    message("default sync is 'pull'")
  } else {
    # check sync
    sync <- args$sync
    if(!sync %in% c("pull", "push")) stop("not known sync")
    states <- names(args)
    states <- states[-which(states == "sync")]
  }

  plots <- target$plots

  if (is.null(states) || any("" %in% states))
    stop("configuration needs key=value pairs")

  for (state in states) {
    arg <- args[[state]]
    lapply(1:length(plots),
           function(i){
             plot <- plots[[i]]
             if(state == "linkingGroup") {
               loon::l_configure(plot, linkingGroup = arg, sync = sync)
             } else if(state == "selected") {
               stop("not implemented yet")
             } else {
               if(is.list(arg)) {
                 if(length(arg) == length(plots)) {
                   plot[state] <- arg[[i]]
                 } else {
                   stop(paste0("the length of argument ", state, " should be equal to the length of facets"))
                 }
               } else {
                 plot[state] <- arg
               }
             }
           }
    )
  }

  invisible(target)
}

# aliased in l_cget
#' @export
`[.l_ggplot` <- function(target, state) {
  loon::l_cget(target, state)
}

# aliased in l_configure
#' @export
`[<-.l_ggplot` <- function(target, state, value) {
  args <- list(target, value)
  names(args) <- c("target", state)
  do.call("l_configure", args)
}
