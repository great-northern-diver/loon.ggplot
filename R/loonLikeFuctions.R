#'@export
names.l_facet_ggplot <- function(x) {attr(x, "names")}

#' @export
l_cget.l_facet_ggplot <- function(target, state) {

  plotNames <- names(target)
  plots <- lapply(plotNames,
                  function(plotName) {
                    target[[plotName]]
                  })
  setNames(lapply(plots, loon::l_cget, state), plotNames)
}


#' @export
l_configure.l_facet_ggplot <- function(target, ...) {

  args <- list(...)

  if(is.null(args$sync)) {
    states <- names(args)
    sync <- "pull"
    # message("default sync is 'pull'")
  } else {
    # check sync
    sync <- args$sync
    if(!sync %in% c("pull", "push")) stop("not known sync", call. = FALSE)
    states <- names(args)
    states <- states[-which(states == "sync")]
  }

  plots <- unclass(target)

  if (is.null(states) || any("" %in% states))
    stop("configuration needs key value pairs",
         call. = FALSE)

  for(state in states) {

    arg <- args[[state]]

    lapply(seq(length(plots)),

           function(i) {

             plot <- plots[[i]]

             if(state == "linkingGroup") {

               loon::l_configure(plot,
                                 linkingGroup = arg,
                                 sync = sync)

             } else {

               if(is.list(arg)) {
                 if(length(arg) == length(plots))
                   plot[state] <- arg[[i]]
                 else
                   stop("the length of argument ",
                        state,
                        " should be equal to the length of facets.",
                        call. = FALSE)
               } else plot[state] <- arg
             }
           }
    )
  }

  invisible(target)
}

# aliased in l_cget
#' @export
`[.l_facet_ggplot` <- function(target, state) {
  loon::l_cget(target, state)
}

# aliased in l_configure
#' @export
`[<-.l_facet_ggplot` <- function(target, state, value) {
  args <- list(target, value)
  names(args) <- c("target", state)
  do.call("l_configure", args)
}

#' @export
`[.l_ggplot` <- function(target, state) {
  message_wrap(
    deparse(substitute(target)),
    " is a not a 'loon' widget. The loon object can be created from its path name,
    to see more details, please check help('l_getFromPath')."
  )
  invisible()
}

#' @export
`[<-.l_ggplot` <- function(target, state, value) {
  message_wrap(
    deparse(substitute(target)),
    " is a not a 'loon' widget. The loon object can be created from its path name,
    to see more details, please check help('l_getFromPath')."
  )
  invisible()
}
