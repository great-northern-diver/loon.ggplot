#' @rdname loon2ggplot
#' @export
loon2ggplot.l_layer_graph <- function(target, ...) {

  widget <- loon::l_create_handle(attr(target, "widget"))
  ggObj <- list(...)$ggObj

  states <- get_layer_states(widget, native_unit = FALSE)

  # no active nodes
  active <- states$active
  if(!any(active)) return(ggObj)

  nav_ids <- loon::l_navigator_ids(widget)

  # with pipe '%>%' may make the code more readable
  ggObj <- if(length(nav_ids) == 0) {
    # graph no navigators
    ggObj <- ggEdges(ggObj, states = states)
    ggObj <- ggNodes(ggObj, states = states)
    ggLabels(ggObj, states = states)
  } else {
    # graph with navigators
    ggObj <- ggEdges(ggObj, states = states)
    ggObj <- ggNavPaths(ggObj,
                        states = states,
                        nav_ids = nav_ids,
                        widget = widget)
    ggObj <- ggNodes(ggObj, states = states)
    ggObj <- ggLabels(ggObj, states = states)
    ggNavPoints(ggObj,
                states = states,
                nav_ids = nav_ids,
                widget = widget)

  }

  return(ggObj)
}
