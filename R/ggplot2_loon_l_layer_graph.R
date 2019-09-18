#' @rdname ggplot2.loon
#' @importFrom magrittr '%>%'
#' @export
ggplot2.loon.l_layer_graph <- function(target, ...) {

  widget <- loon::l_create_handle(attr(target, "widget"))
  ggObj <- list(...)$ggObj

  states <- loon:::get_layer_states(widget, native_unit = FALSE)

  # no active nodes
  active <- states$active
  if(!any(active)) return(ggObj)

  nav_ids <- loon::l_navigator_ids(widget)

  ggObj <- if(length(nav_ids) == 0) {
    # graph no navigators
    ggObj %>%
      ggEdges(states = states) %>%
      ggNodes(states = states) %>%
      ggLabels(states = states)
  } else {
    # graph with navigators
    ggObj %>%
      ggEdges(states = states) %>%
      ggNavPaths(states = states,
                 nav_ids = nav_ids,
                 widget = widget) %>%
      ggNodes(states = states) %>%
      ggLabels(states = states) %>%
      ggNavPoints(states = states,
                  nav_ids = nav_ids,
                  widget = widget)

  }

  return(ggObj)
}
