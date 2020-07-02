#' @title Modify the \code{interactive} component
#' @description Set interactive components (e.g. \code{linking}, \code{selecting}, \code{itemLabel}, etc)
#' @inheritParams linking
#' @inheritParams selecting
#' @inheritParams itemLabel
#' @param ... named arguments to modify \code{loon} plot states. See \code{\link{l_info_states}}
#'
#' @return a \code{ggproto} object
#' @seealso \code{\link{linking}}, \code{\link{selecting}}, \code{\link{itemLabel}}
#' @examples
#' if(interactive()) {
#'   # Modify the 'linkingGroup' and 'origin' of function `l_hist`
#'   l_ggplot(mtcars, mapping = aes(x = wt)) +
#'     geom_histogram() +
#'     interactivity(linkingGroup = "mt", origin = 2)
#'   l_ggplot(mtcars, mapping = aes(x = wt, y = hp)) +
#'     geom_point(size = 4) +
#'     interactivity(linkingGroup = "mt") +
#'     facet_wrap(~cyl)
#' }
#' @export
interactivity <- function(linkingGroup = NULL,
                          linkingKey = NULL,
                          linkedStates = NULL,
                          sync = NULL,
                          activeGeomLayers = NULL,
                          selected = NULL,
                          selectBy = NULL,
                          selectionLogic = NULL,
                          itemLabel = NULL,
                          showItemLabels = NULL,
                          ...) {

  ggplot2::ggproto("Interactivity", NULL,
                   params = list(
                     linkingGroup = linkingGroup,
                     linkingKey = linkingKey,
                     linkedStates = linkedStates,
                     sync = sync,
                     activeGeomLayers = activeGeomLayers,
                     selected = selected,
                     selectBy = selectBy,
                     selectionLogic = selectionLogic,
                     itemLabel = itemLabel,
                     showItemLabels = showItemLabels,
                     ...
                   ),
                   remove_null = function(..., as_list = TRUE) {
                     if(as_list)
                       Filter(Negate(is.null),
                              list(...)
                       )
                     else
                       Filter(Negate(is.null), ...)
                   },
                   warn_nDim_states = function(data, x) {
                     n <- nrow(data) %||% 0
                     if(length(x) != n && length(x) != 1) {
                       warning("The length of ", deparse(substitute(x)), " is ", length(x),
                               " that does not match the number of observations(",
                               n, ")",
                               call. = FALSE)
                     }
                   },
                   check_itemLabel = function(self, data, params) {

                     if(!is.null(params$showItemLabels) && !is.logical(params$showItemLabels)) {
                       stop("`showItemLabels` must be logical", call. = FALSE)
                     }

                     itemLabel <- params$itemLabel
                     if(is.null(itemLabel)) return(params)

                     self$warn_nDim_states(data = data,
                                           x = itemLabel)
                   },
                   check_selected = function(self, data, params) {

                     selected <- params$selected

                     if(is.null(selected)) return(NULL)

                     if(!is.logical(selected)) {
                       stop("`selected` must be logical", call. = FALSE)
                     }

                     self$warn_nDim_states(data = data,
                                           x = selected)
                   },
                   check_linkingKey = function(self, data, params) {

                     linkingKey <- params$linkingKey
                     if(is.null(linkingKey)) return(NULL)

                     if(any(duplicated(linkingKey)))
                       stop("The linkingKey is duplicated")

                     self$warn_nDim_states(data = data,
                                           x = linkingKey)
                   }

  )
}

#' @title Modify the \code{linking} component
#' @description In interactive graphics, \code{linking} is often used to discover the patterns of interest in several plots.
#' @param linkingGroup A character. Plots only in the same linkingGroup can be linked
#' @param linkingKey LinkingKey is the key of linking. Each object in one plot has a unique linking key.
#' Elements in different plots are linked if they share the same linking keys.
#' @param linkedStates The states to be linked. It can be "color", "selected", "active", "size" and "glyph" for a `l_plot` object and
#' "color", "selected", "active" for a `l_hist` object.
#' @param sync The way to synchronize several linked plots. It can be either "pull" (default) or "push".
#' If the \code{sync} is "pull", the linked states (aesthetics attributes, e.g. "color", "selected", ...)
#' of the new plot will be pulled from the linked plots;
#' if the \code{sync} is "push",  the linked states of the new plot will be pushed to the linked plots.
#' @param activeGeomLayers determine which geom layer is interactive. Only \code{geom_point()}
#' and \code{geom_histogram()} can be set as active geom layer(s) so far.
#' (Notice, more than one \code{geom_point()} layers can be set as active layers,
#' but only one \code{geom_histogram()} can be set as an active geom layer)
#' @return a \code{ggproto} object
#'
#' @seealso \code{\link{selecting}}, \code{\link{itemLabel}}, \code{\link{interactivity}}
#'
#' @examples
#' if(interactive()) {
#'   p <- l_hist(mtcars$hp, linkingGroup = "mtcars")
#'   l_ggplot(mtcars, mapping = aes(x = wt, y = hp, color = factor(cyl))) +
#'     geom_point(size = 4) +
#'     # push the states of scatter plot to the histogram
#'     linking(linkingGroup = "mtcars", sync = "push")
#'
#'   # set active layer
#'   l_ggplot(mtcars, aes(mpg, wt, shape = factor(cyl))) +
#'     geom_point(colour = "black", size = 4.5) +
#'     geom_point(colour = "pink", size = 4) +
#'     geom_point(aes(shape = factor(cyl))) +
#'     # only the second layer is interactive
#'     linking(activeGeomLayers = 2)
#' }
#' @export
linking <- function(linkingGroup = NULL,
                    linkingKey = NULL,
                    linkedStates = NULL,
                    sync = NULL,
                    activeGeomLayers = NULL) {

  interactivity(linkingGroup = linkingGroup,
                linkingKey = linkingKey,
                linkedStates = linkedStates,
                sync = sync,
                activeGeomLayers = activeGeomLayers)
}

#' @title Modify the \code{selecting} component
#' @description In interactive graphics, \code{selecting} is one of the most fundamental tool
#' and used to highlight the subset of interest
#' @param selected A logical vector. If it is set as \code{TRUE}, the elements are highlighted
#' as the graphics are constructed. Default is \code{FALSE}
#' @param selectBy Select by "sweeping" (default) or "brushing".
#' @param selectionLogic Selection logic. One of "select" (default), "deselect" and "invert". See details.
#' @details There are two ways to directly select elements on the scatterplot using the mouse:
#' either by "sweep" or by "brushing". "Sweeping" allows us to sweep out a contiguous area of the plot,
#' while, in "brushing", the area is always fixed during the selection.
#'
#' The selection logic give users more flexibility that users cannot only highlight the elements,
#' but also can delight or invert (the highlighted to delighted, vice verse) the elements.
#'
#' @return a \code{ggproto} object
#' @seealso \code{\link{linking}}, \code{\link{itemLabel}}, \code{\link{interactivity}}
#' @examples
#' if(interactive()) {
#'
#'   # highlight the four gear cars
#'   fourGear <- rep(FALSE, nrow(mtcars))
#'   fourGear[mtcars$gear == 4] <- TRUE
#'
#'   l_ggplot(mtcars, mapping = aes(x = wt, y = hp, color = factor(cyl))) +
#'     geom_point(size = 4) +
#'     # push the states of scatter plot to the histogram
#'     selecting(selected = fourGear)
#' }
#' @export
selecting <- function(selected = NULL,
                      selectBy = NULL,
                      selectionLogic = NULL) {

  interactivity(selected = selected,
                selectBy = selectBy,
                selectionLogic = selectionLogic)
}

#' @title Modify the \code{itemLabel} component
#' @description Querying in interactive graphics
#' @param itemLabel The customized querying information.
#' @param showItemLabels A logical value. Show item labels or not. Default is \code{FALSE}
#'
#' @return a \code{ggproto} object
#' @seealso \code{\link{linking}}, \code{\link{selecting}}, \code{\link{interactivity}}
#' @examples
#' if(interactive()) {
#'
#'   l_ggplot(mpg, mapping = aes(x = displ, y = cty)) +
#'     geom_point(size = 4) +
#'     # push the states of scatter plot to the histogram
#'     itemLabel(itemLabel = with(mpg,
#'                  paste0("model: ", manufacturer, " ", model, "\n",
#'                  "year: ", year, "\n",
#'                  "drive way: ", drv, "\n",
#'                  "fuel type: ", fl)
#'               ),
#'               showItemLabels = TRUE
#'     )
#'     # hover the mouse on top of any point
#' }
#' @export
itemLabel <- function(itemLabel = NULL,
                      showItemLabels = NULL) {
  interactivity(itemLabel = itemLabel, showItemLabels = showItemLabels)
}

#' @export
ggplot_add.Interactivity <- function(object, plot, object_name) {

  undate_interactivity(plot, object)
}

undate_interactivity <- function(p, interactivity) {
  p$interactivity <- merge_interactivity(interactivity, p$interactivity)
  p
}

merge_interactivity <- function (new, old) {

  if(is.null(new)) return(old)

  pre <- find(old = old$params,
              new = new$params)

  if(any(pre)) {

    pre_name <- names(pre)[which(pre)]
    message_wrap("Interactive component for '", pre_name,
                 "' is already present. Adding another interactive component for '", pre_name,
                 "', which will replace the existing one.")
  }

  new$params <- replace(new = new$params,
                        old = old$params)
  new
}

find <- function(new, old) {

  old <- remove_null(old, as_list = FALSE)
  new <- remove_null(new, as_list = FALSE)
  if(length(new) == 0) return(FALSE)

  vapply(names(new), function(param) any(names(old) %in% param), logical(1))
}

replace <- function(new, old) {

  old <- remove_null(old, as_list = FALSE)
  new <- remove_null(new, as_list = FALSE)

  if(length(new) == 0 && length(old) == 0) return(list())

  states <- unique(c(names(old), names(new)))
  stats::setNames(
    lapply(states,
           function(state) {
             if(state %in% names(new)) {
               new[[state]]
             } else {
               old[[state]]
             }
           }),
    states
  )
}

params_check <- function(interactivity, data) {
  interactivity$check_selected(data, interactivity$params)
  interactivity$check_itemLabel(data, interactivity$params)
  interactivity$check_linkingKey(data, interactivity$params)
}
