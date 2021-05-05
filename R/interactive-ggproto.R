#' @title Modify the \code{interactivity} component
#' @description Set interactive components (e.g. \code{linking}, \code{selection}, etc)
#' @inheritParams linking
#' @inheritParams active
#' @inheritParams selection
#' @param layerId numerical; which layer to scale to
#' @param scaleToFun scale to function. See \code{\link{zoom}}.
#' @inheritParams hover
#' @param ... named arguments to modify \code{loon} plot states. See \code{\link{l_info_states}}
#'
#' @return a \code{ggproto} object
#' @details
#' In interactive graphics, there are several fundamental infrastructures, such as querying, linking and selection.
#' Component \code{interactivity} is used to set these features.
#' \tabular{lll}{ \strong{Interactivity} \tab \strong{Description} \tab \strong{Subfunction}\cr
#'   Linking \tab Linking several plots to discover the pattern of interest \tab \code{\link{linking}}\cr
#'   Selection \tab Highlight the subset of interest \tab \code{\link{selection}}\cr
#'   Active \tab Determine which points appear \tab \code{\link{active}}\cr
#'   Hover \tab Query in interactive graphics \tab \code{\link{hover}}\cr
#'   Zoom \tab Region Modification \tab \code{\link{zoom}}\cr}
#' @examples
#' if(interactive()) {
#'   # Modify the 'linkingGroup' and 'origin' of a hist object
#'   l_ggplot(mtcars, mapping = aes(x = wt)) +
#'     geom_histogram() +
#'     interactivity(linkingGroup = "mt", origin = 2)
#'
#'   # linking with the histogram
#'   l_ggplot(mtcars, mapping = aes(x = wt, y = hp)) +
#'     geom_point(size = 4) +
#'     interactivity(linkingGroup = "mt") +
#'     facet_wrap(~cyl)
#'
#'   p <- ggplot(economics_long, aes(value)) +
#'          facet_wrap(~variable, scales = 'free_x') +
#'          geom_histogram()
#'   # `p` is a ggplot object
#'   p
#'   # turn static `ggplot` to interactive `loon`
#'   p + interactivity()
#' }
#' @export
interactivity <- function(linkingGroup = NULL,
                          linkingKey = NULL,
                          linkedStates = NULL,
                          sync = NULL,
                          active = NULL,
                          activeGeomLayers = NULL,
                          selected = NULL,
                          selectBy = NULL,
                          selectionLogic = NULL,
                          layerId = NULL,
                          scaleToFun = NULL,
                          itemLabel = NULL,
                          showItemLabels = NULL,
                          ...) {

  ggplot2::ggproto("Interactivity", NULL,
                   params = list(
                     linkingGroup = linkingGroup,
                     linkingKey = linkingKey,
                     linkedStates = linkedStates,
                     sync = sync,
                     active = active,
                     activeGeomLayers = activeGeomLayers,
                     selected = selected,
                     selectBy = selectBy,
                     selectionLogic = selectionLogic,
                     layerId = layerId,
                     scaleToFun = scaleToFun,
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
                       warning(
                         "The length of ", deparse(substitute(x)),
                         " is ", length(x),
                         " that does not match the number of observations(" ,
                         n, ").",
                         call. = FALSE
                       )
                     }
                   },
                   evalObj = function(data, params, x) {

                     obj <- params[[x]]
                     if(is.null(obj)) return(params)

                     if(rlang::is_formula(obj)) {
                       params[[x]] <- eval(obj[[2]], data)
                     } else if (is.atomic(obj)) {
                       # obj can be evaluated
                       NULL
                     } else {
                       params[[x]] <- tryCatch(
                         expr = {eval(obj, data)},
                         error = function(e) {
                           # unknown expression
                           return(NULL)
                         }
                       )
                     }

                     return(params)
                   },
                   check_itemLabel = function(self, data, params) {

                     if(!is.null(params$showItemLabels) && !is.logical(params$showItemLabels)) {
                       stop("`showItemLabels` must be logical", call. = FALSE)
                     }

                     params <- self$evalObj(data, params, "itemLabel")
                     itemLabel <- params$itemLabel
                     if(is.null(itemLabel)) return(NULL)

                     self$warn_nDim_states(data = data,
                                           x = itemLabel)

                     self$params <- params
                   },
                   check_selected = function(self, data, params) {

                     params <- self$evalObj(data, params, "selected")
                     selected <- params$selected
                     if(is.null(selected)) return(NULL)

                     if(!is.logical(selected)) {
                       stop("`selected` must be logical", call. = FALSE)
                     }

                     self$warn_nDim_states(data = data,
                                           x = selected)

                     self$params <- params
                   },
                   check_active = function(self, data, params) {

                     params <- self$evalObj(data, params, "active")
                     active <- params$active
                     if(is.null(active)) return(NULL)

                     if(!is.logical(active)) {
                       stop("`active` must be logical", call. = FALSE)
                     }

                     self$warn_nDim_states(data = data,
                                           x = active)

                     self$params <- params
                   },
                   check_linkingKey = function(self, data, params) {

                     params <- self$evalObj(data, params, "linkingKey")
                     linkingKey <- params$linkingKey
                     if(is.null(linkingKey)) return(NULL)

                     if(any(duplicated(linkingKey)))
                       stop("The linkingKey is duplicated", call. = FALSE)

                     self$warn_nDim_states(data = data,
                                           x = linkingKey)
                     self$params <- params
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
#' @return a \code{ggproto} object
#'
#' @seealso \code{\link{active}}, \code{\link{selection}}, \code{\link{zoom}},
#' \code{\link{hover}}, \code{\link{interactivity}}
#'
#' @examples
#' if(interactive() && requireNamespace("dplyr")) {
#'   h <- l_hist(mtcars$hp,
#'               linkingKey = rownames(mtcars),
#'               linkingGroup = "mtcars")
#'
#'   mtcars %>%
#'     mutate(carName = rownames(mtcars)) %>%
#'     l_ggplot(mapping = aes(x = wt, y = hp, color = factor(cyl))) +
#'        geom_point(size = 4) +
#'        # push the states of scatter plot to the histogram
#'        linking(linkingGroup = "mtcars",
#'                linkingKey = ~carName,
#'                sync = "push")
#' }
#' @export
linking <- function(linkingGroup = NULL,
                    linkingKey = NULL,
                    linkedStates = NULL,
                    sync = NULL) {

  interactivity(linkingGroup = linkingGroup,
                linkingKey = linkingKey,
                linkedStates = linkedStates,
                sync = sync)
}

#' @title Modify the \code{active} component
#' @description Set \code{active} or \code{activeGeomLayers}
#' @param active a logical determining whether points appear or
#' not (default is \code{TRUE} for all points). If a logical vector is
#' given of length equal to the number of points,
#' then it identifies which points appear (\code{TRUE}) and
#' which do not (\code{FALSE}).
#' @param activeGeomLayers determine which geom layer is interactive. Only \code{geom_point()}
#' and \code{geom_histogram()} can be set as active geom layer(s) so far.
#' (Notice, more than one \code{geom_point()} layers can be set as active layers,
#' but only one \code{geom_histogram()} can be set as an active geom layer)
#' @return a \code{ggproto} object
#'
#' @seealso \code{\link{linking}}, \code{\link{selection}}, \code{\link{zoom}},
#' \code{\link{hover}}, \code{\link{interactivity}}
#'
#' @examples
#' if(interactive()) {
#'
#'   # set active layer
#'   l_ggplot(mtcars, aes(mpg, wt, shape = factor(cyl))) +
#'     geom_point(colour = "black", size = 4.5) +
#'     geom_point(colour = "pink", size = 4) +
#'     geom_point(aes(shape = factor(cyl))) +
#'     # only show manual transmission cars
#'     # in the second interactive layer
#'     active(active = mtcars$am == 1,
#'            activeGeomLayers = 2)
#'
#'   # Then, click the `reactivate` button on loon inspector
#'   # to display all interactive points
#' }
#' @export
active <- function(active = NULL,
                   activeGeomLayers = NULL) {

  interactivity(active = active,
                activeGeomLayers = activeGeomLayers)
}

#' @title Modify the \code{selected} component
#' @description In interactive graphics, \code{selection} is one of the most fundamental tool
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
#' @seealso \code{\link{active}}, \code{\link{linking}}, \code{\link{zoom}},
#' \code{\link{hover}}, \code{\link{interactivity}}
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
#'     selection(selected = fourGear)
#' }
#' @export
selection <- function(selected = NULL,
                      selectBy = NULL,
                      selectionLogic = NULL) {

  interactivity(selected = selected,
                selectBy = selectBy,
                selectionLogic = selectionLogic)
}

#' @title Zoom Plot Region
#' @description Modify the \code{zoomX}, \code{zoomY}, \code{panX}, \code{panY},
#' etc to change the plot region
#' @param layerId numerical; which layer to scale to
#' @param scaleToFun scale to function. See details.
#'
#' @details Argument \code{layerId} is used for additional plot region settings.
#' If the \code{layerId} is set as \code{NULL} (default), the region of the
#' interactive graphics \code{loon} will be determined by the \code{ggplot} object
#' (i.e. \code{coord_cartesian}, \code{xlim}, etc);
#' else one can use \code{scaleToFun} to modify the region of the layer.
#'
#' The \code{scaleToFun} is a function to scale the region.
#' If it is \code{NULL} (default), based on different layers, different scale functions
#' will be applied. For example, if the layer is the main graphic model, i.e. \code{l_plot}
#' \code{l_hist}, then the default \code{scaleToFun} is \code{\link{l_scaleto_plot}}; else
#'  if the layer is a general \code{l_layer} widget, the default \code{scaleToFun} would be
#'  \code{\link{l_scaleto_layer}} (see \code{\link{get_activeGeomLayers}}).
#'
#' If it is not \code{NULL}, users can select one that precisely tailor their own
#' problems. The table shows the available \code{scaleToFun} functions
#' \tabular{ll}{\strong{scale to} \tab \strong{Subfunction}\cr
#'   plot \tab  \code{\link{l_scaleto_plot}}\cr
#'   world \tab \code{\link{l_scaleto_world}}\cr
#'   active \tab \code{\link{l_scaleto_active}}\cr
#'   selected \tab \code{\link{l_scaleto_selected}}\cr
#'   layer \tab \code{\link{l_scaleto_layer}}\cr}
#' Expect all these, users can customize their own function. Note that,
#' the arguments should match the ones of functions shown in the table.
#'
#' @return a \code{ggproto} object
#' @seealso \code{\link{active}}, \code{\link{linking}}, \code{\link{selection}},
#' \code{\link{hover}}, \code{\link{interactivity}}
#' @examples
#' if(interactive()) {
#' p <- l_ggplot(mtcars,
#'               mapping = aes(x = hp, y = mpg)) +
#'        geom_point(mapping = aes(color = factor(gear))) +
#'        geom_smooth(data = mtcars[mtcars$gear == 4, ],
#'                    method = "lm")
#' # a scatter plot with a fitted line on 4 gear cars
#' p
#' # scale to the second layer (smooth line)
#' p + zoom(layerId = 2)
#' # highlight the 3 gear cars
#' # scale to the selected points
#' p +
#'   selection(mtcars$gear == 3) +
#'   zoom(layerId = 1,
#'        scaleToFun = loon::l_scaleto_selected)
#' }
#' @export
zoom <- function(layerId = NULL,
                 scaleToFun= NULL) {

  interactivity(layerId = layerId,
                scaleToFun = scaleToFun)
}

#' @title Modify the \code{hover} component
#' @description Query in interactive graphics
#' @param itemLabel The customized querying information.
#' @param showItemLabels A logical value. Show item labels or not. Default is \code{FALSE}
#'
#' @return a \code{ggproto} object
#' @seealso \code{\link{active}}, \code{\link{linking}}, \code{\link{zoom}},
#' \code{\link{selection}}, \code{\link{interactivity}}
#' @examples
#' if(interactive()) {
#'
#'   l_ggplot(mpg, mapping = aes(x = displ, y = cty)) +
#'     geom_point(size = 4) +
#'     # push the states of scatter plot to the histogram
#'     hover(itemLabel =
#'        with(mpg,
#'             paste0("model: ", manufacturer, " ", model, "\n",
#'                    "year: ", year, "\n",
#'                    "drive way: ", drv, "\n",
#'                    "fuel type: ", fl)
#'        ),
#'        showItemLabels = TRUE
#'     )
#'     # hover the mouse on top of any point to query
#' }
#' @export
hover <- function(itemLabel = NULL,
                  showItemLabels = NULL) {
  interactivity(itemLabel = itemLabel,
                showItemLabels = showItemLabels)
}

#' @export
ggplot_add.Interactivity <- function(object, plot, object_name) {
  if(!is.l_ggplot(plot)) {
    class(plot) <- c("l_ggplot", class(plot))
  }
  update_interactivity(plot, object)
}

update_interactivity <- function(p, interactivity) {
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
  interactivity$check_active(data, interactivity$params)
}
