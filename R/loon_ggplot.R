#' @title Create a loon plot from a ggplot2 object
#'
#' @description Interactive loon plots from ggplots
#'
#' @param ggplotObject a ggplot object
#' @param activeGeomLayers to determine which geom layer is active. Only \code{geom_point()}
#'             and \code{geom_histogram()} can be set as active geom layer(s).
#'            (Notice, more than one \code{geom_point()} layers can be set as active layers,
#'             but only one \code{geom_histogram()} can be set as an active geom layer)
#' @param parent parent widget path (Tk toplevel)
#' @param ggGuides logical (default \code{FALSE}) to determine whether to draw a ggplot background or not.
#' @param pack logical (default \code{TRUE}) to pack widgets.
#'             If \code{FALSE}, widgets will be produced but won't be packed and so will not appear in the display.
#' @param tkLabels logical (or \code{NULL}) to indicate whether the plot(s) are to be wrapped with
#'         exterior labels (title, subtitle, xlabel or ylabel) using \code{tk.grid()}.
#'         If \code{NULL} (default), then exterior labels appear only for multiple facets.
#'         If \code{TRUE} exterior labels appear regardless; if \code{FALSE} no exterior labels appear.
#' @param exteriorLabelProportion space assigned to the vertical height/horizontal width of each exterior label
#'          expressed as a proportion of a single plot's height/width.  Default is 0.2.
#'          This is translated to a row/column span = 1 / exteriorLabelProportion for the plot size in
#'          \code{tkgrid()}.
#' @param canvasHeight the height of canvas
#' @param canvasWidth the width of canvas
#' @param ... named arguments to modify loon plot states
#'
#'
#' @import ggplot2 loon tcltk methods grid rlang
#' @importFrom stats quantile approxfun integrate setNames na.omit
#' @importFrom utils packageVersion menu data
#' @importFrom grDevices extendrange
#' @importFrom stringr str_detect
#' @importFrom gridExtra arrangeGrob tableGrob
#'
#' @export
#'
#' @examples
#'  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'  g <- loon.ggplot(p)
#'
#'  # show ggGuides
#'  p <- ggplot(mpg, aes(class, hwy)) + geom_boxplot()
#'  g <- loon.ggplot(p, ggGuides = TRUE)
#'
#'  # tkLabels
#' p <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg,
#'    colour = factor(gear))) + facet_wrap(~am)
#' g1 <- loon.ggplot(p)
#' g2 <- loon.ggplot(p, tkLabels = FALSE)
#'
#' \dontrun{
#' df <- data.frame(
#'   x = rnorm(120, c(0, 2, 4)),
#'   y = rnorm(120, c(1, 2, 1)),
#'   z = letters[1:3]
#' )
#' df2 <- dplyr::select(df, -z)
#' scatterplots <- ggplot(df, aes(x, y)) +
#'   geom_point(data = df2, colour = "grey70") +
#'   geom_point(aes(colour = z)) +
#'   facet_wrap(~z)
#'
#' # We can select the first geom_point layer to be
#' # the active layer as in
#' suppressWarnings(
#'   lp_scatterplots_active1 <- loon.ggplot(scatterplots,
#'                                activeGeomLayers = 1,
#'                                linkingGroup = "test")
#' )
#' # Here the grey points are linked (not the coloured ones)
#'
#' # We can select the second geom_point layer to be
#' # the active layer as in
#' lp_scatterplots_active2 <- loon.ggplot(scatterplots, activeGeomLayers = 2)
#' # Here the colour points are linked
#'
#' # We can also select the both geom_point layers to be
#' # the active layer as in
#' suppressWarnings(
#'  lp_scatterplots_active12 <- loon.ggplot(scatterplots, activeGeomLayers = c(1,2))
#' )
#' # Here the colour points and grey points are both linked
#' }

loon.ggplot <- function(ggplotObject, activeGeomLayers = integer(0), parent = NULL, ggGuides = FALSE,
                        pack = TRUE, tkLabels = NULL, exteriorLabelProportion = 1/5,
                        canvasHeight = 700, canvasWidth = 850, ...) {
  # check arguments
  if(!is(ggplotObject, c("gg", "ggplot"))) {
    stop(paste0(deparse(substitute(ggplotObject)), " is not a ggplot object"))
  }
  if(!is.numeric(activeGeomLayers) | !is.vector(activeGeomLayers)) {
    stop("activeGeomLayers is a numeric argument")
  }
  if(!is.null(parent)) {
    if(!is(parent, "tkwin")) stop("parent must be a Tk toplevel window")
  }
  if(!is.logical(ggGuides)) {
    stop("ggGuides is a logical argument")
  }
  if(!is.logical(pack)) {
    stop("pack is a logical argument")
  }
  if(!is.null(tkLabels)) {
    if(!is.logical(tkLabels)) stop("tkLabels is a logical argument")
  }
  if(!is.numeric(exteriorLabelProportion)) {
    stop("exteriorLabelProportion is a numerical argument")
  } else {
    if(exteriorLabelProportion >= 1 & length(exteriorLabelProportion) != 1) {
      stop("exteriorLabelProportion is a single number between 0 to 1")
    }
  }
  if(!is.numeric(canvasHeight)) {
    stop("canvasHeight is a numerical argument")
  }
  if(!is.numeric(canvasWidth)) {
    stop("canvasWidth is a numerical argument")
  }

  args <- list(...)
  dataFrame <- ggplotObject$data
  linkingKey <- loonLinkingKey(dataFrame, args)
  itemLabel <- loonItemLabel(dataFrame, args)

  # ggplot_build
  buildggplotObject <-  ggBuild2Loon(ggplotObject, linkingKey, itemLabel)
  layout_matrix <- buildggplotObject$layout_matrix
  ggBuild <- buildggplotObject$ggBuild
  # number of panels
  panelNum <- dim(layout_matrix)[1]

  # labels
  title <- ggplotObject$labels$title
  ylabel <- ggplotObject$labels$y
  xlabel <- ggplotObject$labels$x
  span <- round(1/exteriorLabelProportion)

  tkLabels <- tkLabels %||% panelNum != 1

  if (tkLabels) {
    # two ways to separate facets, facet_wrap or facet_grid
    is_facet_wrap <- buildggplotObject$is_facet_wrap
    is_facet_grid <- buildggplotObject$is_facet_grid
    if(is_facet_wrap) {
      byCOLS <- TRUE
      byROWS <- FALSE
    } else if(is_facet_grid) {
      # layout multiple facets by rows or by cols
      layoutByROWS <- names(ggBuild$layout$facet_params$rows)
      layoutByCOLS <- names(ggBuild$layout$facet_params$cols)
      # by columns or by rows?
      byCOLS <- ifelse(length(layoutByCOLS) > 0, TRUE, FALSE)
      byROWS <- ifelse(length(layoutByROWS) > 0, TRUE, FALSE)
    } else {
      byCOLS <- FALSE
      byROWS <- FALSE
    }

    start.xpos <- ifelse(!is.null(ylabel), 1, 0)
    start.ypos <- start.subtitlepos <- if(!is.null(title)) {
      ifelse(is_facet_grid & byCOLS, 2, 1)
    } else {
      ifelse(is_facet_grid & byCOLS, 1, 0)
    }
    showLabels <- FALSE
  } else {
    is_facet_wrap <- FALSE
    is_facet_grid <- FALSE
    byCOLS <- FALSE
    byROWS <- FALSE

    start.xpos <- 0
    start.ypos <- 0
    start.subtitlepos <- 0
    showLabels <- TRUE
  }

  if(is.null(parent)) {
    parent <- tktoplevel(background = "white")
    tktitle(parent) <- paste0("loon.ggplot", as.character(tktitle(parent)))
  }

  column <- max(layout_matrix$COL)
  row <- max(layout_matrix$ROW)
  row.span <- span * row
  column.span <- span * column

  sync <- args$sync %||% "pull"
  if(!sync %in% c("pull", "push")) stop("not known sync")
  if (is.null(args[['linkingGroup']])) {
    args[['linkingGroup']] <- "none"
  }
  # set margins
  if (is.null(args[['scalesMargins']]) & tkLabels) {
    args[['scalesMargins']] <- c(30, 50, 0, 0)
  }
  if (is.null(args[['labelMargins']]) & tkLabels) {
    args[['labelMargins']] <- c(30, 60, 60, 0)
  }
  if (is.null(args[['minimumMargins']]) & tkLabels) {
    args[['minimumMargins']] <- c(20, 20, 10, 10)
  }

  # wrap long codes in a function
  plots_info <- get_loon_plots_info(ggplotObject = ggplotObject,
                                    buildggplotObject = buildggplotObject,
                                    args = args)
  # pack labels
  if(pack)
    pack_loon_plots(plots = plots_info$plots,
                    ggplotObject = ggplotObject,
                    display_info = plots_info$display_info)

  plots <- modify_loon_plots(plots = plots_info$plots,
                             display_info = plots_info$display_info,
                             args = args)
  return(invisible(plots))
}
