#' @title \code{ggplot} to \code{loon}
#'
#' @description Create an interactive `loon` widget from a \code{ggplot} object
#'
#' @param ggObj a \code{ggplot} or \code{ggmatrix} object
#' @param activeGeomLayers to determine which geom layer is active. Only \code{geom_point()}
#'             and \code{geom_histogram()} can be set as active geom layer(s) so far.
#'            (Notice, more than one \code{geom_point()} layers can be set as active layers,
#'             but only one \code{geom_histogram()} can be set as an active geom layer)
#' @param ggGuides logical (default \code{FALSE}) to determine whether to draw a ggplot background or not.
#' @param ... named arguments to modify loon plot states
#' @param parent parent widget path (Tk toplevel)
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
#'
#'
#' @import ggplot2 tcltk loon methods grid rlang
#' @importFrom stats quantile approxfun integrate setNames na.omit
#' @importFrom utils packageVersion menu data
#' @importFrom grDevices extendrange rgb as.raster col2rgb
#' @importFrom stringr str_detect
#' @importFrom gridExtra arrangeGrob tableGrob
#' @importFrom GGally ggmatrix
#'
#' @export
#' @examples
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' g <- ggplot2loon(p)
#'
#' # tkLabels
#' p <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg,
#'    colour = factor(gear))) + facet_wrap(~am)
#' g1 <- ggplot2loon(p)
#' g2 <- ggplot2loon(p, tkLabels = FALSE)
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
#'   lp_scatterplots_active1 <- ggplot2loon(scatterplots,
#'                                activeGeomLayers = 1,
#'                                linkingGroup = "test")
#' )
#' # Here the grey points are linked (not the coloured ones)
#'
#' # We can select the second geom_point layer to be
#' # the active layer as in
#' lp_scatterplots_active2 <- ggplot2loon(scatterplots, activeGeomLayers = 2)
#' # Here the colour points are linked
#'
#' # We can also select the both geom_point layers to be
#' # the active layer as in
#' suppressWarnings(
#'  lp_scatterplots_active12 <- ggplot2loon(scatterplots, activeGeomLayers = c(1,2))
#' )
#' # Here the colour points and grey points are both linked
#'
#' ########### ggmatrix to loon ###########
#' pm <- GGally::ggpairs(iris, column = 1:4, ggplot2::aes(colour=Species))
#' lg <- ggplot2loon(pm)
#' }
#'
#'
ggplot2loon <- function(ggObj, activeGeomLayers = integer(0), ggGuides = FALSE,
                        ..., parent = NULL, pack = TRUE,
                        tkLabels = NULL, exteriorLabelProportion = 1/5,
                        canvasHeight = 700, canvasWidth = 850) {
  UseMethod("ggplot2loon", ggObj)
}

#' @export
ggplot2loon.default <- function(ggObj, ...) {
  stop(paste(deparse(substitute(ggObj)), "is not a 'ggplot' or 'ggmatrix' object"), call. = FALSE)
}

#' @export
ggplot2loon.ggplot <- function(ggObj, activeGeomLayers = integer(0), ggGuides = FALSE,
                               ..., parent = NULL, pack = TRUE,
                               tkLabels = NULL, exteriorLabelProportion = 1/5,
                               canvasHeight = 700, canvasWidth = 850) {

  if(inherits(ggObj, "loon")) {
    error_info <- deparse(substitute(ggObj))
    stop(
      paste0(
        "'ggObj' should be a ggplot object. ",
        "Maybe you want to call `loon2ggplot(",
        error_info,
        ")`?",
        "Or, just call `loon.ggplot(`",
        error_info,
        ")` for simplification."
      ),
      call. = FALSE
    )
  }

  # check arguments
  if(!ggplot2::is.ggplot(ggObj)) {
    stop(paste(deparse(substitute(ggObj)), "is not a ggplot object"), call. = FALSE)
  }
  if(!is.numeric(activeGeomLayers) | !is.vector(activeGeomLayers)) {
    stop("activeGeomLayers is a numeric argument", call. = FALSE)
  }
  if(!is.logical(ggGuides)) {
    stop("ggGuides is a logical argument", call. = FALSE)
  }
  if(!is.logical(pack)) {
    stop("pack is a logical argument", call. = FALSE)
  }
  if(!is.null(tkLabels)) {
    if(!is.logical(tkLabels)) stop("tkLabels is a logical argument", call. = FALSE)
  }
  if(!is.numeric(exteriorLabelProportion)) {
    stop("exteriorLabelProportion is a numerical argument", call. = FALSE)
  } else {
    if(exteriorLabelProportion >= 1 & length(exteriorLabelProportion) != 1) {
      stop("exteriorLabelProportion is a single number between 0 to 1", call. = FALSE)
    }
  }
  if(!is.numeric(canvasHeight)) {
    stop("canvasHeight is a numerical argument", call. = FALSE)
  }
  if(!is.numeric(canvasWidth)) {
    stop("canvasWidth is a numerical argument", call. = FALSE)
  }

  plots_info <- list()

  args <- list(...)
  plots_info$dataFrame <- ggObj$data
  plots_info$linkingKey <- loonLinkingKey(plots_info$dataFrame, args)
  plots_info$itemLabel <- loonItemLabel(plots_info$dataFrame, args)

  # ggplot_build
  plots_info$buildggObj <-  ggBuild2Loon(ggObj, plots_info$linkingKey, plots_info$itemLabel)
  plots_info$layout <- plots_info$buildggObj$layout
  plots_info$ggBuild <- plots_info$buildggObj$ggBuild
  # number of panels
  plots_info$panelNum <- dim(plots_info$layout)[1]

  # labels
  plots_info$title <- ggObj$labels$title
  plots_info$ylabel <- ggObj$labels$y
  plots_info$xlabel <- ggObj$labels$x
  plots_info$span <- round(1/exteriorLabelProportion)

  tkLabels <- tkLabels %||% plots_info$panelNum != 1

  if (tkLabels) {
    # two ways to separate facets, facet_wrap or facet_grid
    plots_info$is_facet_wrap <- plots_info$buildggObj$is_facet_wrap
    plots_info$is_facet_grid <- plots_info$buildggObj$is_facet_grid
    if(plots_info$is_facet_wrap) {
      plots_info$byCOLS <- TRUE
      plots_info$byROWS <- FALSE
    } else if(plots_info$is_facet_grid) {
      # layout multiple facets by rows or by cols
      plots_info$layoutByROWS <- names(plots_info$ggBuild$layout$facet_params$rows)
      plots_info$layoutByCOLS <- names(plots_info$ggBuild$layout$facet_params$cols)
      # by columns or by rows?
      plots_info$byCOLS <- ifelse(length(plots_info$layoutByCOLS) > 0, TRUE, FALSE)
      plots_info$byROWS <- ifelse(length(plots_info$layoutByROWS) > 0, TRUE, FALSE)
    } else {
      plots_info$byCOLS <- FALSE
      plots_info$byROWS <- FALSE
    }

    plots_info$start.xpos <- ifelse(!is.null(plots_info$ylabel), 1, 0)
    plots_info$start.ypos <- plots_info$start.subtitlepos <- if(!is.null(plots_info$title)) {
      ifelse(plots_info$is_facet_grid & plots_info$byCOLS, 2, 1)
    } else {
      ifelse(plots_info$is_facet_grid & plots_info$byCOLS, 1, 0)
    }
    plots_info$showLabels <- FALSE
  } else {
    plots_info$is_facet_wrap <- FALSE
    plots_info$is_facet_grid <- FALSE
    plots_info$byCOLS <- FALSE
    plots_info$byROWS <- FALSE

    plots_info$start.xpos <- 0
    plots_info$start.ypos <- 0
    plots_info$start.subtitlepos <- 0
    plots_info$showLabels <- TRUE
  }

  if(is.null(parent)) {
    parent <- l_toplevel()
    subwin <- loon::l_subwin(parent, 'ggplot')
    tktitle(parent) <- paste("loon.ggplot", "--path:", subwin)
    parent <- as.character(tcltk::tcl('frame', subwin))
  }

  plots_info$column <- max(plots_info$layout$COL)
  plots_info$row <- max(plots_info$layout$ROW)
  plots_info$row.span <- plots_info$span * plots_info$row
  plots_info$column.span <- plots_info$span * plots_info$column

  sync <- args$sync %||% "pull"
  if(!sync %in% c("pull", "push")) stop("not known sync", call. = FALSE)
  plots_info$sync <- sync

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
  plots_info$args <- args

  ## too many args to pass in `get_loon_plots_info`, `pack_loon_plots` and `modify_loon_plots`;
  ## args will be called from environment
  plots_info <- c(
    plots_info,
    get_loon_plots_info(plots_info = plots_info,
                        ggObj = ggObj,
                        parent = parent,
                        activeGeomLayers = activeGeomLayers,
                        ggGuides = ggGuides,
                        pack = pack,
                        tkLabels = tkLabels,
                        canvasHeight = canvasHeight,
                        canvasWidth = canvasWidth)
  )
  # pack labels
  if(pack) pack_loon_plots(plots_info = plots_info,
                           ggObj = ggObj,
                           parent = parent,
                           tkLabels = tkLabels)
  # modify plots
  plots <- modify_loon_plots(plots_info = plots_info)

  plots
}
