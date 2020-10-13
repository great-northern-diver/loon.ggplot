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
#' @return a \code{loon} single or compound widget
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
#'
#' if(interactive()) {
#'   p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'   g <- ggplot2loon(p)
#'
#'   # tkLabels
#'   p <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg,
#'      colour = factor(gear))) + facet_wrap(~am)
#'   g1 <- ggplot2loon(p)
#'   g2 <- ggplot2loon(p, tkLabels = FALSE)
#' }
#'
#' \donttest{
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
  rlang::abort(paste(deparse(substitute(ggObj)), "is not a 'ggplot' or 'ggmatrix' object"))
}

#' @export
ggplot2loon.ggplot <- function(ggObj, activeGeomLayers = integer(0), ggGuides = FALSE,
                               ..., parent = NULL, pack = TRUE,
                               tkLabels = NULL, exteriorLabelProportion = 1/5,
                               canvasHeight = 700, canvasWidth = 850) {

  if(inherits(ggObj, "loon")) {
    error_info <- deparse(substitute(ggObj))
    rlang::abort(
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
    rlang::abort(paste(deparse(substitute(ggObj)), "is not a ggplot object"))
  }
  if(!is.numeric(activeGeomLayers) | !is.vector(activeGeomLayers)) {
    rlang::abort("activeGeomLayers is a numeric argument")
  }
  if(!is.logical(ggGuides)) {
    rlang::abort("ggGuides is a logical argument")
  }
  if(!is.logical(pack)) {
    rlang::abort("pack is a logical argument")
  }
  if(!is.null(tkLabels)) {
    if(!is.logical(tkLabels)) rlang::abort("tkLabels is a logical argument")
  }
  if(!is.numeric(exteriorLabelProportion)) {
    rlang::abort("exteriorLabelProportion is a numerical argument")
  } else {
    if(exteriorLabelProportion >= 1 & length(exteriorLabelProportion) != 1) {
      rlang::abort("exteriorLabelProportion is a single number between 0 to 1")
    }
  }
  if(!is.numeric(canvasHeight)) {
    rlang::abort("canvasHeight is a numerical argument")
  }
  if(!is.numeric(canvasWidth)) {
    rlang::abort("canvasWidth is a numerical argument")
  }

  plotInfo <- list()

  args <- list(...)
  plotInfo$dataFrame <- ggObj$data
  plotInfo$linkingKey <- loonLinkingKey(plotInfo$dataFrame, args)
  plotInfo$itemLabel <- loonItemLabel(plotInfo$dataFrame, args)

  # ggplot_build
  plotInfo$buildggObj <-  ggBuild2Loon(ggObj, plotInfo$linkingKey, plotInfo$itemLabel)
  plotInfo$layout <- plotInfo$buildggObj$layout
  plotInfo$ggBuild <- plotInfo$buildggObj$ggBuild
  # number of panels
  plotInfo$panelNum <- dim(plotInfo$layout)[1]

  # labels
  plotInfo$title <- ggObj$labels$title
  plotInfo$ylabel <- ggObj$labels$y
  plotInfo$xlabel <- ggObj$labels$x
  plotInfo$span <- round(1/exteriorLabelProportion)
  # serialaxes
  plotInfo$isCoordSerialaxes <- is.CoordSerialaxes(ggObj$coordinates)

  tkLabels <- tkLabels %||% plotInfo$panelNum != 1

  if (tkLabels) {
    # two ways to separate facets, facet_wrap or facet_grid
    plotInfo$FacetWrap <- plotInfo$buildggObj$FacetWrap
    plotInfo$FacetGrid <- plotInfo$buildggObj$FacetGrid
    if(plotInfo$FacetWrap) {
      plotInfo$byCOLS <- TRUE
      plotInfo$byROWS <- FALSE
    } else if(plotInfo$FacetGrid) {
      # layout multiple facets by rows or by cols
      plotInfo$layoutByROWS <- names(plotInfo$ggBuild$layout$facet_params$rows)
      plotInfo$layoutByCOLS <- names(plotInfo$ggBuild$layout$facet_params$cols)
      # by columns or by rows?
      plotInfo$byCOLS <- ifelse(length(plotInfo$layoutByCOLS) > 0, TRUE, FALSE)
      plotInfo$byROWS <- ifelse(length(plotInfo$layoutByROWS) > 0, TRUE, FALSE)
    } else {
      plotInfo$byCOLS <- FALSE
      plotInfo$byROWS <- FALSE
    }

    plotInfo$start.xpos <- ifelse(!is.null(plotInfo$ylabel), 1, 0)
    plotInfo$start.ypos <- plotInfo$start.subtitlepos <- if(!is.null(plotInfo$title)) {
      ifelse(plotInfo$FacetGrid & plotInfo$byCOLS, 2, 1)
    } else {
      ifelse(plotInfo$FacetGrid & plotInfo$byCOLS, 1, 0)
    }
    plotInfo$showLabels <- FALSE
  } else {
    plotInfo$FacetWrap <- FALSE
    plotInfo$FacetGrid <- FALSE
    plotInfo$byCOLS <- FALSE
    plotInfo$byROWS <- FALSE

    plotInfo$start.xpos <- 0
    plotInfo$start.ypos <- 0
    plotInfo$start.subtitlepos <- 0
    plotInfo$showLabels <- TRUE
  }

  if(is.null(parent)) {
    parent <- l_toplevel()
    subwin <- loon::l_subwin(parent, 'ggplot')
    tktitle(parent) <- paste("loon.ggplot", "--path:", subwin)
    parent <- as.character(tcltk::tcl('frame', subwin))
  }

  plotInfo$column <- max(plotInfo$layout$COL)
  plotInfo$row <- max(plotInfo$layout$ROW)
  plotInfo$row.span <- plotInfo$span * plotInfo$row
  plotInfo$column.span <- plotInfo$span * plotInfo$column

  sync <- args$sync %||% "pull"
  if(!sync %in% c("pull", "push")) rlang::abort("not known sync")
  plotInfo$sync <- sync
  args$sync <- NULL

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
  plotInfo$args <- args

  ## too many args to pass in `get_loon_plotInfo`, `pack_loon_plots` and `modify_loon_plots`;
  ## args will be called from environment
  plotInfo <- c(
    plotInfo,
    get_loon_plotInfo(plotInfo = plotInfo,
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
  if(pack) pack_loon_plots(plotInfo = plotInfo,
                           ggObj = ggObj,
                           parent = parent,
                           tkLabels = tkLabels)
  # modify plots
  plots <- modify_loon_plots(plotInfo = plotInfo)

  plots
}
