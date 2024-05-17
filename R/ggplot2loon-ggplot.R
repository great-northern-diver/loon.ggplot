#' @title \code{ggplot} to \code{loon}
#'
#' @description Create an interactive `loon` widget from a \code{ggplot} object
#'
#' @param ggObj a \code{ggplot} or \code{ggmatrix} object
#' @param activeGeomLayers to determine which geom layer is active. Only \code{geom_point()}
#'             and \code{geom_histogram()} can be set as active geom layer(s) so far.
#'            (Notice, more than one \code{geom_point()} layers can be set as active layers,
#'             but only one \code{geom_histogram()} can be set as an active geom layer)
#' @inheritParams interactivity
#' @param ggGuides logical (default \code{FALSE}) to determine whether to draw a ggplot background or not.
#' @param ... named arguments to modify loon plot states
#' @param parent parent widget path (Tk toplevel)
#' @param pack logical (default \code{TRUE}) to pack widgets.
#'             If \code{FALSE}, widgets will be produced but won't be packed and so will not appear in the display.
#' @param exteriorLabelProportion space assigned to the vertical height/horizontal width of each exterior label
#'          expressed as a proportion of a single plot's height/width.  Default is 0.2.
#'          This is translated to a row/column span = 1 / exteriorLabelProportion for the plot size in
#'          \code{tkgrid()}.
#' @param canvasHeight the height of canvas
#' @param canvasWidth the width of canvas
#'
#' @param tkLabels Deprecated: logical (or \code{NULL}) to indicate whether the plot(s) are to be wrapped by
#'         exterior labels (title, subtitle, xlabel or ylabel) using \code{tk.grid()}
#'
#' @return a \code{loon} single widget or a compound object
#'
#'
#' @import ggplot2 tcltk loon methods grid rlang stats
#' @importFrom grDevices extendrange rgb as.raster col2rgb
#' @importFrom gridExtra arrangeGrob tableGrob
#' @importFrom ggmulti andrews geom_image_glyph geom_polygon_glyph geom_serialaxes_glyph coord_serialaxes
#' @import patchwork
#'
#' @export
#' @examples
#' if(interactive()) {
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' g <- ggplot2loon(p)
#'
#' p1 <- ggplot(mtcars) +
#'         geom_point(aes(x = wt, y = mpg,
#'                        colour = factor(gear))) +
#'         facet_wrap(~am)
#' g1 <- ggplot2loon(p1)
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
#' # The first point layer is set as the model layer
#' suppressWarnings(
#'   lp_scatterplots_active1 <- ggplot2loon(scatterplots,
#'                                activeGeomLayers = 1,
#'                                linkingGroup = "test")
#' )
#' # Here, the gray points are interactive (not the colourful ones)
#'
#' # The second point layer is set as the model layer
#' lp_scatterplots_active2 <- ggplot2loon(scatterplots,
#'                                        activeGeomLayers = 2)
#' # Here, the colourful points are interactive
#'
#' # Both point layers could be interactive
#' suppressWarnings(
#'  lp_scatterplots_active12 <- ggplot2loon(scatterplots,
#'                                          activeGeomLayers = c(1,2))
#' )
#' # Here, all points are interactive
#'
#' ########### ggmatrix to loon ###########
#' if(requireNamespace("GGally")) {
#' pm <- GGally::ggpairs(iris, column = 1:4,
#'                       ggplot2::aes(colour=Species))
#' lg <- ggplot2loon(pm)
#' }
#'
#' ########### patchwork to loon ###########
#' if(requireNamespace("patchwork")) {
#' p1 <- ggplot(mtcars) +
#'    geom_point(aes(mpg, disp))
#' p2 <- ggplot(mtcars) +
#'    geom_boxplot(aes(gear, disp, group = gear))
#' # place two plots side by side
#' patchwork <- p1 + p2
#' ggplot2loon(patchwork)
#' # See vignette `ggplots --> loon plots` for more details
#' }
#' }
#' }
ggplot2loon <- function(ggObj, ..., activeGeomLayers = integer(0),
                        layerId = NULL, scaleToFun = NULL,
                        ggGuides = FALSE, parent = NULL, pack = TRUE,
                        exteriorLabelProportion = 1/5,
                        canvasHeight = 700, canvasWidth = 850, tkLabels = NULL) {
  UseMethod("ggplot2loon", ggObj)
}

#' @export
ggplot2loon.default <- function(ggObj, ...) {
  stop(deparse(substitute(ggObj)), " is not a 'ggplot' or 'ggmatrix' object", call. = FALSE)
}

#' @export
ggplot2loon.ggplot <- function(ggObj, ..., activeGeomLayers = integer(0),
                               layerId = NULL, scaleToFun = NULL,
                               ggGuides = FALSE, parent = NULL, pack = TRUE,
                               exteriorLabelProportion = 1/5,
                               canvasHeight = 700, canvasWidth = 850, tkLabels = NULL) {

  if(inherits(ggObj, "loon")) {
    error_info <- deparse(substitute(ggObj))
    stop(
      "'ggObj' should be a ggplot object. Maybe you want to call `loon2ggplot(",
      error_info,
      ")`? Or, just call `loon.ggplot(",
      error_info,
      ")` for simplification.",
      call. = FALSE
    )
  }

  layerId <- layerId %||% 0L

  # check arguments
  if(!ggplot2::is.ggplot(ggObj)) {
    stop(deparse(substitute(ggObj)), " is not a ggplot object.", call. = FALSE)
  }
  if(!is.numeric(activeGeomLayers) || !is.vector(activeGeomLayers)) {
    stop("activeGeomLayers is a numeric argument", call. = FALSE)
  }
  if(length(activeGeomLayers) > 1) {
    warning("The length of `activeGeomLayers` is ", length(activeGeomLayers),
            " greater than ", 1,
            " that is not suggested.",
            call. = FALSE)
  }

  if(!is.numeric(layerId)) {
    stop("`layerId` is a numeric argument", call. = FALSE)
  } else {
    if(length(layerId) == 0) layerId <- 0L
    else if(length(layerId) > 1) {
      warning("`layerId` can only change plot region to display all elements of **one** particular layer",
              call. = FALSE)
      layerId <- layerId[1L]
    } else {
      if(layerId > length(ggObj$layers)) {
        layerId <- 0L
        warning("The `ggplot` object has ", length(ggObj$layers), " layers, ",
                "however, the `layerId` is set as ", layerId,
                " which is greater than ", length(ggObj$layers),
                call. = FALSE)
      }
    }
  }

  if(!is.null(scaleToFun)) {
    if(!is.function(scaleToFun))
      stop("`scaleToFun` must be a function", call. = FALSE)
  }

  if(!is.logical(ggGuides)) {
    stop("ggGuides is a logical argument", call. = FALSE)
  }
  if(!is.logical(pack)) {
    stop("pack is a logical argument", call. = FALSE)
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

  plotInfo <- list()

  args <- list(...)
  plotInfo$layerId <- layerId
  plotInfo$scaleToFun <- scaleToFun
  plotInfo$dataFrame <- ggObj$data
  plotInfo$linkingKey <- loonLinkingKey(plotInfo$dataFrame, args)
  plotInfo$itemLabel <- loonItemLabel(plotInfo$dataFrame, args)
  nDimStates <- setdiff(l_allNDimStateNames(), c("itemLabel", "linkingKey"))
  argNames <- names(args)
  nDimArgs <- argNames[argNames %in% nDimStates]
  plotInfo$nDimStates <- args[nDimArgs]
  args[nDimArgs] <- NULL

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

  # showItemLabels; default is FALSE same as loon
  plotInfo$showItemLabels <- args$showItemLabels %||% FALSE
  args$showItemLabels <- NULL

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

  if(is.null(parent)) {
    parent <- loon::l_toplevel()
    subwin <- loon::l_subwin(parent, 'ggplot')
    tcltk::tktitle(parent) <- paste("loon.ggplot", "--path:", subwin)
    parent <- as.character(tcltk::tcl('frame', subwin))
  }

  plotInfo$column <- max(plotInfo$layout$COL)
  plotInfo$row <- max(plotInfo$layout$ROW)
  plotInfo$row.span <- plotInfo$span * plotInfo$row
  plotInfo$column.span <- plotInfo$span * plotInfo$column

  sync <- args$sync %||% "pull"
  if(!sync %in% c("pull", "push")) stop("not known sync", call. = FALSE)
  plotInfo$sync <- sync
  args$sync <- NULL

  if (is.null(args[['linkingGroup']])) {
    args[['linkingGroup']] <- "none"
  }
  # set margins
  if (is.null(args[['scalesMargins']])) {
    args[['scalesMargins']] <- c(30, 50, 0, 0)
  }
  if (is.null(args[['labelMargins']])) {
    args[['labelMargins']] <- c(30, 60, 60, 0)
  }
  if (is.null(args[['minimumMargins']])) {
    args[['minimumMargins']] <- c(20, 20, 10, 10)
  }
  plotInfo$args <- args

  ## too many args to pass in `get_loon_plotInfo`, `pack_loon_plots` and `modify_loon_plots`;
  ## args will be called from environment
  plotInfo <- c(
    plotInfo,
    get_plotInfo(plotInfo = plotInfo,
                 ggObj = ggObj,
                 parent = parent,
                 activeGeomLayers = activeGeomLayers,
                 ggGuides = ggGuides,
                 pack = pack,
                 canvasHeight = canvasHeight,
                 canvasWidth = canvasWidth)
  )

  # modify plots
  plots <- modify_loon_plots(plotInfo = plotInfo)
  plotInfo$plots <- plots

  # pack labels
  if(pack)
    pack_loon_plots(plotInfo = plotInfo,
                    ggObj = ggObj,
                    parent = parent)

  plots
}
