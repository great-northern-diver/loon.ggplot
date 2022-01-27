`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

char2null <- function(x, warn = FALSE, message = "") {
  if(length(x) == 0) {
    if(warn) {
      stop(warn, call. = FALSE)
    }
    return(NULL)
  }
  x
}

is.GlobalEnv <- function(env) {

  if(is.environment(env)) {
    environmentName(env) == environmentName(globalenv())
  } else FALSE

}
is.waive <- function (x) inherits(x, "waiver")
is.formula <- function (x) inherits(x, "formula")
is.sec_axis <- function (x) inherits(x, "AxisSecondary")
is.l_compound <- function(x) inherits(x, "l_compound")
is.l_facet <- function(x) inherits(x, "l_facet")
is.ggmatrix <- function(x) inherits(x, "ggmatrix")
is.ggmatrix_plot_obj <- function(x) inherits(x, "ggmatrix_plot_obj")
is.element_blank <- function(x) inherits(x, "element_blank")
is.patchwork <- function(x) inherits(x, "patchwork")

# default aesthetics attributes in loon
loon_default_setting <- function(x) {
  switch(x,
         "color" = "grey60",
         "size" = 4,
         "glyph" =  "ccircle",
         "linewidth" = 1,
         "radius" = 0.5,
         "boundaryLineWidth" = 1.3)
}

one_dim_state <- function(x) {
  x <- na.omit(x)
  if(length(x) == 0) return(NULL)
  x[1L]
}

is.color <- function(colors) {

  colors <- as.character(colors)

  # get rid of numerical values and NAs
  if(any(is.na(colors))) is_color <- FALSE
  else {

    suppressWarnings(
      {
        is_color <- all(is.na(as.numeric(colors)))
      }
    )

    if(is_color) {
      is_color <- all(
        sapply(colors,
               function(color) {
                 tryCatch(is.matrix(col2rgb(color)),
                          error = function(e) FALSE)
               })
      )
    }
  }

  is_color
}

remove_null <- function(..., as_list = TRUE) {
  if(as_list)
    Filter(Negate(is.null),
           list(...)
    )
  else
    Filter(Negate(is.null), ...)
}

plot_range <- function(x = "x.range", panelParams, flip = FALSE) {
  if(flip) {
    x <- if(grepl("x", x)) {
      gsub("x", "y", x)
    } else {
      gsub("y", "x", x)
    }
  }

  panelParams[[x]] %||% c(0, 1)
}

len_unique <- function(x, incomparables = FALSE, ...) {
  length(unique(x, incomparables, ...))
}


set_tkLabel <- function(labelBackground = tryCatch(loon::l_getOption("facetLabelBackground"),
                                                   error = function(e) "gray80"),
                        labelForeground = loon::l_getOption("foreground"),
                        labelBorderwidth = 2, labelRelief = "ridge",
                        xlabelBackground = loon::l_getOption("canvas_bg_guides"),
                        xlabelForeground = loon::l_getOption("foreground"),
                        xlabelBorderwidth = 2, xlabelRelief = "flat",
                        ylabelBackground = loon::l_getOption("canvas_bg_guides"),
                        ylabelForeground = loon::l_getOption("foreground"),
                        ylabelBorderwidth = 2, ylabelRelief = "flat",
                        titleBackground = loon::l_getOption("canvas_bg_guides"),
                        titleForeground = loon::l_getOption("foreground"),
                        titleBorderwidth = 2, titleRelief = "flat") {
  list(
    labelBackground = labelBackground,
    labelForeground = labelForeground,
    labelBorderwidth = labelBorderwidth,
    labelRelief = labelRelief,
    xlabelBackground = xlabelBackground,
    xlabelForeground = xlabelForeground,
    xlabelBorderwidth = xlabelBorderwidth,
    xlabelRelief = xlabelRelief,
    ylabelBackground = ylabelBackground,
    ylabelForeground = ylabelForeground,
    ylabelBorderwidth = ylabelBorderwidth,
    ylabelRelief = ylabelRelief,
    titleBackground = titleBackground,
    titleForeground = titleForeground,
    titleBorderwidth = titleBorderwidth,
    titleRelief = titleRelief
  )
}

set_lineColor <- function(data, mapping, color) {

  gg_color <- function(color, check = TRUE) {

    if(is.null(color)) return(NULL)

    if(all(is.color(color)) && check)
      return(color)

    color <- if(is.numeric(color)) {

      minColor <- min(color, na.rm = TRUE)
      maxColor <- max(color, na.rm = TRUE)
      n <- length(color)

      minRGB <- grDevices::col2rgb("lightblue")
      maxRGB <- grDevices::col2rgb("darkblue")

      rgbM <- matrix((color - minColor)/(maxColor - minColor), ncol = 1) %*%
        t(minRGB - maxRGB) +
        t(matrix(rep(maxRGB, n), nrow = 3))

      grDevices::rgb(red = rgbM[, "red"],
                     green = rgbM[, "green"],
                     blue = rgbM[, "blue"],
                     maxColorValue = 255)

    } else {
      color <- as.factor(color)
      color_levels <- levels(color)

      n <- length(color_levels)
      # defualt gg color
      gg_color_levels <- grDevices::hcl(h = seq(15, 375, length = n + 1),
                                        l = 65, c = 100)[1:n]

      sapply(color, function(col) gg_color_levels[which(color_levels %in% col)])
    }

    return(color)
  }

  color <- gg_color(color, check = TRUE) %||% {
    if(!"colour" %in% names(mapping))
      "black"
    else {
      gg_color(color = rlang::eval_tidy(rlang::quo(!!mapping$colour),  data),
               check = FALSE)
    }
  }

  return(color)
}

wrap_num <- function(ggLayout, FacetWrap, FacetGrid){
  if(FacetWrap) {
    length(names(ggLayout$facet_params$facets))
  } else if(FacetGrid) {
    length(names(ggLayout$facet_params$rows)) + length(names(ggLayout$facet_params$cols))
  } else 0
}

facet_scales <- function(free) {

  x <- free$x
  y <- free$y

  if(x && y) {
    "free"
  } else if (x && !y) {
    "free_x"
  } else if (!x && y) {
    "free_y"
  } else {
    # !x && !y
    "fixed"
  }
}

utils::globalVariables(c("PANEL", "axes.sequence", "density", "group",
                         "height", "positive", "setup_mapping", "x", "y",
                         "size", "color", "colour", "ymax", "ymin", "fill",
                         "..density..", "label"))

get_textCoords <- function(angle, anchor, just) {

  angle <- angle * pi / 180

  switch(anchor,
         "center" = {
           hjust <- 0
           vjust <- 0
         },
         "n" = {
           hjust <- 1/2 * sin(angle)
           vjust <- -1/2 * cos(angle)
         },
         "e" = {
           hjust <- -1/2 * cos(angle)
           vjust <- -1/2 * sin(angle)
         },
         "s" = {
           hjust <- - 1/2 * sin(angle)
           vjust <- 1/2 * cos(angle)
         },
         "w" = {
           hjust <- 1/2 * cos(angle)
           vjust <- 1/2 * sin(angle)
         },
         "sw" = {
           hjust <- - 1/2 * sin(angle) +
             1/2 * cos(angle)
           vjust <- 1/2 * cos(angle) +
             1/2 * sin(angle)
         },
         "nw" = {
           hjust <- 1/2 * sin(angle) +
             1/2 * cos(angle)
           vjust <- -1/2 * cos(angle) +
             1/2 * sin(angle)
         },
         "ne" =  {
           hjust <-  1/2 * sin(angle)  +
             (-1/2) * cos(angle)
           vjust <- -1/2 * cos(angle)  +
             (-1/2) * sin(angle)
         },
         "se" = {
           hjust <- - 1/2 * sin(angle)  +
             (-1/2) * cos(angle)
           vjust <- 1/2 * cos(angle)  +
             (-1/2) * sin(angle)
         }
  )
  # just can only be "left", "right" and "center"
  if(just == "left") {
    hjust <- hjust - 1/2 * cos(angle)
    vjust <- vjust - 1/2 * sin(angle)
  } else if(just == "right") {
    hjust <- hjust + 1/2 * cos(angle)
    vjust <- vjust + 1/2 * sin(angle)
  }

  c(hjust, vjust)
}

get_hjust <- function(just) {
  sapply(just,
         function(j){
           switch(j,
                  "right" = 0,
                  "left" = 1,
                  "center" = 0.5)
         })
}

l_layer_getUngroupedChildren <- function(widget, target) {

  loon::l_isLoonWidget(widget) || stop("widget does not seem to exist", call. = FALSE)
  children <- loon::l_layer_getChildren(target)
  layer <- lapply(children,
                  function(child) {
                    target <- loon::l_create_handle(c(widget, child))
                    if(is(target, "l_layer_group")) {
                      # do recursive
                      l_layer_getUngroupedChildren(widget, target)
                    } else {
                      target
                    }
                  }
  )

  unlist(layer, recursive = TRUE)
}

not_in_column_names <- function(colnames, name = "", pattern = "") {

  while(name %in% colnames) {
    name <- paste0(name, pattern)
  }
  name
}

################################ TODO List ################################

## loon.ggplot
# 1. l_layer_lines does not work well on "dash" (l_layer_line is fine) *
# 2. arrow for every l_layer_line and l_layer_lines need to be added *
# 3. legend
# 4. transparent color (maybe impossible to do) *
# 5. specfic TODOs (before some loonLayer functions, random guess numbers)
# 6. ggplot_build: need to rebuild for some specific data (eg: ts data) *
# 7. geom_histogram: transform to l_hist() or just leave it as l_plot() adding l_layer_rectangles() *
# 8. bar labels *

## loon2ggplot
# 1. point, line size needs to be modified more correctly
# 2. ```
#    p <- l_pairs(iris, showSerialAxes = TRUE);
#    g <- loon2ggplot(p)
#    ```
#    the serialaxes at left bottom corner; The reason is `ggmatrix` cannot set different layout size for each object.
#    https://stackoverflow.com/questions/57958684/create-a-matrix-of-ggplot-objects-with-different-layout-size-by-ggmatrix
# 3. If we try
#    ```
#    library(GGally)
#    data(flea)
#    ggpairs(flea, columns = 2:4)
#    pm <- ggpairs(flea, columns = 2:4, ggplot2::aes(colour=species))
#    lg <- ggplot2loon(pm)
#    grid.loon(lg)
#    ```
#    Do not build loonGrob.l_ggmatrix(), so there are no strips for static plot. It may be not worthy to do
# 4. So far, pip serialaxes is like following:
#    ```
#    p <- ggplot(data = mtcars, mapping = aes(colour = as.factor(cyl))) %>%
#           ggSerialAxes()
#    ```
#    Is it neccesary to build `geom_serialaxes()` (May take too much effort and no meaningful x y axes)
#    or just set `data` and `mapping` args in `ggSerialAxes()` instead of receive `ggplot` object?
# 5. Unknown bugs...
