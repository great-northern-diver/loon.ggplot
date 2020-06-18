`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

is.ggmatrix <- function(x) {
  inherits(x, "ggmatrix")
}

is.ggmatrix_plot_obj <- function(x) {
  inherits(x, "ggmatrix_plot_obj")
}

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

valid_color <- function(x) {
  if(any(!is.color(x)))
    gg_color_hue(length(x))
  else
    x
}

remove_null <- function(..., as_list = TRUE) {
  if(as_list)
    Filter(Negate(is.null),
           list(...)
    )
  else
    Filter(Negate(is.null), ...)
}

rearrangePolygonData <- function(data) {
  na_x <- is.na(data$x)
  na_y <- is.na(data$y)
  if (all(na_x != na_y)) {
    data <- data[!union(na_x, na_y), ]
    return(data)
  }

  pos <- c(which(na_x), dim(data)[1])
  npolygons <- length(pos)

  if(npolygons > 1) {
    group <- c()

    for(i in 1:npolygons) {
      if(i == 1)
        group <- c(group, rep(i, pos[i]))
      else
        group <- c(group, rep(i, pos[i] - pos[i-1]))
    }

    data$group <- group
    data <- data[!na_x, ]
  }

  return(data)
}

group_id <- function(data, uniGroup) {
  group <- data$group
  vapply(uniGroup,
         function(x) {
           which(group == x)[1]
         }, numeric(1))
}

set_lineColor <- function(data, mapping, color) {

  gg_color <- function(color, check = TRUE) {

    if(is.null(color)) return(NULL)

    if(all(is.color(color)) && check)
      return(color)

    color <- if(is.numeric(color)) {

      minColor <- min(color)
      maxColor <- max(color)
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

set_lineSize <- function(data, mapping, size) {

  size <- size %||% {
    if(!"size" %in% names(mapping))
      0.5
    else
      rlang::eval_tidy(rlang::quo(!!mapping$size),  data)
  }

  if(!is.numeric(size))
    stop(
      paste(sub("~", "", rlang::expr_text(mapping$size)), "is not a numerical variable"),
      call. = FALSE
    )

  return(size)
}

wrap_num <- function(ggLayout, is_facet_wrap, is_facet_grid, tkLabels){
  if(is_facet_wrap | !tkLabels) {
    length(names(ggLayout$facet_params$facets))
  } else if(is_facet_grid) {
    length(names(ggLayout$facet_params$rows)) + length(names(ggLayout$facet_params$cols))
  } else 0
}

as_r_text_size <- function(size, digits = 2) {
  round(size/1.76, digits)
}

as_r_point_size <- function(size, digits = 2) {
  round(2*log(size), digits)
}

as_r_line_size <- function(size, digits = 2) {
  round(size/.pt, digits)
}

pixels_2_lines <- function(x, digits = 2) {
  round(x / 100, digits)
}

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
