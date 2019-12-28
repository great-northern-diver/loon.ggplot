`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

is.ggmatrix <- function(x) {
  inherits(x, "ggmatrix")
}

is.ggmatrix_plot_obj <- function(x) {
  inherits(x, "ggmatrix_plot_obj")
}

default_radius <- function() 0.5

is.color <- function(colors) {
  sapply(colors,
         function(color) {
           tryCatch(is.matrix(grDevices::col2rgb(color)),
                    error = function(e) FALSE)
         })
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

  loon::l_isLoonWidget(widget) || stop("widget does not seem to exist")
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

################################ Unexported functions in `loon` but used in `loon.ggplot` ################################
# Since `Unexported objects imported by ':::' calls` will cause a NOTE in R CMD check
as_r_polygonGlyph_size <- function(size, digits = 2){
  if (is.numeric(size)) {
    # trial and error to choose scale for size
    size <- size/1.25
    size[size < 0.01] <- 0.01
    size
  }
  round(size, digits)
}

get_scaledData <- function(data,
                           sequence = NULL,
                           scaling = c("variable", "observation", "data", "none"),
                           displayOrder = NULL) {

  # data is the original data set
  # since "variable" scaling is based on the original data
  if(missing(data) || is.null(data) || is.null(displayOrder)) return(NULL)

  if(!is.null(sequence)) {
    col_name <- colnames(data)
    if(!all(sequence %in% col_name)) {
      warning("unknown variable names in sequence")
      sequence <- intersect(sequence, col_name)
    }
    data <-  data[, sequence]
  }

  scaling <- match.arg(scaling)

  is_char <- FALSE
  is_factor <- FALSE
  is_logical <- FALSE

  dat <- sapply(data,
                function(x) {
                  if(is.numeric(x)) x
                  else if(is.character(x)) {
                    is_char <<- TRUE
                    as.numeric(as.factor(x))
                  } else if (is.factor(x)) {
                    is_factor <<- TRUE
                    as.numeric(x)
                  } else if(is.logical(x)) {
                    is_logical <<- TRUE
                    as.numeric(x)
                  } else stop("unknown data structure")
                })
  # give warning once
  if(is_char || is_factor || is_logical)
    warning("No numerical columns exist", call. = FALSE)

  if(length(displayOrder) == 1) {
    dat <- setNames(as.data.frame(matrix(dat, nrow = 1)), names(dat))
    if(scaling == "variable") {
      warning("Only one observation in serialAxesData, 'scaling' will be set as 'data' by default")
      scaling <- 'data'
    }
  }

  switch(scaling,
         "variable" = {
           minV <- apply(dat, 2, "min")
           maxV <- apply(dat, 2, "max")
           dat <- dat[displayOrder, ]
           t(
             (t(dat) - minV) / (maxV  - minV)
           )
         },
         "observation" = {
           minO <- apply(dat, 1, "min")
           maxO <- apply(dat, 1, "max")
           dat <- (dat - minO) / (maxO - minO)
           dat[displayOrder, ]
         },
         "data" = {
           minD <- min(dat)
           maxD <- max(dat)
           dat <- dat[displayOrder, ]
           (dat - minD)/ (maxD - minD)
         },
         "none" = {
           dat[displayOrder, ]
         })

}

as_r_serialaxesGlyph_size <- function(size, coord, axesLayout, digits = 2){
  if (is.numeric(size)) {
    # trial and error to choose scale for size
    if (axesLayout == "radial") {
      size <- sqrt(size) * 5
    } else if (axesLayout == "parallel"){
      if (coord == "x") {
        size <- sqrt(size) * 6.4
      } else if (coord == "y"){
        size <- sqrt(size) * 3.2
      } else size <- NA
    } else size <- NA
    size[size == 0] <- 0.01
  }
  round(size, digits)
}

glyph_to_pch <- function(glyph) {

  vapply(glyph, function(x) {
    switch(
      x,
      circle = 16,
      ocircle = 1,
      ccircle = 21,
      square = 15,
      osquare = 0,
      csquare = 22,
      triangle = 17,
      otriangle = 2,
      ctriangle = 24,
      diamond = 18,
      odiamond = 5,
      cdiamond = 23,
      NA_integer_
    )
  }, numeric(1))

}

get_display_color <- function(color, selected) {

  sel_color <- as.character(l_getOption("select-color"))

  if (grepl("^#", sel_color) && nchar(sel_color) == 13) {
    sel_color <- hex12tohex6(sel_color)
  }

  color[selected] <- sel_color
  color
}

as_hex6color <- function(color) {

  if(length(color) > 0){
    col <- vapply(color, function(x) {
      if (x == "") "" else loon::l_hexcolor(x)
    }, character(1))
    col <- suppressWarnings(hex12tohex6(col))
    col[color == ""] <- NA
    col
  } else {
    NA
  }
}

get_font_info_from_tk <- function(tkFont) {

  fontInfo <- as.character(.Tcl(paste("font actual", tkFont)))
  fontInfo <- matrix(fontInfo, ncol = 2, byrow = TRUE)

  fontFamily <- fontInfo[fontInfo[,1] == "-family", 2]
  if (!fontFamily %in% c("sans", "mono", "serif", "symbol")) fontFamily <- "sans"

  fontSize <- fontInfo[fontInfo[,1] == "-size", 2]
  if (fontSize <= 0) fontSize <- 8

  fontFace <- fontInfo[fontInfo[,1] == "-weight", 2]
  if (!fontFace %in% c("plain",
                       "bold",
                       "italic",
                       "oblique",
                       "bold-italic")
  ) fontFace <- "plain"

  list(family = fontFamily, face = fontFace, size = fontSize)
}

xy_coords_layer <- function(layer, native_unit = TRUE) {

  if (!is(layer, "l_layer")) stop("layer argument needs to be an l_layer")

  widget <- loon::l_create_handle(attr(layer, "widget"))

  type <- loon::l_layer_getType(attr(layer, "widget"), layer)

  xy <- if (type %in% c("scatterplot", "graph") ) {


    list(
      x = if (length(widget['xTemp']) == 0) widget['x'] else widget['xTemp'],
      y = if (length(widget['yTemp']) == 0) widget['y'] else widget['yTemp']
    )
  } else if (type %in% c('polygon', 'line', 'rectangle', 'text', 'oval',
                         'points', 'texts', 'polygons', 'rectangles', 'lines')) {
    list(
      x = loon::l_cget(layer, "x"),
      y = loon::l_cget(layer, "y")
    )
  } else if(type == "histogram"){
    list(
      x = loon::l_cget(widget, "x"),
      y = NA
    )
  } else {
    stop("unknown layer type ", type)
  }

  if (widget['swapAxes']) {
    names(xy) <- c("y", "x")
  }

  if (native_unit & length(xy$x) != 0 & length(xy$y) != 0) {
    xy <- if (type %in% c('polygons', 'rectangles', 'lines')) {
      list(x = lapply(xy$x, function(xi) grid::unit(xi, "native")),
           y = lapply(xy$y, function(yi) grid::unit(yi, "native")))
    } else {
      list(x = grid::unit(xy$x, "native"), y = grid::unit(xy$y, "native"))
    }

  }
  xy
}

get_layer_states <- function(target, omit = NULL, native_unit = TRUE) {

  if (!is(target, "loon")) {
    target <- loon::l_create_handle(target)
  }

  if (is(target, "l_layer")) {
    layer <- target
    widget <- loon::l_create_handle(attr(target, "widget"))
    obj_states <- target
  } else {
    widget <- target
    layer <- loon::l_create_handle(c(as.vector(widget), "model"))
    obj_states <- widget
  }

  states_info <- loon::l_info_states(obj_states)
  state_names <- setdiff(names(states_info), c(omit, cartesian_model_widget_states))

  states <- stats::setNames(lapply(state_names,
                                   function(state) l_cget(target, state)),
                            state_names)

  # Add Coordinates
  if (!is(layer, "l_layer_group")) {
    states <- c(xy_coords_layer(layer, native_unit = native_unit), states)
  }


  # Deal with color
  is_color <- vapply(states_info[state_names],
                     function(s) s$type %in% c("color", "colorOrTransparent"),
                     logical(1))
  if (any(is_color)) {
    for (state_name in state_names[is_color]) {
      states[[state_name]] <- as_hex6color(states[[state_name]])
    }
  }

  states
}

getBinData <- function(widget) {

  dict_get <- function(d, keys) {
    tcltk::.Tcl(paste0("dict get $", d, " ", paste(keys, collapse = " ")))
  }

  dict_with <- function(d, expr) {
    as.character(tcltk::.Tcl(paste("dict with", paste(d, collapse = " "), paste("{", expr, "}"))))
  }

  tcl_obj_varname <- function(widget, varname = NULL) {
    x <- tcltk::tcl("info", "object", "namespace", widget)

    if (!is.null(varname)) {
      x <- paste(x, varname, sep="::")
    }
    x
  }


  loon::l_throwErrorIfNotLoonWidget(widget)

  tclbins <- tcl_obj_varname(widget, "bins")

  ## see oo_Histogram_Model.tcl
  sapply(dict_with(tclbins, "dict keys $bin"), function(binid) {

    keys_count <- dict_with(c(tclbins, "bin", binid), "dict keys $count")
    keys_points <- dict_with(c(tclbins, "bin", binid), "dict keys $points")

    list(
      count = sapply(keys_count, function(x) {
        as.numeric(dict_get(tclbins, c("bin", binid, "count", x)))
      }, USE.NAMES = TRUE, simplify = FALSE),
      points = sapply(keys_points, function(x) {
        as.numeric(dict_get(tclbins, c("bin", binid, "points", x)))
      }, USE.NAMES = TRUE, simplify = FALSE),
      x0 = as.numeric(dict_get(tclbins, c("bin", binid, "x0"))),
      x1 = as.numeric(dict_get(tclbins, c("bin", binid, "x1")))
    )
  }, USE.NAMES = TRUE, simplify = FALSE)
}

get_model_display_order <- function(widget) {

  n <- loon::l_cget(widget, "n")

  if (n == 0) {
    numeric(0)
  } else {

    can <- paste0(widget, ".canvas")
    id <- as.numeric(tcl(can, "find", "withtag", paste("layer", "model", sep = "&&")))

    i <- vapply(id, function(id_i) {
      tags <- as.character(tcl(can, "gettags", id_i))

      if (length(tags) >= 4) {
        as.numeric(sub("^item", "", tags[4]))
      } else {
        NA_integer_
      }
    }, numeric(1))

    order <- if (any(is.na(i))) {
      seq_len(n)
    } else {
      ii <- i+1
      ii[!duplicated(ii)]
    }
    # TODO: A bug in l_serialaxes;
    # In l_serialaxes, `tcl` only return active model order instead of full order
    # It should be fixed in `tcl`, so far it is fixed in R temporarily.
    c(setdiff(1:n, order), order)
  }
}

char2num.data.frame <- function(chardataframe){

  dat <- as.data.frame(suppressWarnings(sapply(chardataframe, as.numeric)))
  NAcolumn <- which(
    apply(dat, 2,
          function(column){
            any(is.na(column))
          }
    ) == TRUE
  )
  if(length(NAcolumn) > 0){
    for(i in 1:length(NAcolumn)){
      dat[, NAcolumn[i]] <- as.numeric(as.factor(chardataframe[, NAcolumn[i]])) - 1
    }
    if(!is.data.frame(dat)) {
      as.data.frame(dat)
    } else dat
  } else dat
}

cartesian_model_widget_states <- c(
  "x", "y", "swapAxes",
  "tag", "itemLabel",
  "useLoonInspector", "selectionLogic",  "linkingGroup",
  "zoomX", "zoomY", "panY", "panX", "deltaX", "deltaY",
  "linkingKey", "linkingKey",  "showItemLabels",  "selectBy",
  "background", "foreground", "guidesBackground", "guidelines",
  "minimumMargins", "labelMargins", "scalesMargins", "xTemp", "yTemp",
  "showScales",  "title", "showLabels", "showGuides",  "xlabel", "ylabel"
)


tcl_img_2_r_raster <- function(img) {
  if (!(img %in% as.character(tcl("image", "names"))))
    stop("image does not exist")

  height <- as.numeric(tcl("image", "height", img))
  width <- as.numeric(tcl("image", "width", img))

  img_data <- unlist(strsplit(toupper(as.character(tcl(img, 'data'))), " "))
  img_mat <- matrix(img_data, nrow = height, byrow = TRUE)

  grDevices::as.raster(img_mat)
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
