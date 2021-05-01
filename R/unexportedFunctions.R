################################ Unexported functions in `loon` but used in `loon.ggplot` ################################
# Since `Unexported objects imported by ':::' calls` will cause a NOTE in R CMD check

glyph_to_pch <- utils::getFromNamespace("glyph_to_pch", "loon")
get_display_color <- utils::getFromNamespace("get_display_color", "loon")
as_hex6color <- utils::getFromNamespace("as_hex6color", "loon")
get_font_info_from_tk <- utils::getFromNamespace("get_font_info_from_tk", "loon")
xy_coords_layer <- utils::getFromNamespace("xy_coords_layer", "loon")
get_layer_states <- utils::getFromNamespace("get_layer_states", "loon")
get_model_display_order <- utils::getFromNamespace("get_model_display_order", "loon")
char2num.data.frame <- utils::getFromNamespace("char2num.data.frame", "loon")
cartesian_model_widget_states <- utils::getFromNamespace("cartesian_model_widget_states", "loon")
tcl_img_2_r_raster <- utils::getFromNamespace("tcl_img_2_r_raster", "loon")
color.id <- utils::getFromNamespace("color.id", "loon")

l_allNDimStateNames <- function(plots = c("l_plot", "l_plot3D", "l_serialaxes", "l_hist")) {
  states <- lapply(plots,
                   function(plot) {
                     loon::l_nDimStateNames(plot)
                   })
  unique(unlist(states))
}

## Unexported functions in ggplot2
compute_just <- utils::getFromNamespace("compute_just", "ggplot2")
message_wrap <- utils::getFromNamespace("message_wrap", "ggplot2")
set_sec_axis <- utils::getFromNamespace("set_sec_axis", "ggplot2")

## Unexported functions in ggmulti
pth <- utils::getFromNamespace("pth", "ggmulti")
# It is learned from the function `get_gridAesthetic` in package ggmulti
get_aesthetic <- function(axes.layout, andrews, xpos, ypos, scale.x, scale.y, xaxis, yaxis,
                              dimension, p, show.area, show.enclosing) {

  enclosingX <- enclosingY <- enclosingId <- list()
  axesX <- axesY <- axesId <- list()
  serialCoordX <- serialCoordY <- list()

  N <- length(xpos)

  # side effect
  if(axes.layout == "parallel") {

    lapply(1:N,
           function(i){

             # `<<-` is used inside the function of `lapply`
             # such operation only changes vars of my own namespace
             # (i.e. `loon_get_scaledData`, `get_aesthetic`, etc)
             # and global environment will not be affected.
             # The main reason is to avoid the heavy `for` loop

             # enclosing
             enclosingX[[i]] <<- xpos[i] + (c(0, 0, 1, 0, 0, 1, 1, 1) - 0.5) * scale.x[i]
             enclosingY[[i]] <<- ypos[i] + (c(0, 0, 0, 1, 1, 0, 1, 1) - 0.5) * scale.y[i]
             enclosingId[[i]] <<- rep(((i - 1)*4 + 1):(4 * i), 2)

             # axes
             axesX[[i]] <<- xpos[i] + rep(pth(xaxis[i, ], p), each = 2)
             axesY[[i]] <<- ypos[i] + rep(c(-0.5 * scale.y[i], 0.5 * scale.y[i]), p)
             axesId[[i]] <<- rep(((i - 1)*p + 1):(p * i), each = 2)

             # serialCoord
             if(show.area) {
               serialCoordX[[i]] <<- xpos[i] + c(xaxis[i, ], rev(xaxis[i, ]))
               serialCoordY[[i]] <<- ypos[i] + c(yaxis[i, ], rep(-0.5 * scale.y[i], dimension))
             } else {
               serialCoordX[[i]] <<- xpos[i] + xaxis[i, ]
               serialCoordY[[i]] <<- ypos[i] + yaxis[i, ]
             }
           })

    serialCoordId <- if(show.area) rep(1:N, each = 2*dimension) else rep(1:N, each = dimension)

  } else if (axes.layout == "radial") {

    len_radial <- 101
    angle <- seq(0, 2*base::pi, length.out = dimension + 1)[1:dimension]

    lapply(1:N,
           function(i){

             # `<<-` is used inside the function of `lapply`
             # such operation only changes vars of my own namespace
             # (i.e. `loon_get_scaledData`, `get_aesthetic`, etc)
             # and global environment will not be affected.
             # The main reason is to avoid the heavy `for` loop

             # enclosing
             enclosingX[[i]] <<- xpos[i] +
               scale.x[i] * cos(seq(0, 2*base::pi, length=len_radial))
             enclosingY[[i]] <<- ypos[i] +
               scale.y[i] * sin(seq(0, 2*base::pi, length=len_radial))


             if (show.enclosing) {

               # axes
               axesX[[i]] <<- xpos[i] +
                 c(rep(0, p), pth(scale.x[i] * cos(angle), p, TRUE))
               axesY[[i]] <<- ypos[i] +
                 c(rep(0, p), pth(scale.y[i] * sin(angle), p, TRUE))
               axesId[[i]] <<- rep(((i - 1)*p + 1):(p * i), 2)

             } else {
               # axes
               axesX[[i]] <<- xpos[i] +
                 c(rep(0, p), pth(xaxis[i, ], p, TRUE))
               axesY[[i]] <<- ypos[i] +
                 c(rep(0, p), pth(yaxis[i, ], p, TRUE))
               axesId[[i]] <<- rep(((i - 1)*p + 1):(p * i), 2)
             }

             # serialCoord
             serialCoordX[[i]] <<- xpos[i] +
               c(xaxis[i, ], rev(xaxis[i, 1]))
             serialCoordY[[i]] <<- ypos[i] +
               c(yaxis[i, ], rev(yaxis[i, 1]))
           })

    enclosingId <- rep(1:N, each = len_radial)
    serialCoordId <- rep(1:N, each = (dimension + 1))

  } else stop('unknown axes layout',
              call. = FALSE)

  list(
    enclosingX = enclosingX,
    enclosingY = enclosingY,
    enclosingId = unlist(enclosingId),
    axesX = axesX,
    axesY = axesY,
    axesId = unlist(axesId),
    serialCoordX = serialCoordX,
    serialCoordY = serialCoordY,
    serialCoordId = serialCoordId
  )
}
