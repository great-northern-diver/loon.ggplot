################################ Unexported functions in `loon` but used in `loon.ggplot` ################################
# Since `Unexported objects imported by ':::' calls` will cause a NOTE in R CMD check

# All these functions are exported in loon 1.3.7
get_display_color <- utils::getFromNamespace("get_display_color", "loon")
as_hex6color <- utils::getFromNamespace("as_hex6color", "loon")
get_font_info_from_tk <- utils::getFromNamespace("get_font_info_from_tk", "loon")
get_layer_states <- utils::getFromNamespace("get_layer_states", "loon")
get_model_display_order <- utils::getFromNamespace("get_model_display_order", "loon")
tcl_img_2_r_raster <- utils::getFromNamespace("tcl_img_2_r_raster", "loon")
char2num.data.frame <- utils::getFromNamespace("char2num.data.frame", "loon")

# These functions are temporary
# after loon is updated to 1.3.8
# this function will be switched to
# `loon::l_colorName` and `loon::glyph_to_pch`
l_colorName <- function(color, error = TRUE, precise = FALSE) {

  color.id <- function(x, error = TRUE, precise = FALSE, env = environment()) {

    invalid.color <- c()

    colors <- vapply(x,
                     function(color) {

                       # hex code color
                       # hex12to6 will give warnings if the hex code is not 12
                       # as_hex6color can accommodate 6 digits and 12 digits code
                       tryCatch(
                         expr = {
                           color <- as_hex6color(color)
                           c2 <- grDevices::col2rgb(color)
                           coltab <- grDevices::col2rgb(colors())
                           cdist <- apply(coltab, 2, function(z) sum((z - c2)^2))
                           if(precise) {
                             if(min(cdist) == 0)
                               colors()[which(cdist == min(cdist))][1]
                             else
                               color
                           } else {
                             colors()[which(cdist == min(cdist))][1]
                           }
                         },
                         error = function(e) {

                           assign("invalid.color",
                                  c(invalid.color, color),
                                  envir = env)

                           return(color)

                         }
                       )

                     }, character(1))

    if(error && length(invalid.color) > 0) {
      stop("The input " ,
           paste(invalid.color, collapse = ", "),
           " are not valid color names", call. = FALSE)
    }
    colors
  }

  # the input colors are 6/12 digits hex code
  uniColor <- unique(color)
  colorName <- color.id(uniColor, error = error, precise = precise)
  len <- length(colorName)

  for(i in seq(len)) {
    color[color == uniColor[i]] <- colorName[i]
  }
  color
}

glyph_to_pch <- function(glyph) {

  if(any(is.na(glyph))) {
    # Set NAs of glyph as the most common value
    glyph[is.na(glyph)] <- names(sort(table(na.omit(glyph)),
                                      decreasing=TRUE)[1L])
  }

  vapply(glyph,
         function(x) {
           switch(
             x,
             circle = 19,
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


## Un-exported functions in ggplot2
# utils::getFromNamespace("message_wrap", "ggplot2")
message_wrap <- function (...)  {
  msg <- paste(..., collapse = "", sep = "")
  wrapped <- strwrap(msg, width = getOption("width") -
                       2)
  message(paste0(wrapped, collapse = "\n"))
}

# set_sec_axis <- utils::getFromNamespace("set_sec_axis", "ggplot2")
set_sec_axis <- function(sec.axis, scale) {

  if(is.waive(sec.axis)) return(scale)

  if (is.formula(sec.axis))
    sec.axis <- ggplot2::sec_axis(sec.axis)
  if (!is.sec_axis(sec.axis))
    stop("Secondary axes must be specified using 'sec_axis()'",
         call. = FALSE)
  scale$secondary.axis <- sec.axis
  return(scale)
}

new_aes <- getFromNamespace("new_aes", "ggplot2")

# It is learned from the function `get_gridAesthetic` in package `ggmulti`
# The difference is that for `get_gridAesthetic`, the output coordinate is
# in unit, however, the returned coordinates of `get_aesthetic` are numerical values
get_aesthetic <- function(axes.layout, andrews, xpos, ypos, scale.x, scale.y, xaxis, yaxis,
                          dimension, p, show.area, show.enclosing) {

  enclosingX <- enclosingY <- enclosingId <- list()
  axesX <- axesY <- axesId <- list()
  serialCoordX <- serialCoordY <- list()

  N <- length(xpos)

  # it is for specific use
  pth <- function(x, p, circle = FALSE) {
    len <- length(x)
    if(len == p) return(x)
    # In a circle, the first one and the last one are identical
    if(circle) {
      x[round(seq(1, len, length.out = p + 1))][- (p + 1)]
    } else {
      x[round(seq(1, len, length.out = p))]
    }
  }

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
