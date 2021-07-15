# `as_loon_size` and `as_ggplot_size` are invertible functions
as_ggplot_size <- function(size,
                           type = c("points", "texts", "images",
                                    "radial", "parallel", "polygon",
                                    "lines"),
                           adjust = 1, # A HACK
                           ...) {

  type <- match.arg(type)
  # **`size` is a loon size**
  # **returned a ggplot size**
  if (!is.numeric(size)) {
    warning(
      "The class of the `size` is ",
      class(size),
      " which is not numerical. It will be set as the default `ggplot` point size ",
      ggplot2::GeomPoint$default_aes$size,
      call. = FALSE
    )
    return(ggplot2::GeomPoint$default_aes$size)
  }

  # From Adrian Waddell's Thesis
  # Glyph Type:
  switch(
    type,
    points = {
      ## Non-primitive Glyph
      ## size < 1 --> 8 (area in pixel)
      ## size >= 1 --> 12 * size (area in pixel)
      area <- ifelse(size < 1, 8, 12 * size)
      # pixel(unit)
      diameter.px <- sqrt(area/pi) * 2
      # pixel(unit) to pt(unit)
      diameter.pt <- diameter.px * px2pt(adjust = adjust)
      # formula which is defined by `ggplot2::GeomPoints`
      (diameter.pt - ggplot2::GeomPoint$default_aes$stroke * ggplot2::.stroke/2)/ggplot2::.pt
    },
    texts = {
      ## Text Glyph
      ## size < 1 --> 2 (area in pixel)
      ## size >= 1 --> 2 + size (area in pixel)
      area <- ifelse(size < 1, 2, 2 + size)
      # pixel(unit) to pt(unit)
      area.pt <- area * px2pt(adjust = adjust)
      # formula which is defined by `ggplot2::GeomText`
      (area.pt)/ggplot2::.pt
    },
    images = {
      args <- list(...)
      # ratio = height/width
      ratio <- args$ratio
      ## Image Glyph
      ## size < 1 --> 20 (area in pixel)
      ## size >= 1 --> 600 * size (area in pixel)
      area <- ifelse(size < 1, 20, 600 * size)
      # height
      height.px <- sqrt(area * ratio)
      # output unit is cm
      height.px * px2cm(adjust = adjust)
    },
    polygon = {
      # output unit is cm
      ifelse(size < 1, 4, 6 * sqrt(size)) * px2cm(adjust = adjust)
    },
    radial = {
      area <- ifelse(size < 1, 25, 400 * size)
      # pixel(unit)
      diameter.px <- sqrt(area/pi) * 2
      # output unit is cm
      diameter.px * px2cm(adjust = adjust)
    },
    parallel = {
      args <- list(...)
      # ratio = height/width
      p <- args$p
      area <- ifelse(size < 1, 9 * (p - 1), 64 * (p - 1) * size)
      # height:width = 1:2
      # return height
      sqrt(area/2) * px2cm(adjust = adjust)
    },
    lines = {
      # output unit is mm
      # suppose the unit of loon is in px
      # size/(cm2px()/10 * ggplot2::.pt)
      # suppose the unit of loon is in mm
      size/ggplot2::.pt * px2pt(adjust = adjust)
    }
  )
}

as_loon_size <- function(size,
                         type = c("points", "texts", "images",
                                  "radial", "parallel", "polygon",
                                  "lines"),
                         adjust = 1,
                         ...) {

  type <- match.arg(type)

  # **`size` is a ggplot size**
  # **returned a loon size**
  if(is.null(size))
    return(as.numeric(loon::l_getOption("size")))

  switch(type,
         points = {
           args <- list(...)
           stroke <- args$stroke %||% ggplot2::GeomPoint$default_aes$stroke
           if(any(is.na(stroke))) stroke <- ggplot2::GeomPoint$default_aes$stroke
           # From Adrian Waddell's Thesis
           # Glyph Type:
           ## Non-primitive Glyph
           ## size < 1 --> 8 (area in pixel)
           ## size >= 1 --> 12 * size (area in pixel)
           diameter.pt <- size * .pt + stroke  * .stroke / 2
           diameter.px <- diameter.pt * pt2px(adjust = adjust)
           area <- (diameter.px/2)^2 * pi
           ifelse(area < 8, 1, area/12)
         },
         lines = {
           # output unit is mm
           # suppose the unit of loon is in px
           # (cm2px()/10 * ggplot2::.pt) * size
           # suppose the unit of loon is in mm
           size * ggplot2::.pt * pt2px(adjust = adjust)
         },
         texts = {
           ## Text Glyph
           ## size < 1 --> 2 (area in pixel)
           ## size >= 1 --> 2 + size (area in pixel)
           area.pt <- size * .pt
           area <- area.pt * pt2px(adjust = adjust)
           ifelse(area < 2, 1, (area - 2))
         },
         images = {
           args <- list(...)
           # ratio = height/width
           ratio <- args$ratio
           ## Image Glyph
           ## size < 1 --> 20 (area in pixel)
           ## size >= 1 --> 600 * size (area in pixel)

           # size is height
           height.px <- size * cm2px(adjust = adjust)
           area <- height.px^2/ratio
           ifelse(area < 20, 1, area/600)
         },
         polygon = {
           # unit is cm
           # to px
           size <- size * cm2px(adjust = adjust)
           ifelse(size < 4, 1, (size/6)^2)
         },
         radial = {
           diameter.px <- size * cm2px(adjust = adjust)
           area <- (diameter.px/2)^2 * pi
           ifelse(area < 25, 1, area/400)
         },
         parallel = {
           args <- list(...)
           # ratio = height/width
           p <- args$p
           area <- (size * cm2px(adjust = adjust))^2 * 2
           ifelse(area < 9 * (p - 1), 1, area/(64 * (p - 1)))
         }
  )
}

pt2px <- function(adjust = 1) 4/3 * adjust
px2pt <- function(adjust = 1) 3/4 * adjust
cm2px <- function(adjust = 1) {
  # grid::convertUnit(grid::unit(1, "cm"), "pt",
  #                   valueOnly = TRUE) * pt2px(adjust = adjust)
  .pt * 10 * pt2px(adjust = adjust)
}
px2cm <- function(adjust = 1) {
  # grid::convertUnit(grid::unit(1, "pt"), "cm",
  #                   valueOnly = TRUE) * px2pt(adjust = adjust)
  px2pt(adjust = adjust)/(.pt * 10)
}
