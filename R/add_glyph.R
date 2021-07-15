add_glyph <- function(widget, ggBuild, activeGeomLayers) {
  lapply(activeGeomLayers,
         function(activeGeomLayer) {
           l_add_glyph(widget, ggBuild, activeGeomLayer)
         })
  widget
}

l_add_glyph <- function(widget, ggBuild, activeGeomLayer) {
  geom <- ggBuild$plot$layers[[activeGeomLayer]]$geom
  UseMethod("l_add_glyph", geom)
}

l_add_glyph.default <- function(widget, ggBuild, activeGeomLayer) {
  # no glyphs are added
  NULL
}

l_add_glyph.GeomPolygonGlyph <- function(widget, ggBuild, activeGeomLayer) {
  glyph_data <- ggBuild$data[[activeGeomLayer]]
  fill <- glyph_data$fill
  showArea <- !any(is.na(fill))
  g <- loon::l_glyph_add_polygon(widget,
                                 x = glyph_data$polygon_x,
                                 y = lapply(glyph_data$polygon_y, `-`),
                                 linewidth = glyph_data$linewidth,
                                 showArea = showArea)
  if(showArea) widget['color'] <- fill
  widget['size'] <- as_loon_size(glyph_data$size,
                                 type = "polygon",
                                 adjust = 1/0.6)
  widget['glyph'] <- g
}

l_add_glyph.GeomText <- function(widget, ggBuild, activeGeomLayer) {

  glyph_data <- ggBuild$data[[activeGeomLayer]]
  label <- glyph_data$label
  if(any(grepl("[\r\n\t\a' ']", label))) {
    message("The 'space', 'carriage returns', or 'new lines' is not legal in `l_glyph_add_text()`.",
            "They will be replaced by the underscore `_`.",
            "If you do not want the text to be interactive, ",
            "set `activeGeomLayers = 0L`")
    # remove space \r, \n, \t, ...
    label <- gsub("[\r\n\t\a' ']", ".", label)
  }

  g <- loon::l_glyph_add_text(widget, text = label)
  widget['size'] <- as_loon_size(glyph_data$size, type = "text")
  widget['glyph'] <- g
}

l_add_glyph.GeomTextGlyph <- function(widget, ggBuild, activeGeomLayer) {
  l_add_glyph.GeomText(widget, ggBuild, activeGeomLayer)
}

l_add_glyph.GeomPointrange <- function(widget, ggBuild, activeGeomLayer) {

  glyph_data <- ggBuild$data[[activeGeomLayer]]

  showArea <- if(any(glyph_data$shape %in% c(0, 1, 2))) {
    TRUE
  } else FALSE

  g <- loon::l_glyph_add_pointrange(widget,
                                    showArea = showArea,
                                    ymin = glyph_data$ymin,
                                    ymax = glyph_data$ymax)

  widget['size'] <- as_loon_size(glyph_data$size,
                                 stroke = glyph_data$stroke)
  widget['glyph'] <- g
}

l_add_glyph.GeomPointrangeGlyph <- function(widget, ggBuild, activeGeomLayer) {
  l_add_glyph.GeomPointrange(widget, ggBuild, activeGeomLayer)
}

l_add_glyph.GeomSerialAxesGlyph <- function(widget, ggBuild, activeGeomLayer) {

  glyph_data <- ggBuild$data[[activeGeomLayer]]
  fill <- glyph_data$fill
  showArea <- !any(is.na(fill))

  serialaxes.data <- glyph_data[, grepl("serialaxes.data", colnames(glyph_data))]

  g <- loon::l_glyph_add_serialaxes(widget,
                                    data = serialaxes.data,
                                    linewidth = glyph_data$linewidth,
                                    scaling = one_dim_state(glyph_data$scaling),
                                    andrews = one_dim_state(glyph_data$andrews),
                                    axesLayout = one_dim_state(glyph_data$axes.layout),
                                    showAxes = one_dim_state(glyph_data$show.axes),
                                    showEnclosing = one_dim_state(glyph_data$show.enclosing),
                                    axesColor = one_dim_state(glyph_data$show.enclosing),
                                    bboxColor = one_dim_state(glyph_data$bboxcolour),
                                    showArea = showArea)
  if(showArea) widget['color'] <- fill
  widget['size'] <- as_loon_size(glyph_data$size,
                                 type = one_dim_state(glyph_data$axes.layout),
                                 p = ncol(serialaxes.data))
  widget['glyph'] <- g
}

l_add_glyph.GeomImageGlyph <- function(widget, ggBuild, activeGeomLayer) {

  glyph_data <- ggBuild$data[[activeGeomLayer]]
  images <- glyph_data$images
  n <- nrow(glyph_data)
  # save images temporarily
  temporary_image_paths <- lapply(seq(n),
                                  function(i) {
                                    image <- images[[i]]
                                    # save raster
                                    temporary_path <- set_temporary_path(i)
                                    if(grDevices::is.raster(image)) {
                                      nc <- ncol(image)
                                      nr <- nrow(image)
                                      image <- grDevices::col2rgb(image, alpha = FALSE)/255
                                      dim(image) <- c(3, nc, nr)
                                      image <- aperm(image, c(3,2,1))
                                    }
                                    tryCatch(
                                      expr = {
                                        png::writePNG(image, temporary_path)
                                      },
                                      error = function(e) {
                                        warning(
                                          "The class of the input image is ", class(image),
                                          ", which cannot be handled yet. Welcome to report to ",
                                          "https://github.com/great-northern-diver/loon.ggplot/issues",
                                          call. = FALSE
                                        )
                                      }
                                    )

                                    temporary_path
                                  })
  # load images
  imgs <- sapply(temporary_image_paths, function(path) {loon::l_image_import_files(path)})
  # destroy temporary files
  lapply(temporary_image_paths,
         function(temporary_path) {
           if(file.exists(temporary_path)) {
             file.remove(temporary_path)
           }
         })
  g <- loon::l_glyph_add_image(widget, imgs)
  widget['size'] <- as_loon_size(glyph_data$imageheight * glyph_data$size,
                                 type = "images",
                                 adjust = 1/0.6,
                                 ratio = glyph_data$imageheight/glyph_data$imagewidth)
  widget['glyph'] <- g
}

set_temporary_path <- function(i) {

  tmp <- tempdir()
  temp <- "temporary_image"

  while(file.exists(paste0(tmp, "\\", temp, i, ".png"))) {
    i <- i + 1
  }

  return(paste0(tmp, "\\", temp, i, ".png"))
}
