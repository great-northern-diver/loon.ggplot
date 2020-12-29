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
                                 y = -glyph_data$polygon_y,
                                 linewidth = glyph_data$linewidth,
                                 showArea = showArea)
  if(showArea) widget['color'] <- fill
  widget['size'] <- as_loon_glyph_size(glyph_data$size, type = "polygon")
  widget['glyph'] <- g
}

l_add_glyph.GeomSerialAxesGlyph <- function(widget, ggBuild, activeGeomLayer) {

  glyph_data <- ggBuild$data[[activeGeomLayer]]
  fill <- glyph_data$fill
  showArea <- !any(is.na(fill))
  g <- loon::l_glyph_add_serialaxes(widget,
                                    data = glyph_data[, grepl("serialaxes.data", colnames(glyph_data))],
                                    linewidth = glyph_data$linewidth,
                                    scaling = one_dim_state(glyph_data$scaling),
                                    axesLayout = one_dim_state(glyph_data$axes.layout),
                                    showAxes = one_dim_state(glyph_data$show.axes),
                                    showEnclosing = one_dim_state(glyph_data$show.enclosing),
                                    axesColor = one_dim_state(glyph_data$show.enclosing),
                                    bboxColor = one_dim_state(glyph_data$bboxcolour),
                                    showArea = showArea)
  if(showArea) widget['color'] <- fill
  widget['size'] <- as_loon_glyph_size(glyph_data$size, type = "serialaxes")
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
                                    png::writePNG(image, temporary_path)
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
  g <- loon::l_glyph_add_image(widget,
                               imgs)
  widget['size'] <- as_loon_glyph_size(glyph_data$size, type = "image")
  widget['glyph'] <- g
}

set_temporary_path <- function(i) {

  temp <- "temporary_image"

  while(file.exists(paste0(temp, i, ".png"))) {
    i <- i + 1
  }

  return(paste0(temp, i, ".png"))
}

as_loon_glyph_size <- function(x, type = "image") {
  switch(type,
         "image" = 600 * x,
         {
           4 * x
         })
}
