################################ Unexported functions in `loon` but used in `loon.ggplot` ################################
# Since `Unexported objects imported by ':::' calls` will cause a NOTE in R CMD check

## Unexported functions in loon
as_r_polygonGlyph_size <- getFromNamespace("as_r_polygonGlyph_size", "loon")
get_scaledData <- getFromNamespace("get_scaledData", "loon")
as_r_serialaxesGlyph_size <- getFromNamespace("as_r_serialaxesGlyph_size", "loon")
glyph_to_pch <- getFromNamespace("glyph_to_pch", "loon")
get_display_color <- getFromNamespace("get_display_color", "loon")
as_hex6color <- getFromNamespace("as_hex6color", "loon")
get_font_info_from_tk <- getFromNamespace("get_font_info_from_tk", "loon")
xy_coords_layer <- getFromNamespace("xy_coords_layer", "loon")
get_layer_states <- getFromNamespace("get_layer_states", "loon")
get_model_display_order <- getFromNamespace("get_model_display_order", "loon")
char2num.data.frame <- getFromNamespace("char2num.data.frame", "loon")
cartesian_model_widget_states <- getFromNamespace("cartesian_model_widget_states", "loon")
tcl_img_2_r_raster <- getFromNamespace("tcl_img_2_r_raster", "loon")
l_toplevel <- getFromNamespace("l_toplevel", "loon")
l_nDimStateNames <- getFromNamespace("l_nDimStateNames", "loon")

l_allNDimStateNames <- function(plots) {
  states <- lapply(plots,
                   function(plot) {
                     l_nDimStateNames(plot)
                   })
  unique(unlist(states))
}


## Unexported functions in ggplot2

ggname <- getFromNamespace("ggname", "ggplot2")
compute_just <- getFromNamespace("compute_just", "ggplot2")
message_wrap <- getFromNamespace("message_wrap", "ggplot2")
