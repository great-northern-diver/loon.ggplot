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
l_toplevel <- utils::getFromNamespace("l_toplevel", "loon")
l_nDimStateNames <- utils::getFromNamespace("l_nDimStateNames", "loon")

l_allNDimStateNames <- function(plots) {
  states <- lapply(plots,
                   function(plot) {
                     l_nDimStateNames(plot)
                   })
  unique(unlist(states))
}


## Unexported functions in ggplot2
compute_just <- utils::getFromNamespace("compute_just", "ggplot2")
message_wrap <- utils::getFromNamespace("message_wrap", "ggplot2")
set_sec_axis <- utils::getFromNamespace("set_sec_axis", "ggplot2")
