#' @rdname loon2ggplot
#' @export
loon2ggplot.l_serialaxes <- function(target, ...) {

  widget <- target
  remove(target)
  data <- char2num.data.frame(widget['data'])

  # active or not
  displayOrder <- get_model_display_order(widget)
  active <- widget['active'][displayOrder]
  active_displayOrder <- displayOrder[active]

  ggObj <- ggplot2::ggplot(data = data) %>%
    ggSerialAxes(scaling = widget['scaling'],
                 axesLayout = widget['axesLayout'],
                 color = get_display_color(
                   as_hex6color(widget['color'][active_displayOrder]),
                   widget['selected'][active_displayOrder]
                 ),
                 size = as_r_line_size(widget['linewidth'][active_displayOrder]),
                 axesLabels = widget['sequence'],
                 title = widget['title'],
                 showLabels =  widget['showLabels'],
                 showAxesLabels = widget['showAxesLabels'],
                 showGuides = widget['showGuides'],
                 showAxes = widget['showAxes'],
                 displayOrder = active_displayOrder,
                 showArea = widget['showArea'])

  return(ggObj)
}
