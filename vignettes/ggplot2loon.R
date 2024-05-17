## ----setup, include=FALSE, warning=FALSE--------------------------------------
library(knitr)
knitr::opts_chunk$set(echo = TRUE, 
                      warning = FALSE,
                      message = FALSE,
                      fig.align = "center", 
                      fig.width = 6, 
                      fig.height = 5,
                      out.width = "60%", 
                      collapse = TRUE,
                      comment = "#>",
                      tidy.opts = list(width.cutoff = 65),
                      tidy = FALSE)

set.seed(12314159)
imageDirectory <- file.path(".", "images", "ggplots2loon")

library(ggplot2, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(magrittr, quietly = TRUE)
library(loon, quietly = TRUE)
library(patchwork, quietly = TRUE)

## ----mpg_vs_wt, message = FALSE, warning = FALSE------------------------------
p1 <- ggplot(mtcars, aes(wt, mpg)) + geom_point()

## ----mpg_vs_wt_ggplot_display, message = FALSE, warning = FALSE, fig.width = 4, fig.height = 4, fig.align = "center", out.width = "50%"----
p1 

## ----hp_ggplot, message = FALSE, warning = FALSE, fig.width = 4, fig.height = 4, fig.align = "center", out.width = "50%"----
h1 <- ggplot(mtcars, aes(hp)) + geom_histogram(bins = 30)

## ----hp_ggplot_display, message = FALSE, warning = FALSE, fig.width = 4, fig.height = 4, fig.align = "center", out.width = "50%"----
h1

## ----mpg_vs_wt_loon_display, message = FALSE, eval = FALSE, warning = FALSE, fig.width = 4, fig.height = 4, fig.align = "center", out.width = "50%"----
#  library(loon.ggplot)
#  l_p1 <- ggplot2loon(p1)  # the scatterplot
#  l_h1 <- ggplot2loon(h1)  # the histogram

## ----grid_version_of_l_p1, message = FALSE, eval = FALSE, warning = FALSE, fig.width = 5.5, fig.height = 4, fig.align = "center", out.width = "50%"----
#  grid.loon(l_p1)

## ----grid_version_of_l_p1_graph, echo = FALSE, message = FALSE, warning = FALSE, fig.width = 5.5, fig.height = 6, fig.align = "center", fig.caption = "Loon inspector Layers tab for l_p_fit`", out.width = "70%"----
include_graphics(file.path(imageDirectory, "l_plot_l_p1.png"))

## ----grid_version_of_l_h1, message = FALSE, eval = FALSE, warning = FALSE, fig.width = 5.5, fig.height = 4, fig.align = "center", out.width = "50%"----
#  plot(l_h1)   # equivalent to grid.loon(l_h1)

## ----grid_version_of_l_h1_graph, echo = FALSE, message = FALSE, warning = FALSE, fig.width = 5.5, fig.height = 6, fig.align = "center", fig.caption = "Loon inspector Layers tab for l_p_fit`", out.width = "70%"----
include_graphics(file.path(imageDirectory, "l_hist_l_h1.png"))

## ----inspector_of_l_p1, echo = FALSE, message = FALSE, warning = FALSE, fig.width = 5.5, fig.height = 4, fig.align = "center", fig.caption = "Loon inspector for the scatterplot l_p`", out.width = "25%"----
include_graphics(file.path(imageDirectory, "inspector_l_p1.png"))

## ----loon_data_structure, message = FALSE, eval = FALSE, warning = FALSE, fig.width = 4, fig.height = 4, fig.align = "center", out.width = "50%"----
#  class(l_p1)
#  l_p1
#  # [1] "l_plot" "loon"
#  # [1] ".l0.ggplot.plot"
#  # attr(,"class")
#  # [1] "l_plot" "loon"

## ----loon_scale_to_plot, message = FALSE, eval = FALSE, warning = FALSE, fig.width = 4, fig.height = 4, fig.align = "center", out.width = "50%"----
#  l_scaleto_plot(l_p1)

## ----loon_names, message = FALSE, eval = FALSE, warning = FALSE, fig.width = 4, fig.height = 4, fig.align = "center", out.width = "50%"----
#  names(l_p1)

## ----showGuides, message = FALSE, eval = FALSE, warning = FALSE, fig.width = 4, fig.height = 4, fig.align = "center", out.width = "50%"----
#  l_p1["showGuides"]
#  # [1] TRUE

## ----change_showGuides, message = FALSE, eval = FALSE, warning = FALSE, fig.width = 4, fig.height = 4, fig.align = "center", out.width = "50%"----
#  # Turn the guides off
#  l_p1["showGuides"] <- FALSE
#  # Turn the guides on again
#  l_p1["showGuides"] <- TRUE

## ----title, message = FALSE, eval = FALSE, warning = FALSE, fig.width = 4, fig.height = 4, fig.align = "center", out.width = "50%"----
#  l_p1["title"] <- "1974 Motor Trend Car Road Tests"
#  l_p1["xlabel"] <- "Curb weight (1000s of lbs)"
#  l_p1["ylabel"] <- "Gas mileage (miles per US gallon)"
#  newlabels <- paste0(rownames(mtcars), "\n   ",
#                      c("V-", "straight-")[mtcars$vs + 1], mtcars$cyl, " \n   ",
#                      mtcars$disp, " cubic inch \n   ",
#                      mtcars$gear, " speed \n   ",
#                      c("automatic", "manual")[mtcars$am + 1]
#                      )
#  l_p1["itemLabel"] <- newlabels
#  l_p1["showItemLabels"] <- TRUE

## ----grouping_by_colour_and_symbol, message = FALSE, eval = FALSE, warning = FALSE, fig.width = 4, fig.height = 4, fig.align = "center", out.width = "50%"----
#  l_p1["size"] <- 10
#  l_p1["glyph"][mtcars$am == 0] <- "ccircle"
#  l_p1["glyph"][mtcars$am == 1] <- "triangle"
#  gears <- sort(unique(mtcars$gear))
#  ngears <- length(gears)
#  cols <- c("lightblue", "steelblue", "black")
#  for (i in 1:ngears) {
#    l_p1["color"][mtcars$gear == gears[i]] <- cols[i]
#  }

## ----high_gear_manual, message = FALSE, eval = FALSE, warning = FALSE, fig.width = 4, fig.height = 4, fig.align = "center", out.width = "50%"----
#  l_p1["active"] <- l_p1["color"] == l_hexcolor("black")
#  l_scaleto_active(l_p1)
#  l_p1["title"] <- "5 speed manual transmission"

## ----high_gear_manual_grid_plot, message = FALSE, eval = FALSE, warning = FALSE,  fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
#  plot(l_p1)

## ----high_gear_manual_grid_plot_graph, echo = FALSE, message = FALSE, warning = FALSE, fig.width = 5.5, fig.height = 6, fig.align = "center", fig.caption = "Loon inspector for the scatterplot l_p`", out.width = "70%"----
include_graphics(file.path(imageDirectory, "l_plot_l_p1_zoomIn.png"))

## ----Back_to_all_points, message = FALSE, eval = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
#  l_p1["active"] <- TRUE
#  l_scaleto_plot(l_p1)
#  l_p1["title"] <- "All points again"
#  plot(l_p1)

## ----Back_to_all_points_graph, echo = FALSE, message = FALSE, warning = FALSE, fig.width = 5.5, fig.height = 6, fig.align = "center", fig.caption = "Loon inspector for the scatterplot l_p`", out.width = "70%"----
include_graphics(file.path(imageDirectory, "l_plot_l_p1_zoomOut.png"))

## ----set_initial_linking_Group, eval = FALSE----------------------------------
#  l_p1["linkingGroup"] <- "Motor Trend 1974"

## ----l_configure_linking_Group, eval = FALSE----------------------------------
#  l_configure(l_h1, linkingGroup = "Motor Trend 1974", sync = "pull")

## ----stacked_colours_plot, message = FALSE, eval = FALSE, warning = FALSE,  fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
#  l_h1["showStackedColors"] <- TRUE
#  plot(l_h1)

## ----stacked_colours_plot_graph, echo = FALSE, message = FALSE, warning = FALSE, fig.width = 5.5, fig.height = 6, fig.align = "center", fig.caption = "Loon inspector for the scatterplot l_p`", out.width = "70%"----
include_graphics(file.path(imageDirectory, "l_hist_l_h1_stacked_colours.png"))

## ----ggplot_acceleration, message = FALSE, warning = FALSE, fig.width = 6, fig.height = 5, fig.align = "center", out.width = "50%"----
# First using another ggplot
p2 <- ggplot(mtcars, aes(x = drat, y = qsec)) + geom_point()

## ----ggplot_p2, message = FALSE, warning = FALSE, fig.width = 6, fig.height = 5, fig.align = "center", out.width = "50%"----
p2

## ----stacked_colours_linking, message = FALSE, eval = FALSE, warning = FALSE,  fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
#  l_p2 <- ggplot2loon(p2,
#                      linkingGroup = "Motor Trend 1974",
#                      title = "Acceleration measures",
#                      xlabel = "Drive axle ratio",
#                      ylabel = "Quarter mile (seconds)",
#                      itemLabel = newlabels)

## ----loon_plot_l_p2, message = FALSE, eval = FALSE, warning = FALSE,  fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
#  plot(l_p2)

## ----loon_plot_l_p2_graph, echo = FALSE, message = FALSE, warning = FALSE, fig.width = 5.5, fig.height = 6, fig.align = "center", fig.caption = "Loon inspector for the scatterplot l_p`", out.width = "70%"----
include_graphics(file.path(imageDirectory, "l_plot_l_p2.png"))

## ----default_linked_states_l_plot, message = FALSE, eval = FALSE, warning = FALSE, fig.width = 6, fig.height = 5, fig.align = "center", out.width = "50%"----
#  l_getLinkedStates(l_p1)
#  # [1] "color"    "selected" "active"   "size"

## ----default_linked_states_l_hist, message = FALSE, eval = FALSE, warning = FALSE, fig.width = 6, fig.height = 5, fig.align = "center", out.width = "50%"----
#  l_getLinkedStates(l_h1)
#  # [1] "color"    "selected" "active"

## ----remove_active_from_linked_states, message = FALSE, eval = FALSE, warning = FALSE,  fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
#  l_setLinkedStates(l_p1,  c("color", "selected", "size"))

## ----add_glyph_to_linked_states, message = FALSE, eval = FALSE, warning = FALSE,  fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
#  l_setLinkedStates(l_p1, c("glyph", "active", l_getLinkedStates(l_p1)))
#  l_setLinkedStates(l_p2, c("glyph", l_getLinkedStates(l_p2)))

## ----change_glyphs, message = FALSE, eval = FALSE, warning = FALSE,  fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
#  triangles <- l_p1["glyph"] == "triangle"
#  l_p1["glyph"][triangles] <- "ctriangle"

## ----changed_glyphs_l_p1, message = FALSE, eval = FALSE, warning = FALSE,  fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
#  plot(l_p1)

## ----changed_glyphs_l_p1_graph, echo = FALSE, message = FALSE, warning = FALSE, fig.width = 5.5, fig.height = 6, fig.align = "center", fig.caption = "Loon inspector for the scatterplot l_p`", out.width = "70%"----
include_graphics(file.path(imageDirectory, "l_plot_l_p1_changed_glyphs.png"))

## ----changed_glyphs_l_p2, message = FALSE, eval = FALSE, warning = FALSE,  fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
#  plot(l_p2)

## ----changed_glyphs_l_p2_graph, echo = FALSE, message = FALSE, warning = FALSE, fig.width = 5.5, fig.height = 6, fig.align = "center", fig.caption = "Loon inspector for the scatterplot l_p`", out.width = "70%"----
include_graphics(file.path(imageDirectory, "l_plot_l_p2_changed_glyphs.png"))

## ----mtcars_smooth, message = FALSE, warning = FALSE, fig.width = 4, fig.height = 4, fig.align = "center", out.width = "50%"----
p_fit <- ggplot(mtcars, aes(drat, mpg)) + geom_smooth() + geom_point()
p_fit

## ----loon_mtcars_smooth, message = FALSE, eval = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
#  l_p_fit <- ggplot2loon(p_fit)

## ----inspector_of_l_p_fit, echo = FALSE, message = FALSE, warning = FALSE, fig.width = 5.5, fig.height = 4, fig.align = "center", fig.caption = "Loon inspector Layers tab for l_p_fit`", out.width = "25%"----
include_graphics(file.path(imageDirectory, "inspector_l_p_fit.png"))

## ----lower_layer, message = FALSE, eval = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
#  l_layer_lower(l_p_fit, "model")

## ----raise_layer, message = FALSE, eval = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
#  l_layer_raise(l_p_fit, "model")

## ----multiple_geoms, message = FALSE, warning = FALSE, fig.width = 4, fig.height = 4, fig.align = "center", out.width = "50%"----
p_hdp <- ggplot(mtcars, aes(x = wt, y = ..density..)) +
  geom_histogram(binwidth = 0.5, 
                 fill = "grey", 
                 color = "red") +
  geom_density(color = "firebrick", lwd = 1.5) +
  geom_point(data = data.frame(x = mtcars$wt, y = 0), 
             mapping = aes(x, y), 
             color = "firebrick", size = 3)
# the ggplot
p_hdp

## ----active_selected_geoms, message = FALSE, eval = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
#  # To have the the histogram active (the first geom in p_hdp)
#  l_p_Hdp <- ggplot2loon(p_hdp, activeGeomLayers = 1,
#                         linkingGroup = "Motor Trend 1974")
#  # The following creates an error because the line for the density
#  # is not currently a possible model layer in loon
#  # ggplot2loon(p_hdp, activeGeomLayers = 2,
#  #              linkingGroup = "Motor Trend 1974")
#  #
#  # To have the points be active (the third geom in p_hdp)
#  l_p_hdP <- ggplot2loon(p_hdp, activeGeomLayers = 3,
#                         linkingGroup = "Motor Trend 1974")

## ----multiple_geom_points, message = FALSE, warning = FALSE, fig.width = 4, fig.height = 4, fig.align = "center", out.width = "50%"----
pgps <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(data = subset(mtcars, gear == 3), col = "firebrick") +
  geom_point(data = subset(mtcars, gear == 4), col = "steelblue") +
  geom_point(data = subset(mtcars, gear == 5), col = "black")
pgps

## ----loon_multiple geom_points, message = FALSE, eval = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
#  l_pgps.1 <- ggplot2loon(pgps, linkingGroup = "Motor Trend 1974")
#  l_pgps.13 <- ggplot2loon(pgps, activeGeomLayers = c(1,3),
#                           linkingGroup = "Motor Trend 1974")
#  l_pgps.123 <- ggplot2loon(pgps, activeGeomLayers = c(1, 2, 3),
#                           linkingGroup = "Motor Trend 1974")

## ----linking_Key_l_p1, message = FALSE, eval = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
#  # this group's linking keys can be found from l_p1
#  groupKeys <- l_p1["linkingKey"]

## ----linking_subsets, message = FALSE, eval = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
#  # the subsets were defined as
#  dataGeom1 <- mtcars$gear == 3
#  dataGeom2 <- mtcars$gear == 4
#  dataGeom3 <- mtcars$gear == 5

## ----linking_Key_setting, message = FALSE, eval = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
#  l_configure(l_pgps.1,
#              linkingKey = groupKeys[dataGeom1],
#              sync = "pull")
#  l_configure(l_pgps.13,
#              linkingKey = c(groupKeys[dataGeom1],
#                             groupKeys[dataGeom3]),
#              sync = "pull")
#  l_configure(l_pgps.123,
#              linkingKey = c(groupKeys[dataGeom1],
#                             groupKeys[dataGeom2],
#                             groupKeys[dataGeom3]),
#              sync = "pull")

## ----linking_Key_initialized, message = FALSE, eval = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
#  l_pgps.23 <- ggplot2loon(pgps,
#                           linkingGroup = "Motor Trend 1974",
#                           activeGeomLayers = c(2,3),
#                           linkingKey = c(groupKeys[dataGeom2],
#                                          groupKeys[dataGeom3])
#                           )

## ----fwrap, message = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
fwrap <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  facet_wrap(~ gear + am) 
fwrap

## ----l_fwrap, message = FALSE, eval = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
#  l_fwrap <- ggplot2loon(fwrap, linkingGroup = "Motor Trend 1974")

## ----linking_Key_fwrap, echo = FALSE, message = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
include_graphics(file.path(imageDirectory, "l_fwrap.png"))

## ----class_l_fwrap, eval = FALSE----------------------------------------------
#  class(l_fwrap)
#  # [1] "l_ggplot"   "l_compound" "loon"

## ----l_getPlots, message = FALSE, eval = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
#  l_getPlots(l_fwrap)

## ----l_getLocations, message = FALSE, eval = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
#  l_getLocations(l_fwrap)
#  #     [,1] [,2]
#  # [1,]    1    2
#  # [2,]    3    4

## ----patchwork, message = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
library(patchwork)
p1 <- ggplot(mtcars) + geom_point(aes(mpg, disp))
p2 <- ggplot(mtcars) + geom_boxplot(aes(gear, disp, group = gear))
patchwork <- p1 + p2 # two plots are placed side by side
patchwork

## ----patchwork to loon, message = FALSE, eval = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
#  l_patchwork <- ggplot2loon(patchwork)
#  class(l_patchwork)
#  # [1] "l_patchwork"   "l_compound" "loon"

## ----patchwork linkingGroup, eval = FALSE, message = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
#  ggplot2loon(patchwork, linkingGroup = "none") # un-linked
#  ggplot2loon(patchwork, linkingGroup = "Motor Trend 1974") # join another group

## ----patchwork complex design, message = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
p3 <- ggplot(mtcars) + geom_smooth(aes(disp, qsec))
pp <- (p1 | p2) / p3
pp

## ----patchwork fail, eval = FALSE, message = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
#  # ERROR
#  ggplot2loon(pp)

## ----patchwork success, eval = FALSE, message = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
#  design <- c(
#    area(1,1),
#    area(1,2),
#    area(2,1,2,2)
#  )
#  pp <- p1 + p2 + p3 + plot_layout(design = design)
#  # Success!
#  ggplot2loon(pp)

