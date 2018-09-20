## ----setup, include = FALSE----------------------------------------------
library(knitr)
library(grid)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----basic uasge, message=FALSE, eval = FALSE, echo = TRUE---------------
#  library(loon.ggplot)
#  g_scatter <- ggplot(mtcars, aes(wt, mpg)) + geom_point()

## ----basic show loonplot, fig.width = 5, fig.height = 4, eval = FALSE, echo = TRUE----
#  lp_scatter <- loon.ggplot(g_scatter)
#  grid.newpage()
#  grid.loon(lp_scatter)

## ----png basic show loonplot, eval = TRUE, echo = FALSE, fig.width = 5, fig.height = 4, fig.cap = "basic show loonplot"----
include_graphics("img/loon_ggplot/lp_scatter.png")

## ----selected loonplot, fig.width = 5, fig.height = 4, eval = FALSE, echo = TRUE----
#  cyl4 <- which((mtcars$cyl == 4) == TRUE)
#  lp_scatter$plots$x1y1['selected'][cyl4] <- TRUE
#  grid.loon(lp_scatter)

## ----png selected loonplot, eval = TRUE, echo = FALSE, fig.width = 5, fig.height = 4, fig.cap = "selected loonplot"----
include_graphics("img/loon_ggplot/lp_scatter_selected.png")

## ----ggplot with polar coordinates, fig.width = 4, fig.height = 4, message=FALSE, eval = FALSE, echo = TRUE----
#  library(ElemStatLearn)
#  g_polar <- ggplot(SAheart, aes(obesity, adiposity)) + geom_point() + geom_smooth() +
#    coord_polar(theta = "y")

## ----loonplot with polar coordinates, fig.width = 3.8, fig.height = 4, eval = FALSE, echo = TRUE----
#  lp_polar <- loon.ggplot(g_polar)
#  grid.loon(lp_polar)

## ----png loonplot with polar coordinates, eval = TRUE, echo = FALSE, fig.width = 3.8, fig.height = 4, fig.cap = "loonplot with polar coordinates"----
include_graphics("img/loon_ggplot/lp_polar_noGuides.png")

## ----loonplot with ggGuides, fig.width = 4.5, fig.height = 4.5, eval = FALSE, echo = TRUE----
#  lp_polar_ggGuides <- loon.ggplot(g_polar, ggGuides = TRUE)
#  grid.loon(lp_polar_ggGuides)

## ----png loonplot with ggGuides, fig.width = 4.5, fig.height = 4.5, eval = TRUE, echo = FALSE, fig.cap = "loonplot with ggGuides"----
include_graphics("img/loon_ggplot/lp_polar_withGuides.png")

## ----ggplot histogram and density line, fig.width = 3.5, fig.height = 3, message=FALSE, eval = FALSE, echo = TRUE----
#  g_hist_point <- ggplot(mtcars, aes(x = mpg, y = ..density..)) +
#    geom_histogram() +
#    geom_point(data = data.frame(x = mtcars$mpg, y = 0),
#               mapping = aes(x, y),
#              color = "steelblue", size = 3) +
#    geom_density()

## ----loonplot but with different active layers, fig.width = 7.5, fig.height = 3.5, eval = FALSE, echo = TRUE----
#  library(gridExtra)
#  ## check difference
#  lp_hist_point_active1 <- loon.ggplot(g_hist_point, activeGeomLayers = 1)
#  
#  suppressWarnings(
#    lp_hist_point_active2 <- loon.ggplot(g_hist_point, activeGeomLayers = 2)
#  )
#  
#  grid.arrange(loonGrob(lp_hist_point_active1), loonGrob(lp_hist_point_active2), ncol = 2, top = "lp_hist_point_active1 and lp_hist_point_active2")

## ----png loonplot but with different active layers, fig.width = 7.5, fig.height = 3.5, eval = TRUE, echo = FALSE, fig.cap = "loonplot but with different active layers"----
include_graphics("img/loon_ggplot/loonplot_with_different_activeLayers.png")

## ----selected loonplot with different active layers, fig.width = 7.5, fig.height = 3.5, eval = FALSE, echo = TRUE----
#  lp_hist_point_active1$plots$x1y1['selected'][mtcars$mpg > 20] <- TRUE
#  lp_hist_point_active2$plots$x1y1['selected'][mtcars$mpg > 20] <- TRUE
#  grid.arrange(loonGrob(lp_hist_point_active1), loonGrob(lp_hist_point_active2), ncol = 2, top = "lp_hist_point_active1 and lp_hist_point_active2")

## ----png selected loonplot with different active layers, fig.width = 7.5, fig.height = 3.5, eval = TRUE, echo = FALSE, fig.cap = "selected loonplot with different active layers"----
include_graphics("img/loon_ggplot/loonplot_with_different_activeLayers_selected.png")

## ----show errors, eval = FALSE, echo = TRUE------------------------------
#  # Error
#  loon.ggplot(g_hist_point, activeGeomLayers = 3)
#  # Error in activeInfo(importantLayers, activeGeomLayers, len_layers) : This layer cannot be active
#  loon.ggplot(g_hist_point, activeGeomLayers = c(1,2))
#  # Error in activeInfo(importantLayers, activeGeomLayers, len_layers) : histogram layer and point layer cannot be active at the same time

## ----activeGeomLayers, eval = FALSE, echo = TRUE-------------------------
#  activeGeomLayers(g_hist_point)

## ----ggplot with several point layers, fig.width = 5, fig.height = 3.5, eval = FALSE, echo = TRUE----
#  df <- data.frame(
#    x = rnorm(120, c(0, 2, 4)),
#    y = rnorm(120, c(1, 2, 1)),
#    z = letters[1:3]
#  )
#  df2 <- dplyr::select(df, -z)
#  g_three_scatterplots <- ggplot(df, aes(x, y)) +
#    geom_point(data = df2, colour = "grey70") +
#    geom_point(aes(colour = z)) +
#    facet_wrap(~z)

## ----selected loonplot with several point layers, eval = FALSE, echo = TRUE----
#  suppressWarnings(
#    lp_three_scatterplots_active1 <- loon.ggplot(g_three_scatterplots, activeGeomLayers = 1)
#  )
#  lp_three_scatterplots_active2 <- loon.ggplot(g_three_scatterplots, activeGeomLayers = 2)
#  
#  suppressWarnings(
#    lp_three_scatterplots_active12 <- loon.ggplot(g_three_scatterplots, activeGeomLayers = c(1,2))
#  )
#  grid.arrange(
#    textGrob(
#      c("What","is", "the" ,"difference?"),
#      x = c(0.2, 0.4, 0.6, 0.8),
#      y = c(0.8, 0.6, 0.4, 0.2),
#      gp = gpar(col = "grey80", fontsize = 18),
#      rot = c(30, 60, 30, 60)
#    ),
#    loonGrob(lp_three_scatterplots_active1),
#    loonGrob(lp_three_scatterplots_active2),
#    loonGrob(lp_three_scatterplots_active12),
#    ncol = 2,
#    nrow = 2,
#    top = "plot1 vs plot2 vs plot3")

## ----png loonplot with several point layers layout, fig.width=7.5, fig.height=7, fig.show='hold', eval = TRUE, echo = FALSE, fig.cap="loonplot with several point layers layout"----
include_graphics("img/loon_ggplot/loonplot_with_several_point_layers_layout.png")

## ----selected loonplot with several point layers layout, fig.width=7.5, fig.height=7, fig.show='hold', eval = FALSE, echo = TRUE----
#  # lp_three_scatterplots_active1
#  sel_lp_three_scatterplots_active1 <- lapply(lp_three_scatterplots_active1,
#                        function(loon_plot){
#                          loon_plot['selected'] <- TRUE
#                        })
#  # lp_three_scatterplots_active2
#  sel_lp_three_scatterplots_active2 <- lapply(lp_three_scatterplots_active2,
#                        function(loon_plot){
#                          loon_plot['selected'] <- TRUE
#                        })
#  # lp_three_scatterplots_active12
#  sel_lp_three_scatterplots_active12 <- lapply(lp_three_scatterplots_active12,
#                        function(loon_plot){
#                          loon_plot['selected'] <- TRUE
#                        })
#  grid.arrange(
#    textGrob(
#      c("What","is", "your" ,"conclusion?"),
#      x = c(0.2, 0.4, 0.6, 0.8),
#      y = c(0.8, 0.6, 0.4, 0.2),
#      gp = gpar(col = "grey80", fontsize = 18),
#      rot = c(30, 60, 30, 60)
#    ),
#    loonGrob(lp_three_scatterplots_active1),
#    loonGrob(lp_three_scatterplots_active2),
#    loonGrob(lp_three_scatterplots_active12),
#    ncol = 2,
#    nrow = 2,
#    top = "plot1 vs plot2 vs plot3")

## ----png selected loonplot with several point layers layout, fig.width=7.5, fig.height=7, fig.show='hold', eval = TRUE, echo = FALSE, fig.cap="selected loonplot with several point layers layout"----
include_graphics("img/loon_ggplot/selected_loonplot_with_several_point_layers_layout.png")

## ----reset ggplot, eval = FALSE, echo = TRUE-----------------------------
#  g_three_scatterplots <- ggplot(df, aes(x, y)) +
#    geom_point(aes(colour = z)) +
#    geom_point(data = df2, colour = "grey70") +
#    facet_wrap(~z)

## ----ggplot arranged plots, fig.width = 7, fig.height = 3.5, fig.show='hold', message=FALSE, eval = FALSE, echo = TRUE----
#  g_linking_scatter <- ggplot(mtcars, aes(mpg, wt, colour = as.factor(cyl))) + geom_point()
#  g_linking_hist <- ggplot(mtcars, aes(x = mpg, fill = as.factor(cyl))) + geom_histogram()

## ----selected loonplot with linkingGroup and linkingKey, fig.width=7, fig.height=3.5, fig.show='hold', eval = FALSE, echo = TRUE----
#  lp_linking_scatter <- loon.ggplot(g_linking_scatter, linkingGroup = "mtcars")
#  lp_linking_hist <- loon.ggplot(g_linking_hist, linkingGroup = "mtcars")
#  lp_linking_scatter$plots$x1y1['selected'][which((mtcars$cyl == 4) == TRUE)] <- TRUE
#  # or
#  # lp_linking_hist$plots$x1y1['selected'][which((mtcars$cyl == 4) == TRUE)] <- TRUE
#  grid.arrange(loonGrob(lp_linking_scatter), loonGrob(lp_linking_hist), ncol = 2)

## ----png selected loonplot with linkingGroup and linkingKey, fig.width=7, fig.height=3.5, fig.show='hold', eval = TRUE, echo = FALSE, fig.cap="selected loonplot with linkingGroup and linkingKey"----
include_graphics("img/loon_ggplot/selected_loonplot_with_linkingGroup_and_linkingKey.png")

## ----geom_curve in loonplot, eval = FALSE, echo = TRUE-------------------
#  df <- data.frame(x = 1:3, y = 1:3)
#  df2 <- data.frame(x = c(1,3), y = c(1,3),
#                    xend = c(2,2), yend = c(2,2))
#  g_curve <- ggplot(df, aes(x, y)) +
#    geom_point() +
#    geom_curve(aes(x = x ,y = y,
#                   xend = xend, yend = yend),
#               data = df2,
#               color = c("red", "blue"))
#  loon.ggplot(g_curve)

## ----tidyverse, message=FALSE, eval = FALSE, echo = TRUE-----------------
#  library(magrittr)
#  library(tidyverse)
#  SAheart %>%
#    mutate(ltob = log(tobacco), lsbp = log(sbp)) %>%
#    filter(age < 50) %>%
#    ggplot(aes(x = ltob, y = lsbp)) +
#    geom_point() +
#    facet_wrap(~chd) -> gg
#  # the plot will be drawn at the end
#  lp_pipe <- loon.ggplot(gg)

## ----failed tidyverse when piping ggplot, eval = FALSE, echo = TRUE------
#  ## Error occures
#  SAheart %>%
#    mutate(ltob = log(tobacco), lsbp = log(sbp)) %>%
#    filter(age < 50) %>%
#    ggplot(aes(x = ltob, y = lsbp)) +
#    geom_point() +
#    facet_wrap(~chd) %>%
#    loon.ggplot()

## ----ggpipe, fig.width=7, fig.height=3.5, eval = FALSE, echo = TRUE------
#  ## Works
#  SAheart %>%
#    mutate(ltob = log(tobacco), lsbp = log(sbp)) %>%
#    filter(age < 50) %>%
#    gg_pipe(
#      ggplot(aes(x = ltob, y = lsbp)) +
#        geom_point() +
#        facet_wrap(~chd)
#    ) %>%
#    loon.ggplot() %>%
#    grid.loon()

## ----png ggpipe, fig.width=7, fig.height=3.5, fig.cap="ggpipe", eval = TRUE, echo = FALSE----
include_graphics("img/loon_ggplot/ggpipe.png")

