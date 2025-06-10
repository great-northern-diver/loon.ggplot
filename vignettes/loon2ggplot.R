## ----setup, warning=FALSE, include=FALSE--------------------------------------
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
library(knitr)
set.seed(12314159)
imageDirectory <- file.path(".", "images", "loon2ggplots")

library(ggplot2, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(nycflights13, quietly = TRUE)
library(loon, quietly = TRUE)
library(magrittr, quietly = TRUE)
library(maps, quietly = TRUE)
library(patchwork, quietly = TRUE)

## ----l_plot, message = FALSE,  warning = FALSE, eval = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
#  library(dplyr)
#  library(loon)
#  mt <- mtcars %>%
#    rename(transmission = am, weight = wt, horsepower = hp) %>%
#    mutate(lp100km = (100 * 3.785411784) / (1.609344 * mpg))
#  
#  p <- mt %>%
#    with(
#      l_plot(horsepower, lp100km,
#             color = gear)
#    )

## ----l_plot_to_gg, message = FALSE,  warning = FALSE, eval = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
#  library(loon.ggplot)
#  g1 <- loon2ggplot(p)
#  g1

## ----l_plot_to_gg_graph, echo = FALSE, message = FALSE,  warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
include_graphics(file.path(imageDirectory, "ggScatter.png"))

## ----l_plot_to_gg_modification, message = FALSE, eval = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
#  g1 +
#    scale_fill_manual(values = c("#999999", "#A6CEE3", "#FFC0CB"),
#                      name = "gear",
#                      labels = c("4", "3", "5")) +
#    ggtitle(label = "horsepower versus lp100km",
#            subtitle = "loon --> ggplot") +
#    theme(
#      plot.title = element_text(color = "red", size = 12, face = "bold"),
#      plot.subtitle = element_text(color = "blue")
#    )

## ----l_plot_to_gg_modification_graph, echo = FALSE, message = FALSE,  warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
include_graphics(file.path(imageDirectory, "ggScatter_modification.png"))

## ----loon pairs, message = FALSE, warning = FALSE, eval = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
#  mt %>%
#    select(lp100km, weight, transmission) %>%
#    # and pass the built plot on
#    l_pairs(showHistograms = TRUE,
#            linkingGroup = "Motor Trend 1974") ->  # and assign the result.
#    l_pp

## ----p1_piped_staic  ggplot, message = FALSE, eval = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
#  library(patchwork)
#  g2 <- loon2ggplot(l_pp)
#  g2

## ----p1_piped_staic_graph, echo = FALSE, message = FALSE,  warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
include_graphics(file.path(imageDirectory, "loon_l_pairs.png"))

## ----p1_piped_modification  ggplot, message = FALSE, eval = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
#  # Add a regression line on the `lp100km vs weight` scatterplot
#  g2$patches$plots[[1]] <- g2$patches$plots[[1]] +
#    geom_smooth(method = "lm")
#  # Add a density curve on the `weight` histogram
#  g2$patches$plots[[4]] <- g2$patches$plots[[4]] +
#    geom_density()
#  # Add a title
#  g2 <- g2 +
#    patchwork::plot_annotation(title = "Mtcars Pairs Plot")
#  g2

## ----p1_piped_modification_graph, echo = FALSE, message = FALSE,  warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
include_graphics(file.path(imageDirectory,"loon_l_pairs_modification.png"))

