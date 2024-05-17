## ----setup, include=FALSE, warning=FALSE, message=FALSE-----------------------
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
imageDirectory <- file.path(".", "images", "pipes")

library(ggplot2, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(magrittr, quietly = TRUE)
library(loon, quietly = TRUE)

## ----pipes, message = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
library(dplyr)  # load this to also have dplyr functionality
library(magrittr)
library(ggplot2)
p1_piped <- mtcars %>%
  rename(transmission = am, weight = wt) %>%
  mutate(lp100km = (100 * 3.785411784) / (1.609344 * mpg)) %>%
  select(weight, lp100km) %>%
  ggplot(aes(x = weight, y = lp100km)) +
  geom_point() +
  ylab("Litres per 100 kilometres") +
  ggtitle("Gas usage")

## ----consistent_pipe_flow-----------------------------------------------------
mtcars %>%
  rename(transmission = am, weight = wt) %>%
  mutate(lp100km = (100 * 3.785411784) / (1.609344 * mpg)) %>%
  select(weight, lp100km) %>%
  ggplot(aes(x = weight, y = lp100km)) +
  geom_point() +
  ylab("Litres per 100 kilometres") +
  ggtitle("Gas usage")  ->   # Note assignment occurs here
  p1_piped

## ----p1_piped, message = FALSE, warning = FALSE, fig.width = 4, fig.height = 4, fig.align = "center", out.width = "50%"----
p1_piped

## ----loon_p1_piped, message = FALSE, warning = FALSE, eval = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
#  library(loon.ggplot)
#  ggplot2loon(p1_piped, linkingGroup = "Motor Trend 1974")

## ----fail_pipeline, eval = FALSE----------------------------------------------
#  mtcars %>%
#    rename(transmission = am, weight = wt) %>%
#    mutate(lp100km = (100 * 3.785411784) / (1.609344 * mpg)) %>%
#    select(weight, lp100km) %>%
#    ggplot(aes(x = weight, y = lp100km)) +
#    geom_point() +
#    ylab("Litres per 100 kilometres") +
#    ggtitle("Gas usage") %>%
#    ggplot2loon()

## ----gg_pipe, message = FALSE, warning = FALSE, eval = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
#  mtcars %>%
#    rename(transmission = am, weight = wt) %>%
#    mutate(lp100km = (100 * 3.785411784) / (1.609344 * mpg)) %>%
#    select(weight, lp100km) %>%
#    # encapsulate the ggplot construction with gg_pipe()
#    gg_pipe(ggplot(aes(x = weight, y = lp100km)) +
#              geom_point() +
#              ylab("Litres per 100 kilometres") +
#              ggtitle("Gas usage")
#            )  %>%
#    # and pass the built plot on
#    ggplot2loon(linkingGroup = "Motor Trend 1974")

## ----magrittr_pipe, message = FALSE, warning = FALSE, eval = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
#  mtcars %>%
#    rename(transmission = am, weight = wt) %>%
#    mutate(lp100km = (100 * 3.785411784) / (1.609344 * mpg)) %>%
#    select(weight, lp100km) %>%
#    # encapsulate the ggplot construction with gg_pipe()
#    gg_pipe(ggplot(aes(x = weight, y = lp100km)) +
#              geom_point() +
#              ylab("Litres per 100 kilometres") +
#              ggtitle("Gas usage") )  %>%
#    # and pass the built plot on
#    ggplot2loon(linkingGroup = "Motor Trend 1974") %>%  # pipe the loon plot on
#    l_cget('color')  # Gets and returns the vector of point colours

## ----lggplot_pipe, message = FALSE, eval = FALSE------------------------------
#  obj <- mtcars %>%
#    rename(transmission = am, weight = wt) %>%
#    mutate(lp100km = (100 * 3.785411784) / (1.609344 * mpg)) %>%
#    select(weight, lp100km) %>%
#    # replace `ggplot` to `lggplot`
#    l_ggplot(aes(x = weight, y = lp100km)) +
#    geom_point() +
#    ylab("Litres per 100 kilometres") +
#    ggtitle("Gas usage")
#  obj

## ----l_getFromPath, message = FALSE, eval = FALSE-----------------------------
#  if(utils::packageVersion("loon") >= "1.2.4") {
#    # **THIS IS IMPORTANT**
#    # The path name can be obtained at the top left tk window
#    # Suppose the label is "loon.ggplot --path: .l13.ggplot"
#    # The path would be the char right after "path: " which is ".l13.ggplot"
#    loonWidget <- l_getFromPath(".l13.ggplot")
#    class(loonWidget)
#    # [1] "l_plot" "loon"
#  }

## ----loon_only_pipe, message = FALSE, warning = FALSE, eval = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
#  mtcars %>%
#    rename(transmission = am, weight = wt) %>%
#    mutate(lp100km = (100 * 3.785411784) / (1.609344 * mpg)) %>%
#    select(weight, lp100km) %>%
#    # and pass the built plot on
#    l_plot(title = "Gas Usage",
#           showGuides = TRUE, showScales = TRUE,
#           ylabel = "Litres per 100 kilometres",
#           linkingGroup = "Motor Trend 1974") %>%
#    plot()   # get a static version via grid

## ----loon_only_pipe_graph, echo = FALSE, message = FALSE,  warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
include_graphics(file.path(imageDirectory, "gas_usage.png"))

