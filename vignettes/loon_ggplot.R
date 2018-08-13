## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----basic uasge, message=FALSE------------------------------------------
library(loon.ggplot)
g1 <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
lp1 <- loon.ggplot(g1)

## ----basic show ggplot, fig.width = 4, fig.height = 4--------------------
g1

## ----basic show loonplot, fig.width = 5, fig.height = 4------------------
library(grid)
grid.loon(lp1)

## ----selected loonplot, fig.width = 5, fig.height = 4--------------------
cyl4 <- which((mtcars$cyl == 4) == TRUE)
lp1$x1y1['selected'][cyl4] <- TRUE
grid.loon(lp1)

## ----ggplot with polar coordinates, fig.width = 4, fig.height = 4, message=FALSE----
library(ElemStatLearn)
g1.1 <- ggplot(SAheart, aes(obesity, adiposity)) + geom_point() + geom_smooth() +
  coord_polar(theta = "y")
g1.1

## ----loonplot with polar coordinates, fig.width = 3.8, fig.height = 4----
lp1.1 <- loon.ggplot(g1.1)
grid.loon(lp1.1)

## ----loonplot with ggGuides, fig.width = 4.5, fig.height = 4.5-----------
lp1.1_ggGuides <- loon.ggplot(g1.1, ggGuides = TRUE)
grid.loon(lp1.1_ggGuides)

## ----ggplot histogram and density line, fig.width = 3.5, fig.height = 3, message=FALSE----
g1.2.1 <- ggplot(mtcars, aes(x = mpg, y = ..density..)) +
  geom_histogram() +
  geom_point(data = data.frame(x = mtcars$mpg, y = 0), 
             mapping = aes(x, y), 
            color = "steelblue", size = 3) +
  geom_density()
g1.2.1

## ----loonplot but with different active layers, fig.width = 7.5, fig.height = 3.5----
library(gridExtra)
## check difference
lp1.2.1.1 <- loon.ggplot(g1.2.1, active_geomLayers = 1)

suppressWarnings(
  lp1.2.1.2 <- loon.ggplot(g1.2.1, active_geomLayers = 2)
)

grid.arrange(loonGrob(lp1.2.1.1), loonGrob(lp1.2.1.2), ncol = 2, top = "lp1.2.1.1 and lp1.2.1.2")

## ----selected loonplot with different active layers, fig.width = 7.5, fig.height = 3.5----
lp1.2.1.1$x1y1['selected'][mtcars$mpg > 20] <- TRUE
lp1.2.1.2$x1y1['selected'][mtcars$mpg > 20] <- TRUE
grid.arrange(loonGrob(lp1.2.1.1), loonGrob(lp1.2.1.2), ncol = 2, top = "lp1.2.1.1 and lp1.2.1.2")

## ----show errors, eval=FALSE---------------------------------------------
#  # Error
#  lp1.2.1.3 <- loon.ggplot(g1.2.1, active_geomLayers = 3)
#  # Error in activeInfo(importantLayers, active_geomLayers, len_layers) : This layer cannot be active
#  lp1.2.1.4 <- loon.ggplot(g1.2.1, active_geomLayers = c(1,2))
#  # Error in activeInfo(importantLayers, active_geomLayers, len_layers) : histogram layer and point layer cannot be active at the same time

## ----ggplot with several point layers, fig.width = 5, fig.height = 3.5----
df <- data.frame(
  x = rnorm(120, c(0, 2, 4)),
  y = rnorm(120, c(1, 2, 1)),
  z = letters[1:3]
)
df2 <- dplyr::select(df, -z)
g1.2.2 <- ggplot(df, aes(x, y)) +
  geom_point(data = df2, colour = "grey70") +
  geom_point(aes(colour = z)) +
  facet_wrap(~z)
g1.2.2

## ----selected loonplot with several point layers, warning=FALSE----------
suppressWarnings(
  lp1.2.2.1 <- loon.ggplot(g1.2.2, active_geomLayers = 1)
)
lp1.2.2.2 <- loon.ggplot(g1.2.2, active_geomLayers = 2)

suppressWarnings(
  lp1.2.2.3 <- loon.ggplot(g1.2.2, active_geomLayers = c(1,2))
)

## ----selected loonplot with several point layers layout, fig.width=7.5, fig.height=7, fig.show='hold'----
# lp1.2.2.1
sel_lp1.2.2.1 <- lapply(lp1.2.2.1, 
                      function(loon_plot){
                        loon_plot['selected'] <- TRUE
                      })
# lp1.2.2.2
sel_lp1.2.2.2 <- lapply(lp1.2.2.2, 
                      function(loon_plot){
                        loon_plot['selected'] <- TRUE
                      })
# lp1.2.2.3
sel_lp1.2.2.3 <- lapply(lp1.2.2.3, 
                      function(loon_plot){
                        loon_plot['selected'] <- TRUE
                      })
grid.arrange(
  textGrob(
    c("What","is", "your" ,"conclusion?"), 
    x = c(0.2, 0.4, 0.6, 0.8),
    y = c(0.8, 0.6, 0.4, 0.2),
    gp = gpar(col = "grey80", fontsize = 18),
    rot = c(30, 60, 30, 60)
  ),
  loonGrob(lp1.2.2.1), 
  loonGrob(lp1.2.2.2),
  loonGrob(lp1.2.2.3), 
  ncol = 2, 
  nrow = 2,
  top = "lp1.2.2.1 and lp1.2.2.2 and lp1.2.2.3")

## ----reset ggplot, eval = F----------------------------------------------
#  g1.2.2 <- ggplot(df, aes(x, y)) +
#    geom_point(aes(colour = z)) +
#    geom_point(data = df2, colour = "grey70") +
#    facet_wrap(~z)

## ----ggplot arranged plots, fig.width = 7, fig.height = 3.5, fig.show='hold', message=FALSE----
g1.3.1 <- ggplot(mtcars, aes(mpg, wt, colour = as.factor(cyl))) + geom_point()
g1.3.2 <- ggplot(mtcars, aes(x = mpg, fill = as.factor(cyl))) + geom_histogram()
grid.arrange(g1.3.1, g1.3.2, ncol = 2)

## ----selected loonplot with linkingGroup and linkingKey, fig.width=7, fig.height=3.5, fig.show='hold'----
lp1.3.1 <- loon.ggplot(g1.3.1, linkingGroup = "mtcars")
lp1.3.2 <- loon.ggplot(g1.3.2, linkingGroup = "mtcars")
lp1.3.1$x1y1['selected'][which((mtcars$cyl == 4) == TRUE)] <- TRUE
# or
# lp1.3.2$x1y1['selected'][which((mtcars$cyl == 4) == TRUE)] <- TRUE
grid.arrange(loonGrob(lp1.3.1), loonGrob(lp1.3.2), ncol = 2)

## ----geom_curve in loonplot, eval=FALSE----------------------------------
#  df <- data.frame(x = 1:3, y = 1:3)
#  df2 <- data.frame(x = c(1,3), y = c(1,3),
#                    xend = c(2,2), yend = c(2,2))
#  g1.4 <- ggplot(df, aes(x, y)) +
#    geom_point() +
#    geom_curve(aes(x = x ,y = y,
#                   xend = xend, yend = yend),
#               data = df2,
#               color = c("red", "blue"))
#  loon.ggplot(g1.4)

## ----tidyverse, message=FALSE--------------------------------------------
library(magrittr)
library(tidyverse)
SAheart %>%
  mutate(ltob = log(tobacco), lsbp = log(sbp)) %>%
  filter(age < 50) %>%
  ggplot(aes(x = ltob, y = lsbp)) +
  geom_point() +
  facet_wrap(~chd) -> gg
# the plot will be drawn at the end
lp1.2 <- loon.ggplot(gg)

## ----failed tidyverse when piping ggplot, eval=FALSE---------------------
#  ## Error occures
#  SAheart %>%
#    mutate(ltob = log(tobacco), lsbp = log(sbp)) %>%
#    filter(age < 50) %>%
#    ggplot(aes(x = ltob, y = lsbp)) +
#    geom_point() +
#    facet_wrap(~chd) %>%
#    loon.ggplot()

## ----ggpipe, fig.width=7, fig.height=3.5---------------------------------
## Works
SAheart %>%
  mutate(ltob = log(tobacco), lsbp = log(sbp)) %>%
  filter(age < 50) %>%
  gg_pipe( 
    ggplot(aes(x = ltob, y = lsbp)) +
      geom_point() +
      facet_wrap(~chd) 
  ) %>% 
  loon.ggplot() %>%
  grid.loon()

