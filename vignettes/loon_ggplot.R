## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
setwd("C:/Users/Zehao/Documents/GitHub/loon.ggplot/vignettes")

## ----message=FALSE-------------------------------------------------------
library(loon.ggplot)
g1 <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
lp1 <- loon.ggplot(g1)

## ----fig.width=7, fig.show='hold'----------------------------------------
library(grid)
library(gridExtra)
grid.arrange(loonGrob(lp1), g1, ncol = 2, top = "loon plot vs ggplot")

## ------------------------------------------------------------------------
cyl4 <- which((mtcars$cyl == 4) == TRUE)
lp1$x1y1['selected'][cyl4] <- TRUE

## ----echo = F------------------------------------------------------------
grid.loon(lp1)

## ------------------------------------------------------------------------
library(ElemStatLearn)
g1.1 <- ggplot(SAheart, aes(obesity, adiposity)) + geom_point() + geom_smooth() +
  coord_polar(theta = "y")
g1.1

## ----fig.width=3---------------------------------------------------------
lp1.1 <- loon.ggplot(g1.1)
grid.loon(lp1.1)

## ----fig.width=3.5, fig.height= 3.5--------------------------------------
lp1.1_ggGuides <- loon.ggplot(g1.1, ggGuides = TRUE)
grid.loon(lp1.1_ggGuides)

## ----fig.width=3---------------------------------------------------------
den <- density(mtcars$mpg)
g1.2.1 <- ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(mapping = aes(y = ..density..)) +
  geom_point(data = data.frame(x = den$x, y = den$y), mapping = aes(x, y))
g1.2.1

## ----fig.width=7, fig.show='hold'----------------------------------------
## check difference
suppressWarnings(
  # warnings occure, why?
  lp1.2.1 <- loon.ggplot(g1.2.1, 
                        activeLayer = list(
                          l_plot = 1,
                          l_hist = 1))
)
lp1.2.2 <- loon.ggplot(g1.2.1,
                      activeLayer = list(
                        l_hist = 1,
                        l_plot = 1)
)
grid.arrange(loonGrob(lp1.2.1), loonGrob(lp1.2.2), ncol = 2, top = "lp1.2.1 vs lp1.2.2")

## ------------------------------------------------------------------------
lp1.2.1$x1y1['selected'][den$x > 20] <- TRUE
lp1.2.2$x1y1['selected'][mtcars$mpg > 20] <- TRUE

## ----fig.width=7, fig.show='hold'----------------------------------------
grid.arrange(loonGrob(lp1.2.1), loonGrob(lp1.2.2), ncol = 2, top = "lp1.2.1 vs lp1.2.2")

## ----fig.width=4---------------------------------------------------------
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

## ----warning=FALSE-------------------------------------------------------
suppressWarnings(
  # warnings occure, why?
  lp1.2.3 <- loon.ggplot(g1.2.2, 
                         activeLayer = list(
                           l_plot = 1
                         ))
)
lp1.2.4 <- loon.ggplot(g1.2.2,
                       activeLayer = list(
                         l_plot = 2)
)
suppressWarnings(
  # warnings occure, why?
  lp1.2.5 <- loon.ggplot(g1.2.2,
                         activeLayer = list(
                           l_plot = c(1,2))
  )
)

## ----fig.width=8, fig.height=8, fig.show='hold'--------------------------
# lp1.2.3
sel_lp1.2.3 <- lapply(lp1.2.3, 
                      function(loon_plot){
                        loon_plot['selected'] <- TRUE
                      })
# lp1.2.4
sel_lp1.2.4 <- lapply(lp1.2.4, 
                      function(loon_plot){
                        loon_plot['selected'] <- TRUE
                      })
# lp1.2.5
sel_lp1.2.5 <- lapply(lp1.2.5, 
                      function(loon_plot){
                        loon_plot['selected'] <- TRUE
                      })
grid.arrange(
  textGrob(
    c("Check","the", "big" ,"difference!"), 
    x = c(0.2, 0.4, 0.6, 0.8),
    y = c(0.8, 0.6, 0.4, 0.2),
    gp = gpar(col = "grey80", fontsize = 18),
    rot = c(30, 60, 30, 60)
  ),
  loonGrob(lp1.2.3), 
  loonGrob(lp1.2.4),
  loonGrob(lp1.2.5), 
  ncol = 2, 
  nrow = 2,
  top = "lp1.2.3 vs lp1.2.4 vs lp1.2.5")

## ---- eval = F-----------------------------------------------------------
#  g1.2.2 <- ggplot(df, aes(x, y)) +
#    geom_point(aes(colour = z)) +
#    geom_point(data = df2, colour = "grey70") +
#    facet_wrap(~z)

## ---- fig.width=7, fig.show='hold'---------------------------------------
g1.3.1 <- ggplot(mtcars, aes(mpg, wt, colour = as.factor(cyl))) + geom_point()
g1.3.2 <- ggplot(mtcars, aes(x = mpg, fill = as.factor(cyl))) + geom_histogram()
grid.arrange(g1.3.1, g1.3.2, ncol = 2)

## ----fig.width=7, fig.show='hold'----------------------------------------
lp1.3.1 <- loon.ggplot(g1.3.1, linkingGroup = "mtcars")
lp1.3.2 <- loon.ggplot(g1.3.2, linkingGroup = "mtcars")
lp1.3.1$x1y1['selected'][which((mtcars$cyl == 4) == TRUE)] <- TRUE
# or
# lp1.3.2$x1y1['selected'][which((mtcars$cyl == 4) == TRUE)] <- TRUE
grid.arrange(loonGrob(lp1.3.1), loonGrob(lp1.3.2), ncol = 2)

## ---- eval=FALSE---------------------------------------------------------
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

## ----message=FALSE-------------------------------------------------------
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

## ---- eval=FALSE---------------------------------------------------------
#  ## Error occures
#  SAheart %>%
#    mutate(ltob = log(tobacco), lsbp = log(sbp)) %>%
#    filter(age < 50) %>%
#    ggplot(aes(x = ltob, y = lsbp)) +
#    geom_point() +
#    facet_wrap(~chd) %>%
#    loon.ggplot()

## ----fig.width=6---------------------------------------------------------
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

