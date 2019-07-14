## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(knitr)
set.seed(12314159)
imageDirectory <- "./img/ggplots2loon/"
dataDirectory <- "./data"
path_concat <- function(path1, path2, sep="/") {paste(path1, path2, sep = sep)}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----mpg vs wt, message = FALSE, warning = FALSE-------------------------
library(ggplot2)
p1 <- ggplot(mtcars, aes(wt, mpg)) + geom_point()

## ----mpg vs wt ggplot display, message = FALSE, warning = FALSE, fig.width = 4, fig.height = 4, fig.align = "center", out.width = "50%"----
p1 

## ----hp ggplot, message = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "50%"----
h1 <- ggplot(mtcars, aes(hp)) + geom_histogram()

## ----hp ggplot display, message = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "50%"----
h1

## ----mpg vs wt loon display, message = FALSE, warning = FALSE, fig.width = 4, fig.height = 4, fig.align = "center", out.width = "50%"----
library(loon.ggplot)
l_p1 <- loon.ggplot(p1)  # the scatterplot
l_h1 <- loon.ggplot(h1)  # the histogram

## ----grid version of l_p1, message = FALSE, warning = FALSE, fig.width = 5.5, fig.height = 4, fig.align = "center", out.width = "50%"----
grid.loon(l_p1)

## ----grid version of l_h1, message = FALSE, warning = FALSE, fig.width = 5.5, fig.height = 4, fig.align = "center", out.width = "50%"----
grid.loon(l_h1)

## ----inspector of l_p1, echo = FALSE, message = FALSE, warning = FALSE, fig.width = 5.5, fig.height = 4, fig.align = "center", fig.caption = "Loon inspector for the scatterplot l_p`", out.width = "25%"----
include_graphics(path_concat(imageDirectory, "inspector_l_p1.png"))

## ----loon data structure, message = FALSE, warning = FALSE, fig.width = 4, fig.height = 4, fig.align = "center", out.width = "50%"----
class(l_p1)
l_p1

## ----loon scale to plot, message = FALSE, warning = FALSE, fig.width = 4, fig.height = 4, fig.align = "center", out.width = "50%"----
l_scaleto_plot(l_p1)

## ----loon names, message = FALSE, warning = FALSE, fig.width = 4, fig.height = 4, fig.align = "center", out.width = "50%"----
names(l_p1)

## ----showGuides, message = FALSE, warning = FALSE, fig.width = 4, fig.height = 4, fig.align = "center", out.width = "50%"----
l_p1["showGuides"]

## ----change showGuides, message = FALSE, warning = FALSE, fig.width = 4, fig.height = 4, fig.align = "center", out.width = "50%"----
# Turn the guides off
l_p1["showGuides"] <- FALSE
# Turn the guides on again
l_p1["showGuides"] <- TRUE

## ----title, message = FALSE, warning = FALSE, fig.width = 4, fig.height = 4, fig.align = "center", out.width = "50%"----
l_p1["title"] <- "1974 Motor Trend Car Road Tests"
l_p1["xlabel"] <- "Curb weight (1000s of lbs)"
l_p1["ylabel"] <- "Gas mileage (miles per US gallon)"
newlabels <- paste0(rownames(mtcars), "\n   ",
                    c("V-", "straight-")[mtcars$vs + 1], mtcars$cyl, " \n   ", 
                    mtcars$disp, " cubic inch \n   ", 
                    mtcars$gear, " speed \n   ", 
                    c("automatic", "manual")[mtcars$am + 1]
                    )
l_p1["itemLabel"] <- newlabels
l_p1["showItemLabels"] <- TRUE

## ----grouping by colour and symbol, message = FALSE, warning = FALSE, fig.width = 4, fig.height = 4, fig.align = "center", out.width = "50%"----
l_p1["size"] <- 10
l_p1["glyph"][mtcars$am == 0] <- "ccircle"
l_p1["glyph"][mtcars$am == 1] <- "triangle"
gears <- sort(unique(mtcars$gear))
ngears <- length(gears)
cols <- c("lightblue", "steelblue", "black")
for (i in 1:ngears) {
  l_p1["color"][mtcars$gear == gears[i]] <- cols[i]
}

## ----high gear manual, message = FALSE, warning = FALSE, fig.width = 4, fig.height = 4, fig.align = "center", out.width = "50%"----
l_p1["active"] <- l_p1["color"] == l_hexcolor("black")
l_scaleto_active(l_p1)
l_p1["title"] <- "5 speed manual transmission"

## ----high gear manual grid plot, message = FALSE, warning = FALSE,  fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
grid.loon(l_p1)

## ----Back to all points, message = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
l_p1["active"] <- TRUE
l_scaleto_plot(l_p1)
l_p1["title"] <- "All points again"
grid.loon(l_p1)

## ----set initial linking Group-------------------------------------------
l_p1["linkingGroup"] <- "Motor Trend 1974"

## ----l_configure linking Group-------------------------------------------
l_configure(l_h1, linkingGroup = "Motor Trend 1974", sync = "pull")

## ----stacked colours grid.loon, message = FALSE, warning = FALSE,  fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
l_h1["showStackedColors"] <- TRUE
grid.loon(l_h1)

## ----ggplot acceleration, message = FALSE, warning = FALSE, fig.width = 6, fig.height = 5, fig.align = "center", out.width = "50%"----
# First using another ggplot
p2 <- ggplot(mtcars, aes(x = drat, y = qsec)) + geom_point()

## ----ggplot p2, message = FALSE, warning = FALSE, fig.width = 6, fig.height = 5, fig.align = "center", out.width = "50%"----
p2

## ----stacked colours linking, message = FALSE, warning = FALSE,  fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
l_p2 <- loon.ggplot(p2, 
                    linkingGroup = "Motor Trend 1974",
                    title = "Acceleration measures", 
                    xlabel = "Drive axle ratio",
                    ylabel = "Quarter mile (seconds)",
                    itemLabel = newlabels)

## ----loon plot l_p2, message = FALSE, warning = FALSE,  fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
grid.loon(l_p2)

## ----default linked states l_plot, message = FALSE, warning = FALSE, fig.width = 6, fig.height = 5, fig.align = "center", out.width = "50%"----
l_getLinkedStates(l_p1)

## ----default linked states l_hist, message = FALSE, warning = FALSE, fig.width = 6, fig.height = 5, fig.align = "center", out.width = "50%"----
l_getLinkedStates(l_h1)

## ----remove active from linked states, message = FALSE, warning = FALSE,  fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
l_setLinkedStates(l_p1,  c("color", "selected", "size"))

## ----add glyph to linked states, message = FALSE, warning = FALSE,  fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
l_setLinkedStates(l_p1, c("glyph", l_getLinkedStates(l_p1)))
l_setLinkedStates(l_p2, c("glyph", l_getLinkedStates(l_p2)))

## ----change glyphs, message = FALSE, warning = FALSE,  fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
triangles <- l_p1["glyph"] == "triangle"
l_p1["glyph"][triangles] <- "ctriangle"

## ----changed glyphs l_p1, message = FALSE, warning = FALSE,  fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
grid.loon(l_p1)

## ----changed glyphs l_p2, message = FALSE, warning = FALSE,  fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
grid.loon(l_p2)

## ----mtcars smooth, message = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
p_fit <- ggplot(mtcars, aes(drat, mpg)) + geom_smooth() + geom_point()
p_fit

## ----loon mtcars smooth, message = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
l_p_fit <- loon.ggplot(p_fit)

## ----inspector of l_p_fit, echo = FALSE, message = FALSE, warning = FALSE, fig.width = 5.5, fig.height = 4, fig.align = "center", fig.caption = "Loon inspector Layers tab for l_p_fit`", out.width = "25%"----
include_graphics(path_concat(imageDirectory, "inspector_l_p_fit.png"))

## ----lower layer, message = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
l_layer_lower(l_p_fit, "model")

## ----raise layer, message = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
l_layer_raise(l_p_fit, "model")

## ----multiple geoms, message = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
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

## ----active selected geoms, message = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
# To have the the histogram active (the first geom in p_hdp)
l_p_Hdp <- loon.ggplot(p_hdp, activeGeomLayers = 1,
                       linkingGroup = "Motor Trend 1974")

# The following creates an error because the line for the density
# is not currently a possible model layer in loon
# loon.ggplot(p_hdp, activeGeomLayers = 2,
#              linkingGroup = "Motor Trend 1974")
# 
# To have the points be active (the third geom in p_hdp)
l_p_hdP <- loon.ggplot(p_hdp, activeGeomLayers = 3,
                       linkingGroup = "Motor Trend 1974")

## ----multiple geom_points, message = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
pgps <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(data = subset(mtcars, gear == 3), col = "firebrick") +
  geom_point(data = subset(mtcars, gear == 4), col = "steelblue") +
  geom_point(data = subset(mtcars, gear == 5), col = "black")
pgps

## ----loon multiple geom_points, message = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
l_pgps.1 <- loon.ggplot(pgps, linkingGroup = "Motor Trend 1974")
l_pgps.13 <- loon.ggplot(pgps, activeGeomLayers = c(1,3),
                         linkingGroup = "Motor Trend 1974")
l_pgps.123 <- loon.ggplot(pgps, activeGeomLayers = c(1, 2, 3),
                         linkingGroup = "Motor Trend 1974")

## ----linking Key l_p1, message = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
# this group's linking keys can be found from l_p1
groupKeys <- l_p1["linkingKey"]
groupKeys

## ----linking subsets, message = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
# the subsets were defined as
dataGeom1 <- mtcars$gear == 3
dataGeom2 <- mtcars$gear == 4
dataGeom3 <- mtcars$gear == 5

## ----linking Key setting, message = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
l_configure(l_pgps.1, 
            linkingKey = groupKeys[dataGeom1], 
            sync = "pull")
l_configure(l_pgps.13, 
            linkingKey = c(groupKeys[dataGeom1],
                           groupKeys[dataGeom3]), 
            sync = "pull")
l_configure(l_pgps.123, 
            linkingKey = c(groupKeys[dataGeom1],
                           groupKeys[dataGeom2],
                           groupKeys[dataGeom3]), 
            sync = "pull")


## ----linking Key initialized, message = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
l_pgps.23 <- loon.ggplot(pgps, 
                         linkingGroup = "Motor Trend 1974",
                         activeGeomLayers = c(2,3),
                         linkingKey = c(groupKeys[dataGeom2],
                                        groupKeys[dataGeom3])
                         )

## ----fwrap, message = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
fwrap <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  facet_wrap(~ gear + am) 
fwrap

## ----l_fwrap, message = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
l_fwrap <- loon.ggplot(fwrap, linkingGroup = "Motor Trend 1974")

## ----linking Key fwrap, echo = FALSE, message = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
include_graphics(path_concat(imageDirectory, "l_fwrap.png"))

## ----class l_fwrap-------------------------------------------------------
class(l_fwrap)

## ----l_getPlots, message = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
l_getPlots(l_fwrap)

## ----l_getLocations, message = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
l_getLocations(l_fwrap)

## ----pipes, message = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
library(tidyverse)  # load this to also have dplyr functionality
p1_piped <- mtcars %>%
  rename(transmission = am, weight = wt) %>%
  mutate(lp100km = (100 * 3.785411784) / (1.609344 * mpg)) %>%
  select(weight, lp100km) %>%
  ggplot(aes(x = weight, y = lp100km)) +
  geom_point() +
  ylab("Litres per 100 kilometres") +
  ggtitle("Gas usage")

## ----consistent pipe flow------------------------------------------------
mtcars %>%
  rename(transmission = am, weight = wt) %>%
  mutate(lp100km = (100 * 3.785411784) / (1.609344 * mpg)) %>%
  select(weight, lp100km) %>%
  ggplot(aes(x = weight, y = lp100km)) +
  #geom_point() +
  ylab("Litres per 100 kilometres") +
  ggtitle("Gas usage")  ->   # Note assignment occurs here
  p1_piped

## ----p1_piped, message = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
p1_piped

## ----loon p1_piped, message = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
loon.ggplot(p1_piped, linkingGroup = "Motor Trend 1974")

## ----fail pipeline, eval = FALSE-----------------------------------------
#  mtcars %>%
#    rename(transmission = am, weight = wt) %>%
#    mutate(lp100km = (100 * 3.785411784) / (1.609344 * mpg)) %>%
#    select(weight, lp100km) %>%
#    ggplot(aes(x = weight, y = lp100km)) +
#    geom_point() +
#    ylab("Litres per 100 kilometres") +
#    ggtitle("Gas usage") %>%
#    loon.ggplot()

## ----gg_pipe, message = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
mtcars %>%
  rename(transmission = am, weight = wt) %>%
  mutate(lp100km = (100 * 3.785411784) / (1.609344 * mpg)) %>%
  select(weight, lp100km) %>%
  # encapsulate the ggplot construction with gg_pipe()
  gg_pipe(ggplot(aes(x = weight, y = lp100km)) +
            geom_point() +
            ylab("Litres per 100 kilometres") +
            ggtitle("Gas usage")
          )  %>% 
  # and pass the built plot on
  loon.ggplot(linkingGroup = "Motor Trend 1974")

## ----magrittr pipe, message = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
library(magrittr)
mtcars %>%
  rename(transmission = am, weight = wt) %>%
  mutate(lp100km = (100 * 3.785411784) / (1.609344 * mpg)) %>%
  select(weight, lp100km) %>%
  # encapsulate the ggplot construction with gg_pipe()
  gg_pipe(ggplot(aes(x = weight, y = lp100km)) +
            geom_point() +
            ylab("Litres per 100 kilometres") +
            ggtitle("Gas usage")
          )  %>% 
  # and pass the built plot on
  loon.ggplot(linkingGroup = "Motor Trend 1974") %>%  # pipe the loon plot on
  l_cget('color')  # Gets and returns the vector of point colours

## ----loon only pipe, message = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
mtcars %>%
  rename(transmission = am, weight = wt) %>%
  mutate(lp100km = (100 * 3.785411784) / (1.609344 * mpg)) %>%
  select(weight, lp100km) %>%
  # and pass the built plot on
  l_plot(title = "Gas Usage", 
         showGuides = TRUE, showScales = TRUE,
         ylabel = "Litres per 100 kilometres", 
         linkingGroup = "Motor Trend 1974") %>%
  grid.loon()   # get a static version via grid

## ----loon pairs  pipe, message = FALSE, warning = FALSE, fig.width = 5, fig.height = 4, fig.align = "center", out.width = "70%"----
mtcars %>%
  rename(transmission = am, weight = wt) %>%
  mutate(lp100km = (100 * 3.785411784) / (1.609344 * mpg)) %>%
  select(lp100km, weight, transmission) %>%
  # and pass the built plot on
  l_pairs(showHistograms = TRUE,
          linkingGroup = "Motor Trend 1974") ->  # and assign the result.
  l_pp


