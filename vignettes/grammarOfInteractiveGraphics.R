## ----setup, include=FALSE, warning=FALSE, message=FALSE-----------------------
knitr::opts_chunk$set(echo = TRUE, 
                      warning = FALSE,
                      message = FALSE,
                      fig.align = "center", 
                      fig.width = 6, 
                      fig.height = 5,
                      out.width = "40%", 
                      collapse = TRUE,
                      comment = "#>",
                      tidy.opts = list(width.cutoff = 65),
                      tidy = FALSE)
library(knitr)
set.seed(12314159)
imageDirectory <- "./l_ggplot"
dataDirectory <- "./l_ggplot"

library(gridExtra, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(loon, quietly = TRUE)
library(loon.ggplot, quietly = TRUE)

## -----------------------------------------------------------------------------
data("airquality")
summary(airquality)

## -----------------------------------------------------------------------------
airquality$Date <- with(airquality,
                        as.Date(paste("1973", Month, Day, sep = "-")))
# Could also look up the day of the week for each date and add it
airquality$Weekday <- factor(weekdays(airquality$Date),
                             levels = c("Monday", "Tuesday", "Wednesday",
                                        "Thursday", "Friday",
                                        "Saturday", "Sunday"))

## -----------------------------------------------------------------------------
airquality$Month <- factor(month.abb[airquality$Month], 
                           levels = month.abb[unique(airquality$Month)])

## -----------------------------------------------------------------------------
head(airquality, n = 3)

## -----------------------------------------------------------------------------
lgp <- l_ggplot(airquality, 
                mapping = aes(x = Wind, y = Ozone)) +
  ggtitle("Air quality in New York (1973)") +
  geom_point(size = 3) 

## -----------------------------------------------------------------------------
class(lgp)

## -----------------------------------------------------------------------------
plot(lgp)

## ----echo = FALSE-------------------------------------------------------------
lp <- loon.ggplot(lgp)

## ----eval = FALSE-------------------------------------------------------------
#  lgp                   # or print(lgp)

## ----echo = FALSE-------------------------------------------------------------
plot(lp)

## ----echo = FALSE-------------------------------------------------------------
as.character(lp)

## ----eval = FALSE-------------------------------------------------------------
#  lp <- l_getFromPath(".lXX.ggplot.plot")  # replace XX by whatever number appeared

## ----eval = FALSE-------------------------------------------------------------
#  lp <- l_getFromPath(".lXX.ggplot")  # replace XX by whatever number appeared

## ----eval = FALSE-------------------------------------------------------------
#  lp <- print(lgp)

## ----eval = FALSE-------------------------------------------------------------
#  lp <- loon.ggplot(lgp)

## -----------------------------------------------------------------------------
plot(lgp + geom_smooth())

## -----------------------------------------------------------------------------
# Change glyph aesthetics of ALL points
lp["color"] <- "lightgrey"
lp["glyph"] <- "ctriangle"  # closed triangle
lp["size"]  <- 10           # proportional to area in loon

# Dynamically change the scaling (magnify or zoom in and out)
for (mag in rep(c(0.8, 1, 1.2), times = 5)){
   lp["zoomX"] <- mag
   lp["zoomY"] <- mag
   Sys.sleep(0.1) # slow down to see effect
 }
# Settle on
lp["zoomX"] <- 1.2
lp["zoomY"] <- 1.2
#
# Or, similarly, change the location/origin of the plot
xlocs <- seq(min(lp["x"]),
             median(lp["x"]),
             length.out = 10)

ylocs <- seq(min(lp["y"]),
             median(lp["y"]),
             length.out = 10)
# Dynamically change the origin
for (i in 1:length(xlocs)){
  lp["panX"] <- xlocs[i]
  lp["panY"] <- ylocs[i]
  Sys.sleep(0.1) # slow down to see effect
}
# and back
xlocs <- rev(xlocs)
ylocs <- rev(ylocs)
# dynamically
for (i in 1:length(xlocs)){
  lp["panX"] <- xlocs[i]
  lp["panY"] <- ylocs[i]
  Sys.sleep(0.1) # slow down to see effect
}
# Perhaps settle on
lp["panX"] <- 7
lp["panY"] <- 0

## -----------------------------------------------------------------------------
# First the l_ggplot 
plot(lgp)  

## -----------------------------------------------------------------------------
# Now the l_plot 
plot(lp)   

## ----eval = FALSE-------------------------------------------------------------
#  lp <- loon.ggplot(lgp)

## -----------------------------------------------------------------------------
loon.ggplot(lp)

## -----------------------------------------------------------------------------
gp <- loon.ggplot(lp)  # loon to ggplot
class(gp)
gp + geom_smooth()

## -----------------------------------------------------------------------------
new_lp <- loon.ggplot(gp + geom_smooth())

## -----------------------------------------------------------------------------
loon.ggplot(new_lp)

## ----eval = FALSE-------------------------------------------------------------
#  + interactivity(linkingGroup, linkingKey, linkedStates, sync,  # linking
#                  active, activeGeomLayers,                      # active
#                  selected, selectBy, selectionLogic,            # selection
#                  layerId, scaleToFun,                           # zoom
#                  itemLabel,  showItemLabels,                    # hover
#                  ... )

## ----ordinary ggplot----------------------------------------------------------
ggp <- ggplot(airquality, mapping = aes(Solar.R, Temp)) +
  geom_point(size = 3) +
  ggtitle("Air quality in New York (1973)")
# which is an ordinary ggplot and prints as one
ggp

## ----interactive ggplot-------------------------------------------------------
lggp <- ggp + 
  linking(linkingGroup = "airquwqality") +
  selection(selected = airquality$Solar.R < 100) +
  zoom(layerId = 1, scaleToFun = l_scaleto_selected) +
  geom_smooth()
# which is an interactive loon and prints as one
lggp
# but plots as a ggplot
plot(lggp)

## ----print the interactive plot, eval = FALSE---------------------------------
#  l_ggp <- l_getFromPath(".l3.ggplot")
#  # Alternatively, the loon plot could have been captured when first
#  # created by using loon.ggplot(lggp) in stead of print(lggp) as follows
#  #
#  # l_ggp <- loon.ggplot(lggp)
#  #
#  # Either way, it will look like the following as a grid graphics plot
#  plot(l_ggp)
#  # and as below when presented as a ggplot
#  loon.ggplot(l_ggp)

