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

library(grid, quietly = TRUE)
library(gridExtra, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(loon, quietly = TRUE)
library(loon.ggplot, quietly = TRUE)

## ----fake data----------------------------------------------------------------
data <- data.frame(A = c(19, 19, 25, 62, 34, 
                         98, 62, 40, 24, 60, 
                         70, 40, 40, 34, 26),
                   B = c(68, 63, 63,  4, 95,
                         78, 14, 14, NA, 28,
                         NA, 95, 74, 40, 78),
                   C = c(48, 56, 48, 39, 64,
                         52, 48, 24, 41, 52,
                         35, 35, 41, NA, 39)
                   )
summary(data)

## ----fake ggplots-------------------------------------------------------------
# prelims
rowNums <- 1:nrow(data)
AorB <- is.na(data$A)|is.na(data$B)
AorC <- is.na(data$A)|is.na(data$C)
BorC <- is.na(data$B)|is.na(data$C)
size <- 5

# B vs A scatterplot
titleStringBvsA <- paste0("plot 1, missing: ", 
                          paste0(rowNums[AorB], collapse = ", "))
ggp1 <- ggplot(data, 
               mapping = aes(x = A, y = B)) +
  ggtitle(titleStringBvsA) +
  geom_point(color = "grey", size = size) +
  linking(linkingGroup = "NA example")

# C vs A scatterplot
titleStringCvsA <- paste0("plot 2, missing: ", 
                          paste0(rowNums[AorC], collapse = ", "))
ggp2 <- ggplot(data, 
               mapping = aes(x = A, y = C)) +
  ggtitle(titleStringCvsA) +
  geom_point(color = "grey", size = size) +
  linking(linkingGroup = "NA example")

# C vs B scatterplot
titleStringCvsB <- paste0("plot 3, missing: ", 
                          paste0(rowNums[BorC], collapse = ", "))
ggp3 <- ggplot(data, 
               mapping = aes(x = B, y = C)) +
  ggtitle(titleStringCvsB) +
  geom_point(color = "grey", size = size) +
  linking(linkingGroup = "NA example")

## ----interactive plots, fig.width = 12, fig.height = 4, out.width = "100%", warning = FALSE----
lp1 <- loon.ggplot(ggp1)
#> Warning: Removed {9, 11} as the 2 observations which contain missing values.
lp2 <- loon.ggplot(ggp2)
#> Warning: Removed {14} as the 1 observation which contains missing values.
lp3 <- loon.ggplot(ggp3)
#> Warning: Removed {9, 11, 14} as the 3 observations which contain missing values.
#
# and (using gridExtra's grid.arrange() function)
# appear as 
library(gridExtra)
grid.arrange(plot(lp1, draw = FALSE), 
             plot(lp2, draw = FALSE), 
             plot(lp3, draw = FALSE), 
             nrow = 1)

## ----changes to fake data plot, fig.width = 12, fig.height = 4, out.width = "100%"----
# First choose some points in the first interactive plot
selection <- lp1["x"] > 50 & lp1["x"] < 80
lp1["selected"] <- selection
colorMeRed <- lp1["x"] == 34
lp1["color"][colorMeRed] <- "red"

# And the plots now look like
grid.arrange(plot(lp1, draw = FALSE), 
             plot(lp2, draw = FALSE), 
             plot(lp3, draw = FALSE), 
             nrow = 1)

## ----linking keys-------------------------------------------------------------
lp1["linkingKey"]
lp2["linkingKey"]
lp3["linkingKey"]

## -----------------------------------------------------------------------------
# The row numbers of `data` in each plot
# lp1
1 + as.numeric(lp1["linkingKey"])
# lp2
1 + as.numeric(lp2["linkingKey"])
# lp3
1 + as.numeric(lp3["linkingKey"])

## -----------------------------------------------------------------------------
newUniqueKeys <- paste("linking key number", 1:nrow(data))

# Using these keys, the calls would now appear as
ggp1_stringKeys <- ggplot(data, 
                          mapping = aes(x = A, y = B)) +
  ggtitle("plot 1: string keys") +
  geom_point(color = "grey", size = size) +
  linking(linkingGroup = "NA example", linkingKey = newUniqueKeys)

# C vs A scatterplot
ggp2_stringKeys <- ggplot(data, 
                          mapping = aes(x = A, y = C)) +
  ggtitle("plot 2: string keys") +
  geom_point(color = "grey", size = size) +
  linking(linkingGroup = "NA example", linkingKey = newUniqueKeys)

# C vs B scatterplot
ggp3_stringKeys <- ggplot(data, 
                          mapping = aes(x = B, y = C)) +
  ggtitle("plot 3: string keys") +
  geom_point(color = "grey", size = size) +
  linking(linkingGroup = "NA example", linkingKey = newUniqueKeys)

## -----------------------------------------------------------------------------
lp1_stringKeys <- loon.ggplot(ggp1_stringKeys)
#> Warning: Removed {9, 11} as the 2 observations which contain missing values.
lp2_stringKeys <- loon.ggplot(ggp2_stringKeys)
#> Warning: Removed {14} as the 1 observation which contains missing values.
lp3_stringKeys <- loon.ggplot(ggp3_stringKeys)
#> Warning: Removed {9, 11, 14} as the 3 observations which contain missing values.

## -----------------------------------------------------------------------------
colorMeBlue <- lp1_stringKeys["x"] < 50 
lp1_stringKeys["color"][colorMeBlue] <- "blue"

## ----fig.width = 12, fig.height = 8, out.width = "100%"-----------------------
grid.arrange(plot(lp1, draw = FALSE), 
             plot(lp2, draw = FALSE), 
             plot(lp3, draw = FALSE), 
             plot(lp1_stringKeys, draw = FALSE), 
             plot(lp2_stringKeys, draw = FALSE), 
             plot(lp3_stringKeys, draw = FALSE), 
             nrow = 2)

## -----------------------------------------------------------------------------
lp1["linkingGroup"]
lp1_stringKeys["linkingGroup"]

## -----------------------------------------------------------------------------
length(intersect(lp1["linkingKey"], lp2["linkingKey"]))
length(intersect(lp1_stringKeys["linkingKey"], lp2_stringKeys["linkingKey"]))
length(intersect(lp1["linkingKey"], lp1_stringKeys["linkingKey"]))

## ----eval= FALSE--------------------------------------------------------------
#  # Get a ggplot from the loon plot, make sure the selected points
#  # do not change the order of the
#  ggp_lp1 <- loon.ggplot(lp1, selectedOnTop = FALSE)

## ----eval= FALSE--------------------------------------------------------------
#  lp_ggp_l1_lk <- loon.ggplot(ggp_lp1,
#                              linkingKey = lp1["linkingKey"],
#                              linkingGroup = "NA example")

