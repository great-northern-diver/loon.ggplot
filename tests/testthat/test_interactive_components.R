context("test interactive components")
library(dplyr)
library(magrittr)
library(tidyverse)

test_that("test linking", {
  p0 <- l_plot(mtcars, linkingGroup = "mtcars")
  p1 <- l_ggplot(mtcars, aes(wt, mpg, shape = factor(am))) +
    geom_point(color = "red") +
    scale_shape_manual(values = c("0" = 19, "1" = 2)) +
    coord_flip() +
    linking(linkingGroup = "mtcars", sync = "push") +
    ggtitle("mtcars") +
    theme ( plot.title = element_text ( hjust = 0.5))
  q1 <- loon.ggplot(p1)

  # linking group
  expect_equal(q1['linkingGroup'], "mtcars")
  # sync (p0 color is red)
  expect_equal(p0['color'][1], "#FFFF00000000")

  p2 <- p1 +
    facet_wrap(~cyl)
  q2 <- loon.ggplot(p2)
  expect_equal(length(q2), 3)

  # test set linkingKey
  p3 <- l_ggplot(mtcars, aes(x = wt, fill = factor(cyl))) +
    geom_histogram() +
    linking(linkingKey = rev(seq(nrow(mtcars))), linkingGroup = "mt")
  q3 <- loon.ggplot(p3)
  expect_equal(q3['linkingKey'], as.character(rev(seq(nrow(mtcars)))))

  # multiple facets
  p4 <- p3 + facet_wrap(~cyl)
  q4 <- loon.ggplot(p4)
  expect_equal(q4$plots$x1y1['linkingKey'],
               q3['linkingKey'][mtcars$cyl == 4])

  # expect warning
  p5 <- l_ggplot() +
    geom_point(data = mtcars,
               mapping = aes(x = cyl, y = hp)) +
    linking(linkingKey = rev(seq(nrow(mtcars))),
            linkingGroup = "mt")
  expect_warning(q5 <- loon.ggplot(p5))
  expect_equal(q5['linkingKey'],
               as.character(rev(seq(nrow(mtcars)))))
})

test_that("test selecting", {

  fourGear <- rep(FALSE, nrow(mtcars))
  fourGear[mtcars$gear == 4] <- TRUE

  p1 <- l_ggplot(mtcars, aes(wt, mpg, shape = factor(am))) +
    geom_point(color = "red") +
    scale_shape_manual(values = c("0" = 19, "1" = 2)) +
    coord_polar() +
    selecting(selected = fourGear, selectionLogic = "invert") +
    ggtitle("mtcars") +
    theme ( plot.title = element_text ( hjust = 0.5)) +
    facet_wrap(~cyl)


  q1 <- loon.ggplot(p1)
  expect_equal(q1$plots[[1]]['selected'],
               fourGear[mtcars$cyl == 4])

  expect_equal(q1$plots[[1]]['selectionLogic'],
               "invert")

  expect_message(p2 <- l_ggplot() +
                   geom_point(data = mtcars,
                              mapping = aes(x = cyl, y = hp)) +
                   selecting(selected = TRUE) +
                   interactivity(selected = FALSE, selectionLogic = "deselect"))
  q2 <- loon.ggplot(p2)
  expect_equal(all(q2['selected']),
               FALSE)
  expect_equal(q2['selectionLogic'],
               'deselect')
})

test_that("test itemLabel", {

  p1 <- l_ggplot(data = data.frame(x = 1:26,
                                   y = 1:26),
                 aes(x, y)) +
    geom_point() +
    itemLabel(itemLabel = letters,
              showItemLabels = TRUE)
  expect_message(p1["itemLabel"])
  q1 <- loon.ggplot(p1)
  expect_equal(q1["itemLabel"][2], "b")
  expect_true(q1["showItemLabels"])
})

