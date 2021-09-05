context("test themes (ggplot to loon)")
library(dplyr)
library(magrittr)
library(png)

pdf(NULL)

test_that("themes (ggplot to loon)", {

  p <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg,
                                       colour = factor(gear))) + facet_wrap(~am)
  g <- ggplot2loon(p, ggGuides = T)
  expect_equal(class(g), c("l_facet_ggplot", "l_facet", "l_compound", "loon"))

  p1 <- p + theme_gray() # the default
  g <- ggplot2loon(p1, ggGuides = T)
  expect_equal(class(g), c("l_facet_ggplot", "l_facet", "l_compound", "loon"))

  p1 <- p + theme_bw()
  g <- ggplot2loon(p1, ggGuides = T)
  expect_equal(class(g), c("l_facet_ggplot", "l_facet", "l_compound", "loon"))

  p1 <- p + theme_linedraw()
  g <- ggplot2loon(p1, ggGuides = T)
  expect_equal(class(g), c("l_facet_ggplot", "l_facet", "l_compound", "loon"))

  p1 <- p + theme_light()
  g <- ggplot2loon(p1, ggGuides = T)
  expect_equal(class(g), c("l_facet_ggplot", "l_facet", "l_compound", "loon"))

  p1 <- p + theme_dark()
  g <- ggplot2loon(p1, ggGuides = T)
  expect_equal(class(g), c("l_facet_ggplot", "l_facet", "l_compound", "loon"))

  p1 <- p + theme_minimal()
  g <- ggplot2loon(p1, ggGuides = T)
  expect_equal(class(g), c("l_facet_ggplot", "l_facet", "l_compound", "loon"))

  p1 <- p + theme_classic()
  g <- ggplot2loon(p1, ggGuides = T)
  expect_equal(class(g), c("l_facet_ggplot", "l_facet", "l_compound", "loon"))

  p1 <- p + theme_void()
  g <- ggplot2loon(p1, ggGuides = T)
  expect_equal(class(g), c("l_facet_ggplot", "l_facet", "l_compound", "loon"))

  p <- ggplot(mtcars) +
    geom_point(aes(x = wt, y = mpg,
                   colour = factor(gear))) +
    facet_wrap(~am) +
    coord_polar()
  g <- ggplot2loon(p, ggGuides = T)
  expect_equal(class(g), c("l_facet_ggplot", "l_facet", "l_compound", "loon"))

  p1 <- p + theme_gray() # the default
  g <- ggplot2loon(p1, ggGuides = T)
  expect_equal(class(g), c("l_facet_ggplot", "l_facet", "l_compound", "loon"))

  p1 <- p + theme_bw()
  g <- ggplot2loon(p1, ggGuides = T)
  expect_equal(class(g), c("l_facet_ggplot", "l_facet", "l_compound", "loon"))

  p1 <- p + theme_linedraw()
  g <- ggplot2loon(p1, ggGuides = T)
  expect_equal(class(g), c("l_facet_ggplot", "l_facet", "l_compound", "loon"))

  p1 <- p + theme_light()
  g <- ggplot2loon(p1, ggGuides = T)
  expect_equal(class(g), c("l_facet_ggplot", "l_facet", "l_compound", "loon"))

  p1 <- p + theme_dark()
  g <- ggplot2loon(p1, ggGuides = T)
  expect_equal(class(g), c("l_facet_ggplot", "l_facet", "l_compound", "loon"))

  p1 <- p + theme_minimal()
  g <- ggplot2loon(p1, ggGuides = T)
  expect_equal(class(g), c("l_facet_ggplot", "l_facet", "l_compound", "loon"))

  p1 <- p + theme_classic()
  g <- ggplot2loon(p1, ggGuides = T)
  expect_equal(class(g), c("l_facet_ggplot", "l_facet", "l_compound", "loon"))

  p1 <- p + theme_void()
  g <- ggplot2loon(p1, ggGuides = T)
  expect_equal(class(g), c("l_facet_ggplot", "l_facet", "l_compound", "loon"))
})
