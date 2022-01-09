context("test different API")
library(dplyr)
library(ggmulti)

pdf(NULL)

test_that("set data in layer", {

  # point
  p<- ggplot() +
    geom_point(data = mtcars, mapping = aes(mpg, wt))
  g <- ggplot2loon(p)
  expect_true(length(g['x']) == 32)

  p <- p +
    geom_point(data = mtcars, mapping = aes(mpg, hp))
  expect_warning(g <- ggplot2loon(p, activeGeomLayers = c(1,2)))
  expect_true(length(g['x']) == 64)

  # histogram
  h <- ggplot() +
    geom_histogram(data = mtcars, mapping = aes(mpg))
  g <- ggplot2loon(h)
  expect_true(length(g['x']) == 32)

  # barplot
  b <- ggplot() +
    geom_bar(data = mtcars, mapping = aes(factor(cyl)))
  g <- ggplot2loon(b)
  expect_true(length(g['x']) == 32)

  # serialaxes plot: TODO
  # s <- ggplot() +
  #   geom_path(iris, mapping = aes(x1 = "Sepal.Length",
  #                                 x2 = "Sepal.Width",
  #                                 x3 = "Petal.Length",
  #                                 x4 = "Petal.Width")) +
  #   coord_serialaxes()
})
