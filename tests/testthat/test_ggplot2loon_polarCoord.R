context("test polar coordinate system (ggplot to loon)")
library(dplyr)
library(magrittr)
library(png)

pdf(NULL)

test_that("Polar coordinate system (ggplot to loon)", {
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
  pp <- p + geom_abline(intercept = 37, slope = -5) + coord_polar(theta = "x")
  g <- ggplot2loon(pp)
  expect_equal(class(g), c("l_plot", "loon"))

  pie <- ggplot(mtcars, aes(x = factor(1), fill = factor(cyl))) +
    geom_bar(width = 1)
  pp <- pie + coord_polar(theta = "x")
  expect_warning(ggplot2loon(pp))

  pp <- pie + coord_polar(theta = "y")
  expect_warning(ggplot2loon(pp))

  rect <- data.frame(x = 50, y = 50)
  line <- data.frame(x = c(1, 200), y = c(100, 1))
  base <- ggplot(mapping = aes(x, y)) +
    geom_tile(data = rect, aes(width = 50, height = 50)) +
    geom_line(data = line) +
    xlab(NULL) + ylab(NULL)
  pp <- base + coord_polar("x")
  g <- ggplot2loon(pp)
  expect_equal(class(g), c("l_plot", "loon"))

  pp <- ggplot(data = mtcars, mapping = aes(x = mpg, y = hp)) + geom_point() + geom_smooth() +
    coord_polar(theta = "y")
  g <- ggplot2loon(pp)
  expect_equal(class(g), c("l_plot", "loon"))

  pp <- p + geom_abline(intercept = 10, slope = 2.5) + coord_polar(theta = "y")
  g <- ggplot2loon(pp)
  expect_equal(class(g), c("l_plot", "loon"))
})
