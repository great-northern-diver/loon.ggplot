context("test input n dimensional states (ggplot to loon)")
library(dplyr)
library(magrittr)

pdf(NULL)

test_that("n dimensional states input with NA", {
  dat <- data.frame(
    x = c(1:9, NA),
    y = c(NA, NA, 3:10),
    selected = c(T,F,T,F,NA,T,F,T,T,F),
    facet = c(rep(1, 5), rep(2, 5))
  )

  # scatter plot
  ggplot(dat, mapping = aes(x = x, y = y)) +
    geom_point() +
    facet_wrap(~facet) -> pp
  expect_warning(
    p <- loon.ggplot(pp)
  )
  expect_equal(length(p$x1y1['x']), 3)
  expect_equal(length(p$x1y2['x']), 4)

  # serial axes plot
  pp <- ggplot(iris) +
    geom_path(alpha = 0.2) +
    coord_serialaxes(axes.sequence = colnames(iris))
  expect_warning(
    p <- loon.ggplot(pp,
                     color = c(rep("red", 100), rep(NA, 50)),
                     selected = c(rep(NA, 50), rep(TRUE, 100)))
  )
  expect_equal(length(p['color']), 50)
  expect_equal(p['selected'], rep(TRUE, 50))

  # histogram
  ggplot(dat, mapping = aes(x = x)) +
    geom_histogram(binwidth = 1) +
    facet_wrap(~facet) -> pp
  expect_warning(
    p <- loon.ggplot(pp)
  )
  expect_equal(length(p$x1y1['x']), 5)
  expect_equal(length(p$x1y2['x']), 4)

})



