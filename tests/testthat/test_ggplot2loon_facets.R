context("test facets (ggplot to loon)")
library(dplyr)
library(magrittr)
library(png)
pdf(NULL)

test_that("facet wrap basic (ggplot to loon)", {

  p <- ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_wrap(~class)
  g <- ggplot2loon(p)
  expect_equal(length(g), 7)

  p <- ggplot(economics_long, aes(date, value)) +
    geom_line() +
    facet_wrap(~variable, scales = "free_y", ncol = 1)
  g <- ggplot2loon(p)
  expect_equal(class(g), c("l_facet_ggplot", "l_facet", "l_compound", "loon"))

  # data(Oats, package = "MEMSS")
  # pg.oats <- ggplot(Oats, aes(nitro, yield)) +
  #   geom_line() +
  #   geom_point() +
  #   ggtitle("foo") +
  #   facet_wrap(~Block + Variety, ncol = 3)
  # g <- ggplot2loon(pg.oats, linkingGroup = "A")
  # expect_equal(class(g), c("l_facet_ggplot", "l_facet", "l_compound", "loon"))

  # SAheart %>%
  #   mutate(ltob = log(tobacco), lsbp = log(sbp)) %>%
  #   filter(age < 50) %>%
  #   ggplot(aes(x = ltob, y = lsbp)) +
  #   geom_point() +
  #   facet_wrap(~chd) -> p
  # g <- ggplot2loon(p)
  # expect_equal(class(g), c("l_facet_ggplot", "l_facet", "l_compound", "loon"))
})

test_that("facet wrap scales (ggplot to loon)", {

  df <- data.frame(
    x = rnorm(120, c(0, 2, 4)),
    y = rnorm(120, c(1, 2, 1)),
    z = letters[1:3]
  )
  df2 <- dplyr::select(df, -z)
  pp <- ggplot(df, aes(x, y)) +
    geom_point(data = df2, colour = "grey70") +
    geom_point(aes(colour = z)) +
    facet_wrap(~z, scales = "free")
  g <- ggplot2loon(pp, activeGeomLayers = 2)
  expect_equal(class(g), c("l_facet_ggplot", "l_facet", "l_compound", "loon"))

  # example 68
  pp <- ggplot(df, aes(x, y)) +
    geom_point(data = df2, colour = "grey70") +
    geom_point(aes(colour = z)) +
    facet_wrap(~z, scales = "free_x")
  g <- ggplot2loon(pp, activeGeomLayers = 1)
  expect_equal(class(g), c("l_facet_ggplot", "l_facet", "l_compound", "loon"))

  # example 69
  expect_warning(ggplot2loon(ggObj = ggplot(df, aes(x, y)) +
                               geom_point(data = df2, colour = "grey70") +
                               geom_point(aes(colour = z)) +
                               facet_wrap(~z, scales = "free_y"),
                             activeGeomLayers = c(1,2)))
})

test_that("facet grid basic (ggplot to loon)", {

  p <- ggplot(mpg, aes(displ, cty)) + geom_point() + ggtitle("foo")
  # Use vars() to supply variables from the dataset:
  p1 <- p + facet_grid(rows = vars(drv))
  g <- ggplot2loon(p1)
  expect_equal(class(g), c("l_facet_ggplot", "l_facet", "l_compound", "loon"))
  # loonGrob
  lg <- loonGrob(g)
  expect_is(lg, c("gTree", "grob", "gDesc"))
  l <- layout_coords(g)
  expect_equal(l, data.frame(row = 1:3, col = rep(1,3)))

  # example 71
  p2 <- p + facet_grid(cols = vars(cyl))
  g <- ggplot2loon(p2)
  expect_equal(class(g), c("l_facet_ggplot", "l_facet", "l_compound", "loon"))

  # example 72
  p3 <- p + facet_grid(vars(drv), vars(cyl))
  g <- ggplot2loon(p3)
  expect_equal(class(g), c("l_facet_ggplot", "l_facet", "l_compound", "loon"))
})

