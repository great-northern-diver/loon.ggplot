context("test deprecated functions")
library(ggmulti)
library(testthat)
pdf(NULL)

test_that("deprecated functions", {
  expect_warning(
    p <- ggplot(data = data.frame(x = 1:4, y = 1:4),
                mapping = aes(x = x, y = y)) +
      geom_polygonGlyph(polygon_x = list(x_star, x_cross, x_hexagon, x_airplane),
                        polygon_y = list(y_star, y_cross, y_hexagon, y_airplane),
                        colour = 'black', fill = 'red')
  )
  b <- ggplot_build(p)
  expect_equal(length(b$data[[1]]$polygon_x), 4)

  expect_warning(
    p <- ggplot(data = iris,
                mapping = aes(x = Sepal.Length, y = Sepal.Width)) +
      geom_serialAxesGlyph(serialAxesData = iris, axesLayout = "radial")
  )
  b <- ggplot_build(p)
  expect_equal(length(b$data[[1]]$serialaxes.data.Sepal.Length), 150)
})
