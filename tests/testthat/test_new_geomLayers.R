context("test new geom layers (glyphs)")
library(dplyr)
library(magrittr)
library(tidyverse)
library(GGally)

test_that("test geom glyphs in ggplot", {

  p <- ggplot(data = data.frame(x = 1:3, y = 1:3),
              mapping = aes(x = x, y = y)) +
    geom_pointrangeGlyph(ymin=(1:3)-(1:3)/5, ymax=(1:3)+(1:3)/5)
  p
  expect_equal(class(p), c("gg", "ggplot"))

  p <- ggplot(data = data.frame(x = 1:26, y = 1:26),
              mapping = aes(x = x, y = y)) +
    geom_textGlyph(text = LETTERS, size = (1:26)/5)
  p
  expect_equal(class(p), c("gg", "ggplot"))

  x_star <-
    c(-0.000864304235090734, 0.292999135695765, 0.949870354364736,
      0.474503025064823, 0.586862575626621, -0.000864304235090734,
      -0.586430423509075, -0.474070872947277, -0.949438202247191,
      -0.29256698357822)
  y_star <-
    c(-1, -0.403630077787381, -0.308556611927398, 0.153846153846154,
      0.808556611927398, 0.499567847882455, 0.808556611927398,
      0.153846153846154, -0.308556611927398, -0.403630077787381)
  x_cross <-
    c(-0.258931143762604, -0.258931143762604, -0.950374531835206,
      -0.950374531835206, -0.258931143762604, -0.258931143762604,
      0.259651397291847, 0.259651397291847, 0.948934024776722,
      0.948934024776722, 0.259651397291847, 0.259651397291847)
  y_cross <-
    c(-0.950374531835206, -0.258931143762604, -0.258931143762604,
      0.259651397291847, 0.259651397291847, 0.948934024776722,
      0.948934024776722, 0.259651397291847, 0.259651397291847,
      -0.258931143762604, -0.258931143762604, -0.950374531835206)
  x_hexagon <-
    c(0.773552290406223, 0, -0.773552290406223, -0.773552290406223,
      0, 0.773552290406223)
  y_hexagon <-
    c(0.446917314894843, 0.894194756554307, 0.446917314894843,
      -0.447637568424085, -0.892754249495822, -0.447637568424085)
  p <- ggplot(data = data.frame(x = 1:3, y = 1:3),
              mapping = aes(x = x, y = y)) +
    geom_polygonGlyph(polygon_x = list(x_star, x_cross, x_hexagon),
                      polygon_y = list(y_star, y_cross, y_hexagon),
                      colour = 'black', fill = 'red')
  p
  expect_equal(class(p), c("gg", "ggplot"))

  # img_paths <- list.files(file.path(find.package(package = 'loon'), "images"), full.names = TRUE)
  # images <- lapply(img_paths, function(path) png::readPNG(path))
  # p <- ggplot(data = data.frame(x = 1:6, y = 1:6),
  #             mapping = aes(x = x, y = y)) +
  #   geom_imageGlyph(images = images, alpha = 0.4)
  # p
  # expect_equal(class(p), c("gg", "ggplot"))

  p <- ggplot(data = iris, mapping = aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
    geom_serialAxesGlyph(serialAxesData = iris[, -5], axesLayout = "radial")
  p
  expect_equal(class(p), c("gg", "ggplot"))

  p <- ggplot(data = iris, mapping = aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
    geom_serialAxesGlyph(serialAxesData = iris[, -5], axesLayout = 'parallel')
  p
  expect_equal(class(p), c("gg", "ggplot"))
})

test_that("test geom glyphs in ggplot", {

  ###### parallel
  ordSeq <- c(1, 2, 3, 1, 4, 2, 3, 4)
  g <- ggSerialAxes(
    ggObj = ggplot(data = iris, mapping = aes(colour = Species, size = Sepal.Length)),
    axesLabels = colnames(iris)[ordSeq]
  )
  g
  expect_equal(class(g), c("gg", "ggplot"))
  g <- ggSerialAxes(
    ggObj = ggplot(data = iris, mapping = aes(colour = Species)),
    size = 0.1
  )
  g
  expect_equal(class(g), c("gg", "ggplot"))
  g <- ggSerialAxes(
    ggObj = ggplot(data = iris, mapping = aes(size = Sepal.Length)),
    color = "pink"
  )
  g
  expect_equal(class(g), c("gg", "ggplot"))
  g <- ggSerialAxes(
    ggObj = ggplot(data = iris),
    color = "pink",
    size = 2,
    showArea = TRUE
  )
  g
  expect_equal(class(g), c("gg", "ggplot"))
  ###### radial
  g <- ggSerialAxes(
    ggObj = ggplot(data = iris, mapping = aes(colour = Species, size = Sepal.Length)),
    axesLabels = colnames(iris)[ordSeq],
    layout = "radial"
  )
  g
  expect_equal(class(g), c("gg", "ggplot"))
  g <- ggSerialAxes(
    ggObj = ggplot(data = iris, mapping = aes(colour = Species)),
    layout = "radial",
    size = 0.1
  )
  g
  expect_equal(class(g), c("gg", "ggplot"))
  g <- ggSerialAxes(
    ggObj = ggplot(data = iris, mapping = aes(size = Sepal.Length)),
    layout = "radial",
    color = "pink"
  )
  g
  expect_equal(class(g), c("gg", "ggplot"))
  g <- ggSerialAxes(
    ggObj = ggplot(data = iris),
    layout = "radial",
    color = "pink",
    size = 2,
    showArea = TRUE
  )
  g
  expect_equal(class(g), c("gg", "ggplot"))
})
