context("test (loon to ggplot)")
library(dplyr)
library(magrittr)
library(ggmulti)

pdf(NULL)

test_that("test l_plot (loon to ggplot)", {
  set.seed(500)
  x <- rnorm(30)
  y <- 4 + 3*x + rnorm(30)
  fit <- lm(y~x)
  xseq <- seq(min(x)-1, max(x)+1, length.out = 50)
  fit_line <- predict(fit, data.frame(x=range(xseq)))
  ci <- predict(fit, data.frame(x=xseq),
                interval="confidence", level=0.95)
  pi <- predict(fit, data.frame(x=xseq),
                interval="prediction", level=0.95)

  p <- l_plot(y~x, color='black', showScales=TRUE, showGuides=TRUE)
  gLayer <- l_layer_group(
    p, label="simple linear regression",
    parent="root", index="end"
  )
  fitLayer <- l_layer_line(
    p, x=range(xseq), y=fit_line, color="#04327F",
    linewidth=4, label="fit", parent=gLayer
  )
  ciLayer <- l_layer_polygon(
    p,
    x = c(xseq, rev(xseq)),
    y = c(ci[,'lwr'], rev(ci[,'upr'])),
    color = "#96BDFF", linecolor="",
    label = "95 % confidence interval",
    parent = gLayer, index='end'
  )
  piLayer <- l_layer_polygon(
    p,
    x = c(xseq, rev(xseq)),
    y = c(pi[,'lwr'], rev(pi[,'upr'])),
    color = "#E2EDFF", linecolor="",
    label = "95 % prediction interval",
    parent = gLayer, index='end'
  )
  g <- loon2ggplot(p)
  g
  expect_equal(class(g), c("gg", "ggplot"))
})


test_that("test l_hist (loon to ggplot)", {
  p <- l_hist(iris$Sepal.Length, color = iris$Species, swapAxes = TRUE)
  g <- loon2ggplot(p)
  g
  expect_equal(class(g), c("gg", "ggplot"))
})

test_that("test l_navgraph (loon to ggplot)", {
  ng <- l_navgraph(oliveAcids, color=olive$Area)
  g <- loon2ggplot(ng$graph)
  g
  expect_equal(class(g), c("gg", "ggplot"))
})

test_that("test l_serialaxes (loon to ggplot)", {

  s <- l_serialaxes(iris)
  g <- loon2ggplot(s)
  g
  expect_equal(class(g), c("gg", "ggplot"))

  s['axesLayout'] <- "parallel"
  g <- loon2ggplot(s)
  g
  expect_equal(class(g), c("gg", "ggplot"))
})

test_that("test loon basic layers (loon to ggplot)", {

  p <- l_plot()
  l <- l_layer_line(p, x=c(1,2,3,4), y=c(1,3,2,4), color='red', linewidth=2)
  l_scaleto_world(p)
  g <- loon2ggplot(p)
  g
  expect_equal(class(g), c("gg", "ggplot"))

  p <- l_plot()
  l <- l_layer_rectangle(p, x=c(2,3), y=c(1,10), color='steelblue')
  l_scaleto_layer(l)
  g <- loon2ggplot(p)
  g
  expect_equal(class(g), c("gg", "ggplot"))

  p <- l_plot()
  l <- l_layer_oval(p, c(1,5), c(2,12), color='steelblue')
  l_configure(p, panX=0, panY=0, deltaX=20, deltaY=20)
  g <- loon2ggplot(p)
  g
  expect_equal(class(g), c("gg", "ggplot"))

  p <- l_plot()
  l <- l_layer_points(p, x = 1:10, y = 1:10, size = seq(4, 30, length.out = 10))
  l_scaleto_world(p)
  g <- loon2ggplot(p)
  g
  expect_equal(class(g), c("gg", "ggplot"))

  p <- l_plot()
  l <- l_layer_points(p, x = 1:10, y = 1:10, size = seq(4, 30, length.out = 10))
  g <- loon2ggplot(p)
  g
  expect_equal(class(g), c("gg", "ggplot"))

  p <- l_plot()
  l <- l_layer_polygons(
    p,
    x = list(c(1,2,1.5), c(3,4,6,5,2), c(1,3,5,3)),
    y = list(c(1,1,2), c(1,1.5,1,4,2), c(3,5,6,4)),
    color = c('red', 'green', 'blue'),
    linecolor = ""
  )
  l_scaleto_world(p)
  g <- loon2ggplot(p)
  g
  expect_equal(class(g), c("gg", "ggplot"))

  p <- l_plot()
  l <- l_layer_rectangles(
    p,
    x = list(c(0,1), c(1,2), c(2,3), c(5,6)),
    y = list(c(0,1), c(1,2), c(0,1), c(3,4)),
    color = c('red', 'blue', 'green', 'orange'),
    linecolor = "black"
  )
  l_scaleto_world(p)
  g <- loon2ggplot(p)
  g
  expect_equal(class(g), c("gg", "ggplot"))

  s <- Filter(function(df)nrow(df) > 1, split(UsAndThem, UsAndThem$Country))
  sUaT <- Map(function(country){country[order(country$Year),]} , s)
  xcoords <- Map(function(x)x$Year, sUaT)
  ycoords <- Map(function(x)x$LifeExpectancy, sUaT)
  region <- sapply(sUaT, function(x)as.character(x$Geographic.Region[1]))

  p <- l_plot(showItemLabels=TRUE)
  l <- l_layer_lines(p, xcoords, ycoords, itemLabel=names(sUaT), color=region)
  l_scaleto_layer(l)
  g <- loon2ggplot(p)
  g
  expect_equal(class(g), c("gg", "ggplot"))

  p <- l_plot()
  l <- l_layer_texts(p, x=1:10, y=10:1, text=LETTERS[1:10],
                     size= as.integer(seq(5, 30, length.out = 10)))
  l_scaleto_world(p)
  g <- loon2ggplot(p)
  g
  expect_equal(class(g), c("gg", "ggplot"))
})


test_that("test compound loon widgets to ggplot", {

  co2_stl <- stl(co2, "per")
  p <- l_plot(co2_stl, title = "Atmospheric carbon dioxide over Mauna Loa")
  g <- loon2ggplot(p)
  g
  expect_equal(class(g), c("patchwork", "gg", "ggplot"))
  g <- loon.ggplot(p)
  expect_equal(class(g), c("patchwork", "gg", "ggplot"))

  p <- l_pairs(iris[, 1:4], showHistograms = TRUE)
  g <- loon2ggplot(p)
  g
  expect_equal(class(g), c("patchwork", "gg", "ggplot"))

})


test_that("test loon non-primitive glyph to ggplot", {

  p <- l_plot(1:3, 1:3)

  gl <- l_glyph_add_polygon(p, x = list(x_star, x_cross, x_hexagon),
                            y = list(y_star, y_cross, y_hexagon))

  p['glyph'] <- gl
  gl['showArea'] <- c(F, T, F)
  p['color'] <- c('red', 'blue', 'green')
  expect_warning(g <- loon.ggplot(p))

  p <- with(olive, l_plot(oleic, stearic, color=Area))
  gs <- l_glyph_add_serialaxes(p, data=olive[,-c(1,2)], showArea=FALSE)
  p['glyph'] <- gs
  g <- loon.ggplot(p)
  gb <- ggplot2::ggplot_build(g)
  expect_equal(is.na(gb$data[[1]]$fill), rep(TRUE, 572))
  gs['showArea'] <- TRUE
  g <- loon.ggplot(p)
  gb <- ggplot2::ggplot_build(g)
  expect_equal(is.na(gb$data[[1]]$fill), rep(FALSE, 572))
})
