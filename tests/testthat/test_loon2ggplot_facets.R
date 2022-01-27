context("test facets (loon to ggplot)")
library(png)
library(maps)
pdf(NULL)

test_that("facet basic (loon to ggplot)", {

  ## scatter plot with layers
  p <- with(quakes, l_plot(long, lat, linkingGroup = "quakes"))
  p["color"][quakes$mag < 5 & quakes$mag >= 4] <- "lightgreen"
  p["color"][quakes$mag < 6 & quakes$mag >= 5] <- "lightblue"
  p["color"][quakes$mag >= 6] <- "firebrick"
  # A Fiji map
  NZFijiMap <- map("world2", regions = c("New Zealand", "Fiji"), plot = FALSE)
  l_layer(p, NZFijiMap,
          label = "New Zealand and Fiji",
          color = "forestgreen",
          index = "end")
  fp <- l_facet(p, by = "color", layout = "wrap",
                linkingGroup = "quakes")

  expect_warning(gg <- loon2ggplot(fp))
  gg
  expect_true(inherits(gg$facet, "FacetWrap"))
  expect_warning(gg <- loon2ggplot(fp, asAes = FALSE))
  gg
  expect_true(inherits(gg$facet, "FacetWrap"))
  fp <- l_facet(p, by = "color", layout = "grid",
                linkingGroup = "quakes")
  expect_warning(gg <- loon2ggplot(fp))
  gg
  expect_true(inherits(gg$facet, "FacetGrid"))

  ## text glyph
  size <- c(rep(50, 2), rep(25, 2), rep(50, 2))
  color <- c(rep("red", 3), rep("green", 3))
  p <- l_plot(x = 1:6, y = 1:6,
              size = size,
              color = color)
  g <- l_glyph_add_text(p, text = 1:6)
  p['glyph'] <- g
  on <- data.frame(Factor1 = c(rep("A", 3), rep("B", 3)),
                   Factor2 = rep(c("C", "D"), 3))
  cbind(on, size = size, color = color)
  fp <- l_facet(p, by = Factor1 ~ Factor2, on = on, layout = "wrap")
  gg <- loon2ggplot(fp)
  gg
  expect_true(inherits(gg$facet, "FacetWrap"))
  gg <- loon2ggplot(fp, asAes = FALSE)
  gg
  expect_true(inherits(gg$facet, "FacetWrap"))

  ## polygon glyph
  p <- l_plot(1:4, 1:4,
              color = c("red", "blue", "red", "blue"))
  gl <- l_glyph_add_polygon(p,

                            x = list(x_star, x_star, x_cross, x_hexagon),
                            y = list(y_star, y_star, y_cross, y_hexagon))

  p['glyph'] <- gl
  fp <- l_facet(p, by = "color", layout = "wrap")
  gg <- loon2ggplot(fp)
  gg
  expect_true(inherits(gg$facet, "FacetWrap"))
  gg <- loon2ggplot(fp, asAes = FALSE)
  gg
  expect_true(inherits(gg$facet, "FacetWrap"))

  ## serialaxes glyph
  p <- with(olive, l_plot(oleic, stearic, color=Area))
  gs <- l_glyph_add_serialaxes(p, data=olive[,-c(1,2)], showArea=FALSE)
  p['glyph'] <- gs
  fp <- l_facet(p, by = "color", layout = "wrap")
  gg <- loon2ggplot(fp)
  gg
  expect_true(inherits(gg$facet, "FacetWrap"))
  gg <- loon2ggplot(fp, asAes = FALSE)
  gg
  expect_true(inherits(gg$facet, "FacetWrap"))

  ## text glyph
  p <- l_plot(iris, color = iris$Species)
  g <- l_glyph_add_text(p, iris$Species, "test_label")
  p['glyph'] <- g
  fp <- l_facet(p, by = "color", layout = "wrap")
  gg <- loon2ggplot(fp)
  gg
  expect_true(inherits(gg$facet, "FacetWrap"))
  gg <- loon2ggplot(fp, asAes = FALSE)
  gg
  expect_true(inherits(gg$facet, "FacetWrap"))

  ## pointrange
  p <- l_plot(x = 1:3, color = c('red', 'blue', 'green'), showScales=TRUE)
  g <- l_glyph_add_pointrange(p, ymin=(1:3)-(1:3)/5, ymax=(1:3)+(1:3)/5)
  p['glyph'] <- g
  fp <- l_facet(p, by = "color", layout = "wrap")
  gg <- loon2ggplot(fp)
  gg
  expect_true(inherits(gg$facet, "FacetWrap"))
  gg <- loon2ggplot(fp, asAes = FALSE)
  gg
  expect_true(inherits(gg$facet, "FacetWrap"))

  ## images
  p <- l_plot(1:6, color = rep(c('red', 'blue', 'green'), 2))
  img_paths <- list.files(file.path(find.package(package = 'loon'), "images"),
                          full.names = TRUE)
  imgs <- setNames(l_image_import_files(img_paths),
                   tools::file_path_sans_ext(basename(img_paths)))
  g <- l_glyph_add_image(p, imgs, label="Flags")
  p['glyph'] <- g
  fp <- l_facet(p, by = "color", layout = "wrap")
  gg <- loon2ggplot(fp)
  gg
  expect_true(inherits(gg$facet, "FacetWrap"))
  gg <- loon2ggplot(fp, asAes = FALSE)
  gg
  expect_true(inherits(gg$facet, "FacetWrap"))

  # histogram
  h <- l_hist(olive$palmitic, color = olive$Area)
  fp <- l_facet(h, by = "color", layout = "wrap")
  gg <- loon.ggplot(fp)
  gg
  expect_true(inherits(gg$facet, "FacetWrap"))
  expect_warning(loon.ggplot(fp, asAes = FALSE))

  h <- l_hist(mtcars, by = gear ~ cyl + am, on = mtcars,
              showOutlines = FALSE)
  gg <- loon.ggplot(h)
  gg
  expect_true(inherits(gg$facet, "FacetGrid"))

  # serialaxes
  s <- l_serialaxes(iris, color = iris$Species, scaling = "data")
  fp <- l_facet(s, by = "color", layout = "wrap")
  gg <- loon.ggplot(fp)
  gg
  expect_true(inherits(gg$facet, "FacetWrap"))
})

test_that("facet basic (ggplot to loon to ggplot)", {

  gg <- mpg %>%
    mutate(drv2 = paste(drv, drv)) %>%
    ggplot(aes(displ, hwy)) +
    geom_point()
  gg1 <-  gg + facet_wrap(vars(cyl))
  lg <- loon.ggplot(gg1)
  gg1 <- loon.ggplot(lg)
  gg1
  expect_true(inherits(gg1$facet, "FacetWrap"))

  gg2 <-  gg + facet_wrap(vars(cyl, drv2))
  lg <- loon.ggplot(gg2)
  gg2 <- loon.ggplot(lg)
  gg2
  expect_true(inherits(gg2$facet, "FacetWrap"))
  gg3 <-  gg + facet_wrap(vars(cyl, drv2), drop = FALSE)
  lg <- loon.ggplot(gg3)
  gg3 <- loon.ggplot(lg)
  expect_warning(plot(gg3))
  expect_true(inherits(gg3$facet, "FacetWrap"))

  gg1 <- gg + facet_grid(cyl ~ fl)
  lg <- loon.ggplot(gg1)
  gg1 <- loon.ggplot(lg)
  expect_warning(plot(gg1))
  expect_true(inherits(gg1$facet, "FacetGrid"))

  gg2 <- gg + facet_grid( ~ fl + cyl)
  lg <- loon.ggplot(gg2)
  gg2 <- loon.ggplot(lg)
  plot(gg2)
  expect_true(inherits(gg1$facet, "FacetGrid"))


  gg3 <- gg + facet_grid(fl + cyl ~.)
  lg <- loon.ggplot(gg3)
  gg3 <- loon.ggplot(lg)
  plot(gg3)
  expect_true(inherits(gg1$facet, "FacetGrid"))

  # lines
  gg <- ggplot(economics_long, aes(date, value)) +
    geom_line() +
    facet_wrap(vars(variable), scales = "free_y", nrow = 2, strip.position = "top") +
    theme(strip.background = element_blank(), strip.placement = "outside")
  xx <- loon.ggplot(loon.ggplot(gg))
  xx
  expect_true(inherits(xx$facet, "FacetWrap"))
})
