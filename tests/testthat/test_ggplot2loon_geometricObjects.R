context("test geometric objects (ggplot to loon)")
library(dplyr)
library(magrittr)
library(ggmulti)
library(hexbin)

pdf(NULL)

test_that("geometric layers (ggplot to loon)", {

  # point
  p<- ggplot(mtcars, aes(mpg, wt)) + geom_point( aes(colour = "darkblue"))
  expect_equal(get_activeGeomLayers(p), c(l_point = 1))
  g <- ggplot2loon(p)
  expect_is(g, c("l_plot", "loon"))

  ids <- factor(c("1.1", "2.1", "1.2", "2.2", "1.3", "2.3"))
  values <- data.frame(
    id = ids,
    value = c(3, 3.1, 3.1, 3.2, 3.15, 3.5)
  )
  positions <- data.frame(
    id = rep(ids, each = 4),
    x = c(2, 1, 1.1, 2.2, 1, 0, 0.3, 1.1, 2.2, 1.1, 1.2, 2.5, 1.1, 0.3,
          0.5, 1.2, 2.5, 1.2, 1.3, 2.7, 1.2, 0.5, 0.6, 1.3),
    y = c(-0.5, 0, 1, 0.5, 0, 0.5, 1.5, 1, 0.5, 1, 2.1, 1.7, 1, 1.5,
          2.2, 2.1, 1.7, 2.1, 3.2, 2.8, 2.1, 2.2, 3.3, 3.2)
  )
  # Currently we need to manually merge the two together
  datapoly <- merge(values, positions, by = c("id"))

  p <- ggplot(datapoly, aes(x = x, y = y)) +
    geom_polygon(aes(fill = value, group = id))
  expect_equal(length(get_activeGeomLayers(p)), 0)
  g <- ggplot2loon(p)
  expect_equal(class(g), c("l_plot", "loon"))
  g <- loon.ggplot(p)
  expect_equal(class(g), c("l_plot", "loon"))

  df <- data.frame(
    x = c(3, 1, 5),
    y = c(2, 4, 6),
    label = c("a","b","c")
  )
  p <- ggplot(df, aes(x, y, label = label)) +
    labs(x = NULL, y = NULL) + # Hide axis label
    theme(plot.title = element_text(size = 12))+ geom_point() + ggtitle("point") +
    geom_tile()+ geom_polygon() + geom_text()+
    geom_bar(stat = "identity")+ ggtitle("polygon")+ geom_path()
  g <- ggplot2loon(p)
  expect_equal(class(g), c("l_plot", "loon"))

  p <- ggplot(economics) +
    geom_rect(
      aes(xmin = start, xmax = end, fill = party),
      ymin = -Inf, ymax = Inf, alpha = 0.2,
      data = presidential
    ) +
    geom_vline(
      aes(xintercept = as.numeric(start)),
      data = presidential,
      colour = "grey50", alpha = 0.5
    ) +
    geom_text(
      aes(x = start, y = 2500, label = name),data = presidential,
      size = 3, vjust = 0, hjust = 0, nudge_x = 50
    ) +
    geom_line(aes(date, unemploy)) +
    scale_fill_manual(values = c("blue", "red"))
  g <- ggplot2loon(p)
  expect_equal(class(g), c("l_plot", "loon"))

  yrng <- range(economics$unemploy)
  xrng <- range(economics$date)
  caption <- paste(strwrap("Unemployment rates in the US have varied a lot over the years",
                           40), collapse = "\n")

  p <- ggplot(economics, aes(date, unemploy)) +
    geom_line() +
    annotate("text", x = xrng[1], y = yrng[2], label = caption,
             hjust = 0, vjust = 1, size = 4
    )
  g <- ggplot2loon(p, activeGeomLayers = 0L)
  expect_equal(class(g), c("l_plot", "loon"))

  mod_coef <- coef(lm(log10( mpg)~ log10(hp), data = mtcars))
  p <- ggplot(mtcars, aes(log10(hp), log10(mpg))) +
    geom_bin2d() +
    geom_abline(intercept = mod_coef[1], slope = mod_coef[2],
                colour = "white", size = 1) +
    facet_wrap(~am, nrow = 1)

  g <- ggplot2loon(p)
  expect_equal(length(g), 2)

  df <- data.frame(x = 1:3, y = 1:3, colour = c(1,3,5))
  xgrid <- with(df, seq(min(x), max(x), length = 50))
  interp <- data.frame(
    x = xgrid,
    y = approx(df$x, df$y, xout = xgrid)$y,
    colour = approx(df$x, df$colour, xout = xgrid)$y
  )
  p <- ggplot(interp, aes(x, y, colour = colour)) +
    geom_line(size = 2) +
    geom_point(data = df, size = 5)
  g <- ggplot2loon(p)
  expect_equal(class(g), c("l_plot", "loon"))

  df <- data.frame(trt = c("a", "b", "c"), outcome = c(2.3, 1.9, 3.2))
  p <- ggplot(df, aes(trt, outcome)) +
    geom_col()
  g <- ggplot2loon(p, ggGuides = TRUE)
  expect_equal(class(g), c("l_plot", "loon"))

  d <- data.frame(x=c(1,2,4,5,7,8,9), y=c(1,2,3,5,6,7,9))
  pp <- ggplot() +
    geom_step(data=d, mapping=aes(x=x, y=y)) +
    geom_step(data=d, mapping=aes(x=x, y=y), direction="vh", linetype=3) +
    geom_point(data=d, mapping=aes(x=x, y=y), color="red")
  g <- ggplot2loon(pp)
  expect_equal(class(g), c("l_plot", "loon"))

  huron <- data.frame(year = 1875:1972, level = as.vector(LakeHuron))
  # Add aesthetic mappings
  pp <- ggplot(huron, aes(year)) +
    geom_ribbon(aes(ymin = level - 1, ymax = level + 1), fill = "grey70", colour = "black") +
    geom_line(aes(y = level))
  g <- ggplot2loon(pp)
  expect_equal(class(g), c("l_plot", "loon"))

  df= data.frame(Time=as.numeric(strsplit('1939 1949 1959 1969 1979 1989 1999 2009 2019 2029 2039 2049 1939 1949 1959 1969 1979 1989 1999 2009 2019 2029 2039 2049', split=' ')[[1]] ),
                 Acres=as.numeric(strsplit('139504.2 233529.0 392105.3 502983.9 685159.9 835594.7 882945.1 1212671.4 1475211.9 1717971.7 1862505.7 1934308.0 308261.4 502460.8 834303.1 1115150.7 1430797.8 1712085.8 1973366.1 1694907.7 1480506.0 1280047.6 1164200.5 1118045.3', split=' ')[[1]] ),
                 WUClass= strsplit('DenseUrban DenseUrban DenseUrban DenseUrban DenseUrban DenseUrban DenseUrban DenseUrban DenseUrban DenseUrban DenseUrban DenseUrban Urban Urban Urban Urban Urban Urban Urban Urban Urban Urban Urban Urban', split=' ')[[1]]
  )
  pp <- ggplot(df,aes(x = Time,y = Acres,fill=WUClass)) +
    geom_area( position = 'stack') +
    geom_area( position = 'stack', colour="black", show.legend=FALSE)
  g <- ggplot2loon(pp)
  expect_equal(class(g), c("l_plot", "loon"))

  p <- ggplot(data = mtcars, mapping = aes(x = mpg, y = hp))
  pp <- p + geom_point() + geom_density_2d(lwd = 1.5, col = "steelblue")
  g <- ggplot2loon(pp)
  expect_equal(class(g), c("l_plot", "loon"))

  pp <- ggplot(mtcars, aes(hp, colour = am)) +
    geom_density(na.rm = TRUE) +
    xlim(100, 200)
  g <- ggplot2loon(pp)
  expect_equal(class(g), c("l_plot", "loon"))

  df <- data.frame(
    trt = factor(c(1, 1, 2, 2)),
    resp = c(1, 5, 3, 4),
    group = factor(c(1, 2, 1, 2)),
    upper = c(1.1, 5.3, 3.3, 4.2),
    lower = c(0.8, 4.6, 2.4, 3.6)
  )
  p <- ggplot(df, aes(trt, resp, colour = group))
  pp <- p + geom_linerange(aes(ymin = lower, ymax = upper))
  expect_equal(class(ggplot2loon(pp)), c("l_plot", "loon"))

  pp <- p + geom_pointrange(aes(ymin = lower, ymax = upper))
  expect_equal(class(ggplot2loon(pp)), c("l_plot", "loon"))

  pp <- p + geom_crossbar(aes(ymin = lower, ymax = upper), width = 0.2)
  expect_equal(class(ggplot2loon(pp)), c("l_plot", "loon"))

  pp <- p + geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2)
  expect_equal(class(ggplot2loon(pp)), c("l_plot", "loon"))

  df <- data.frame(
    trt = factor(c(1, 1, 2, 2)),
    resp = c(1, 5, 3, 4),
    group = factor(c(1, 2, 1, 2)),
    se = c(0.1, 0.3, 0.3, 0.2)
  )
  p <- ggplot(df, aes(resp, trt, colour = group))
  pp <- p +
    geom_errorbarh(aes(xmax = resp + se, xmin = resp - se))
  expect_equal(class(ggplot2loon(pp)), c("l_plot", "loon"))

  # skip_on_cran()
  # pp <- ggplot(faithfuld, aes(waiting, eruptions)) +
  #   geom_raster(aes(fill = density))
  # expect_equal(class(ggplot2loon(pp)), c("l_hist", "loon"))

  pp <- ggplot(mtcars, aes(x = mpg)) + geom_dotplot(binwidth = 1.5)
  expect_equal(class(ggplot2loon(pp)), c("l_plot", "loon"))

  p <- ggplot(mpg, aes(class, hwy))
  pp <- p + geom_boxplot()
  expect_equal(class(ggplot2loon(pp, ggGuides = TRUE)), c("l_plot", "loon"))

  p <- ggplot(mpg, aes(hwy, class))
  pp <- p + geom_boxplot()
  expect_equal(class(ggplot2loon(pp, ggGuides = TRUE)), c("l_plot", "loon"))

  p <- ggplot(mtcars, aes(factor(cyl), mpg))
  pp <- p + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))
  expect_equal(class(ggplot2loon(pp)), c("l_plot", "loon"))

  df <- data.frame(y = rt(200, df = 5))
  p <- ggplot(df, aes(sample = y))
  pp <- p + stat_qq() + stat_qq_line()
  g <- ggplot2loon(pp)
  expect_equal(class(g), c("l_plot", "loon"))

  d <- ggplot(diamonds, aes(x, y)) + xlim(4, 10) + ylim(4, 10)
  pp <- d + geom_bin2d(na.rm = TRUE)
  g <- ggplot2loon(pp)
  expect_equal(class(g), c("l_plot", "loon"))

  d <- ggplot(diamonds, aes(carat, price))
  pp <- d + geom_hex()
  g <- ggplot2loon(pp)
  expect_equal(class(g), c("l_plot", "loon"))

  v <- ggplot(faithfuld, aes(waiting, eruptions, z = density))
  pp <- v + geom_contour()
  g <- ggplot2loon(pp)
  expect_equal(class(g), c("l_plot", "loon"))

  p <- ggplot(mtcars, aes(as.numeric(wt), mpg)) +
    geom_point()
  pp <- p + geom_rug()
  g <- ggplot2loon(pp)
  expect_equal(class(g), c("l_plot", "loon"))

  # plotList <- list()
  # for (i in 1:6) {
  #   plotList[[i]] <- suppressWarnings(ggally_text(paste("Plot #", i, sep = "")))
  # }
  # pm <- ggmatrix(
  #   plotList,
  #   2, 3,
  #   c("A", "B", "C"),
  #   c("D", "E"),
  #   byrow = TRUE
  # )
  # g <- ggplot2loon(pm)
  # expect_equal(class(g), c("l_ggmatrix", "l_compound", "loon"))
})

test_that("geometric (histogram, bar) layers (ggplot to loon)", {
  pp <- ggplot(mtcars, aes((hp), fill = factor(am))) +
    geom_histogram()
  expect_equal(length(get_activeGeomLayers(pp)), 1)
  g <- ggplot2loon(pp)
  expect_equal(class(g), c("l_hist", "loon"))

  pp <- ggplot(mtcars, aes(hp, fill = as.character(am))) +
    geom_histogram() + geom_freqpoly()+ facet_wrap(~cyl)
  g <- ggplot2loon(pp)
  expect_equal(class(g), c("l_facet_ggplot", "l_facet", "l_compound", "loon"))

  # mtlong <- reshape2::melt(mtcars)
  # pp <- ggplot(mtlong, aes(value)) + facet_wrap(~variable, scales = 'free_x') +
  #   geom_histogram(binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3)))
  # g <- ggplot2loon(pp)
  # expect_equal(class(g), c("l_facet_ggplot", "l_facet", "l_compound", "loon"))

  g <- ggplot(mpg, aes(class))
  pp <- g +
    geom_bar(aes(fill = drv), position = position_stack(reverse = TRUE)) +
    coord_flip() +
    theme(legend.position = "top")
  gg <- ggplot2loon(pp, ggGuides = TRUE)
  expect_equal(class(gg), c("l_hist", "loon"))

  #### TODO:This is an interesting case
  #### FIX LATER
  pp <- ggplot() + geom_histogram(mpg, mapping = aes(x = cty, y = ..density.., fill = trans))
  g <- ggplot2loon(pp)
  expect_equal(class(g), c("l_hist", "loon"))

  # h <- ggplot(data = SAheart, mapping = aes(x = adiposity)) +
  #   geom_histogram(mapping = aes(y = ..density..),
  #                  bins = 10, fill = "steelblue",
  #                  col = "black", alpha = 0.5) +
  #   geom_density(mapping = aes(y = ..density..),
  #                fill = "grey", alpha = 0.5)
  # g <- ggplot2loon(h)
  # expect_equal(class(g), c("l_hist", "loon"))

})

test_that("test some helper functions", {
  # gg_pipe
  g <- mtcars %>%
    gg_pipe(
      ggplot(aes(mpg, wt, colour = cyl)) + geom_point()
    ) %>%
    ggplot2loon()
  expect_equal(class(g), c("l_plot", "loon"))

  # layout coordinates
  l <- layout_coords(g)
  expect_is(l, "matrix")
  h <- l_hist(iris)
  l <- layout_coords(h)
  expect_is(l, "matrix")
})


test_that("test static objects", {
  # polygon
  p <- ggplot(data = data.frame(x = (1:4) * 1000, y = (1:4) * 10),
              mapping = aes(x = x, y = y)) +
    geom_polygon_glyph(polygon_x = list(x_star, x_cross, x_hexagon, x_airplane),
                       polygon_y = list(y_star, y_cross, y_hexagon, y_airplane),
                       colour = 'black', fill = 'red')
  lp <- loon.ggplot(p, activeGeomLayers = 0L)
  layers <- loon::l_layer_getChildren(lp)
  childrenLayers <- loon::l_layer_getChildren(loon::l_create_handle(c(lp, layers[1])))
  expect_equal(length(childrenLayers), 4)

  # radial
  p <- ggplot(data = olive[1:100,],
              mapping = aes(x = oleic, y = stearic)) +
    geom_serialaxes_glyph(serialaxes.data = olive[1:100, -c(1, 2)],
                          show.axes = TRUE,
                          show.enclosing = TRUE,
                          axes.layout = "radial")
  lp <- loon.ggplot(p, activeGeomLayers = 0L)
  layers <- loon::l_layer_getChildren(lp)
  childrenLayers <- loon::l_layer_getChildren(loon::l_create_handle(c(lp, layers[1])))
  expect_equal(length(childrenLayers), 3)

  # parallel
  p <- ggplot(data = olive[1:100,],
              mapping = aes(x = oleic, y = stearic)) +
    geom_serialaxes_glyph(serialaxes.data = olive[1:100, -c(1, 2)],
                          show.axes = TRUE,
                          show.enclosing = TRUE,
                          axes.layout = "parallel")
  lp <- loon.ggplot(p, activeGeomLayers = 0L)
  layers <- loon::l_layer_getChildren(lp)
  childrenLayers <- loon::l_layer_getChildren(loon::l_create_handle(c(lp, layers[1])))
  expect_equal(length(childrenLayers), 3)

  # image
  image <- as.raster(matrix(seq(0, 1, length.out = 15), ncol = 5, nrow = 3))
  mat <- matrix(c(0,0,0,0, 1,1), ncol=2)

  p <- ggplot(data = data.frame(x = c(1, 2), y = c(1, 2)),
              mapping = aes(x = x, y = y)) +
    geom_image_glyph(images = list(image, mat),
                     imagewidth = 1,
                     imageheight = 1)
  lp <- loon.ggplot(p, activeGeomLayers = 0L)
  layers <- loon::l_layer_getChildren(lp)
  childrenLayers <- loon::l_layer_getChildren(loon::l_create_handle(c(lp, layers[1])))
  expect_equal(length(childrenLayers), 2)

})
