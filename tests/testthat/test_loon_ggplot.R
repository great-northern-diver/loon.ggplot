context("test examples")
library(ElemStatLearn)
library(dplyr)
library(magrittr)
library(tidyverse)
library(GGally)
library(png)

test_that("example works", {
  # example 1
  # p1 <- ggplot(data = SAheart) + aes( x = age, y = chd, color = famhist) + geom_point()
  # g1 <- ggplot2loon(p1, linkingGroup = "SAheart")
  # expect_equal(class(g1), c("l_plot", "loon"))

  # example 2
  p<- ggplot(mtcars, aes(mpg, wt)) + geom_point( aes(colour = "darkblue"))
  g <- ggplot2loon(p)
  expect_equal(class(g), c("l_plot", "loon"))

  # examle 3
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
  g <- ggplot2loon(p)
  expect_equal(class(g), c("l_plot", "loon"))

  # example 4
  # p <- ggplot(data = SAheart, mapping = aes(x = age, y = chd, col = famhist)) +
  #
  #   geom_smooth(method = "loess", colour = "steelblue") +
  #   geom_point(size = 3, alpha = 0.4) +
  #   facet_wrap(~famhist)
  # g <- ggplot2loon(p)
  # expect_equal(class(g), c("l_ggplot", "l_compound", "loon"))

  # example 5
  p <- ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_wrap(~class)
  g <- ggplot2loon(p)
  expect_equal(length(g$plots), 7)

  # example 6
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

  # example 7
  p <- ggplot(economics_long, aes(date, value)) +
    geom_line() +
    facet_wrap(~variable, scales = "free_y", ncol = 1)
  g <- ggplot2loon(p)
  expect_equal(class(g), c("l_ggplot", "l_compound", "loon"))

  # example 8
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

  # example 9
  yrng <- range(economics$unemploy)
  xrng <- range(economics$date)
  caption <- paste(strwrap("Unemployment rates in the US have varied a lot over the years",
                           40), collapse = "\n")

  p <- ggplot(economics, aes(date, unemploy)) +
    geom_line() +
    annotate("text", x = xrng[1], y = yrng[2], label = caption,
             hjust = 0, vjust = 1, size = 4
    )
  g <- ggplot2loon(p)
  expect_equal(class(g), c("l_plot", "loon"))

  # example 10
  mod_coef <- coef(lm(log10(price)~ log10(carat), data = diamonds))
  p <- ggplot(diamonds, aes(log10(carat), log10(price))) +
    geom_bin2d() +
    geom_abline(intercept = mod_coef[1], slope = mod_coef[2],
                colour = "white", size = 1) +
    facet_wrap(~cut, nrow = 1)

  g <- ggplot2loon(p)
  expect_equal(length(g$plots), 5)

  # example 11
  # mi_counties <- map_data("county", "michigan") %>%
  #   select(lon = long, lat, group, id = subregion)
  # poly <- ggplot(mi_counties, aes(lon, lat)) +
  #   geom_polygon(aes(group = group)) +
  #   coord_quickmap()
  # g <- ggplot2loon(poly)
  # expect_equal(class(g), c("l_plot", "loon"))

  # example 12
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

  # example 13
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
  pp <- p + geom_abline(intercept = 37, slope = -5) + coord_polar(theta = "x")
  g <- ggplot2loon(pp)
  expect_equal(class(g), c("l_plot", "loon"))

  # example 14
  pp <- p + geom_abline(intercept = 10, slope = 2.5) + coord_polar(theta = "y")
  g <- ggplot2loon(pp)
  expect_equal(class(g), c("l_plot", "loon"))

  # example 15
  pp <- ggplot(diamonds, aes((carat))) +
    geom_histogram()
  g <- ggplot2loon(pp)
  expect_equal(class(g), c("l_hist", "loon"))

  # example 16
  pp <- ggplot(diamonds, aes(price, fill = cut)) +
    geom_histogram(binwidth = 500) + geom_freqpoly()+ facet_wrap(~color)
  g <- ggplot2loon(pp)
  expect_equal(class(g), c("l_ggplot", "l_compound", "loon"))

  # example 17
  # mtlong <- reshape2::melt(mtcars)
  # pp <- ggplot(mtlong, aes(value)) + facet_wrap(~variable, scales = 'free_x') +
  #   geom_histogram(binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3)))
  # g <- ggplot2loon(pp)
  expect_equal(class(g), c("l_ggplot", "l_compound", "loon"))

  # example 18
  g <- ggplot(mpg, aes(class))
  pp <- g + geom_bar(aes(fill = drv)) +
    geom_bar(aes(fill = drv), position = position_stack(reverse = TRUE)) +
    coord_flip() +
    theme(legend.position = "top")
  gg <- ggplot2loon(pp, ggGuides = TRUE)
  expect_equal(class(gg), c("l_plot", "loon"))

  # example 19
  df <- data.frame(trt = c("a", "b", "c"), outcome = c(2.3, 1.9, 3.2))
  p <- ggplot(df, aes(trt, outcome)) +
    geom_col()
  g <- ggplot2loon(p, ggGuides = TRUE)
  expect_equal(class(g), c("l_plot", "loon"))

  # example 20
  d <- data.frame(x=c(1,2,4,5,7,8,9), y=c(1,2,3,5,6,7,9))
  pp <- ggplot() +
    geom_step(data=d, mapping=aes(x=x, y=y)) +
    geom_step(data=d, mapping=aes(x=x, y=y), direction="vh", linetype=3) +
    geom_point(data=d, mapping=aes(x=x, y=y), color="red")
  g <- ggplot2loon(pp)
  expect_equal(class(g), c("l_plot", "loon"))

  # example 21
  huron <- data.frame(year = 1875:1972, level = as.vector(LakeHuron))
  # Add aesthetic mappings
  pp <- ggplot(huron, aes(year)) +
    geom_ribbon(aes(ymin = level - 1, ymax = level + 1), fill = "grey70", colour = "black") +
    geom_line(aes(y = level))
  g <- ggplot2loon(pp)
  expect_equal(class(g), c("l_plot", "loon"))

  # example 22
  df= data.frame(Time=as.numeric(strsplit('1939 1949 1959 1969 1979 1989 1999 2009 2019 2029 2039 2049 1939 1949 1959 1969 1979 1989 1999 2009 2019 2029 2039 2049', split=' ')[[1]] ),
                 Acres=as.numeric(strsplit('139504.2 233529.0 392105.3 502983.9 685159.9 835594.7 882945.1 1212671.4 1475211.9 1717971.7 1862505.7 1934308.0 308261.4 502460.8 834303.1 1115150.7 1430797.8 1712085.8 1973366.1 1694907.7 1480506.0 1280047.6 1164200.5 1118045.3', split=' ')[[1]] ),
                 WUClass= strsplit('DenseUrban DenseUrban DenseUrban DenseUrban DenseUrban DenseUrban DenseUrban DenseUrban DenseUrban DenseUrban DenseUrban DenseUrban Urban Urban Urban Urban Urban Urban Urban Urban Urban Urban Urban Urban', split=' ')[[1]]
  )
  pp <- ggplot(df,aes(x = Time,y = Acres,fill=WUClass)) +
    geom_area( position = 'stack') +
    geom_area( position = 'stack', colour="black", show.legend=FALSE)
  g <- ggplot2loon(pp)
  expect_equal(class(g), c("l_plot", "loon"))

  # example 23
  p <- ggplot(data = SAheart, mapping = aes(x = tobacco, y = sbp))
  pp <- p + geom_point() + geom_density_2d(lwd = 1.5, col = "steelblue")
  g <- ggplot2loon(pp)
  expect_equal(class(g), c("l_plot", "loon"))

  # example 24
  pp <- ggplot(diamonds, aes(depth, colour = cut)) +
    geom_density(na.rm = TRUE) +
    xlim(55, 70)
  g <- ggplot2loon(pp)
  expect_equal(class(g), c("l_plot", "loon"))

  # example 25
  pp <- ggplot(diamonds, aes(carat, fill = cut)) +
    geom_density(position = "stack")
  g <- ggplot2loon(pp)
  expect_equal(class(g), c("l_plot", "loon"))

  # example 26
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

  # example 27
  pp <- p + geom_pointrange(aes(ymin = lower, ymax = upper))
  expect_equal(class(ggplot2loon(pp)), c("l_plot", "loon"))

  # example 28
  pp <- p + geom_crossbar(aes(ymin = lower, ymax = upper), width = 0.2)
  expect_equal(class(ggplot2loon(pp)), c("l_plot", "loon"))

  # example 29
  pp <- p + geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2)
  expect_equal(class(ggplot2loon(pp)), c("l_plot", "loon"))

  # example 30
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

  # example 31
  # skip_on_cran()
  # pp <- ggplot(faithfuld, aes(waiting, eruptions)) +
  #   geom_raster(aes(fill = density))
  # expect_equal(class(ggplot2loon(pp)), c("l_hist", "loon"))

  # example 32
  pp <- ggplot(mtcars, aes(x = mpg)) + geom_dotplot(binwidth = 1.5)
  expect_equal(class(ggplot2loon(pp)), c("l_plot", "loon"))

  # example 33
  p <- ggplot(mpg, aes(class, hwy))
  pp <- p + geom_boxplot()
  expect_equal(class(ggplot2loon(pp, ggGuides = TRUE)), c("l_plot", "loon"))

  # # example 34
  # pp <- ggplot(diamonds, aes(carat, price)) +
  #   geom_boxplot(aes(group = cut_width(carat, 0.25)), outlier.alpha = 0.1)
  # expect_equal(class(ggplot2loon(pp)), c("l_plot", "loon"))

  # example 35
  p <- ggplot(mtcars, aes(factor(cyl), mpg))
  pp <- p + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))
  expect_equal(class(ggplot2loon(pp)), c("l_plot", "loon"))

  # example 36
  df <- data.frame(y = rt(200, df = 5))
  p <- ggplot(df, aes(sample = y))
  pp <- p + stat_qq() + stat_qq_line()
  g <- ggplot2loon(pp)
  expect_equal(class(g), c("l_plot", "loon"))

  # example 37
  d <- ggplot(diamonds, aes(x, y)) + xlim(4, 10) + ylim(4, 10)
  pp <- d + geom_bin2d(na.rm = TRUE)
  g <- ggplot2loon(pp)
  expect_equal(class(g), c("l_plot", "loon"))

  # example 38
  d <- ggplot(diamonds, aes(carat, price))
  pp <- d + geom_hex()
  g <- ggplot2loon(pp)
  expect_equal(class(g), c("l_plot", "loon"))

  # example 39
  v <- ggplot(faithfuld, aes(waiting, eruptions, z = density))
  pp <- v + geom_contour()
  g <- ggplot2loon(pp)
  expect_equal(class(g), c("l_plot", "loon"))

  # example 40
  p <- ggplot(mtcars, aes(as.numeric(wt), mpg)) +
    geom_point()
  pp <- p + geom_rug()
  g <- ggplot2loon(pp)
  expect_equal(class(g), c("l_plot", "loon"))

  # example 41
  pie <- ggplot(mtcars, aes(x = factor(1), fill = factor(cyl))) +
    geom_bar(width = 1)
  pp <- pie + coord_polar(theta = "x")
  g <- ggplot2loon(pp)
  expect_equal(class(g), c("l_plot", "loon"))

  # example 42
  pp <- pie + coord_polar(theta = "y")
  g <- ggplot2loon(pp)
  expect_equal(class(g), c("l_plot", "loon"))

  # example 43
  rect <- data.frame(x = 50, y = 50)
  line <- data.frame(x = c(1, 200), y = c(100, 1))
  base <- ggplot(mapping = aes(x, y)) +
    geom_tile(data = rect, aes(width = 50, height = 50)) +
    geom_line(data = line) +
    xlab(NULL) + ylab(NULL)
  pp <- base + coord_polar("x")
  g <- ggplot2loon(pp)
  expect_equal(class(g), c("l_plot", "loon"))

  # example 44
  pp <- ggplot(SAheart, aes(obesity, adiposity)) + geom_point() + geom_smooth() +
    coord_polar(theta = "y")
  g <- ggplot2loon(pp)
  expect_equal(class(g), c("l_plot", "loon"))

  # example 45
  # data(Oats, package = "MEMSS")
  # pg.oats <- ggplot(Oats, aes(nitro, yield)) +
  #   geom_line() +
  #   geom_point() +
  #   ggtitle("foo") +
  #   facet_wrap(~Block + Variety, ncol = 3)
  # g <- ggplot2loon(pg.oats, linkingGroup = "A")
  # expect_equal(class(g), c("l_ggplot", "l_compound", "loon"))

  # example 46
  # SAheart %>%
  #   mutate(ltob = log(tobacco), lsbp = log(sbp)) %>%
  #   filter(age < 50) %>%
  #   ggplot(aes(x = ltob, y = lsbp)) +
  #   geom_point() +
  #   facet_wrap(~chd) -> p
  # g <- ggplot2loon(p)
  # expect_equal(class(g), c("l_ggplot", "l_compound", "loon"))

  # example 47
  pp <- ggplot() + geom_histogram(mpg, mapping = aes(x = cty, y = ..density..))
  g <- ggplot2loon(pp)
  expect_equal(class(g), c("l_hist", "loon"))

  # example 48
  # h <- ggplot(data = SAheart, mapping = aes(x = adiposity)) +
  #   geom_histogram(mapping = aes(y = ..density..),
  #                  bins = 10, fill = "steelblue",
  #                  col = "black", alpha = 0.5) +
  #   geom_density(mapping = aes(y = ..density..),
  #                fill = "grey", alpha = 0.5)
  # g <- ggplot2loon(h)
  # expect_equal(class(g), c("l_hist", "loon"))

  # example 49 theme
  p <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg,
                                       colour = factor(gear))) + facet_wrap(~am)
  g <- ggplot2loon(p, ggGuides = T)
  expect_equal(class(g), c("l_ggplot", "l_compound", "loon"))

  # example 50
  p1 <- p + theme_gray() # the default
  g <- ggplot2loon(p1, ggGuides = T)
  expect_equal(class(g), c("l_ggplot", "l_compound", "loon"))

  # exmaple 51
  p1 <- p + theme_bw()
  g <- ggplot2loon(p1, ggGuides = T)
  expect_equal(class(g), c("l_ggplot", "l_compound", "loon"))

  # exmaple 52
  p1 <- p + theme_linedraw()
  g <- ggplot2loon(p1, ggGuides = T)
  expect_equal(class(g), c("l_ggplot", "l_compound", "loon"))

  # example 53
  p1 <- p + theme_light()
  g <- ggplot2loon(p1, ggGuides = T)
  expect_equal(class(g), c("l_ggplot", "l_compound", "loon"))

  # example 54
  p1 <- p + theme_dark()
  g <- ggplot2loon(p1, ggGuides = T)
  expect_equal(class(g), c("l_ggplot", "l_compound", "loon"))

  # exmaple 55
  p1 <- p + theme_minimal()
  g <- ggplot2loon(p1, ggGuides = T)
  expect_equal(class(g), c("l_ggplot", "l_compound", "loon"))

  # example 56
  p1 <- p + theme_classic()
  g <- ggplot2loon(p1, ggGuides = T)
  expect_equal(class(g), c("l_ggplot", "l_compound", "loon"))

  # example 57
  p1 <- p + theme_void()
  g <- ggplot2loon(p1, ggGuides = T)
  expect_equal(class(g), c("l_ggplot", "l_compound", "loon"))

  # example 58 theme polar cood
  p <- ggplot(mtcars) +
    geom_point(aes(x = wt, y = mpg,
                   colour = factor(gear))) +
    facet_wrap(~am) +
    coord_polar()
  g <- ggplot2loon(p, ggGuides = T)
  expect_equal(class(g), c("l_ggplot", "l_compound", "loon"))

  # example 59
  p1 <- p + theme_gray() # the default
  g <- ggplot2loon(p1, ggGuides = T)
  expect_equal(class(g), c("l_ggplot", "l_compound", "loon"))

  # exmaple 60
  p1 <- p + theme_bw()
  g <- ggplot2loon(p1, ggGuides = T)
  expect_equal(class(g), c("l_ggplot", "l_compound", "loon"))

  # exmaple 61
  p1 <- p + theme_linedraw()
  g <- ggplot2loon(p1, ggGuides = T)
  expect_equal(class(g), c("l_ggplot", "l_compound", "loon"))

  # example 62
  p1 <- p + theme_light()
  g <- ggplot2loon(p1, ggGuides = T)
  expect_equal(class(g), c("l_ggplot", "l_compound", "loon"))

  # example 63
  p1 <- p + theme_dark()
  g <- ggplot2loon(p1, ggGuides = T)
  expect_equal(class(g), c("l_ggplot", "l_compound", "loon"))

  # exmaple 64
  p1 <- p + theme_minimal()
  g <- ggplot2loon(p1, ggGuides = T)
  expect_equal(class(g), c("l_ggplot", "l_compound", "loon"))

  # example 65
  p1 <- p + theme_classic()
  g <- ggplot2loon(p1, ggGuides = T)
  expect_equal(class(g), c("l_ggplot", "l_compound", "loon"))

  # example 66
  p1 <- p + theme_void()
  g <- ggplot2loon(p1, ggGuides = T)
  expect_equal(class(g), c("l_ggplot", "l_compound", "loon"))

  # example 67
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
  expect_equal(class(g), c("l_ggplot", "l_compound", "loon"))

  # example 68
  pp <- ggplot(df, aes(x, y)) +
    geom_point(data = df2, colour = "grey70") +
    geom_point(aes(colour = z)) +
    facet_wrap(~z, scales = "free_x")
  g <- ggplot2loon(pp, activeGeomLayers = 1)
  expect_equal(class(g), c("l_ggplot", "l_compound", "loon"))

  # example 69
  pp <- ggplot(df, aes(x, y)) +
    geom_point(data = df2, colour = "grey70") +
    geom_point(aes(colour = z)) +
    facet_wrap(~z, scales = "free_y")
  g <- ggplot2loon(pp, activeGeomLayers = c(1,2))
  expect_equal(class(g), c("l_ggplot", "l_compound", "loon"))

  # example 70
  p <- ggplot(mpg, aes(displ, cty)) + geom_point() + ggtitle("foo")
  # Use vars() to supply variables from the dataset:
  p1 <- p + facet_grid(rows = vars(drv))
  g <- ggplot2loon(p1)
  expect_equal(class(g), c("l_ggplot", "l_compound", "loon"))

  # example 71
  p2 <- p + facet_grid(cols = vars(cyl))
  g <- ggplot2loon(p2)
  expect_equal(class(g), c("l_ggplot", "l_compound", "loon"))

  # example 72
  p3 <- p + facet_grid(vars(drv), vars(cyl))
  g <- ggplot2loon(p3, tkLabels = TRUE)
  expect_equal(class(g), c("l_ggplot", "l_compound", "loon"))

  # example 73
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

  # example 74
  p <- l_plot()
  l <- l_layer_line(p, x=c(1,2,3,4), y=c(1,3,2,4), color='red', linewidth=2)
  l_scaleto_world(p)
  g <- loon2ggplot(p)
  g
  expect_equal(class(g), c("gg", "ggplot"))

  # example 75
  p <- l_plot()
  l <- l_layer_rectangle(p, x=c(2,3), y=c(1,10), color='steelblue')
  l_scaleto_layer(l)
  g <- loon2ggplot(p)
  g
  expect_equal(class(g), c("gg", "ggplot"))

  # example 76
  p <- l_plot()
  l <- l_layer_oval(p, c(1,5), c(2,12), color='steelblue')
  l_configure(p, panX=0, panY=0, deltaX=20, deltaY=20)
  g <- loon2ggplot(p)
  g
  expect_equal(class(g), c("gg", "ggplot"))

  # example 77
  p <- l_plot()
  l <- l_layer_points(p, x = 1:10, y = 1:10, size = seq(4, 30, length.out = 10))
  l_scaleto_world(p)
  g <- loon2ggplot(p)
  g
  expect_equal(class(g), c("gg", "ggplot"))

  # example 78
  p <- l_plot()
  l <- l_layer_points(p, x = 1:10, y = 1:10, size = seq(4, 30, length.out = 10))
  g <- loon2ggplot(p)
  g
  expect_equal(class(g), c("gg", "ggplot"))

  # ex 79
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

  # ex 80
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

  # ex 81
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

  # ex 82
  p <- l_plot()
  l <- l_layer_texts(p, x=1:10, y=10:1, text=LETTERS[1:10], size= as.integer(seq(5, 30, length.out = 10)))
  l_scaleto_world(p)
  g <- loon2ggplot(p)
  g
  expect_equal(class(g), c("gg", "ggplot"))

  # ex83
  p <- ggplot(data = data.frame(x = 1:3, y = 1:3),
              mapping = aes(x = x, y = y)) +
    geom_pointrangeGlyph(ymin=(1:3)-(1:3)/5, ymax=(1:3)+(1:3)/5)
  p
  expect_equal(class(p), c("gg", "ggplot"))

  # ex 84
  p <- ggplot(data = data.frame(x = 1:26, y = 1:26),
              mapping = aes(x = x, y = y)) +
    geom_textGlyph(text = LETTERS, size = (1:26)/5)
  p
  expect_equal(class(p), c("gg", "ggplot"))

  # ex 85
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

  # ex 86
  img_paths <- list.files(file.path(find.package(package = 'loon'), "images"), full.names = TRUE)
  images <- lapply(img_paths, function(path) png::readPNG(path))
  p <- ggplot(data = data.frame(x = 1:6, y = 1:6),
              mapping = aes(x = x, y = y)) +
    geom_imageGlyph(images = images, alpha = 0.4)
  p
  expect_equal(class(p), c("gg", "ggplot"))

  # ex 87
  p <- ggplot(data = iris, mapping = aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
    geom_serialAxesGlyph(serialAxesData = iris[, -5], axesLayout = "radial")
  p
  expect_equal(class(p), c("gg", "ggplot"))

  # ex 88
  p <- ggplot(data = iris, mapping = aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
    geom_serialAxesGlyph(serialAxesData = iris[, -5], axesLayout = 'parallel')
  p
  expect_equal(class(p), c("gg", "ggplot"))

  # ex 89
  p <- l_hist(iris$Sepal.Length, color = iris$Species)
  g <- loon2ggplot(p)
  g
  expect_equal(class(g), c("gg", "ggplot"))

  # ex90
  ng <- l_navgraph(oliveAcids, color=olive$Area)
  g <- loon2ggplot(ng$graph)
  g
  expect_equal(class(g), c("gg", "ggplot"))

  # ex91
  p <- l_pairs(iris, showHistograms = TRUE)
  g <- loon2ggplot(p)
  g
  expect_equal(class(g), c("gg", "ggmatrix"))

  # ex92
  plotList <- list()
  for (i in 1:6) {
    plotList[[i]] <- ggally_text(paste("Plot #", i, sep = ""))
  }
  pm <- ggmatrix(
    plotList,
    2, 3,
    c("A", "B", "C"),
    c("D", "E"),
    byrow = TRUE
  )
  g <- ggplot2loon(pm)
  expect_equal(class(g), c("l_ggmatrix", "l_ggplot", "l_compound", "loon"))

  # ex93
  co2_stl <- stl(co2, "per")
  p <- l_plot(co2_stl, title = "Atmospheric carbon dioxide over Mauna Loa")
  g <- loon2ggplot(p)
  g
  expect_equal(class(g), c("gg", "ggmatrix"))

  # ex94
  s <- l_serialaxes(iris)
  g <- loon2ggplot(s)
  g
  expect_equal(class(g), c("gg", "ggplot"))

  # ex95
  s['axesLayout'] <- "parallel"
  g <- loon2ggplot(s)
  g
  expect_equal(class(g), c("gg", "ggplot"))

  # ex95
  g <- ggplot(iris,
         mapping = aes(colour = as.factor(Species))) %>%
    ggSerialAxes()
  g
  expect_equal(class(g), c("gg", "ggplot"))
})
