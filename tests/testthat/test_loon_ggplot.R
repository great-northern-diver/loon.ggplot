context("test examples")
library(ElemStatLearn)
library(dplyr)
library(lattice)
library(magrittr)
library(tidyverse)

test_that("example works", {
  # example 1
  p1 <- ggplot(data = SAheart) + aes( x = age, y = chd, color = famhist) + geom_point()
  g1 <- loon.ggplot(p1, linkingGroup = "SAheart")
  expect_equal(class(g1), c("l_ggplot", "loon"))

  # example 2
  p<- ggplot(mtcars, aes(mpg, wt)) + geom_point( aes(colour = "darkblue"))
  g <- loon.ggplot(p)
  expect_equal(class(g), c("l_ggplot", "loon"))

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
  g <- loon.ggplot(p)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # example 4
  p <- ggplot(data = SAheart, mapping = aes(x = age, y = chd, col = famhist)) +

    geom_smooth(method = "loess", colour = "steelblue") +
    geom_point(size = 3, alpha = 0.4) +
    facet_wrap(~famhist)
  g <- loon.ggplot(p)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # example 5
  p <- ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_wrap(~class)
  g <- loon.ggplot(p)
  expect_equal(length(g), 7)

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
  g <- loon.ggplot(p)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # example 7
  p <- ggplot(economics_long, aes(date, value)) +
    geom_line() +
    facet_wrap(~variable, scales = "free_y", ncol = 1)
  g <- loon.ggplot(p)
  expect_equal(class(g), c("l_ggplot", "loon"))

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
  g <- loon.ggplot(p)
  expect_equal(class(g), c("l_ggplot", "loon"))

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
  g <- loon.ggplot(p)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # example 10
  mod_coef <- coef(lm(log10(price)~ log10(carat), data = diamonds))
  p <- ggplot(diamonds, aes(log10(carat), log10(price))) +
    geom_bin2d() +
    geom_abline(intercept = mod_coef[1], slope = mod_coef[2],
                colour = "white", size = 1) +
    facet_wrap(~cut, nrow = 1)

  g <- loon.ggplot(p)
  expect_equal(length(g), 5)

  # example 11
  mi_counties <- map_data("county", "michigan") %>%
    select(lon = long, lat, group, id = subregion)
  poly <- ggplot(mi_counties, aes(lon, lat)) +
    geom_polygon(aes(group = group)) +
    coord_quickmap()
  g <- loon.ggplot(poly)
  expect_equal(class(g), c("l_ggplot", "loon"))

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
  g <- loon.ggplot(p)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # example 13
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
  pp <- p + geom_abline(intercept = 37, slope = -5) + coord_polar(theta = "x")
  g <- loon.ggplot(pp)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # example 14
  pp <- p + geom_abline(intercept = 10, slope = 2.5) + coord_polar(theta = "y")
  g <- loon.ggplot(pp)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # example 15
  pp <- ggplot(diamonds, aes((carat))) +
    geom_histogram()
  g <- loon.ggplot(pp)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # example 16
  pp <- ggplot(diamonds, aes(price, fill = cut)) +
    geom_histogram(binwidth = 500) + geom_freqpoly()+ facet_wrap(~color)
  g <- loon.ggplot(pp)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # example 17
  mtlong <- reshape2::melt(mtcars)
  pp <- ggplot(mtlong, aes(value)) + facet_wrap(~variable, scales = 'free_x') +
    geom_histogram(binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3)))
  g <- loon.ggplot(pp)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # example 18
  g <- ggplot(mpg, aes(class))
  pp <- g + geom_bar(aes(fill = drv)) +
    geom_bar(aes(fill = drv), position = position_stack(reverse = TRUE)) +
    coord_flip() +
    theme(legend.position = "top")
  gg <- loon.ggplot(pp, ggGuides = TRUE)
  expect_equal(class(gg), c("l_ggplot", "loon"))

  # example 19
  df <- data.frame(trt = c("a", "b", "c"), outcome = c(2.3, 1.9, 3.2))
  p <- ggplot(df, aes(trt, outcome)) +
       geom_col()
  g <- loon.ggplot(p, ggGuides = TRUE)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # example 20
  d <- data.frame(x=c(1,2,4,5,7,8,9), y=c(1,2,3,5,6,7,9))
  pp <- ggplot() +
    geom_step(data=d, mapping=aes(x=x, y=y)) +
    geom_step(data=d, mapping=aes(x=x, y=y), direction="vh", linetype=3) +
    geom_point(data=d, mapping=aes(x=x, y=y), color="red")
  g <- loon.ggplot(pp)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # example 21
  huron <- data.frame(year = 1875:1972, level = as.vector(LakeHuron))
  # Add aesthetic mappings
  pp <- ggplot(huron, aes(year)) +
    geom_ribbon(aes(ymin = level - 1, ymax = level + 1), fill = "grey70", colour = "black") +
    geom_line(aes(y = level))
  g <- loon.ggplot(pp)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # example 22
  df= data.frame(Time=as.numeric(strsplit('1939 1949 1959 1969 1979 1989 1999 2009 2019 2029 2039 2049 1939 1949 1959 1969 1979 1989 1999 2009 2019 2029 2039 2049', split=' ')[[1]] ),
                 Acres=as.numeric(strsplit('139504.2 233529.0 392105.3 502983.9 685159.9 835594.7 882945.1 1212671.4 1475211.9 1717971.7 1862505.7 1934308.0 308261.4 502460.8 834303.1 1115150.7 1430797.8 1712085.8 1973366.1 1694907.7 1480506.0 1280047.6 1164200.5 1118045.3', split=' ')[[1]] ),
                 WUClass= strsplit('DenseUrban DenseUrban DenseUrban DenseUrban DenseUrban DenseUrban DenseUrban DenseUrban DenseUrban DenseUrban DenseUrban DenseUrban Urban Urban Urban Urban Urban Urban Urban Urban Urban Urban Urban Urban', split=' ')[[1]]
  )
  pp <- ggplot(df,aes(x = Time,y = Acres,fill=WUClass)) +
    geom_area( position = 'stack') +
    geom_area( position = 'stack', colour="black", show.legend=FALSE)
  g <- loon.ggplot(pp)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # example 23
  p <- ggplot(data = SAheart, mapping = aes(x = tobacco, y = sbp))
  pp <- p + geom_point() + geom_density_2d(lwd = 1.5, col = "steelblue")
  g <- loon.ggplot(pp)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # example 24
  pp <- ggplot(diamonds, aes(depth, colour = cut)) +
    geom_density(na.rm = TRUE) +
    xlim(55, 70)
  g <- loon.ggplot(pp)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # example 25
  pp <- ggplot(diamonds, aes(carat, fill = cut)) +
    geom_density(position = "stack")
  g <- loon.ggplot(pp)
  expect_equal(class(g), c("l_ggplot", "loon"))

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
  expect_equal(class(loon.ggplot(pp)), c("l_ggplot", "loon"))

  # example 27
  pp <- p + geom_pointrange(aes(ymin = lower, ymax = upper))
  expect_equal(class(loon.ggplot(pp)), c("l_ggplot", "loon"))

  # example 28
  pp <- p + geom_crossbar(aes(ymin = lower, ymax = upper), width = 0.2)
  expect_equal(class(loon.ggplot(pp)), c("l_ggplot", "loon"))

  # example 29
  pp <- p + geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2)
  expect_equal(class(loon.ggplot(pp)), c("l_ggplot", "loon"))

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
  expect_equal(class(loon.ggplot(pp)), c("l_ggplot", "loon"))

  # example 31
  # skip_on_cran()
  # pp <- ggplot(faithfuld, aes(waiting, eruptions)) +
  #   geom_raster(aes(fill = density))
  # expect_equal(class(loon.ggplot(pp)), c("l_ggplot", "loon"))

  # example 32
  pp <- ggplot(mtcars, aes(x = mpg)) + geom_dotplot(binwidth = 1.5)
  expect_equal(class(loon.ggplot(pp)), c("l_ggplot", "loon"))

  # example 33
  p <- ggplot(mpg, aes(class, hwy))
  pp <- p + geom_boxplot()
  expect_equal(class(loon.ggplot(pp, ggGuides = TRUE)), c("l_ggplot", "loon"))

  # # example 34
  # pp <- ggplot(diamonds, aes(carat, price)) +
  #   geom_boxplot(aes(group = cut_width(carat, 0.25)), outlier.alpha = 0.1)
  # expect_equal(class(loon.ggplot(pp)), c("l_ggplot", "loon"))

  # example 35
  p <- ggplot(mtcars, aes(factor(cyl), mpg))
  pp <- p + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))
  expect_equal(class(loon.ggplot(pp)), c("l_ggplot", "loon"))

  # example 36
  df <- data.frame(y = rt(200, df = 5))
  p <- ggplot(df, aes(sample = y))
  pp <- p + stat_qq() + stat_qq_line()
  g <- loon.ggplot(pp)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # example 37
  d <- ggplot(diamonds, aes(x, y)) + xlim(4, 10) + ylim(4, 10)
  pp <- d + geom_bin2d(na.rm = TRUE)
  g <- loon.ggplot(pp)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # example 38
  d <- ggplot(diamonds, aes(carat, price))
  pp <- d + geom_hex()
  g <- loon.ggplot(pp)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # example 39
  v <- ggplot(faithfuld, aes(waiting, eruptions, z = density))
  pp <- v + geom_contour()
  g <- loon.ggplot(pp)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # example 40
  p <- ggplot(mtcars, aes(as.numeric(wt), mpg)) +
    geom_point()
  pp <- p + geom_rug()
  g <- loon.ggplot(pp)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # example 41
  pie <- ggplot(mtcars, aes(x = factor(1), fill = factor(cyl))) +
    geom_bar(width = 1)
  pp <- pie + coord_polar(theta = "x")
  g <- loon.ggplot(pp)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # example 42
  pp <- pie + coord_polar(theta = "y")
  g <- loon.ggplot(pp)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # example 43
  rect <- data.frame(x = 50, y = 50)
  line <- data.frame(x = c(1, 200), y = c(100, 1))
  base <- ggplot(mapping = aes(x, y)) +
    geom_tile(data = rect, aes(width = 50, height = 50)) +
    geom_line(data = line) +
    xlab(NULL) + ylab(NULL)
  pp <- base + coord_polar("x")
  g <- loon.ggplot(pp)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # example 44
  pp <- ggplot(SAheart, aes(obesity, adiposity)) + geom_point() + geom_smooth() +
    coord_polar(theta = "y")
  g <- loon.ggplot(pp)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # example 45
  data(Oats, package = "MEMSS")
  tp1.oats <- xyplot(yield ~ nitro | Variety + Block, data = Oats, type = "o")
  tp1.oats
  pg.oats <- ggplot(Oats, aes(nitro, yield)) +
    geom_line() +
    geom_point() +
    ggtitle("foo") +
    facet_wrap(~Block + Variety, ncol = 3)
  g <- loon.ggplot(pg.oats, linkingGroup = "A")
  expect_equal(class(g), c("l_ggplot", "loon"))

  # example 46
  SAheart %>%
    mutate(ltob = log(tobacco), lsbp = log(sbp)) %>%
    filter(age < 50) %>%
    ggplot(aes(x = ltob, y = lsbp)) +
    geom_point() +
    facet_wrap(~chd) -> p
  g <- loon.ggplot(p)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # example 47
  pp <- ggplot() + geom_histogram(mpg, mapping = aes(x = cty, y = ..density..))
  g <- loon.ggplot(pp)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # example 48
  h <- ggplot(data = SAheart, mapping = aes(x = adiposity)) +
    geom_histogram(mapping = aes(y = ..density..),
                   bins = 10, fill = "steelblue",
                   col = "black", alpha = 0.5) +
    geom_density(mapping = aes(y = ..density..),
                 fill = "grey", alpha = 0.5)
  g <- loon.ggplot(h)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # example 49 theme
  p <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg,
                                       colour = factor(gear))) + facet_wrap(~am)
  g <- loon.ggplot(p, ggGuides = T)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # example 50
  p1 <- p + theme_gray() # the default
  g <- loon.ggplot(p1, ggGuides = T)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # exmaple 51
  p1 <- p + theme_bw()
  g <- loon.ggplot(p1, ggGuides = T)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # exmaple 52
  p1 <- p + theme_linedraw()
  g <- loon.ggplot(p1, ggGuides = T)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # example 53
  p1 <- p + theme_light()
  g <- loon.ggplot(p1, ggGuides = T)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # example 54
  p1 <- p + theme_dark()
  g <- loon.ggplot(p1, ggGuides = T)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # exmaple 55
  p1 <- p + theme_minimal()
  g <- loon.ggplot(p1, ggGuides = T)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # example 56
  p1 <- p + theme_classic()
  g <- loon.ggplot(p1, ggGuides = T)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # example 57
  p1 <- p + theme_void()
  g <- loon.ggplot(p1, ggGuides = T)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # example 58 theme polar cood
  p <- ggplot(mtcars) +
    geom_point(aes(x = wt, y = mpg,
                   colour = factor(gear))) +
    facet_wrap(~am) +
    coord_polar()
  g <- loon.ggplot(p, ggGuides = T)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # example 59
  p1 <- p + theme_gray() # the default
  g <- loon.ggplot(p1, ggGuides = T)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # exmaple 60
  p1 <- p + theme_bw()
  g <- loon.ggplot(p1, ggGuides = T)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # exmaple 61
  p1 <- p + theme_linedraw()
  g <- loon.ggplot(p1, ggGuides = T)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # example 62
  p1 <- p + theme_light()
  g <- loon.ggplot(p1, ggGuides = T)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # example 63
  p1 <- p + theme_dark()
  g <- loon.ggplot(p1, ggGuides = T)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # exmaple 64
  p1 <- p + theme_minimal()
  g <- loon.ggplot(p1, ggGuides = T)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # example 65
  p1 <- p + theme_classic()
  g <- loon.ggplot(p1, ggGuides = T)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # example 66
  p1 <- p + theme_void()
  g <- loon.ggplot(p1, ggGuides = T)
  expect_equal(class(g), c("l_ggplot", "loon"))

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
  g <- loon.ggplot(pp, active_geomLayers = 2)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # example 68
  pp <- ggplot(df, aes(x, y)) +
    geom_point(data = df2, colour = "grey70") +
    geom_point(aes(colour = z)) +
    facet_wrap(~z, scales = "free_x")
  g <- loon.ggplot(pp, active_geomLayers = 1)
  expect_equal(class(g), c("l_ggplot", "loon"))

  # example 69
  pp <- ggplot(df, aes(x, y)) +
    geom_point(data = df2, colour = "grey70") +
    geom_point(aes(colour = z)) +
    facet_wrap(~z, scales = "free_y")
  g <- loon.ggplot(pp, active_geomLayers = c(1,2))
  expect_equal(class(g), c("l_ggplot", "loon"))
})
