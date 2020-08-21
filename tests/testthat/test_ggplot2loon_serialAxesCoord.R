context("test serialaxes (ggplot to loon)")
library(dplyr)
library(magrittr)
library(tidyverse)
library(GGally)

test_that("test serialaxes ggplot",
          {
            # histogram
            ### parallel
            ggplot(iris) +
              geom_path(alpha = 0.2) +
              geom_histogram(alpha = 0.8,
                             mapping = aes(fill = factor(Species))) +
              coord_serialaxes() -> p
            x <- ggplot_build(p)
            expect_equal(length(x$plot$layers), 2)
            expect_true("CoordSerialaxes" %in% class(p$coordinates))
            expect_false("CoordSerialaxes" %in% class(x$coordinates))

            ### radial
            p$coordinates$axesLayout <- "radial"
            x <- ggplot_build(p)
            expect_false("CoordSerialaxes" %in% class(x$coordinates))

            # freqpoly
            ### parallel
            suppressWarnings(
              ggplot(iris) +
                geom_path(alpha = 0.2) +
                geom_freqpoly(alpha = 0.8,
                              mapping = aes(fill = factor(Species))) +
                coord_serialaxes() -> p
            )
            x <- ggplot_build(p)
            expect_equal(length(x$plot$layers), 2)

            ### radial
            p$coordinates$axesLayout <- "radial"
            x <- ggplot_build(p)
            expect_false("CoordSerialaxes" %in% class(x$coordinates))

            # density
            ### parallel
            ggplot(mtcars) +
              geom_path(alpha = 0.2, mapping = aes(colour = factor(cyl))) +
              geom_density(alpha = 0.8,
                           mapping = aes(fill = factor(cyl))) +
              coord_serialaxes() -> p
            x <- ggplot_build(p)
            expect_true("CoordSerialaxes" %in% class(p$coordinates))
            expect_false("CoordSerialaxes" %in% class(x$coordinates))

            ### radial
            p$coordinates$axesLayout <- "radial"
            x <- ggplot_build(p)
            expect_false("CoordSerialaxes" %in% class(x$coordinates))
          })

test_that("test serialaxes ggplot 2 loon",
          {
            l_ggplot(iris, mapping = aes(colour = factor(Species))) +
              geom_path(alpha = 0.2) +
              coord_serialaxes() -> p
            q <- loon.ggplot(p)
            expect_true("l_serialaxes" %in% class(q))
            expect_equal(q['axesLayout'], "parallel")

            p$coordinates$axesLayout <- "radial"
            q <- loon.ggplot(p)
            expect_true("l_serialaxes" %in% class(q))
            expect_equal(q['axesLayout'], "radial")

            l_ggplot(iris, mapping = aes(colour = factor(Species))) +
              geom_ribbon(alpha = 0.2) +
              coord_serialaxes() -> p
            q <- loon.ggplot(p)
            expect_true(q['showArea'])
          })
