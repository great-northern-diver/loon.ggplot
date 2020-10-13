context("test serialaxes (ggplot to loon)")
library(dplyr)
library(magrittr)
library(tidyverse)
library(GGally)
library(ggmulti)

test_that("test serialaxes ggplot 2 loon",
          {
            l_ggplot(iris, mapping = aes(colour = factor(Species))) +
              geom_path(alpha = 0.2) +
              ggmulti::coord_serialaxes() -> p
            q <- loon.ggplot(p)
            expect_true("l_serialaxes" %in% class(q))
            expect_equal(q['axesLayout'], "parallel")

            p$coordinates$axes.layout <- "radial"
            expect_warning(q <- loon.ggplot(p))
            expect_true("l_serialaxes" %in% class(q))
            expect_equal(q['axesLayout'], "radial")

            # TODO
            # l_ggplot(iris, mapping = aes(colour = factor(Species))) +
            #   geom_ribbon(alpha = 0.2) +
            #   coord_serialaxes() -> p
            # q <- loon.ggplot(p)
            # expect_true(q['showArea'])
          })
