context("test serialaxes (ggplot to loon)")
library(dplyr)
library(magrittr)
library(ggmulti)

pdf(NULL)

test_that("test serialaxes ggplot 2 loon",
          {
            l_ggplot(iris, mapping = aes(colour = factor(Species))) +
              geom_path(alpha = 0.2) +
              ggmulti::coord_serialaxes(axes.sequence = colnames(iris)) -> p
            q <- loon.ggplot(p)
            expect_true("l_serialaxes" %in% class(q))
            expect_equal(q['axesLayout'], "parallel")
            expect_false(q['showItemLabels'])

            p <- p + hover(showItemLabels = TRUE,
                    itemLabel = iris$Species)
            q <- loon.ggplot(p)
            expect_true(q['showItemLabels'])
            expect_equal(q['itemLabel'], as.character(iris$Species))

            p$coordinates$axes.layout <- "radial"
            q <- loon.ggplot(p)
            expect_true("l_serialaxes" %in% class(q))
            expect_equal(q['axesLayout'], "radial")

            l_ggplot(iris, mapping = aes(Sepal.Length = Sepal.Length,
                                         Sepal.Width = Sepal.Width,
                                         Petal.Length = Petal.Length,
                                         Petal.Width = Petal.Width,
                                         colour = factor(Species))) +
              geom_serialaxes(alpha = 0.2) +
              geom_serialaxes_density(alpha = 0.2) +
              geom_serialaxes_hist() +
              geom_serialaxes_quantile(quantiles = c(0.25, 0.5, 0.75),
                                       colour = c("red", "blue", "green"),
                                       linewidth = 4) -> p
            expect_warning(q <- loon.ggplot(p))
            expect_true("l_plot" %in% class(q))

            l <- l_layer_getChildren(q)
            expect_true(length(l) == 5)

            p <- mpg %>%
              dplyr::filter(drv != "f") %>%
              l_ggplot(mapping = aes(x = drv, y = cty, fill = factor(cyl))) +
              geom_density_(alpha = 0.1)
            q <- loon.ggplot(p)
            expect_true("l_plot" %in% class(q))

            p <- mpg %>%
              dplyr::filter(drv != "f") %>%
              l_ggplot(mapping = aes(x = drv, y = cty, fill = factor(cyl))) +
              geom_hist_(alpha = 0.1)
            expect_warning(q <- loon.ggplot(p))
            expect_true("l_plot" %in% class(q))
          })
