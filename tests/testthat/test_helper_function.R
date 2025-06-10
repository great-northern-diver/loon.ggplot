context("test helper functions")

pdf(NULL)

############## geom_hist_ and geom_density_ ##############
test_that("test helper functions", {

  ### is.Coord
  x <- coord_flip()
  expect_true(is.CoordFlip(x))

  x <- coord_polar()
  expect_true(is.CoordPolar(x))

  x <- coord_serialaxes()
  expect_true(is.CoordSerialaxes(x))

  ### is.Facet
  p <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  q <- p + facet_wrap(vars(class))
  expect_true(is.FacetWrap(q$facet))
  q <- p + facet_grid(class~am)
  expect_true(is.FacetGrid(q$facet))

  # layout_matrix <- matrix(c(1,NA,3,NA,NA,3,2,2,2), nrow = 3)
  # k <- 3
  # new <- matrix(c(5,5,NA,5,5,NA,6,6,7), nrow = 3)
  # newl <- layout_matrixExtend(layout_matrix, k, new)
  # truel <- matrix(c(rep(1, 3), rep(NA, 3), 5, 5, NA,
  #                   rep(1, 3), rep(NA, 3), 5, 5, NA,
  #                   rep(1, 3), rep(NA, 3), 5, 5, NA,
  #                   rep(NA, 6), 5, 5, NA,
  #                   rep(NA, 6), 6, 6, 7,
  #                   rep(NA, 6), 6, 6, 7,
  #                   rep(2, 9),rep(2, 9),rep(2, 9)), nrow = 9)
  # expect_equal(newl, truel)
})
