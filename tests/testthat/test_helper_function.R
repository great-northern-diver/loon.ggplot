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
})
