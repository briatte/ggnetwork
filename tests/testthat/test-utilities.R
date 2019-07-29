# test data
data(emon, package = "network")

test_that("theme_blank works", {

  expect_s3_class({
    ggplot2::ggplot(ggnetwork(emon[[1]]), ggplot2::aes(x, y, xend = xend, yend = yend)) +
      geom_edges() +
      geom_nodes() +
      theme_blank()
  }, class = "ggplot")

})

test_that("theme_facet works", {

  expect_s3_class({
    ggplot2::ggplot(ggnetwork(emon[[1]]), ggplot2::aes(x, y, xend = xend, yend = yend)) +
      geom_edges() +
      geom_nodes() +
      theme_facet()
  }, class = "ggplot")

})

test_that("scale_safely works", {

  # scale
  testthat::expect_equal(scale_safely(1:5), c(0, 0.25, 0.5, 0.75, 1))

  # !scale
  testthat::expect_equal(scale_safely(rep(999, 5)), rep(0.5, 5))

})
