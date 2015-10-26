context("Test ggnetwork")

library(intergraph)
data(emon, package = "network")

test_that("ggnetwork works", {

  expect_error(ggnetwork(-999, "could not coerce"))
  expect_error(ggnetwork(emon[[1]], layout = -999, "unsupported layout"))

  # facet by edge attribute
  ggnetwork(emon[[1]], arrow.gap = 0.02, by = "Frequency")

  # user-provided layout
  ggnetwork(emon[[1]], layout = matrix(runif(28), ncol = 2))

  # edge weights in layout
  ggnetwork(emon[[1]], layout = "kamadakawai", weights = "Frequency")

  # duplicated edges warning
  ggnetwork(rbind(matrix(c(1:2, 2:1), nrow = 2), matrix(c(1:2, 2:1), nrow = 2)))

})

test_that("intergraph works", {

  ggnetwork(intergraph::asIgraph(emon[[1]]), layout = "circle")

})

context("utilities")

test_that("load_pkg works", {

  expect_error(expect_warning(load_pkg(-999), "no package"), "install the")

})

test_that("theme_blank works", {

  ggplot(ggnetwork(emon[[1]]), aes(x, y, xend = xend, yend = yend)) +
    geom_edges() +
    geom_nodes() +
    theme_blank()

})

test_that("theme_facet works", {

  ggplot(ggnetwork(emon[[1]]), aes(x, y, xend = xend, yend = yend)) +
    geom_edges() +
    geom_nodes() +
    theme_facet()

})
