# test data
utils::data(flo, package = "network")
n <- igraph::graph_from_adjacency_matrix(flo, mode = "undirected")

test_that("fortify.igraph works", {

  expect_s3_class(fortify(n), "data.frame")
  expect_true(all(c("x", "y", "xend", "yend") %in% names(fortify(n))))

})

# test data
# note: `p` used to be `p.or.m` until igraph version 0.8.0
n <- igraph::sample_gnp(n = 1, p = 0)

# wrong layout matrix
test_that("fortify.igraph rejects layout matrix of wrong dimensions", {

  expect_error(fortify(n, layout = matrix(1, 9, 9)), "match network size")

})

# this test also covers the '!scale' part of utilities/`scale_safely` (see #32)
test_that("fortify.igraph works with zero-edge networks", {

  expect_s3_class(fortify(n), "data.frame")
  expect_true(all(c("x", "y", "xend", "yend") %in% names(fortify(n))))

})
