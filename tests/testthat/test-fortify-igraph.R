# test data
utils::data(flo, package = "network")
n <- igraph::graph_from_adjacency_matrix(flo, mode = "undirected")

test_that("fortify.igraph works", {

  expect_s3_class(fortify(n), "data.frame")
  expect_true(all(c("x", "y", "xend", "yend") %in% names(fortify(n))))

})

# test data
n <- igraph::random.graph.game(n = 1, p.or.m = 0)

# this test also covers the '!scale' part of utilities/`scale_safely` (see #32)
test_that("fortify.igraph works with zero-edge networks", {

  expect_s3_class(fortify(n), "data.frame")
  expect_true(all(c("x", "y", "xend", "yend") %in% names(fortify(n))))

})
