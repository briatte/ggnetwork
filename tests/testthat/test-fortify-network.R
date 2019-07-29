# test data
utils::data(flo, package = "network")
n <- network::network(flo, directed = FALSE)

test_that("fortify.network works", {

  expect_s3_class(fortify(n), "data.frame")
  expect_true(all(c("x", "y", "xend", "yend") %in% names(fortify(n))))

})

# test data
n <- igraph::random.graph.game(n = 1, p.or.m = 0)
n <- network::network(igraph::as_adjacency_matrix(n))

# this test also covers the '!scale' part of utilities/`scale_safely` (see #32)
test_that("fortify.network works with zero-edge networks", {

  expect_s3_class(fortify(n), "data.frame")
  expect_true(all(c("x", "y", "xend", "yend") %in% names(fortify(n))))

})
