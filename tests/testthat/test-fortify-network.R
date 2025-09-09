# test data
utils::data(flo, package = "network")
n <- network::network(flo, directed = FALSE)

test_that("fortify.network works", {

  expect_s3_class(fortify(n), "data.frame")
  expect_true(all(c("x", "y", "xend", "yend") %in% names(fortify(n))))

})

# test data
# note: `p` used to be `p.or.m` until igraph version 0.8.0
n <- igraph::sample_gnp(n = 1, p = 0)
n <- network::network(igraph::as_adjacency_matrix(n))

# wrong layout matrix
test_that("fortify.network rejects layout matrix of wrong dimensions", {

  expect_error(fortify(n, layout = matrix(1, 9, 9)), "match network size")

})

# this test also covers the '!scale' part of utilities/`scale_safely` (see #32)
test_that("fortify.network works with zero-edge networks", {

  expect_s3_class(fortify(n), "data.frame")
  expect_true(all(c("x", "y", "xend", "yend") %in% names(fortify(n))))

})
