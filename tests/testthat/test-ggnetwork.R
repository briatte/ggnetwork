test_that("ggnetwork works", {
  expect_error(ggnetwork(-123), "could not coerce")

  # test data
  data(emon, package = "network")
  # with igraph
  n <- igraph::graph_from_adjacency_matrix(as.matrix(emon[[1]]))
  igraph::E(n)$Frequency <- network::get.edge.attribute(emon[[1]], "Frequency")

  #
  # unsupported layouts
  #
  expect_error(ggnetwork(emon[[1]], layout = -999), "not an exported object")
  # with igraph
  expect_error(ggnetwork(n, layout = igraph::foobar), "not an exported object")

  #
  # facet by edge attribute
  #
  expect_s3_class({
    ggnetwork(emon[[1]], arrow.gap = 0.02, by = "Frequency")
  }, class = "data.frame")
  # with igraph
  expect_s3_class({
    ggnetwork(n, arrow.gap = 0.02, by = "Frequency")
  }, class = "data.frame")

  #
  # user-provided layout
  #
  expect_s3_class({
    ggnetwork(emon[[1]], layout = matrix(runif(28), ncol = 2))
  }, class = "data.frame")
  # with igraph
  expect_s3_class({
    ggnetwork(n, layout = matrix(runif(28), ncol = 2))
  }, class = "data.frame")

  #
  # edge weights in layout
  #
  expect_s3_class({
    ggnetwork(emon[[1]], layout = "kamadakawai", weights = "Frequency")
  }, class = "data.frame")
  # with igraph
  expect_s3_class({
    ggnetwork(n, layout = igraph::with_kk(weights = igraph::E(n)$Frequency))
  }, class = "data.frame")

  #
  # duplicated edges warning
  #
  n <- rbind(
    matrix(c(1:2, 2:1), nrow = 2),
    matrix(c(1:2, 2:1), nrow = 2)
  )
  expect_warning(
    ggnetwork(n),
    "duplicated edges"
  )
  # with igraph
  expect_warning(
    ggnetwork(igraph::graph_from_edgelist(n)),
    "duplicated edges"
  )
})
