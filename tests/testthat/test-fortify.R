test_that("fortify.network", {
  utils::data(flo, package = "network")
  n <- network::network(flo, directed = FALSE)

  expect_s3_class(fortify(n), "data.frame")
  expect_true(all(c("x", "y", "xend", "yend") %in% names(fortify(n))))

  expect_s3_class({
    ggplot2::ggplot(n, ggplot2::aes(x, y, xend = xend, yend = yend)) +
      geom_nodes() +
      geom_edges() +
      geom_nodetext(aes(label = vertex.names)) +
      geom_edgetext(aes(label = 1)) +
      theme_blank()
  }, class = "ggplot")

  expect_s3_class({
    utils::data(emon, package = "network")
    ggplot2::ggplot(emon[[1]], ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
      geom_edges() +
      geom_nodes(color = "tomato", size = 4) +
      theme_blank()
  }, class = "ggplot")
})

test_that("fortify.igraph", {
  n <- igraph::graph_from_adjacency_matrix(flo, mode = "undirected")

  expect_s3_class(fortify(n), "data.frame")
  expect_true(all(c("x", "y", "xend", "yend") %in% names(fortify(n))))

  expect_s3_class({
    ggplot2::ggplot(n, ggplot2::aes(x, y, xend = xend, yend = yend)) +
      geom_edges() +
      geom_nodes() +
      geom_nodetext(aes(label = name)) +
      geom_edgetext(aes(label = 1)) +
      theme_blank()
  }, class = "ggplot")
})

# next test should also test the 'no scaling' part of `scale_safely` (see #32)
test_that("zero-edge networks fortify alright", {
  # with `igraph`
  n <- igraph::random.graph.game(n = 1, p.or.m = 0)

  expect_s3_class(fortify(n), "data.frame")
  expect_true(all(c("x", "y", "xend", "yend") %in% names(fortify(n))))

  expect_s3_class({
    ggplot2::ggplot(n, ggplot2::aes(x, y, xend = xend, yend = yend)) +
      geom_edges() +
      geom_nodes() +
      theme_blank()
  }, class = "ggplot")

  # with `network`
  n <- network::network(igraph::as_adjacency_matrix(n))

  expect_s3_class(fortify(n), "data.frame")
  expect_true(all(c("x", "y", "xend", "yend") %in% names(fortify(n))))

  expect_s3_class({
    ggplot2::ggplot(n, ggplot2::aes(x, y, xend = xend, yend = yend)) +
      geom_edges() +
      geom_nodes() +
      theme_blank()
  }, class = "ggplot")
})
