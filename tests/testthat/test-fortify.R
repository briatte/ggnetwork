test_that("fortify.network", {
  data(flo, package = "network")
  n <- network::network(flo, directed = FALSE)

  expect_s3_class(fortify(n), "data.frame")
  expect_true(all(c("x", "y", "xend", "yend") %in% names(fortify(n))))

  expect_s3_class({
    ggplot(n, aes(x, y, xend = xend, yend = yend)) +
    geom_nodes() +
    geom_edges() +
    geom_nodetext(aes(label = vertex.names)) +
    geom_edgetext(aes(label = 1)) +
    theme_blank()
  }, class = "ggplot")

  expect_s3_class({
    data(emon, package = "network")
    ggplot(emon[[1]], aes(x = x, y = y, xend = xend, yend = yend)) +
      geom_edges() +
      geom_nodes(color = "tomato", size = 4) +
      theme_blank()
  }, class = "ggplot")
})

test_that("fortify.igraph", {
  n <- igraph::graph_from_adjacency_matrix(flo, mode = "undirected")

  expect_s3_class(fortify(n), "data.frame")
  expect_true(all(c("x", "y", "xend", "yend") %in% names(fortify(n))))

  ggplot(n, aes(x, y, xend = xend, yend = yend)) +
    geom_edges() +
    geom_nodes() +
    geom_nodetext(aes(label = name)) +
    geom_edgetext(aes(label = 1)) +
    theme_blank()
})
