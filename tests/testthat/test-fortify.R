context("Test ggnetwork")

library(intergraph)
data(flo, package = "network")

library(network)
library(sna)

test_that("fortify.network works", {

  n <- network(flo, directed = FALSE)

  expect_is(fortify(n), "data.frame")
  expect_true(all(c("x", "y", "xend", "yend") %in% names(fortify(n))))

  ggplot(n, aes(x, y, xend = xend, yend = yend)) +
    geom_nodes() +
    geom_edges() +
    geom_nodetext(aes(label = vertex.names)) +
    geom_edgetext(aes(label = 1)) +
    theme_blank()

})

test_that("fortify.igraph works", {

  n <- asIgraph(network(flo, directed = FALSE))

  expect_is(fortify(n), "data.frame")
  expect_true(all(c("x", "y", "xend", "yend") %in% names(fortify(n))))

  ggplot(n, aes(x, y, xend = xend, yend = yend)) +
    geom_edges() +
    geom_nodes() +
    geom_nodetext(aes(label = vertex.names)) +
    geom_edgetext(aes(label = 1)) +
    theme_blank()

})
