context("geom-nodes")

library(network)
data(emon)

test_that("geom-nodes works", {

  ggplot(ggnetwork(emon[[1]]), aes(x, y, xend = xend, yend = yend)) +
    geom_nodes()

})

test_that("geom-nodetext works", {

  ggplot(ggnetwork(emon[[1]]), aes(x, y, xend = xend, yend = yend)) +
    geom_nodetext(aes(label = vertex.names))

  ggplot(ggnetwork(emon[[1]]), aes(x, y, xend = xend, yend = yend)) +
    geom_nodetext(aes(label = Frequency), nudge_x = 1, nudge_y = 1)

  expect_error(
    ggplot(ggnetwork(emon[[1]]), aes(x, y, xend = xend, yend = yend)) +
      geom_nodetext(aes(label = Frequency), nudge_x = 1, nudge_y = 1, position = "identity")
  )

})

context("geom-edges")

test_that("geom-edges works", {

  ggplot(ggnetwork(emon[[1]]), aes(x, y, xend = xend, yend = yend)) +
    geom_edges()

})

test_that("geom-edgetext works", {

  ggplot(ggnetwork(emon[[1]]), aes(x, y, xend = xend, yend = yend)) +
    geom_edgetext(aes(label = Frequency))

  ggplot(ggnetwork(emon[[1]]), aes(x, y, xend = xend, yend = yend)) +
    geom_edgetext(aes(label = Frequency), nudge_x = 1, nudge_y = 1)

  expect_error(
    ggplot(ggnetwork(emon[[1]]), aes(x, y, xend = xend, yend = yend)) +
      geom_edgetext(aes(label = Frequency), nudge_x = 1, nudge_y = 1, position = "identity")
  )

})
