context("Test all geoms")

data(emon, package = "network")

test_that("geom_nodes works", {
  ggplot(emon[[1]], aes(x, y)) +
    geom_nodes()

  ggplot(emon[[1]], aes(x, y, xend = xend, yend = yend)) +
    geom_nodes()
})

test_that("geom_nodetext works", {
  ggplot(emon[[1]], aes(x, y, xend = xend, yend = yend)) +
    geom_nodetext(aes(label = vertex.names))

  ggplot(emon[[1]], aes(x, y, xend = xend, yend = yend)) +
    geom_nodetext(aes(label = Paid.Staff), nudge_x = 1, nudge_y = 1)

  expect_error(
    ggplot(emon[[1]], aes(x, y, xend = xend, yend = yend)) +
      geom_nodetext(aes(label = Paid.Staff),
        nudge_x = 1, nudge_y = 1,
        position = "identity"
      ),
    "Specify either"
  )
})

test_that("geom_nodetext_repel works", {
  ggplot(emon[[1]], aes(x, y, xend = xend, yend = yend)) +
    geom_nodetext_repel(aes(label = vertex.names))
})

test_that("geom_nodelabel works", {
  ggplot(emon[[1]], aes(x, y, xend = xend, yend = yend)) +
    geom_nodelabel(aes(label = vertex.names))

  ggplot(emon[[1]], aes(x, y, xend = xend, yend = yend)) +
    geom_nodelabel(aes(label = Paid.Staff), nudge_x = 1, nudge_y = 1)

  expect_error(
    ggplot(emon[[1]], aes(x, y, xend = xend, yend = yend)) +
      geom_nodelabel(aes(label = Paid.Staff),
        nudge_x = 1, nudge_y = 1,
        position = "identity"
      ),
    "Specify either"
  )
})

test_that("geom_nodelabel_repel works", {
  ggplot(emon[[1]], aes(x, y, xend = xend, yend = yend)) +
    geom_nodelabel_repel(aes(label = vertex.names))
})

test_that("geom_edges works", {

  # straight
  ggplot(emon[[1]], aes(x, y, xend = xend, yend = yend)) +
    geom_edges()

  # curved
  ggplot(emon[[1]], aes(x, y, xend = xend, yend = yend)) +
    geom_edges(curvature = 0.1)
})

test_that("geom_edgetext works", {
  ggplot(emon[[1]], aes(x, y, xend = xend, yend = yend)) +
    geom_edgetext(aes(label = Frequency))

  ggplot(emon[[1]], aes(x, y, xend = xend, yend = yend)) +
    geom_edgetext(aes(label = Frequency), nudge_x = 1, nudge_y = 1)

  expect_error(
    ggplot(emon[[1]], aes(x, y, xend = xend, yend = yend)) +
      geom_edgetext(aes(label = Frequency),
        nudge_x = 1, nudge_y = 1,
        position = "identity"
      ),
    "Specify either"
  )
})

test_that("geom_edgelabel works", {
  ggplot(emon[[1]], aes(x, y, xend = xend, yend = yend)) +
    geom_edgelabel(aes(label = Frequency))
})

test_that("geom_edgetext_repel works", {
  ggplot(emon[[1]], aes(x, y, xend = xend, yend = yend)) +
    geom_edgetext_repel(aes(label = Frequency))
})

test_that("geom_edgelabel_repel works", {
  ggplot(emon[[1]], aes(x, y, xend = xend, yend = yend)) +
    geom_edgelabel_repel(aes(label = Frequency))
})
