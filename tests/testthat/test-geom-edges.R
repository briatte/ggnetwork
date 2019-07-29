# test data
data(emon, package = "network")

test_that("geom_edges works", {

  # straight edges
  expect_s3_class({
    ggplot(emon[[1]], aes(x, y, xend = xend, yend = yend)) +
      geom_edges()
  }, class = "ggplot")

  # curved edges
  expect_s3_class({
    ggplot(emon[[1]], aes(x, y, xend = xend, yend = yend)) +
      geom_edges(curvature = 0.1)
  }, class = "ggplot")

})

test_that("geom_edgetext works", {

  expect_s3_class({
    ggplot2::ggplot(emon[[1]], ggplot2::aes(x, y, xend = xend, yend = yend)) +
      geom_edgetext(aes(label = Frequency))
  }, class = "ggplot")

  expect_s3_class({
    ggplot2::ggplot(emon[[1]], ggplot2::aes(x, y, xend = xend, yend = yend)) +
      geom_edgetext(aes(label = Frequency), nudge_x = 1, nudge_y = 1)
  }, class = "ggplot")

  expect_error(
    ggplot2::ggplot(emon[[1]], ggplot2::aes(x, y, xend = xend, yend = yend)) +
      geom_edgetext(aes(label = Frequency),
        nudge_x = 1, nudge_y = 1,
        position = "identity"
      ),
    "Specify either"
  )

})

test_that("geom_edgelabel works", {

  expect_s3_class({
    ggplot2::ggplot(emon[[1]], ggplot2::aes(x, y, xend = xend, yend = yend)) +
      geom_edgelabel(aes(label = Frequency))
  }, class = "ggplot")

})

test_that("geom_edgetext_repel works", {

  expect_s3_class({
    ggplot2::ggplot(emon[[1]], ggplot2::aes(x, y, xend = xend, yend = yend)) +
      geom_edgetext_repel(aes(label = Frequency))
  }, class = "ggplot")

})

test_that("geom_edgelabel_repel works", {

  expect_s3_class({
    ggplot2::ggplot(emon[[1]], ggplot2::aes(x, y, xend = xend, yend = yend)) +
      geom_edgelabel_repel(aes(label = Frequency))
  }, class = "ggplot")

})
