test_that("theme_blank works", {
  expect_s3_class({
    ggplot2::ggplot(ggnetwork(emon[[1]]), ggplot2::aes(x, y, xend = xend, yend = yend)) +
      geom_edges() +
      geom_nodes() +
      theme_blank()
  }, class = "ggplot")
})

test_that("theme_facet works", {
  expect_s3_class({
    ggplot2::ggplot(ggnetwork(emon[[1]]), ggplot2::aes(x, y, xend = xend, yend = yend)) +
      geom_edges() +
      geom_nodes() +
      theme_facet()
  }, class = "ggplot")
})
