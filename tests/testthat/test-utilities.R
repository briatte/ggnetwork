context("Test utilities")

test_that("load_pkg works", {

  expect_error(expect_warning(load_pkg(-999), "no package"), "install the")

})

test_that("theme_blank works", {

  ggplot(ggnetwork(emon[[1]]), aes(x, y, xend = xend, yend = yend)) +
    geom_edges() +
    geom_nodes() +
    theme_blank()

})

test_that("theme_facet works", {

  ggplot(ggnetwork(emon[[1]]), aes(x, y, xend = xend, yend = yend)) +
    geom_edges() +
    geom_nodes() +
    theme_facet()

})
